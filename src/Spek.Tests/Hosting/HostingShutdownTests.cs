using System.Diagnostics;
using Spek.Hosting.AspNetCore;
using Spek.Hosting.Console;
using Spek.Runtime;
using Xunit;

namespace Spek.Tests.Hosting;

/// <summary>
/// Focused coverage for the hosting adapters' shutdown plumbing —
/// specifically the two paths the broader <see cref="AspNetCoreHostTests"/>
/// and <see cref="ConsoleHostTests"/> don't exercise:
///
/// 1. <see cref="SpekHostedService{TActor}.StopAsync"/> honoring an
///    *external* cancellation token. The existing grace test always
///    passes <see cref="CancellationToken.None"/>, so the
///    <c>!cancellationToken.IsCancellationRequested</c> branch of the
///    wait loop is otherwise untested. In production this token carries
///    <c>HostOptions.ShutdownTimeout</c> — the Generic Host cancels it
///    when its own shutdown budget expires, and StopAsync must
///    short-circuit even though the adapter's own grace window is far
///    longer.
///
/// 2. The Console host's exit-code round-trip driven through the
///    <c>OnShutdownRequested</c> path (the same path Ctrl+C / SIGTERM
///    use), rather than via a side-channel <c>Tell</c>. An entry actor
///    replies a typed code and stops; <see cref="SpekConsoleHost.RunAsync"/>
///    surfaces that code through its internal receiver.
/// </summary>
public class HostingShutdownTests
{
    public sealed record Shutdown();

    // ---------------------------------------------------------------
    // Scenario 1 — StopAsync external cancellation token bridge.
    // ---------------------------------------------------------------

    /// <summary>An actor that never stops itself and never replies —
    /// so the only thing that can end StopAsync's wait loop is the
    /// grace deadline or the cancellation token.</summary>
    private sealed class UnstoppableActor : ActorBase
    {
        protected override Task DispatchAsync(object message, ActorRef sender)
            => Task.CompletedTask; // deliberately ignores Shutdown
    }

    [Fact]
    public async Task StopAsync_HonorsExternalCancellationToken_ShortCircuitsLongGraceAsync()
    {
        // Adapter grace is huge (the host's ShutdownTimeout bridge is the
        // *real* bound here). We construct the hosted service directly so we
        // can hand StopAsync a token we control — the DI/host path only ever
        // feeds it CancellationToken.None.
        await using var svc = new SpekHostedService<UnstoppableActor>(
            shutdownFactory: () => new Shutdown(),
            shutdownGrace:   TimeSpan.FromSeconds(60));

        await svc.StartAsync(CancellationToken.None);

        // Token mimics HostOptions.ShutdownTimeout: cancelled shortly after
        // shutdown begins. The wait loop's `!cancellationToken.IsCancellationRequested`
        // guard must break the loop long before the 60s grace deadline.
        using var cts = new CancellationTokenSource(TimeSpan.FromMilliseconds(50));

        var sw = Stopwatch.StartNew();
        await svc.StopAsync(cts.Token);
        sw.Stop();

        // Must return via the cancellation branch, not the 60s grace. A
        // generous ceiling (10s) tolerates a CPU-saturated parallel run
        // where the 50ms `Task.Delay` continuations stack up, while still
        // being unmistakably shorter than the 60s grace window. If the
        // token branch were broken, StopAsync would block the full 60s.
        Assert.True(sw.Elapsed < TimeSpan.FromSeconds(10),
            $"StopAsync should short-circuit on the cancelled token, but took {sw.Elapsed} " +
            "(grace window was 60s — it ignored the token).");
    }

    [Fact]
    public async Task StopAsync_NoneToken_StillBoundedByGrace_ForContrastAsync()
    {
        // Contrast case: same unstoppable actor, NO external cancellation,
        // a short grace. Proves the loop's grace deadline is the bound when
        // the token never fires — so scenario 1 above is genuinely measuring
        // the token branch, not the grace branch.
        await using var svc = new SpekHostedService<UnstoppableActor>(
            shutdownFactory: () => new Shutdown(),
            shutdownGrace:   TimeSpan.FromMilliseconds(200));

        await svc.StartAsync(CancellationToken.None);

        var sw = Stopwatch.StartNew();
        await svc.StopAsync(CancellationToken.None);
        sw.Stop();

        // Bounded (not hung). Wide ceiling for parallel-load tolerance, same
        // rationale as AspNetCoreHostTests.ShutdownGrace_BoundsTheStopWait.
        Assert.True(sw.Elapsed < TimeSpan.FromSeconds(60),
            $"StopAsync should return via the grace deadline, but took {sw.Elapsed}.");
    }

    // ---------------------------------------------------------------
    // Scenario 2 — Console host exit-code round-trip via OnShutdownRequested.
    // ---------------------------------------------------------------

    /// <summary>Entry actor that, on <see cref="Shutdown"/>, replies a
    /// typed exit code to its sender (Spek's Option-D reply convention)
    /// and stops itself.</summary>
    private sealed class ExitingEntryActor : ActorBase
    {
        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            if (message is Shutdown)
            {
                _currentSender.Tell(42);   // Option-D return → exit code 42
                StopSelf();
            }
            return Task.CompletedTask;
        }
    }

    [Fact]
    public async Task ConsoleHost_ExitCodeRoundTrip_ViaOnShutdownRequestedPathAsync()
    {
        // Drive the FULL signal flow without OS signals: RunAsync wires
        // system.OnShutdownRequested(RequestShutdown) internally; calling
        // system.RequestShutdown() invokes that same handler, which Tells
        // the entry actor the Shutdown message with the host's internal
        // receiver as the apparent sender. The actor replies 42, the
        // receiver captures it, and RunAsync returns 42 — NOT defaultExitCode.
        using var system = new ActorSystem("console-host-shutdown-roundtrip");
        var entry        = system.Spawn<ExitingEntryActor>();

        var hostTask = SpekConsoleHost.RunAsync(
            system, entry,
            shutdownFactory: () => new Shutdown(),
            shutdownGrace:   TimeSpan.FromSeconds(5),
            defaultExitCode: 7);

        // Wait until RunAsync has registered its OnShutdownRequested handler
        // (it does so synchronously inside RunAsync, but RunAsync only starts
        // running after the first await yields). Poll with a deadline rather
        // than a fixed sleep so the test is robust under load.
        var armed   = false;
        var deadline = DateTime.UtcNow + TimeSpan.FromSeconds(5);
        while (!armed && DateTime.UtcNow < deadline)
        {
            await Task.Delay(25);
            // RequestShutdown is idempotent; we only fire it once below, so
            // here we just give RunAsync time to start. We can't directly
            // observe the handler being set, so a short bounded settle is the
            // best signal — but keep it generous.
            armed = !entry.IsStopped; // entry is alive ⇒ RunAsync's loop is live
            if (armed) break;
        }

        // Trigger the host shutdown through the production path.
        system.RequestShutdown();

        // RunAsync returns once the entry actor stops (or grace expires).
        var exitCode = await hostTask;

        Assert.True(entry.IsStopped,
            "Entry actor should have stopped after handling Shutdown via the OnShutdownRequested path.");
        Assert.Equal(42, exitCode);   // round-tripped Option-D reply, NOT defaultExitCode (7)
    }
}

