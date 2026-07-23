using Spek.Runtime;
using Xunit;

namespace Spek.Tests.Runtime;

/// <summary>
/// Edges of the shared ask-deadline sweeper (perf r11) that the existing
/// timeout tests don't reach: the park/re-arm cycle of its single timer,
/// deadline-vs-reply races under concurrency, and the dispose contract —
/// a system that can no longer produce replies fails its pending deadline
/// asks immediately instead of letting them run out their windows.
/// </summary>
public class AskDeadlineSweeperTests
{
    public record Ping();
    public record Pong();

    private sealed class Wedged : ActorBase
    {
        // Handler never returns — a genuinely hung dispatch. Fail-fast can't
        // resolve an ask parked on it (its no-reply check runs only after the
        // handler returns), so the deadline sweeper is the sole backstop. A
        // handler that merely returned without replying would now fail the ask
        // fast with an AskException, bypassing the sweeper entirely.
        protected override Task DispatchAsync(object message, ActorRef sender) =>
            new TaskCompletionSource<Pong>().Task;   // never completes
    }

    private sealed class Echo : ActorBase
    {
        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            _currentSender = sender;
            sender.Tell(new Pong());
            return Task.CompletedTask;
        }
    }

    [Fact]
    public async Task TimerReParks_AskAfterAnIdlePeriod_StillTimesOutAsync()
    {
        // First timeout drains the queue and parks the sweep timer; a later
        // ask must re-arm it. A stranded second ask would hang forever.
        // Wedged (never-returning) handler: fail-fast can't resolve these, so
        // only the deadline unblocks them — exactly what re-arming protects.
        using var system = new ActorSystem("sweeper-repark");
        var hole = system.Spawn<Wedged>();

        await Assert.ThrowsAsync<TimeoutException>(() =>
            hole.AskAsync<Pong>(new Ping(), TimeSpan.FromMilliseconds(50)).AsTask());

        await Task.Delay(300);   // give the sweeper time to drain and park

        await Assert.ThrowsAsync<TimeoutException>(() =>
            hole.AskAsync<Pong>(new Ping(), TimeSpan.FromMilliseconds(50)).AsTask());
    }

    [Fact]
    public async Task ConcurrentTimeoutAsks_RepliesAlwaysBeatGenerousDeadlinesAsync()
    {
        // 32 concurrent deadline asks against a replying actor: every reply
        // must win its race with the armed deadline — no spurious timeouts,
        // no cross-completion.
        using var system = new ActorSystem("sweeper-race");
        var echo = system.Spawn<Echo>();

        var asks = new Task<Pong>[32];
        for (var i = 0; i < asks.Length; i++)
            asks[i] = echo.AskAsync<Pong>(new Ping(), TimeSpan.FromSeconds(10)).AsTask();

        var all = await Task.WhenAll(asks);
        Assert.All(all, Assert.NotNull);
    }

    [Fact]
    public async Task Dispose_FailsPendingDeadlineAsks_LongBeforeTheirWindowsAsync()
    {
        // After Dispose the system can produce no more replies, so a pending
        // deadline ask waiting out a 60s window could only delay the caller's
        // failure. The sweeper fails them at disposal instead. The wedged
        // (never-returning) handler keeps the ask genuinely pending — a
        // handler that returned without replying would fail fast on its own,
        // never reaching the dispose path this test exercises.
        var system = new ActorSystem("sweeper-dispose");
        var hole = system.Spawn<Wedged>();

        var pending = hole.AskAsync<Pong>(new Ping(), TimeSpan.FromSeconds(60)).AsTask();
        system.Dispose();

        // Dispose fails the cell synchronously, so the task must fault at
        // once — far inside the 10s ceiling, nowhere near the 60s window.
        // (Status-polled rather than awaited: the task was created outside
        // this context, and VSTHRD003 objects to awaiting those.)
        var sw = System.Diagnostics.Stopwatch.StartNew();
        while (!pending.IsCompleted && sw.Elapsed < TimeSpan.FromSeconds(10))
            await Task.Delay(10);
        Assert.True(pending.IsFaulted,
            $"pending ask had not failed {sw.Elapsed} after Dispose");
        Assert.IsType<TimeoutException>(pending.Exception!.InnerException);
    }
}
