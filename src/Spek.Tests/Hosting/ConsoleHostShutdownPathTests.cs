using Spek.Hosting.Console;
using Spek.Runtime;
using Xunit;

namespace Spek.Tests.Hosting;

/// <summary>
/// Coverage for the <see cref="SpekConsoleHost"/> paths the original
/// integration tests leave dark: the system-initiated shutdown hook
/// (<see cref="ActorSystem.RequestShutdown"/> — the same funnel Ctrl+C /
/// SIGTERM feed), the Option-D exit-code round trip through the host's
/// internal receiver, the bounded grace window when the entry actor
/// refuses to stop, and the generic <c>RunAsync&lt;TActor&gt;</c>
/// overload that owns its system. Real OS signals stay untested by
/// design — firing them would race the test runner — but every signal
/// handler is a one-line call into the exact
/// <c>RequestShutdown</c> plumbing exercised here.
/// </summary>
public sealed class ConsoleHostShutdownPathTests
{
    public sealed record ShutdownSignal();

    /// <summary>Replies with exit code 42 (Option D) and stops.</summary>
    private sealed class GracefulActor : ActorBase
    {
        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            if (message is ShutdownSignal)
            {
                _currentSender.Tell(42);
                StopSelf();
            }
            return Task.CompletedTask;
        }
    }

    /// <summary>Ignores everything, never stops.</summary>
    private sealed class StubbornActor : ActorBase
    {
        protected override Task DispatchAsync(object message, ActorRef sender)
            => Task.CompletedTask;
    }

    [Fact]
    public async Task RequestShutdown_TellsEntryActor_AndSurfacesOptionDExitCodeAsync()
    {
        using var system = new ActorSystem("console-request-shutdown");
        var entry = system.Spawn<GracefulActor>();

        var hostTask = SpekConsoleHost.RunAsync(
            system, entry,
            shutdownFactory: () => new ShutdownSignal(),
            shutdownGrace: TimeSpan.FromSeconds(5),
            defaultExitCode: 0);

        // The host registers its shutdown handler synchronously inside
        // RunAsync, so an actor-initiated `self.System.Shutdown()`
        // (RequestShutdown) funnels through the same Tell-with-receiver
        // path the signal handlers use.
        system.RequestShutdown();

        var exitCode = await hostTask.WaitAsync(TimeSpan.FromSeconds(10));

        Assert.Equal(42, exitCode);   // Option D reply captured by the receiver
        Assert.True(entry.IsStopped);
    }

    [Fact]
    public async Task GraceWindowExpiry_HardExitsWithDefaultCode_WhenActorRefusesToStopAsync()
    {
        using var system = new ActorSystem("console-grace-expiry");
        var entry = system.Spawn<StubbornActor>();

        var hostTask = SpekConsoleHost.RunAsync(
            system, entry,
            shutdownFactory: () => new ShutdownSignal(),
            shutdownGrace: TimeSpan.FromMilliseconds(300),
            defaultExitCode: 91);

        system.RequestShutdown();

        // The wait loop must give up after the grace window rather than
        // hanging on the still-running actor.
        var exitCode = await hostTask.WaitAsync(TimeSpan.FromSeconds(10));

        Assert.Equal(91, exitCode);
        Assert.False(entry.IsStopped);
    }

    // ─── Generic overload: the host owns system + entry actor ──────────────

    private static TaskCompletionSource<ActorSystemHandle> _handleReady = new();

    /// <summary>
    /// Entry actor for the generic overload. The host creates the
    /// ActorSystem internally, so the actor smuggles its
    /// <see cref="ActorSystemHandle"/> out through a static TCS — the
    /// test then drives shutdown exactly the way user code would
    /// (<c>self.System.Shutdown()</c>).
    /// </summary>
    private sealed class EntryWithHandleActor : ActorBase
    {
        protected override void OnPreStart() => _handleReady.TrySetResult(SpekSystem);

        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            if (message is ShutdownSignal)
            {
                _currentSender.Tell(17);
                StopSelf();
            }
            return Task.CompletedTask;
        }
    }

    [Fact]
    public async Task GenericOverload_OwnsSystem_ActorInitiatedShutdown_ReturnsTypedExitCodeAsync()
    {
        _handleReady = new TaskCompletionSource<ActorSystemHandle>(
            TaskCreationOptions.RunContinuationsAsynchronously);

        var hostTask = SpekConsoleHost.RunAsync<EntryWithHandleActor>(
            shutdownFactory: () => new ShutdownSignal(),
            systemName: "console-generic-overload",
            shutdownGrace: TimeSpan.FromSeconds(5),
            defaultExitCode: 0);

        var handle = await _handleReady.Task.WaitAsync(TimeSpan.FromSeconds(5));
        handle.Shutdown();   // what `self.System.Shutdown()` compiles to

        var exitCode = await hostTask.WaitAsync(TimeSpan.FromSeconds(10));

        Assert.Equal(17, exitCode);
    }
}
