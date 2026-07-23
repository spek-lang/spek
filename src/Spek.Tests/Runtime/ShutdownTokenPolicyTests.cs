using System.Diagnostics;
using Spek.Runtime;
using Xunit;

namespace Spek.Tests.Runtime;

/// <summary>
/// The ShutdownToken cancel POLICY: the token fires only when shutdown
/// turns forceful (a graceful drain that times out), not while a clean drain is
/// letting in-flight work finish. Exercises the full chain — a handler awaiting
/// on ShutdownToken actually unwinds when a wedged drain times out.
/// </summary>
public class ShutdownTokenPolicyTests
{
    public sealed record Begin();

    /// Awaits a long delay bound to the actor's ShutdownToken (what the emitter
    /// would thread for you). Signals when it has entered the delay.
    private sealed class WedgedActor(TaskCompletionSource started) : ActorBase
    {
        protected override async Task DispatchAsync(object message, ActorRef sender)
        {
            started.TrySetResult();
            await Task.Delay(TimeSpan.FromSeconds(30), ShutdownToken);
        }
    }

    /// A fast actor that finishes its message immediately (clean drain).
    private sealed class FastActor : ActorBase
    {
        protected override Task DispatchAsync(object message, ActorRef sender) => Task.CompletedTask;
    }

    [Fact]
    public async Task DrainTimeout_TurnsForceful_CancelsToken_AndWedgedHandlerUnwinds()
    {
        var started = new TaskCompletionSource();
        var system  = new ActorSystem("t");
        var actor   = system.Spawn<WedgedActor>(started);
        var token   = system.ShutdownToken;

        actor.Tell(new Begin());
        // Generous: the handler's first dispatch can be starved for seconds under
        // the fully parallel suite before it enters the delay.
        await started.Task.WaitAsync(TimeSpan.FromSeconds(30));
        Assert.False(token.IsCancellationRequested);             // not cancelled while draining

        var sw = Stopwatch.StartNew();
        var drained = system.GracefulShutdown(TimeSpan.FromMilliseconds(300));
        sw.Stop();

        Assert.False(drained, "the wedged actor can't go idle, so the drain must time out");
        Assert.True(token.IsCancellationRequested, "a timed-out drain turns forceful → token cancels");
        // Well under the 30s delay (proves the token unwound it); generous ceiling
        // for teardown under heavy parallel load.
        Assert.True(sw.ElapsedMilliseconds < 20_000, "must not wait out the 30s delay — the token unwound it");
    }

    [Fact]
    public void CleanDrain_ReturnsTrue_AndDoesNotTimeOut()
    {
        var system = new ActorSystem("t");
        var actor  = system.Spawn<FastActor>();
        actor.Tell(new Begin());

        var sw = Stopwatch.StartNew();
        var drained = system.GracefulShutdown(TimeSpan.FromSeconds(5));
        sw.Stop();

        Assert.True(drained, "a fast actor drains cleanly within the window");
        Assert.True(sw.ElapsedMilliseconds < 5_000, "clean drain returns promptly, not at the timeout");
    }
}
