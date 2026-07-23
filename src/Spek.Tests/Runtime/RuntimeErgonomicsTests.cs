using Spek.Runtime;
using Xunit;

namespace Spek.Tests.Runtime;

/// <summary>
/// Runtime paper-cuts from the codebase re-review:
/// <list type="bullet">
///   <item><see cref="ActorRef.AskAsync{TResponse}(object, TimeSpan)"/> — an
///     ask with a deadline faults with <see cref="TimeoutException"/> instead
///     of hanging forever on a reply-less target.</item>
///   <item><see cref="ActorSystem.GracefulShutdown"/> — drain + dispose in one
///     call, replacing the documented <c>AwaitTermination(); Dispose();</c>
///     two-step; safe on a system that never spawned (no infinite wait).</item>
/// </list>
/// </summary>
public class RuntimeErgonomicsTests
{
    public record Ping();
    public record Pong();

    private sealed class Replier : ActorBase
    {
        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            _currentSender = sender;
            sender.Tell(new Pong());
            return Task.CompletedTask;
        }
    }

    private sealed class BlackHole : ActorBase
    {
        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            _currentSender = sender;
            // Never replies — the pathological ask target.
            return Task.CompletedTask;
        }
    }

    [Fact]
    public async Task AskWithTimeout_RepliesInTime_ReturnsReply()
    {
        using var system = new ActorSystem("t");
        var actor = system.Spawn<Replier>();

        var reply = await actor.AskAsync<Pong>(new Ping(), TimeSpan.FromSeconds(3));

        Assert.IsType<Pong>(reply);
    }

    [Fact]
    public async Task AskWithTimeout_NoReply_FaultsWithTimeoutException()
    {
        using var system = new ActorSystem("t");
        var actor = system.Spawn<BlackHole>();

        await Assert.ThrowsAsync<TimeoutException>(() =>
            actor.AskAsync<Pong>(new Ping(), TimeSpan.FromMilliseconds(100)));
    }

    [Fact]
    public void GracefulShutdown_DrainsThenDisposes()
    {
        var system = new ActorSystem("t");
        var actor = system.Spawn<Replier>();
        actor.Tell(new Ping());

        var drained = system.GracefulShutdown(TimeSpan.FromSeconds(3));

        Assert.True(drained, "System should have drained before the timeout.");
        // Dispose released the slots (it does not Stop actors — supervision
        // owns stopping); a second dispose must be harmless.
        system.Dispose();
    }

    [Fact]
    public void GracefulShutdown_EmptySystem_ReturnsWithoutWaiting()
    {
        // AwaitTermination on a system with no spawns blocks forever by
        // contract; the convenience wrapper must skip the wait instead.
        var system = new ActorSystem("t");

        var started = DateTime.UtcNow;
        var drained = system.GracefulShutdown();   // no timeout — would hang without the guard
        var elapsed = DateTime.UtcNow - started;

        Assert.True(drained);
        Assert.True(elapsed < TimeSpan.FromSeconds(1),
            $"Empty-system shutdown should be immediate; took {elapsed.TotalMilliseconds}ms.");
    }
}
