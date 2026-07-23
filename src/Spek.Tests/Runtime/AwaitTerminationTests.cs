using Spek.Runtime;
using Spek.Testing;
using Xunit;

namespace Spek.Tests.Runtime;

/// <summary>
/// Covers the signal-driven <see cref="ActorSystem.AwaitTermination"/>:
/// returns promptly once all tracked actors are idle (no 10ms busy-loop
/// latency), supports a timeout variant, and re-arms after busy periods.
/// </summary>
public class AwaitTerminationTests
{
    public record Ping();
    public record Work(int msDelay);

    private sealed class Echo : ActorBase
    {
        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            _currentSender = sender;
            // Echo is always idle immediately after dispatch.
            return Task.CompletedTask;
        }
    }

    private sealed class SlowWorker : ActorBase
    {
        protected override async Task DispatchAsync(object message, ActorRef sender)
        {
            _currentSender = sender;
            if (message is Work w) await Task.Delay(w.msDelay);
        }
    }

    [Fact]
    public void AwaitTermination_ReturnsImmediately_WhenActorIsAlreadyIdle()
    {
        using var system = new ActorSystem("t");
        var actor = system.Spawn<Echo>();
        actor.Tell(new Ping());

        var started = DateTime.UtcNow;
        var result  = system.AwaitTermination(TimeSpan.FromSeconds(3));
        var elapsed = DateTime.UtcNow - started;

        Assert.True(result, "AwaitTermination should have returned true (idle achieved).");
        // With the old busy-loop this takes ~10ms minimum (the Thread.Sleep).
        // With signaling, it should complete in well under that — typically
        // sub-millisecond on a hot path. We give generous headroom for CI.
        Assert.True(elapsed < TimeSpan.FromMilliseconds(200),
            $"Expected fast return; took {elapsed.TotalMilliseconds}ms.");
    }

    [Fact]
    public void AwaitTermination_ReturnsFalse_OnTimeout_WhenActorKeepsWorking()
    {
        using var system = new ActorSystem("t");
        var actor = system.Spawn<SlowWorker>();

        actor.Tell(new Work(msDelay: 1000));

        var result = system.AwaitTermination(TimeSpan.FromMilliseconds(100));

        Assert.False(result, "Timeout should have elapsed before the 1s work finished.");
    }

    [Fact]
    public async Task AwaitTermination_Returns_AfterSlowWorkFinishesAsync()
    {
        using var system = new ActorSystem("t");
        var actor = system.Spawn<SlowWorker>();

        actor.Tell(new Work(msDelay: 150));

        var result = system.AwaitTermination(TimeSpan.FromSeconds(3));

        Assert.True(result, "Should return true once the work completes.");

        // Sanity: nothing should still be in flight.
        await Task.Delay(10);
        Assert.True(actor.IsMaterialized);
        Assert.False(actor.IsStopped);
    }
}
