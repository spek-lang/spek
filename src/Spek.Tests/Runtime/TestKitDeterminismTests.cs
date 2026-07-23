using Spek.Runtime;
using Spek.Testing;
using Xunit;

namespace Spek.Tests.Runtime;

/// <summary>
/// Determinism helpers on <see cref="TestActorSystem"/> that replace the
/// Thread.Sleep(N)-and-hope pattern: wait on an observable condition, fail with
/// a diagnosable timeout if it never holds.
/// </summary>
public class TestKitDeterminismTests
{
    public record Work(int msDelay);

    // Observable completion via a static flag — ActorBase.Underlying is
    // runtime-internal, so the test can't reach the instance; a static is the
    // simplest cross-thread signal for a poll helper to watch.
    private static volatile bool _workDone;

    private sealed class SlowWorker : ActorBase
    {
        protected override async Task DispatchAsync(object message, ActorRef sender)
        {
            _currentSender = sender;
            if (message is Work w) { await Task.Delay(w.msDelay); _workDone = true; }
        }
    }

    [Fact]
    public void WaitForIdle_ReturnsTrue_OnceTheWorkDrains()
    {
        using var system = new TestActorSystem("idle");
        var actor = system.Spawn<SlowWorker>();
        actor.Tell(new Work(120));

        Assert.True(system.WaitForIdle(TimeSpan.FromSeconds(3)),
            "WaitForIdle should observe the system drain");
    }

    [Fact]
    public void WaitForIdle_ReturnsFalse_WhenWorkOutlastsTimeout()
    {
        using var system = new TestActorSystem("busy");
        var actor = system.Spawn<SlowWorker>();
        actor.Tell(new Work(1500));

        Assert.False(system.WaitForIdle(TimeSpan.FromMilliseconds(100)),
            "WaitForIdle should report a timeout when work outlasts it");
    }

    [Fact]
    public async Task WaitUntilAsync_ReturnsOnceConditionHolds()
    {
        _workDone = false;
        using var system = new TestActorSystem("until");
        var actor = system.Spawn<SlowWorker>();
        actor.Tell(new Work(80));

        await TestActorSystem.WaitUntilAsync(
            () => _workDone, TimeSpan.FromSeconds(3), "worker finished");

        Assert.True(_workDone);
    }

    [Fact]
    public async Task WaitUntilAsync_ThrowsDescriptiveTimeout()
    {
        var ex = await Assert.ThrowsAsync<TimeoutException>(() =>
            TestActorSystem.WaitUntilAsync(
                () => false, TimeSpan.FromMilliseconds(50), "never happens"));
        Assert.Contains("never happens", ex.Message);
    }
}
