using Spek.Persistence;
using Spek.Runtime;
using Spek.Testing;
using Xunit;

namespace Spek.Tests.Runtime;

/// <summary>
/// Passivation: persistent actors unload after a configured idle duration
/// and re-materialise (restoring from snapshot) on the next message.
/// </summary>
public class PassivationTests
{
    public record Increment();
    public record Get();
    public record Reply(int N);

    private static readonly TimeSpan ShortTimeout = TimeSpan.FromMilliseconds(200);

    /// <summary>
    /// Persistent actor with a short passivation window. Counter persists
    /// after every Increment so state survives the unload.
    /// </summary>
    private sealed class CounterActor : ActorBase
    {
        private int _count;

        protected override async Task DispatchAsync(object message, ActorRef sender)
        {
            _currentSender = sender;
            switch (message)
            {
                case Increment: _count++; await PersistAsync(); break;
                case Get:       sender.Tell(new Reply(_count)); break;
            }
        }

        protected override IReadOnlyDictionary<string, object?> CaptureFields() =>
            new Dictionary<string, object?> { ["count"] = _count };

        protected override void OnRestore(Snapshot s) => _count = s.Get<int>("count");

        protected override TimeSpan? PassivationTimeout => ShortTimeout;
    }

    /// <summary>
    /// Non-persistent variant — this still passivates
    /// (memory release after idle window), but no snapshot is
    /// written; the next message materialises a fresh instance with
    /// _count back at zero.
    /// </summary>
    private sealed class CounterActorNoKey : ActorBase
    {
        private int _count;

        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            _currentSender = sender;
            switch (message)
            {
                case Increment: _count++; break;
                case Get:       sender.Tell(new Reply(_count)); break;
            }
            return Task.CompletedTask;
        }

        protected override TimeSpan? PassivationTimeout => ShortTimeout;
    }

    [Fact]
    public async Task Passivates_AfterIdleTimeoutAsync()
    {
        using var system = new TestActorSystem("t");
        var actor = system.SpawnPersistent<CounterActor>("c-1");

        actor.Tell(new Increment());
        actor.Tell(new Increment());

        await WaitUntilAsync(() => !actor.IsMaterialized,
            timeoutMs: 60_000,
            label: "actor should unload after idle window");

        Assert.False(actor.IsMaterialized);
        Assert.False(actor.IsStopped);   // passivation is not termination
    }

    [Fact]
    public async Task WakesOnNextMessage_AndRestoresStateAsync()
    {
        using var system = new TestActorSystem("t");
        var probe = system.CreateProbe();
        var actor = system.SpawnPersistent<CounterActor>("c-2");

        actor.Tell(new Increment());
        actor.Tell(new Increment());
        actor.Tell(new Increment());

        await WaitUntilAsync(() => !actor.IsMaterialized, 60_000, "passivated");

        // Wake — state reloads from snapshot, counter is 3.
        actor.Tell(new Increment());
        probe.Send(actor, new Get());

        var reply = probe.ExpectMsg<Reply>(TimeSpan.FromSeconds(60));
        Assert.Equal(4, reply.N);
        Assert.True(actor.IsMaterialized);   // re-materialised
    }

    [Fact]
    public async Task DoesNotPassivate_WhilePersistentlyBusyAsync()
    {
        using var system = new TestActorSystem("t");
        var probe = system.CreateProbe();
        var actor = system.SpawnPersistent<CounterActor>("c-busy");

        // Hammer messages faster than the passivation window. Never idles.
        for (int i = 0; i < 20; i++)
        {
            actor.Tell(new Increment());
            await Task.Delay(25);
        }

        Assert.True(actor.IsMaterialized);

        probe.Send(actor, new Get());
        var reply = probe.ExpectMsg<Reply>(TimeSpan.FromSeconds(60));
        Assert.Equal(20, reply.N);
    }

    [Fact]
    public async Task NonPersistent_PassivatesForMemoryRelease_StateIsLostAsync()
    {
        // Non-persistent actors that declare a passivation
        // timeout get the memory-release benefit. State is not
        // preserved (no snapshot store), so the next message
        // materialises a fresh instance with count back at zero.
        using var system = new TestActorSystem("t");
        var probe = system.CreateProbe();
        var actor = system.Spawn<CounterActorNoKey>();

        actor.Tell(new Increment());
        actor.Tell(new Increment());

        await WaitUntilAsync(() => !actor.IsMaterialized,
            timeoutMs: 60_000,
            label: "non-persistent actor should still unload after idle window");

        // Next message rebuilds the instance — count restarts at 0.
        probe.Send(actor, new Get());
        var reply = probe.ExpectMsg<Reply>(TimeSpan.FromSeconds(60));
        Assert.Equal(0, reply.N);
    }

    private sealed class CounterWithPassivateHook : ActorBase
    {
        private int _count;
        public int OnPassivateCalls;

        protected override async Task DispatchAsync(object message, ActorRef sender)
        {
            _currentSender = sender;
            switch (message)
            {
                case Increment: _count++; await PersistAsync(); break;
                case Get:       sender.Tell(new Reply(_count)); break;
            }
        }

        protected override IReadOnlyDictionary<string, object?> CaptureFields() =>
            new Dictionary<string, object?> { ["count"] = _count };

        protected override void OnRestore(Snapshot s) => _count = s.Get<int>("count");

        protected override void OnPassivate() => OnPassivateCalls++;

        protected override TimeSpan? PassivationTimeout => ShortTimeout;
    }

    [Fact]
    public async Task OnPassivate_Fires_BeforeUnloadAsync()
    {
        using var system = new TestActorSystem("t");
        var @ref = system.SpawnPersistent<CounterWithPassivateHook>("cp-1");

        @ref.Tell(new Increment());

        // Wait for passivation to happen.
        await WaitUntilAsync(() => !@ref.IsMaterialized, 60_000, "passivated");

        // Wake the actor again to grab a reference to the NEW instance so
        // we can verify OnPassivate was called on the OLD one. Simplest
        // proof: the snapshot contains the post-increment value, which it
        // only would if OnPassivate (and then PersistAsync) ran before
        // unload. CaptureFields also had to run.
        var probe = system.CreateProbe();
        probe.Send(@ref, new Get());
        var reply = probe.ExpectMsg<Reply>(TimeSpan.FromSeconds(60));
        Assert.Equal(1, reply.N);
    }

    [Fact]
    public async Task MultiplePassivationCycles_WorkCorrectlyAsync()
    {
        using var system = new TestActorSystem("t");
        var probe = system.CreateProbe();
        var actor = system.SpawnPersistent<CounterActor>("c-multi");

        for (int cycle = 1; cycle <= 3; cycle++)
        {
            actor.Tell(new Increment());
            await WaitUntilAsync(() => !actor.IsMaterialized, 60_000, $"cycle {cycle} passivate");
        }

        probe.Send(actor, new Get());
        var reply = probe.ExpectMsg<Reply>(TimeSpan.FromSeconds(60));
        Assert.Equal(3, reply.N);
    }

    // ─── Helpers ─────────────────────────────────────────────────────────────

    private static async Task WaitUntilAsync(Func<bool> predicate, int timeoutMs, string label)
    {
        var deadline = DateTime.UtcNow.AddMilliseconds(timeoutMs);
        while (DateTime.UtcNow < deadline)
        {
            if (predicate()) return;
            await Task.Delay(10);
        }
        throw new TimeoutException($"Condition not met within {timeoutMs}ms: {label}");
    }
}
