using System.Reflection;
using Spek.Persistence;
using Spek.Runtime;
using Xunit;

namespace Spek.Tests.Runtime;

/// <summary>
/// Contract coverage for the async spawn family —
/// <see cref="ActorSystem.SpawnAsync{TActor}"/> /
/// <see cref="ActorSystem.SpawnPersistentAsync{TActor}"/> and their
/// non-generic <see cref="Type"/>-taking variants. The async spawns share
/// the sync path's semantics (init completes before the caller proceeds)
/// but await the snapshot store instead of blocking a pool thread.
/// Pinned here:
/// <list type="bullet">
///   <item>Awaiting the spawn task means init has fully completed —
///         <c>OnPreStart</c> has run, and for the persistent variant
///         <c>OnRestore</c> has already rehydrated prior snapshot state.</item>
///   <item>Failures during init fault the returned task: a throwing
///         constructor surfaces as <see cref="TargetInvocationException"/>
///         (the <see cref="System.Activator"/> wrapper), a throwing
///         <c>OnPreStart</c> and a throwing store <c>LoadAsync</c> surface
///         raw. A failed spawn tracks nothing in the system.</item>
///   <item>The non-generic variants enforce the ActorBase type guard.</item>
/// </list>
/// Plus a few opportunistic <see cref="ActorSystem"/> lifecycle edges
/// (empty-system shutdown, await-after-dispose, double dispose).
/// </summary>
public class SpawnAsyncTests
{
    private static readonly TimeSpan AskTimeout = TimeSpan.FromSeconds(15);

    // ─── messages ────────────────────────────────────────────────────────────
    public record Bump();
    public record Get();
    public record Reply(int N);

    /// <summary>Out-of-band observation channel handed to actors via their
    /// constructor — avoids static state so parallel tests can't cross-talk.</summary>
    private sealed class Gate
    {
        public volatile bool PreStarted;
        public volatile int RestoredN = -1;
    }

    private sealed class Counter : ActorBase
    {
        private readonly Gate _gate;
        private int _n;

        public Counter(Gate gate) => _gate = gate;

        protected override void OnPreStart() => _gate.PreStarted = true;

        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            _currentSender = sender;
            switch (message)
            {
                case Bump: _n++; break;
                case Get:  sender.Tell(new Reply(_n)); break;
            }
            return Task.CompletedTask;
        }

        protected override IReadOnlyDictionary<string, object?> CaptureFields() =>
            new Dictionary<string, object?> { ["n"] = _n };

        protected override void OnRestore(Snapshot s)
        {
            _n = s.Get<int>("n");
            _gate.RestoredN = _n;
        }
    }

    private sealed class CtorBoom : ActorBase
    {
        public CtorBoom() => throw new InvalidOperationException("ctor boom");

        protected override Task DispatchAsync(object message, ActorRef sender) =>
            Task.CompletedTask;
    }

    private sealed class PreStartBoom : ActorBase
    {
        protected override void OnPreStart() =>
            throw new NotSupportedException("prestart boom");

        protected override Task DispatchAsync(object message, ActorRef sender) =>
            Task.CompletedTask;
    }

    /// <summary>Store whose Load always fails — simulates a dead database
    /// behind the persistent async spawn.</summary>
    private sealed class ThrowingLoadStore : ISnapshotStore
    {
        public Task SaveAsync(string key, Snapshot snapshot) => Task.CompletedTask;
        public Task<Snapshot?> LoadAsync(string key) =>
            throw new IOException("load boom");
    }

    private sealed class NotAnActor { }

    // ─── SpawnAsync: happy paths ─────────────────────────────────────────────

    [Fact]
    public async Task SpawnAsync_InitCompletesBeforeTheTaskResolves_AndActorHandlesMailAsync()
    {
        using var system = new ActorSystem("spawn-async");
        var gate = new Gate();

        var actor = await system.SpawnAsync<Counter>(gate);

        // The await IS the init barrier: OnPreStart has already run — no
        // polling, no sleep. This is the async spawn's headline contract.
        Assert.True(gate.PreStarted,
            "SpawnAsync resolved before OnPreStart ran — init must complete first.");

        actor.Tell(new Bump());
        actor.Tell(new Bump());
        actor.Tell(new Bump());

        var reply = await actor.AskAsync<Reply>(new Get(), AskTimeout);
        Assert.Equal(3, reply.N);
    }

    [Fact]
    public async Task SpawnAsync_NonGenericVariant_SpawnsAndHandlesMailAsync()
    {
        using var system = new ActorSystem("spawn-async-nongeneric");
        var gate = new Gate();

        var actor = await system.SpawnAsync(typeof(Counter), gate);

        Assert.True(gate.PreStarted);
        actor.Tell(new Bump());

        var reply = await actor.AskAsync<Reply>(new Get(), AskTimeout);
        Assert.Equal(1, reply.N);
    }

    [Fact]
    public async Task SpawnAsync_NonGeneric_NonActorType_ThrowsArgumentExceptionAsync()
    {
        using var system = new ActorSystem("spawn-async-guard");

        // Same ActorBase guard as the sync non-generic overload; the throw
        // surfaces on the awaited task (the async method faults).
        var ex = await Assert.ThrowsAsync<ArgumentException>(
            () => system.SpawnAsync(typeof(NotAnActor)));
        Assert.Contains(nameof(ActorBase), ex.Message);
    }

    // ─── SpawnPersistentAsync: restore semantics ─────────────────────────────

    [Fact]
    public async Task SpawnPersistentAsync_RestoresPriorSnapshot_BeforeTheTaskResolvesAsync()
    {
        var store = new InMemorySnapshotStore();
        await store.SaveAsync("counter-async-1",
            new Snapshot(new Dictionary<string, object?> { ["n"] = 41 }));

        using var system = new ActorSystem("spawn-persistent-async", store);
        var gate = new Gate();

        var actor = await system.SpawnPersistentAsync<Counter>("counter-async-1", gate);

        // OnRestore already ran when the spawn task resolved — asserted
        // immediately, with no wait: the await is the restore barrier.
        Assert.Equal(41, gate.RestoredN);

        // And the restored state is live: one bump lands on top of it.
        actor.Tell(new Bump());
        var reply = await actor.AskAsync<Reply>(new Get(), AskTimeout);
        Assert.Equal(42, reply.N);
    }

    [Fact]
    public async Task SpawnPersistentAsync_NonGenericVariant_RestoresTooAsync()
    {
        var store = new InMemorySnapshotStore();
        await store.SaveAsync("counter-async-2",
            new Snapshot(new Dictionary<string, object?> { ["n"] = 7 }));

        using var system = new ActorSystem("spawn-persistent-async-ng", store);
        var gate = new Gate();

        var actor = await system.SpawnPersistentAsync(typeof(Counter), "counter-async-2", gate);

        Assert.Equal(7, gate.RestoredN);
        var reply = await actor.AskAsync<Reply>(new Get(), AskTimeout);
        Assert.Equal(7, reply.N);
    }

    [Fact]
    public async Task SpawnPersistentAsync_NonGeneric_NonActorType_ThrowsArgumentExceptionAsync()
    {
        using var system = new ActorSystem("spawn-persistent-async-guard");

        await Assert.ThrowsAsync<ArgumentException>(
            () => system.SpawnPersistentAsync(typeof(NotAnActor), "key"));
    }

    // ─── Failures during init fault the spawn task ───────────────────────────

    [Fact]
    public async Task SpawnAsync_ConstructorThrows_FaultsWithTargetInvocationException_AndTracksNothingAsync()
    {
        using var system = new ActorSystem("spawn-async-ctor-boom");

        // The factory goes through Activator.CreateInstance, which wraps
        // constructor exceptions — the caller sees TargetInvocationException
        // with the real failure as InnerException. (Same shape as the sync
        // Spawn; pinned so a future factory change is a conscious one.)
        var ex = await Assert.ThrowsAsync<TargetInvocationException>(
            () => system.SpawnAsync<CtorBoom>());
        Assert.IsType<InvalidOperationException>(ex.InnerException);
        Assert.Equal("ctor boom", ex.InnerException!.Message);

        // A failed spawn leaves no half-initialised slot behind.
        Assert.Empty(system.SnapshotActors());
    }

    [Fact]
    public async Task SpawnAsync_OnPreStartThrows_FaultsWithTheRawException_AndTracksNothingAsync()
    {
        using var system = new ActorSystem("spawn-async-prestart-boom");

        // OnPreStart runs inside InitializeAsync (no Activator in between),
        // so the exception surfaces raw.
        var ex = await Assert.ThrowsAsync<NotSupportedException>(
            () => system.SpawnAsync<PreStartBoom>());
        Assert.Equal("prestart boom", ex.Message);

        Assert.Empty(system.SnapshotActors());
    }

    [Fact]
    public async Task SpawnPersistentAsync_StoreLoadThrows_FaultsWithTheStoreException_AndTracksNothingAsync()
    {
        using var system = new ActorSystem("spawn-async-load-boom", new ThrowingLoadStore());
        var gate = new Gate();

        var ex = await Assert.ThrowsAsync<IOException>(
            () => system.SpawnPersistentAsync<Counter>("doomed", gate));
        Assert.Equal("load boom", ex.Message);

        Assert.Empty(system.SnapshotActors());
    }

    // ─── Opportunistic ActorSystem lifecycle edges ───────────────────────────

    [Fact]
    public void GracefulShutdown_SystemThatNeverSpawned_ReturnsTrue()
    {
        var system = new ActorSystem("empty-shutdown");

        // Nothing to drain — the wait is skipped entirely and shutdown
        // reports a clean drain.
        Assert.True(system.GracefulShutdown(TimeSpan.FromSeconds(1)));
    }

    [Fact]
    public void AwaitTermination_AfterDispose_ReturnsTrue()
    {
        var system = new ActorSystem("await-after-dispose");
        system.Dispose();

        // _terminated is a terminal state: a disposed system reports
        // terminated instead of hanging on the "no slots yet" idle check.
        Assert.True(system.AwaitTermination(TimeSpan.FromSeconds(5)));
    }

    [Fact]
    public async Task Dispose_IsIdempotent_EvenWithATrackedActorAsync()
    {
        var system = new ActorSystem("double-dispose");
        var gate = new Gate();
        var actor = await system.SpawnAsync<Counter>(gate);
        actor.Tell(new Bump());

        system.Dispose();
        system.Dispose();   // second call must be a no-op, not a throw

        Assert.True(actor.IsStopped);
    }
}
