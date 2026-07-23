using Spek.Persistence;
using Spek.Runtime;
using Spek.Testing;
using Xunit;

namespace Spek.Tests.Runtime;

/// <summary>
/// Proves the async spawn path works with a snapshot store that does
/// real async I/O (simulated via <see cref="Task.Delay"/>). The sync
/// <see cref="ActorSystem.Spawn{TActor}"/> path would block on the
/// delay and potentially deadlock on single-threaded sync contexts;
/// <see cref="ActorSystem.SpawnPersistentAsync{TActor}"/> does not.
/// </summary>
public class AsyncSpawnTests
{
    public record Increment();
    public record Get();
    public record Reply(int N);

    private sealed class Counter : ActorBase
    {
        private int _n;

        protected override async Task DispatchAsync(object message, ActorRef sender)
        {
            _currentSender = sender;
            switch (message)
            {
                case Increment: _n++; await PersistAsync(); break;
                case Get:       sender.Tell(new Reply(_n)); break;
            }
        }

        protected override IReadOnlyDictionary<string, object?> CaptureFields() =>
            new Dictionary<string, object?> { ["n"] = _n };

        protected override void OnRestore(Snapshot s) => _n = s.Get<int>("n");
    }

    /// <summary>Synthetic async store — every Save/Load yields for 50ms before returning.</summary>
    private sealed class DelayedStore : ISnapshotStore
    {
        private readonly InMemorySnapshotStore _inner = new();
        public TimeSpan Delay { get; set; } = TimeSpan.FromMilliseconds(50);

        public async Task SaveAsync(string key, Snapshot snapshot)
        {
            await Task.Delay(Delay);
            await _inner.SaveAsync(key, snapshot);
        }

        public async Task<Snapshot?> LoadAsync(string key)
        {
            await Task.Delay(Delay);
            return await _inner.LoadAsync(key);
        }
    }

    [Fact]
    public async Task SpawnPersistentAsync_LoadsSnapshotWithoutBlocking()
    {
        var store = new DelayedStore();

        // First run — write a snapshot through the async path.
        await using (var sys1 = new DisposableAsyncWrapper(new ActorSystem("t1", store)))
        {
            var actor = await sys1.Inner.SpawnPersistentAsync<Counter>("counter-1");
            actor.Tell(new Increment());
            actor.Tell(new Increment());
            actor.Tell(new Increment());

            await WaitUntil(async () =>
                await store.LoadAsync("counter-1") is { } s && s.Get<int>("n") == 3);
        }

        // Second run — respawn and assert state restored. Measure that the
        // async spawn actually yielded on the restore. Use a tolerance of
        // Delay/2 to keep the assertion meaningful (proving we went through
        // the store) without being flaky under timer jitter in CI.
        using var sys2 = new ActorSystem("t2", store);
        var started = DateTime.UtcNow;
        var revived = await sys2.SpawnPersistentAsync<Counter>("counter-1");
        var elapsed = DateTime.UtcNow - started;

        var floor = TimeSpan.FromMilliseconds(store.Delay.TotalMilliseconds / 2);
        Assert.True(elapsed >= floor,
            $"Async spawn returned in {elapsed}, well under half the store's {store.Delay} delay — the restore didn't go through the store.");

        // And the state is correct.
        using var probeSystem = new TestActorSystem("probe");
        var probe = probeSystem.CreateProbe();
        probe.Send(revived, new Get());

        var reply = probe.ExpectMsg<Reply>();
        Assert.Equal(3, reply.N);
    }

    [Fact]
    public async Task SpawnAsync_NonPersistent_WorksToo()
    {
        using var system = new ActorSystem("t");
        var actor = await system.SpawnAsync<Counter>();

        actor.Tell(new Increment());
        actor.Tell(new Increment());

        using var probeSystem = new TestActorSystem("probe");
        var probe = probeSystem.CreateProbe();
        probe.Send(actor, new Get());

        var reply = probe.ExpectMsg<Reply>();
        Assert.Equal(2, reply.N);
    }

    // ─── Helpers ─────────────────────────────────────────────────────────────

    private static async Task WaitUntil(Func<Task<bool>> predicate, int timeoutMs = 3000)
    {
        var deadline = DateTime.UtcNow.AddMilliseconds(timeoutMs);
        while (DateTime.UtcNow < deadline)
        {
            if (await predicate()) return;
            await Task.Delay(10);
        }
        throw new TimeoutException($"Condition not met within {timeoutMs}ms.");
    }

    /// <summary>
    /// Thin <see cref="IAsyncDisposable"/> wrapper — ActorSystem is
    /// <see cref="IDisposable"/>, not <see cref="IAsyncDisposable"/>,
    /// but we want to <c>await using</c> in tests to keep cleanup ordered
    /// relative to the async spawn calls.
    /// </summary>
    private sealed class DisposableAsyncWrapper(ActorSystem inner) : IAsyncDisposable
    {
        public ActorSystem Inner { get; } = inner;
        public ValueTask DisposeAsync()
        {
            Inner.Dispose();
            return ValueTask.CompletedTask;
        }
    }
}
