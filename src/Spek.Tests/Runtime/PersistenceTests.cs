using Spek.Persistence;
using Spek.Runtime;
using Spek.Testing;
using Xunit;

namespace Spek.Tests.Runtime;

/// <summary>
/// End-to-end runtime persistence tests. Uses a hand-written C# actor to
/// exercise <see cref="ActorSystem.SpawnPersistent"/>, <see cref="ISnapshotStore"/>,
/// and the <see cref="ActorBase.OnRestore(Snapshot)"/> lifecycle hook
/// without needing the transpiler end-to-end.
/// </summary>
public class PersistenceTests
{
    public record Deposit(decimal Amount);
    public record GetBalance();
    public record BalanceReply(decimal Amount);

    /// <summary>
    /// Minimal persistent actor — tracks a running balance, persists after each
    /// deposit, restores the balance from snapshot on respawn.
    /// </summary>
    private sealed class Wallet : ActorBase
    {
        private decimal _balance;

        protected override async Task DispatchAsync(object message, ActorRef sender)
        {
            _currentSender = sender;
            switch (message)
            {
                case Deposit d:
                    _balance += d.Amount;
                    await PersistAsync();
                    break;
                case GetBalance:
                    sender.Tell(new BalanceReply(_balance));
                    break;
            }
        }

        protected override IReadOnlyDictionary<string, object?> CaptureFields() =>
            new Dictionary<string, object?> { ["balance"] = _balance };

        protected override void OnRestore(Snapshot s) => _balance = s.Get<decimal>("balance");
    }

    [Fact]
    public async Task Persist_WritesSnapshotToStoreAsync()
    {
        var store = new InMemorySnapshotStore();
        using var system = new TestActorSystem("t", store);

        var wallet = system.SpawnPersistent<Wallet>("wallet-1");
        wallet.Tell(new Deposit(50m));

        await WaitUntilAsync(async () =>
            await store.LoadAsync("wallet-1") is { } s && s.Get<decimal>("balance") == 50m);

        var persisted = await store.LoadAsync("wallet-1");
        Assert.NotNull(persisted);
        Assert.Equal(50m, persisted.Get<decimal>("balance"));
    }

    [Fact]
    public async Task Respawn_WithSameKey_RestoresStateAsync()
    {
        var store = new InMemorySnapshotStore();

        // First run — persist a balance, then let the system dispose.
        using (var sys1 = new TestActorSystem("t1", store))
        {
            var wallet = sys1.SpawnPersistent<Wallet>("wallet-42");
            wallet.Tell(new Deposit(100m));
            wallet.Tell(new Deposit(25m));
            await WaitUntilAsync(async () =>
                await store.LoadAsync("wallet-42") is { } s && s.Get<decimal>("balance") == 125m);
        }

        // Second run — fresh system, same store, same key → OnRestore fires.
        using var sys2 = new TestActorSystem("t2", store);
        var probe = sys2.CreateProbe();
        var revived = sys2.SpawnPersistent<Wallet>("wallet-42");

        probe.Send(revived, new GetBalance());
        var reply = probe.ExpectMsg<BalanceReply>();

        Assert.Equal(125m, reply.Amount);
    }

    [Fact]
    public async Task Respawn_WithDifferentKey_StartsFreshAsync()
    {
        var store = new InMemorySnapshotStore();
        using var system = new TestActorSystem("t", store);

        var a = system.SpawnPersistent<Wallet>("wallet-A");
        a.Tell(new Deposit(999m));
        await WaitUntilAsync(async () =>
            await store.LoadAsync("wallet-A") is { } s && s.Get<decimal>("balance") == 999m);

        var probe = system.CreateProbe();
        var b = system.SpawnPersistent<Wallet>("wallet-B");
        probe.Send(b, new GetBalance());
        var reply = probe.ExpectMsg<BalanceReply>();

        Assert.Equal(0m, reply.Amount);
    }

    [Fact]
    public async Task NonPersistentSpawn_DoesNotWriteToStoreAsync()
    {
        var store = new InMemorySnapshotStore();
        using var system = new TestActorSystem("t", store);

        // Plain Spawn — no persistence key, so persist() becomes a no-op.
        var wallet = system.Spawn<Wallet>();
        wallet.Tell(new Deposit(10m));

        await Task.Delay(50);

        Assert.Null(await store.LoadAsync("wallet-anything"));
    }

    [Fact]
    public async Task MultipleDeposits_SnapshotReflectsLatestAsync()
    {
        var store = new InMemorySnapshotStore();
        using var system = new TestActorSystem("t", store);

        var wallet = system.SpawnPersistent<Wallet>("wallet-multi");
        wallet.Tell(new Deposit(1m));
        wallet.Tell(new Deposit(2m));
        wallet.Tell(new Deposit(3m));

        await WaitUntilAsync(async () =>
            await store.LoadAsync("wallet-multi") is { } s && s.Get<decimal>("balance") == 6m);

        var snapshot = await store.LoadAsync("wallet-multi");
        Assert.Equal(6m, snapshot!.Get<decimal>("balance"));
    }

    // ─── Helpers ─────────────────────────────────────────────────────────────

    // Generous window on purpose: under full-parallel suite load the
    // persist write can starve for seconds (see the timing-tests lesson in
    // the test-kit work — widen windows, don't throttle the runner). The
    // happy path returns the instant the condition holds.
    private static async Task WaitUntilAsync(Func<Task<bool>> predicate, int timeoutMs = 60_000)
    {
        var deadline = DateTime.UtcNow.AddMilliseconds(timeoutMs);
        while (DateTime.UtcNow < deadline)
        {
            if (await predicate()) return;
            await Task.Delay(10);
        }
        throw new TimeoutException($"Condition not met within {timeoutMs}ms.");
    }
}
