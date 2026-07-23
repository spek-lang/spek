using Spek.Persistence;
using Spek.Runtime;
using Spek.Testing;
using Xunit;

namespace Spek.Tests.Runtime;

/// <summary>
/// Seed-sweep fuzz tier: each test runs an invariant-shaped scenario under
/// <see cref="SeedCount"/> different seeds. The seed drives BOTH the
/// generated data (its own <see cref="Random"/>) and the simulator's
/// schedule, so every failure message quotes the seed and the whole failing
/// run replays exactly from it — deterministic fuzzing, not flaky fuzzing.
///
/// Invariants over exact values: totals conserved, ledgers exactly-once,
/// state surviving churn, fault arithmetic honored — properties that must
/// hold under EVERY legal schedule, which is precisely what sweeping seeds
/// explores.
/// </summary>
[Trait("Category", "SimFuzz")]
public sealed class SimFuzzTests
{
    /// <summary>Seeds per scenario. 200 keeps the whole class around a
    /// second; a 2000-seed soak still finishes in under three. Raise locally
    /// for a deeper hunt.</summary>
    private const int SeedCount = 200;

    // ─── Scenario 1: bank-transfer conservation ──────────────────────────

    private sealed record Credit(int Amount);
    private sealed record Debit(int Amount);
    private sealed record GetBalance();
    private sealed record Balance(int Value);

    private sealed class Account : ActorBase
    {
        private int _balance;
        public Account(int opening) => _balance = opening;

        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            switch (message)
            {
                case Credit c:   _balance += c.Amount; break;
                case Debit d:    _balance -= d.Amount; break;
                case GetBalance: sender.Tell(new Balance(_balance), _selfRef!); break;
            }
            return Task.CompletedTask;
        }
    }

    [Fact]
    public void BankTransfers_ConserveTotalBalance_AcrossSeeds()
    {
        const int accounts = 4, opening = 100, transfers = 25;

        for (int seed = 0; seed < SeedCount; seed++)
        {
            using var sim = new SimulatedActorSystem(seed, audit: true);
            var refs = new ActorRef[accounts];
            for (int i = 0; i < accounts; i++) refs[i] = sim.Spawn<Account>(opening);

            // Transfer data draws from its own Random(seed) — deterministic
            // per seed and independent of the schedule stream, so the quoted
            // seed alone reproduces data AND interleaving.
            var rng = new Random(seed);
            for (int t = 0; t < transfers; t++)
            {
                var from   = rng.Next(accounts);
                var to     = rng.Next(accounts);
                var amount = rng.Next(1, 21);
                refs[from].Tell(new Debit(amount));
                refs[to].Tell(new Credit(amount));
                if (rng.Next(4) == 0) sim.Run();   // partial drains vary the interleaving shape
            }
            sim.Run();

            var total = 0;
            foreach (var r in refs)
                total += sim.Ask<Balance>(r, new GetBalance()).Value;

            Assert.True(total == accounts * opening,
                $"seed {seed}: total balance {total} != {accounts * opening} — money created or destroyed");
            Assert.True(sim.DeadLetters.Records.Count == 0,
                $"seed {seed}: {sim.DeadLetters.Records.Count} unexpected dead letters");
        }
    }

    // ─── Scenario 2: supervision — exactly-once under Restart ────────────

    private sealed record WorkItem(int Id, bool Poison);

    /// <summary>Per-simulation processed-message ledger, shared across the
    /// worker pool via constructor injection (no statics — sims sweep in a
    /// loop). The lock is belt-and-braces; simulator dispatch is
    /// single-stepped.</summary>
    private sealed class Ledger
    {
        private readonly Dictionary<int, int> _counts = [];
        public void Record(int id)
        {
            lock (_counts) _counts[id] = _counts.GetValueOrDefault(id) + 1;
        }
        public int CountOf(int id)
        {
            lock (_counts) return _counts.GetValueOrDefault(id);
        }
    }

    private sealed class CrashyWorker : ActorBase
    {
        private readonly Ledger _ledger;
        public CrashyWorker(Ledger ledger) => _ledger = ledger;

        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            if (message is WorkItem w)
            {
                if (w.Poison) throw new InvalidOperationException($"poison {w.Id}");
                _ledger.Record(w.Id);
            }
            return Task.CompletedTask;
        }

        protected override FailureDirective OnFailure(Exception exception, object message)
            => FailureDirective.Restart;
    }

    [Fact]
    public void CrashProneWorkers_UnderRestart_ProcessEveryNonPoisonMessageExactlyOnce()
    {
        const int workers = 3, items = 30;

        for (int seed = 0; seed < SeedCount; seed++)
        {
            using var sim = new SimulatedActorSystem(seed, audit: true);
            var ledger = new Ledger();
            var pool = new ActorRef[workers];
            for (int i = 0; i < workers; i++) pool[i] = sim.Spawn<CrashyWorker>(ledger);

            var rng = new Random(seed);
            var poison = new bool[items];
            for (int id = 0; id < items; id++)
            {
                poison[id] = rng.Next(5) == 0;   // ~20% poison
                pool[rng.Next(workers)].Tell(new WorkItem(id, poison[id]));
            }
            sim.Run();

            var poisonCount = poison.Count(p => p);
            for (int id = 0; id < items; id++)
            {
                var expected = poison[id] ? 0 : 1;
                var actual = ledger.CountOf(id);
                Assert.True(actual == expected,
                    $"seed {seed}: item {id} ({(poison[id] ? "poison" : "clean")}) " +
                    $"processed {actual} times, expected {expected}");
            }

            // Every poison message reached a terminal state: dead-lettered by
            // the Restart directive. Crashes never stop a restarting worker.
            Assert.True(sim.DeadLetters.Records.Count == poisonCount,
                $"seed {seed}: {sim.DeadLetters.Records.Count} dead letters, expected {poisonCount}");
            for (int i = 0; i < workers; i++)
                Assert.True(!pool[i].IsStopped, $"seed {seed}: worker {i} stopped under Restart supervision");
        }
    }

    // ─── Scenario 3: passivation churn ───────────────────────────────────

    private sealed record Bump();
    private sealed record GetCount();
    private sealed record CountReply(int N);

    /// <summary>Persistent counter that relies on the persist-before-passivate
    /// path (no explicit persist in the handler) — churn exercises the
    /// unload/snapshot/restore cycle itself.</summary>
    private sealed class ChurnCounter : ActorBase
    {
        private int _count;

        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            switch (message)
            {
                case Bump:     _count++; break;
                case GetCount: sender.Tell(new CountReply(_count), _selfRef!); break;
            }
            return Task.CompletedTask;
        }

        protected override IReadOnlyDictionary<string, object?> CaptureFields() =>
            new Dictionary<string, object?> { ["count"] = _count };

        protected override void OnRestore(Snapshot s) => _count = s.Get<int>("count");

        protected override TimeSpan? PassivationTimeout => TimeSpan.FromMinutes(1);
    }

    [Fact]
    public void PassivationChurn_CountsSurviveUnloadReloadCycles_AcrossSeeds()
    {
        const int rounds = 3;

        for (int seed = 0; seed < SeedCount; seed++)
        {
            using var sim = new SimulatedActorSystem(seed, audit: true);
            var actor = sim.SpawnPersistent<ChurnCounter>("churn");
            var rng = new Random(seed);
            var expected = 0;

            for (int round = 0; round < rounds; round++)
            {
                var hits = rng.Next(1, 6);
                for (int i = 0; i < hits; i++) actor.Tell(new Bump());
                expected += hits;
                sim.Run();
                Assert.True(actor.IsMaterialized,
                    $"seed {seed}: actor unloaded while busy (round {round})");

                // Two idle minutes on the virtual clock: the periodic
                // passivation check fires, persists, and drops the instance.
                sim.Advance(TimeSpan.FromMinutes(2));
                Assert.True(!actor.IsMaterialized,
                    $"seed {seed}: actor still loaded after the idle window (round {round})");
                Assert.True(!actor.IsStopped,
                    $"seed {seed}: passivation stopped the actor (round {round})");
            }

            // The ask re-materializes the actor; OnRestore must rehydrate the
            // full total accumulated across every churn cycle.
            var count = sim.Ask<CountReply>(actor, new GetCount()).N;
            Assert.True(count == expected,
                $"seed {seed}: count {count} != {expected} after {rounds} passivation cycles");
            Assert.True(sim.DeadLetters.Records.Count == 0,
                $"seed {seed}: {sim.DeadLetters.Records.Count} unexpected dead letters");
        }
    }

    // ─── Scenario 4: chaos drops — the documented Fires arithmetic ───────

    private sealed record Sample(int N);

    /// <summary>Shared handled-count box (constructor-injected — see
    /// <see cref="Ledger"/> for why no statics).</summary>
    private sealed class Box
    {
        public int Count;
    }

    private sealed class BoxCounter : ActorBase
    {
        private readonly Box _box;
        public BoxCounter(Box box) => _box = box;

        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            _box.Count++;
            return Task.CompletedTask;
        }
    }

    [Fact]
    public void ChaosDrops_FollowTheDocumentedFiresArithmetic_AcrossSeeds()
    {
        for (int seed = 0; seed < SeedCount; seed++)
        {
            var rng = new Random(seed);
            var n     = rng.Next(10, 61);
            var every = rng.Next(2, 6);

            var chaos = new ChaosPlan().Drop<Sample>(every);
            using var sim = new SimulatedActorSystem(seed, chaos, audit: true);
            var box = new Box();
            var a = sim.Spawn<BoxCounter>(box);
            var b = sim.Spawn<BoxCounter>(box);

            for (int i = 1; i <= n; i++) (i % 2 == 0 ? a : b).Tell(new Sample(i));
            sim.Run();

            // Documented arithmetic: a Drop rule counts MATCHES at the
            // enqueue point and fires on every `every`-th, so n sends drop
            // exactly floor(n / every) messages — counted in Fires, never
            // dead-lettered (a drop is the fault being modeled), and a
            // dropped message never costs a dispatch step.
            var drops = n / every;
            Assert.True(chaos.Fires == drops,
                $"seed {seed}: chaos fired {chaos.Fires} times for n={n}, every={every}; expected {drops}");
            Assert.True(box.Count == n - drops,
                $"seed {seed}: handled {box.Count}, expected {n - drops} (n={n}, every={every})");
            Assert.True(sim.DispatchSteps == n - drops,
                $"seed {seed}: {sim.DispatchSteps} dispatch steps for {n - drops} delivered messages");
            Assert.True(sim.DeadLetters.Records.Count == 0,
                $"seed {seed}: drops must not dead-letter; got {sim.DeadLetters.Records.Count}");
        }
    }
}
