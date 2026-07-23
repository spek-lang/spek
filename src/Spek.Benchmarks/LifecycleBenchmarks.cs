using BenchmarkDotNet.Attributes;
using Spek;
using Spek.Runtime;

namespace Spek.Benchmarks;

/// <summary>
/// Actor lifetime costs beyond the bare spawn that
/// <see cref="MessagingBenchmarks.SpawnCost"/> prices: the full
/// spawn→message→stop cycle, the persistent spawn's slot-plus-restore-check
/// wiring, the system teardown sweep over a 1k fleet, and a deep child chain
/// spawned handler-side. Short-lived actors pay these on every request, so
/// they compound exactly like the messaging numbers do.
/// </summary>
[MemoryDiagnoser]
public class LifecycleBenchmarks
{
    private const int Cycles = 10_000;
    private const int FleetSize = 1_000;
    private const int Chains = 100;
    private const int ChainDepth = 100;

    /// <summary>
    /// 10k spawn → one message → stop cycles (stop is voluntary, via a
    /// poison-pill message whose handler calls <c>StopSelf</c> — the public
    /// stop surface; there is no external ActorRef.Stop). Per cycle:
    /// slot + instance wiring, two enqueue/dispatches, the stop bookkeeping
    /// (stopped flag, stop-metric call, observer completion), and the loop
    /// unwinding. Divide the mean by 10k for the per-cycle figure.
    /// </summary>
    [Benchmark]
    public int SpawnStopCycle()
    {
        using var system = new ActorSystem("bench");
        for (int i = 0; i < Cycles; i++)
        {
            var actor = system.Spawn<StopOnPoisonActor>();
            actor.Tell(new Ping());
            actor.Tell(new PoisonPill());
        }
        system.AwaitTermination();
        return Cycles;
    }

    /// <summary>
    /// 10k <c>SpawnPersistent</c> calls with unique keys against the
    /// in-memory store, no messages sent: the plain spawn cost plus the
    /// persistence wiring — key binding, the synchronous snapshot-store
    /// lookup that comes back empty (a cold start, so no OnRestore), and the
    /// per-key string. Compare against
    /// <see cref="MessagingBenchmarks.SpawnCost"/> (same 10k count); the
    /// delta is what carrying a persistence identity costs at spawn.
    /// </summary>
    [Benchmark]
    public int SpawnPersistentCold()
    {
        using var system = new ActorSystem("bench", snapshotStore: new InMemorySnapshotStore());
        for (int i = 0; i < Cycles; i++)
            system.SpawnPersistent<PersistingCounterActor>($"bench/cold-{i}");
        return Cycles;
    }

    /// <summary>
    /// Construct a system, spawn 1000 actors, dispose: the teardown sweep.
    /// Dispose walks every tracked slot twice (StopGracefully — stop flag
    /// plus the OnPostStop/OnTerm hooks — then slot disposal), unwinds the
    /// region list, and tears down the shutdown plumbing. The spawn phase is
    /// included (there is no teardown without a fleet); subtract 1k×SpawnCost
    /// to isolate the sweep.
    /// </summary>
    [Benchmark]
    public int SystemDisposeWith1kActors()
    {
        var system = new ActorSystem("bench");
        for (int i = 0; i < FleetSize; i++)
            system.Spawn<CounterActor>();
        system.Dispose();
        return FleetSize;
    }

    /// <summary>
    /// 100 chains, each 100 children deep, grown handler-side through the
    /// protected <c>SpawnChildAsync</c> surface (what <c>spawn</c> in a
    /// handler compiles to). Each link pays child-slot construction with a
    /// parent link, RegisterChild, TrackSlot, and one message hop to carry
    /// the countdown down the chain — the supervision-tree analogue of
    /// SpawnCost. Divide the mean by 10k for the per-child figure.
    /// </summary>
    [Benchmark]
    public async Task DeepChildTree()
    {
        using var system = new ActorSystem("bench");
        for (int c = 0; c < Chains; c++)
        {
            var done = new TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously);
            var root = system.Spawn<ChildChainActor>();
            root.Tell(new GrowChain(ChainDepth, done));
            await done.Task;
        }
    }
}
