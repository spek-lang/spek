using BenchmarkDotNet.Attributes;
using Spek;
using Spek.Persistence.File;
using Spek.Persistence.Sqlite;
using Spek.Runtime;

namespace Spek.Benchmarks;

/// <summary>
/// Cost of the persistence surfaces: a persist-every-message actor measured
/// against each shipped snapshot store, plus the passivate/respawn round trip.
/// The message count is deliberately smaller than the messaging suite's —
/// the file store fsyncs every save, so each message here is real I/O, not a
/// mailbox hop. Compare the per-message means across stores, not against
/// <see cref="MessagingBenchmarks.TellThroughput"/>.
/// </summary>
[MemoryDiagnoser]
public class PersistenceBenchmarks
{
    [Params(1_000)]
    public int Messages;

    private const int PassivateCycles = 5;

    private string _root = null!;

    [GlobalSetup]
    public void Setup() =>
        _root = Directory.CreateDirectory(Path.Combine(
            Path.GetTempPath(), $"spek-bench-persist-{Guid.NewGuid():N}")).FullName;

    [GlobalCleanup]
    public void Cleanup()
    {
        try { Directory.Delete(_root, recursive: true); }
        catch { /* best effort — temp dir, OS will reap it */ }
    }

    /// <summary>Persist-per-message against the in-memory store: the floor.
    /// Dominated by the snapshot capture (one dictionary per save) and the
    /// async persist plumbing — no serialization, no I/O.</summary>
    [Benchmark(Baseline = true)]
    public int PersistPerMessage_InMemory()
    {
        using var system = new ActorSystem("bench", snapshotStore: new InMemorySnapshotStore());
        var actor = system.SpawnPersistent<PersistingCounterActor>("bench/counter");
        for (int i = 0; i < Messages; i++)
            actor.Tell(new Ping());
        system.AwaitTermination();
        return Messages;
    }

    /// <summary>Persist-per-message against the file store. Dominated by
    /// durability: each save JSON-serializes, writes a temp file, flushes to
    /// disk, and atomically renames — the fsync is the number you see.</summary>
    [Benchmark]
    public int PersistPerMessage_File()
    {
        var store = new FileSnapshotStore(Path.Combine(_root, "file"));
        using var system = new ActorSystem("bench", snapshotStore: store);
        var actor = system.SpawnPersistent<PersistingCounterActor>("bench/counter");
        for (int i = 0; i < Messages; i++)
            actor.Tell(new Ping());
        system.AwaitTermination();
        return Messages;
    }

    /// <summary>Persist-per-message against the SQLite store. Dominated by
    /// the per-save connection open plus the upsert commit; WAL mode with
    /// synchronous=NORMAL keeps commits off the fsync path, which is why this
    /// usually lands well under the file store.</summary>
    [Benchmark]
    public int PersistPerMessage_Sqlite()
    {
        using var store = new SqliteSnapshotStore(Path.Combine(_root, "snapshots.db"));
        using var system = new ActorSystem("bench", snapshotStore: store);
        var actor = system.SpawnPersistent<PersistingCounterActor>("bench/counter");
        for (int i = 0; i < Messages; i++)
            actor.Tell(new Ping());
        system.AwaitTermination();
        return Messages;
    }

    /// <summary>
    /// The full passivation round trip, <see cref="PassivateCycles"/> times:
    /// message → idle → timer-driven unload (persist + drop instance) →
    /// re-Tell → re-materialise + restore. Wall time is dominated by the
    /// actor's 50ms idle window and the runtime's passivation-check period by
    /// design — the numbers to watch are Allocated (the unload/reload
    /// machinery) and that the cycle completes at all.
    /// </summary>
    [Benchmark]
    public void PassivateRespawnCycle()
    {
        using var system = new ActorSystem("bench", snapshotStore: new InMemorySnapshotStore());
        var actor = system.SpawnPersistent<PassivatingCounterActor>("bench/passivate");
        for (int i = 0; i < PassivateCycles; i++)
        {
            actor.Tell(new Ping());
            system.AwaitTermination();

            // Wait for the idle timer to unload the instance; fail loud
            // rather than wedging the harness if passivation never fires.
            var start = System.Diagnostics.Stopwatch.GetTimestamp();
            while (actor.IsMaterialized)
            {
                if (System.Diagnostics.Stopwatch.GetElapsedTime(start) > TimeSpan.FromSeconds(10))
                    throw new TimeoutException("actor failed to passivate within 10s");
                Thread.Sleep(5);
            }
        }
    }
}
