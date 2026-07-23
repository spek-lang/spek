using BenchmarkDotNet.Attributes;
using Spek;
using Spek.Runtime;

namespace Spek.Benchmarks;

/// <summary>
/// Core runtime throughput / latency / allocation benchmarks.
///
/// MemoryDiagnoser reports bytes allocated per operation — the figure to watch
/// for per-message hot-path allocations (mailbox tuple, the observability tag
/// array on every Enqueue, Activity.Current capture). ThreadingDiagnoser
/// reports completed/queued work items and lock contentions — the figures that
/// expose the thread-pool dispatch model.
/// </summary>
[MemoryDiagnoser]
public class MessagingBenchmarks
{
    [Params(100_000)]
    public int Messages;

    /// <summary>Raw fire-and-forget Tell throughput into a single actor:
    /// enqueue N, drain to idle. Dominated by mailbox + dispatch cost.</summary>
    [Benchmark]
    public int TellThroughput()
    {
        using var system = new ActorSystem("bench");
        var actor = system.Spawn<CounterActor>();
        for (int i = 0; i < Messages; i++)
            actor.Tell(new Ping());
        system.AwaitTermination();   // drain to idle — all N processed
        return Messages;
    }

    /// <summary>Sequential ask round-trip latency (send → handler → reply →
    /// awaiter resumed), 1000 round-trips.</summary>
    [Benchmark]
    public async Task AskRoundTrip()
    {
        using var system = new ActorSystem("bench");
        var actor = system.Spawn<EchoActor>();
        for (int i = 0; i < 1_000; i++)
            await actor.AskAsync<Pong>(new Ping());
    }

    /// <summary>Cost of spawning actors (factory + slot wiring), 10k spawns.</summary>
    [Benchmark]
    public void SpawnCost()
    {
        using var system = new ActorSystem("bench");
        for (int i = 0; i < 10_000; i++)
            system.Spawn<CounterActor>();
    }
}
