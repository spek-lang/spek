using BenchmarkDotNet.Attributes;
using Spek;
using Spek.Runtime;

namespace Spek.Benchmarks;

/// <summary>
/// The per-message tax of the debugging surfaces — inbox observers, the
/// flight recorder, and chaos plans. The docs claim these cost roughly
/// nothing when nothing is attached and stay cheap when something is; this
/// class turns the claim into a measured number. Every benchmark is the same
/// Tell-throughput-to-idle shape as
/// <see cref="MessagingBenchmarks.TellThroughput"/>, so the delta against
/// <see cref="Baseline"/> isolates the surface itself.
/// </summary>
[MemoryDiagnoser]
public class DiagnosticsBenchmarks
{
    [Params(100_000)]
    public int Messages;

    /// <summary>Plain system, nothing attached — the reference every other
    /// benchmark in this class is compared against. Per message, the
    /// diagnostic surfaces cost one volatile read + null check each here.</summary>
    [Benchmark(Baseline = true)]
    public int Baseline()
    {
        using var system = new ActorSystem("bench");
        var actor = system.Spawn<CounterActor>();
        for (int i = 0; i < Messages; i++)
            actor.Tell(new Ping());
        system.AwaitTermination();
        return Messages;
    }

    /// <summary>
    /// An inbox observer held for the whole run. Dominated by the enqueue-side
    /// publish: one <see cref="ObservedMessage"/> allocation plus a bounded-channel
    /// TryWrite per message. Under a 100k burst the tap saturates its buffer and
    /// sheds into <see cref="InboxObserverHandle.Dropped"/> by design — the
    /// measured cost is the actor-side tax, not the observer keeping up.
    /// </summary>
    [Benchmark]
    public int ObserverAttached()
    {
        using var system = new ActorSystem("bench");
        var actor = system.Spawn<CounterActor>();
        using var tap = system.Observe(actor, static _ => { });
        for (int i = 0; i < Messages; i++)
            actor.Tell(new Ping());
        system.AwaitTermination();
        return Messages;
    }

    /// <summary>
    /// A <see cref="FlightRecorder"/> attached at construction. Every Tell here
    /// carries no actor sender, so every message is ingress and gets journaled:
    /// dominated by JSON-serializing the payload plus ring-buffer maintenance
    /// (one enqueue, and one trim dequeue once the ring is full).
    /// </summary>
    [Benchmark]
    public int FlightRecorderAttached()
    {
        using var system = new ActorSystem("bench", trace: new FlightRecorder());
        var actor = system.Spawn<CounterActor>();
        for (int i = 0; i < Messages; i++)
            actor.Tell(new Ping());
        system.AwaitTermination();
        return Messages;
    }

    /// <summary>
    /// A chaos plan whose rules never match — a drop rule on a message type
    /// never sent and a crash rule on an actor type never spawned — so the
    /// fault never fires and what remains is the pure rule-scan cost, paid at
    /// both choke points: enqueue (drop/delay/duplicate scan) and dispatch
    /// (crash scan). The loud CHAOS ENABLED banner is construction-time only.
    /// </summary>
    [Benchmark]
    public int ChaosAttachedNoMatch()
    {
        var chaos = new ChaosPlan()
            .Drop<NeverSentMessage>()
            .CrashOnNth<NeverSpawnedActor>(1);
        using var system = new ActorSystem("bench", chaos: chaos);
        var actor = system.Spawn<CounterActor>();
        for (int i = 0; i < Messages; i++)
            actor.Tell(new Ping());
        system.AwaitTermination();
        return Messages;
    }

    /// <summary>
    /// The post-detach residue: attach an observer, dispose it, then measure.
    /// Detaching empties the slot's observer array back to null, so this
    /// should be indistinguishable from <see cref="Baseline"/> — a tap you
    /// removed must not keep taxing the actor.
    /// </summary>
    [Benchmark]
    public int ObserverDetached()
    {
        using var system = new ActorSystem("bench");
        var actor = system.Spawn<CounterActor>();
        var tap = system.Observe(actor, static _ => { });
        tap.Dispose();
        for (int i = 0; i < Messages; i++)
            actor.Tell(new Ping());
        system.AwaitTermination();
        return Messages;
    }
}
