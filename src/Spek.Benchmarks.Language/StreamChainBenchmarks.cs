using BenchmarkDotNet.Attributes;
using Spek.Runtime;

namespace Spek.Benchmarks.Language;

/// <summary>
/// What a stream-shaped handler (<c>on X =&gt; distinct(...) =&gt; { }</c>)
/// costs per message when NOTHING is dropped — the operator chain itself
/// plus the re-admission hop, isolated from the rate-shaping the operators
/// exist for.
///
/// The emitted shape matters here: a chain-passed message does not run the
/// body inline. The chain's dispatch posts a synthetic body-trigger record
/// back to self (<c>_selfRef.Tell(new __FireBody_...(msg))</c>) so the body
/// runs through the mailbox under the actor lock — every passed message
/// costs a SECOND mailbox hop by design. The ratio against
/// <see cref="BareHandler"/> surfaces that doubling; it is the number this
/// suite exists to publish.
///
/// Nothing-dropped is arranged per operator: distinct sees alternating keys
/// (every key differs from the previous emit), and throttle uses a zero
/// interval — <c>elapsed &gt;= interval</c> holds for every message, so the
/// leading-edge window opens each time and every message passes.
/// </summary>
[MemoryDiagnoser]
public class StreamChainBenchmarks
{
    private const int MessagesPerInvoke = 10_000;

    private const string Source = """
        using Spek.Streams;

        message Tick(int id);

        actor BareCounter
        {
            int fired = 0;

            on Tick t => { fired = fired + 1; }
        }

        actor DistinctCounter
        {
            int fired = 0;

            on Tick t => StreamOperators.distinct<Tick, int>(by: x => x.id) => { fired = fired + 1; }
        }

        actor ThrottleCounter
        {
            int fired = 0;

            on Tick t => throttle(0) => { fired = fired + 1; }
        }

        actor ComposedCounter
        {
            int fired = 0;

            on Tick t => StreamOperators.distinct<Tick, int>(by: x => x.id) => throttle(0) => { fired = fired + 1; }
        }
        """;

    private ActorSystem _system = null!;
    private ActorRef _bare = null!;
    private ActorRef _distinct = null!;
    private ActorRef _throttle = null!;
    private ActorRef _composed = null!;
    private object _tick0 = null!;
    private object _tick1 = null!;

    [GlobalSetup]
    public void Setup()
    {
        var assembly = SpekSourceCompiler.Compile(Source, "StreamChainShapes");
        _system   = new ActorSystem("langbench-streams");
        _bare     = _system.Spawn(SpekSourceCompiler.TypeNamed(assembly, "BareCounter"));
        _distinct = _system.Spawn(SpekSourceCompiler.TypeNamed(assembly, "DistinctCounter"));
        _throttle = _system.Spawn(SpekSourceCompiler.TypeNamed(assembly, "ThrottleCounter"));
        _composed = _system.Spawn(SpekSourceCompiler.TypeNamed(assembly, "ComposedCounter"));
        _tick0    = SpekSourceCompiler.NewMessage(assembly, "Tick", 0);
        _tick1    = SpekSourceCompiler.NewMessage(assembly, "Tick", 1);

        // Prime each actor: builds the lazy operator chain and runs one full
        // offer + body-trigger round before measurement.
        _bare.Tell(_tick0);
        _distinct.Tell(_tick0);
        _throttle.Tell(_tick0);
        _composed.Tell(_tick0);
        _system.AwaitTermination();
    }

    [GlobalCleanup]
    public void Cleanup() => _system.Dispose();

    /// <summary>Baseline: a plain <c>on</c> handler counting messages — one
    /// mailbox hop per message, no operator chain.</summary>
    [Benchmark(Baseline = true, OperationsPerInvoke = MessagesPerInvoke)]
    public void BareHandler() => Drive(_bare);

    /// <summary>A <c>distinct(by:)</c> chain fed alternating keys so every
    /// message passes. Prices the operator's key extraction + comparison
    /// plus the body-trigger self-Tell re-admission per message.</summary>
    [Benchmark(OperationsPerInvoke = MessagesPerInvoke)]
    public void DistinctChain() => Drive(_distinct);

    /// <summary>A <c>throttle(0)</c> chain: the zero interval means every
    /// message opens a fresh window and passes, so the measured cost is the
    /// operator's clock read + CAS plus re-admission — not drops.</summary>
    [Benchmark(OperationsPerInvoke = MessagesPerInvoke)]
    public void ThrottleChainAllPass() => Drive(_throttle);

    /// <summary>distinct + throttle(0) composed. Compare against
    /// <see cref="DistinctChain"/> for the per-extra-operator tax: one more
    /// chain link, still a single re-admission hop.</summary>
    [Benchmark(OperationsPerInvoke = MessagesPerInvoke)]
    public void ComposeDepth() => Drive(_composed);

    private void Drive(ActorRef actor)
    {
        for (int i = 0; i < MessagesPerInvoke; i++)
            actor.Tell((i & 1) == 0 ? _tick0 : _tick1);
        _system.AwaitTermination();
    }
}
