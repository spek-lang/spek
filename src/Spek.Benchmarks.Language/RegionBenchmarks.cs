using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Configs;
using Spek.Runtime;

namespace Spek.Benchmarks.Language;

/// <summary>
/// The region-arbitration tax: what <c>shared X { }</c> + <c>use X x;</c>
/// adds per message over identical actor-local state. Both actors below are
/// the same shape — one increments a field in a shared region (the emitter
/// wraps the handler body in the region's writer lock), the other mutates
/// its own field with no lock at all. Regions materialise lazily through
/// <c>GetSharedRegion&lt;T&gt;()</c> with no host scaffolding, so the whole
/// shape is driven end-to-end from compiled Spek source.
///
/// The Read pair does the same comparison through <c>reader on</c> arms:
/// the region read runs under the region's concurrent reader lock, the
/// local read under none. Every delta in this suite is lock arbitration —
/// the handler bodies are byte-identical either side.
/// </summary>
[MemoryDiagnoser]
[GroupBenchmarksBy(BenchmarkLogicalGroupRule.ByCategory)]
[CategoriesColumn]
public class RegionBenchmarks
{
    private const int MessagesPerInvoke = 10_000;
    private const int RoundTrips = 1_000;

    private const string Source = """
        shared Tally
        {
            int hits = 0;
        }

        message Bump();
        message GetHits();
        message HitsReply(int hits);

        actor RegionCounter
        {
            use Tally tally;

            on Bump => { tally.hits = tally.hits + 1; }

            reader on GetHits => return new HitsReply(tally.hits);
        }

        actor LocalCounter
        {
            int hits = 0;

            on Bump => { hits = hits + 1; }

            reader on GetHits => return new HitsReply(hits);
        }
        """;

    private ActorSystem _system = null!;
    private ActorRef _regionCounter = null!;
    private ActorRef _localCounter = null!;
    private object _bump = null!;
    private object _getHits = null!;

    [GlobalSetup]
    public async Task Setup()
    {
        var assembly = SpekSourceCompiler.Compile(Source, "RegionShapes");
        _system        = new ActorSystem("langbench-regions");
        _regionCounter = _system.Spawn(SpekSourceCompiler.TypeNamed(assembly, "RegionCounter"));
        _localCounter  = _system.Spawn(SpekSourceCompiler.TypeNamed(assembly, "LocalCounter"));
        _bump          = SpekSourceCompiler.NewMessage(assembly, "Bump");
        _getHits       = SpekSourceCompiler.NewMessage(assembly, "GetHits");

        // Prime both actors: forces Initialize, the lazy GetSharedRegion<T>
        // resolution, and one pass through every measured arm.
        _regionCounter.Tell(_bump);
        _localCounter.Tell(_bump);
        _system.AwaitTermination();
        await _regionCounter.AskAsync<object>(_getHits);
        await _localCounter.AskAsync<object>(_getHits);
    }

    [GlobalCleanup]
    public void Cleanup() => _system.Dispose();

    /// <summary>Baseline: the handler mutates the actor's own field —
    /// serialized by the mailbox alone, no lock in the emitted body.</summary>
    [BenchmarkCategory("Increment"), Benchmark(Baseline = true, OperationsPerInvoke = MessagesPerInvoke)]
    public void OwnFieldIncrement()
    {
        for (int i = 0; i < MessagesPerInvoke; i++)
            _localCounter.Tell(_bump);
        _system.AwaitTermination();
    }

    /// <summary>The identical increment against a region field. The ratio
    /// against the baseline is the per-message writer-lock tax
    /// (EnterWriterAsync / ExitWriter around the emitted body).</summary>
    [BenchmarkCategory("Increment"), Benchmark(OperationsPerInvoke = MessagesPerInvoke)]
    public void RegionFieldIncrement()
    {
        for (int i = 0; i < MessagesPerInvoke; i++)
            _regionCounter.Tell(_bump);
        _system.AwaitTermination();
    }

    /// <summary>Baseline: a <c>reader on</c> ask that reads the actor's own
    /// field — reader-arm accounting only, no region lock.</summary>
    [BenchmarkCategory("Read"), Benchmark(Baseline = true, OperationsPerInvoke = RoundTrips)]
    public async Task OwnFieldReaderAsk()
    {
        for (int i = 0; i < RoundTrips; i++)
            await _localCounter.AskAsync<object>(_getHits);
    }

    /// <summary>The same reader ask against region state — adds the region's
    /// concurrent reader-lock acquisition to the arm.</summary>
    [BenchmarkCategory("Read"), Benchmark(OperationsPerInvoke = RoundTrips)]
    public async Task RegionReaderAsk()
    {
        for (int i = 0; i < RoundTrips; i++)
            await _regionCounter.AskAsync<object>(_getHits);
    }
}
