using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Configs;
using Spek.Runtime;

namespace Spek.Benchmarks.Language;

/// <summary>
/// What a <c>reader on</c> arm costs relative to a plain writer arm, on the
/// SAME emitted actor with byte-identical handler bodies. <c>Lookup</c> is
/// declared <c>reader</c>; <c>WriteLookup</c> is the default writer — so
/// every delta in this suite is reader-arm accounting (the in-flight reader
/// counter, queue-to-pool dispatch, idle pulse, reader/writer lock
/// arbitration), never a difference in user code.
///
/// Sequential asks price the accounting itself; the Concurrent pair shows
/// the payoff side — 16 workers ask at once, and readers overlap by
/// construction while writer asks serialize. Interleave alternates a writer
/// Tell with a reader ask, the drain/idle handoff between the two modes.
/// </summary>
[MemoryDiagnoser]
[GroupBenchmarksBy(BenchmarkLogicalGroupRule.ByCategory)]
[CategoriesColumn]
public class ReaderWriterBenchmarks
{
    private const int RoundTrips = 1_000;
    private const int Workers = 16;
    private const int AsksPerWorker = 1_000;
    private const int ConcurrentOps = Workers * AsksPerWorker;
    private const int InterleavePairs = 1_000;
    private const int Keys = 64;

    private const string Source = """
        message Post(int key, int value);
        message Lookup(int key);
        message WriteLookup(int key);
        message LookupReply(int value);

        actor Board
        {
            System.Collections.Generic.Dictionary<int, int> scores =
                new System.Collections.Generic.Dictionary<int, int>();

            on Post p => { scores[p.key] = p.value; }

            on WriteLookup l =>
            {
                var v = 0;
                if (scores.ContainsKey(l.key)) { v = scores[l.key]; }
                return new LookupReply(v);
            }

            reader on Lookup l =>
            {
                var v = 0;
                if (scores.ContainsKey(l.key)) { v = scores[l.key]; }
                return new LookupReply(v);
            }
        }
        """;

    private ActorSystem _system = null!;
    private ActorRef _board = null!;
    private object _lookup = null!;
    private object _writeLookup = null!;
    private object _post = null!;

    [GlobalSetup]
    public async Task Setup()
    {
        var assembly = SpekSourceCompiler.Compile(Source, "ReaderWriterShapes");
        _system = new ActorSystem("langbench-readers");
        _board  = _system.Spawn(SpekSourceCompiler.TypeNamed(assembly, "Board"));

        // Seed the table (setup-only reflection), then cache one instance of
        // each ask/tell message for the measured loops.
        for (int k = 0; k < Keys; k++)
            _board.Tell(SpekSourceCompiler.NewMessage(assembly, "Post", k, k * 7));
        _system.AwaitTermination();

        _lookup      = SpekSourceCompiler.NewMessage(assembly, "Lookup", 7);
        _writeLookup = SpekSourceCompiler.NewMessage(assembly, "WriteLookup", 7);
        _post        = SpekSourceCompiler.NewMessage(assembly, "Post", 7, 49);

        // Prime both ask arms once so first-dispatch work stays out of the
        // measured iterations.
        await _board.AskAsync<object>(_writeLookup);
        await _board.AskAsync<object>(_lookup);
    }

    [GlobalCleanup]
    public void Cleanup() => _system.Dispose();

    /// <summary>Baseline: sequential ask round-trips through the writer arm
    /// (send, handler, reply, awaiter resumed).</summary>
    [BenchmarkCategory("SequentialAsk"), Benchmark(Baseline = true, OperationsPerInvoke = RoundTrips)]
    public async Task WriterAskRoundTrip()
    {
        for (int i = 0; i < RoundTrips; i++)
            await _board.AskAsync<object>(_writeLookup);
    }

    /// <summary>The same round-trips through the <c>reader on</c> arm. With
    /// no concurrency to exploit, the ratio against the baseline is the pure
    /// reader-arm accounting cost per ask.</summary>
    [BenchmarkCategory("SequentialAsk"), Benchmark(OperationsPerInvoke = RoundTrips)]
    public async Task ReaderAskRoundTrip()
    {
        for (int i = 0; i < RoundTrips; i++)
            await _board.AskAsync<object>(_lookup);
    }

    /// <summary>Baseline: 16 concurrent workers asking the WRITER arm —
    /// every dispatch serializes against the rest.</summary>
    [BenchmarkCategory("ConcurrentAsk"), Benchmark(Baseline = true, OperationsPerInvoke = ConcurrentOps)]
    public Task ConcurrentWriterAsks() => ConcurrentAsksAsync(_writeLookup);

    /// <summary>16 concurrent workers asking the <c>reader on</c> arm —
    /// readers run in parallel by design. The ratio against the writer twin
    /// is the scaling win the runtime promises.</summary>
    [BenchmarkCategory("ConcurrentAsk"), Benchmark(OperationsPerInvoke = ConcurrentOps)]
    public Task ConcurrentReaderAsks() => ConcurrentAsksAsync(_lookup);

    /// <summary>Alternating writer Tells and reader asks: each operation is
    /// one Post Tell followed by one awaited Lookup — the drain/idle handoff
    /// the slot performs every time it flips between writer and reader
    /// modes.</summary>
    [BenchmarkCategory("Interleave"), Benchmark(OperationsPerInvoke = InterleavePairs)]
    public async Task ReaderWriterInterleave()
    {
        for (int i = 0; i < InterleavePairs; i++)
        {
            _board.Tell(_post);
            await _board.AskAsync<object>(_lookup);
        }
    }

    private async Task ConcurrentAsksAsync(object message)
    {
        var workers = new Task[Workers];
        for (int w = 0; w < Workers; w++)
            workers[w] = WorkerAsync(_board, message);
        await Task.WhenAll(workers);

        static async Task WorkerAsync(ActorRef board, object message)
        {
            for (int i = 0; i < AsksPerWorker; i++)
                await board.AskAsync<object>(message);
        }
    }
}
