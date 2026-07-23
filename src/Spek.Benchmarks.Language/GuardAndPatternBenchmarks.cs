using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Configs;
using Spek.Runtime;

namespace Spek.Benchmarks.Language;

/// <summary>
/// Dispatch-shape details: conditional handling and message width.
///
/// Spek's grammar has no dispatch-level guard (<c>on Msg m when ...</c> is
/// not a language shape — <c>when</c> exists only on <c>case</c> patterns
/// and catch filters), so the Guard pair prices the idiom users actually
/// write: an <c>if</c> at the top of the handler body versus an
/// unconditional body, with a message that always passes the test.
///
/// The Width pair sends PRE-CREATED 1-field and 8-field messages, so it
/// isolates pure dispatch cost of a wider record — construction is kept out
/// of the measured operation on purpose. The ReplyConstruct pair puts
/// construction back in on the reply path: <c>return new Narrow(...)</c>
/// versus <c>return new Wide(...)</c> inside the handler, measured as full
/// ask round-trips.
/// </summary>
[MemoryDiagnoser]
[GroupBenchmarksBy(BenchmarkLogicalGroupRule.ByCategory)]
[CategoriesColumn]
public class GuardAndPatternBenchmarks
{
    private const int MessagesPerInvoke = 10_000;
    private const int RoundTrips = 1_000;

    private const string Source = """
        message Val(int value);
        message Narrow(int f0);
        message Wide(int f0, int f1, int f2, int f3, int f4, int f5, int f6, int f7);
        message GetNarrow();
        message GetWide();

        actor GuardedCounter
        {
            int count = 0;

            on Val v => { if (v.value > 0) { count = count + 1; } }
        }

        actor UnguardedCounter
        {
            int count = 0;

            on Val v => { count = count + 1; }
        }

        actor WidthCounter
        {
            int count = 0;

            on Narrow n => { count = count + 1; }

            on Wide w => { count = count + 1; }
        }

        actor ReplyBuilder
        {
            on GetNarrow => return new Narrow(1);

            on GetWide => return new Wide(1, 2, 3, 4, 5, 6, 7, 8);
        }
        """;

    private ActorSystem _system = null!;
    private ActorRef _guarded = null!;
    private ActorRef _unguarded = null!;
    private ActorRef _width = null!;
    private ActorRef _replier = null!;
    private object _val = null!;
    private object _narrow = null!;
    private object _wide = null!;
    private object _getNarrow = null!;
    private object _getWide = null!;

    [GlobalSetup]
    public async Task Setup()
    {
        var assembly = SpekSourceCompiler.Compile(Source, "GuardPatternShapes");
        _system    = new ActorSystem("langbench-guards");
        _guarded   = _system.Spawn(SpekSourceCompiler.TypeNamed(assembly, "GuardedCounter"));
        _unguarded = _system.Spawn(SpekSourceCompiler.TypeNamed(assembly, "UnguardedCounter"));
        _width     = _system.Spawn(SpekSourceCompiler.TypeNamed(assembly, "WidthCounter"));
        _replier   = _system.Spawn(SpekSourceCompiler.TypeNamed(assembly, "ReplyBuilder"));

        _val       = SpekSourceCompiler.NewMessage(assembly, "Val", 1);   // always passes the guard
        _narrow    = SpekSourceCompiler.NewMessage(assembly, "Narrow", 1);
        _wide      = SpekSourceCompiler.NewMessage(assembly, "Wide", 1, 2, 3, 4, 5, 6, 7, 8);
        _getNarrow = SpekSourceCompiler.NewMessage(assembly, "GetNarrow");
        _getWide   = SpekSourceCompiler.NewMessage(assembly, "GetWide");

        // Prime every measured arm once.
        _guarded.Tell(_val);
        _unguarded.Tell(_val);
        _width.Tell(_narrow);
        _width.Tell(_wide);
        _system.AwaitTermination();
        await _replier.AskAsync<object>(_getNarrow);
        await _replier.AskAsync<object>(_getWide);
    }

    [GlobalCleanup]
    public void Cleanup() => _system.Dispose();

    /// <summary>Baseline: unconditional handler body.</summary>
    [BenchmarkCategory("Guard"), Benchmark(Baseline = true, OperationsPerInvoke = MessagesPerInvoke)]
    public void UnguardedDispatch()
    {
        for (int i = 0; i < MessagesPerInvoke; i++)
            _unguarded.Tell(_val);
        _system.AwaitTermination();
    }

    /// <summary>The in-body <c>if (v.value &gt; 0)</c> guard idiom, always
    /// taken. The ratio against the baseline is the per-dispatch cost of a
    /// field read plus one branch in emitted code.</summary>
    [BenchmarkCategory("Guard"), Benchmark(OperationsPerInvoke = MessagesPerInvoke)]
    public void GuardedDispatch()
    {
        for (int i = 0; i < MessagesPerInvoke; i++)
            _guarded.Tell(_val);
        _system.AwaitTermination();
    }

    /// <summary>Baseline: dispatch of a pre-created 1-field message.</summary>
    [BenchmarkCategory("Width"), Benchmark(Baseline = true, OperationsPerInvoke = MessagesPerInvoke)]
    public void NarrowMessageDispatch()
    {
        for (int i = 0; i < MessagesPerInvoke; i++)
            _width.Tell(_narrow);
        _system.AwaitTermination();
    }

    /// <summary>Dispatch of a pre-created 8-field message through the same
    /// actor — pure dispatch cost of the wider record, no construction.</summary>
    [BenchmarkCategory("Width"), Benchmark(OperationsPerInvoke = MessagesPerInvoke)]
    public void WideMessageDispatch()
    {
        for (int i = 0; i < MessagesPerInvoke; i++)
            _width.Tell(_wide);
        _system.AwaitTermination();
    }

    /// <summary>Baseline: ask round-trip whose handler constructs a 1-field
    /// reply record.</summary>
    [BenchmarkCategory("ReplyConstruct"), Benchmark(Baseline = true, OperationsPerInvoke = RoundTrips)]
    public async Task NarrowReplyConstruct()
    {
        for (int i = 0; i < RoundTrips; i++)
            await _replier.AskAsync<object>(_getNarrow);
    }

    /// <summary>The same round-trip constructing an 8-field reply — the
    /// delta is <c>new Wide(...)</c> on the reply path.</summary>
    [BenchmarkCategory("ReplyConstruct"), Benchmark(OperationsPerInvoke = RoundTrips)]
    public async Task WideReplyConstruct()
    {
        for (int i = 0; i < RoundTrips; i++)
            await _replier.AskAsync<object>(_getWide);
    }
}
