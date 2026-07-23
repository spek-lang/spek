using BenchmarkDotNet.Attributes;
using Spek.Streams;
using static Spek.Streams.StreamOperators;

namespace Spek.Benchmarks;

/// <summary>
/// The raw stream-operator hot path — 100k offers driven straight into each
/// operator through its public contract (Configure / OfferAsync), no actors,
/// no mailbox. This isolates what one <c>=&gt; operator</c> link in a handler
/// chain costs per message; the chain-through-an-actor shape (self-Tell
/// re-entry and all) is a separate concern measured elsewhere.
///
/// <see cref="PassthroughBaseline"/> is the floor every operator pays before
/// doing any work: one virtual <c>OfferAsync</c> call plus the protected
/// <c>Dispatch</c> null-check and delegate invocation. Every other row is
/// that floor plus the operator's own decision logic. The sink is a static
/// lambda returning <see cref="Task.CompletedTask"/>, so any allocation in
/// the Allocated column belongs to the operator, not the harness. Messages
/// are unboxed ints (the operators are generic over T).
/// </summary>
[MemoryDiagnoser]
public class StreamOperatorBenchmarks
{
    [Params(100_000)]
    public int Offers;

    /// <summary>No-op terminal sink — completes synchronously, allocates
    /// nothing, so the operator under test dominates.</summary>
    private static readonly Func<int, Task> NoopSink = static _ => Task.CompletedTask;

    /// <summary>The virtual-call + Dispatch floor: a minimal operator that
    /// forwards every message unchanged.</summary>
    [Benchmark(Baseline = true)]
    public async Task PassthroughBaseline()
    {
        var op = new PassthroughStreamOperator<int>();
        op.Configure(NoopSink);
        for (int i = 0; i < Offers; i++)
            await op.OfferAsync(i);
    }

    /// <summary>DistinctOperator with alternating keys, so nothing is ever
    /// dropped: per offer, one key-selector invocation, one lock, one
    /// EqualityComparer compare, then the dispatch.</summary>
    [Benchmark]
    public async Task DistinctAllPass()
    {
        var op = distinct(static (int x) => x);
        op.Configure(NoopSink);
        for (int i = 0; i < Offers; i++)
            await op.OfferAsync(i & 1);
    }

    /// <summary>DistinctOperator fed the same key every time: the first
    /// message emits, the rest take the drop path — the same selector +
    /// lock + compare, minus the dispatch.</summary>
    [Benchmark]
    public async Task DistinctAllDrop()
    {
        var op = distinct(static (int x) => x);
        op.Configure(NoopSink);
        for (int i = 0; i < Offers; i++)
            await op.OfferAsync(0);
    }

    /// <summary>ThrottleOperator with a zero window — every message opens a
    /// fresh window and passes: a UtcNow read plus two interlocked ops plus
    /// the dispatch, per offer. The all-pass bound of the throttle.</summary>
    [Benchmark]
    public async Task ThrottleZeroWindow()
    {
        var op = throttle<int>(0);
        op.Configure(NoopSink);
        for (int i = 0; i < Offers; i++)
            await op.OfferAsync(i);
    }

    /// <summary>ThrottleOperator with a window far longer than the run — the
    /// first message passes, every other offer is the drop path: a UtcNow
    /// read, an interlocked read, and a compare. The all-drop bound.</summary>
    [Benchmark]
    public async Task ThrottleAllDrop()
    {
        var op = throttle<int>(TimeSpan.FromDays(1));
        op.Configure(NoopSink);
        for (int i = 0; i < Offers; i++)
            await op.OfferAsync(i);
    }

    /// <summary>
    /// DebounceOperator with a 10s quiet window: each offer takes the lock,
    /// records the message, and re-arms the shared timer via
    /// <c>Timer.Change</c> — the record + re-arm cost per offer. Emission is
    /// deliberately out of scope: with offers arriving microseconds apart,
    /// only the final message would ever emit, ten seconds after the loop
    /// ends, and the closing StopAsync cancels it — by design this measures
    /// the offer path, never a dispatch.
    /// </summary>
    [Benchmark]
    public async Task DebounceOfferOnly()
    {
        var op = debounce<int>(10_000);
        op.Configure(NoopSink);
        for (int i = 0; i < Offers; i++)
            await op.OfferAsync(i);
        await op.StopAsync();   // cancel the armed timer — no stray emit
    }

    /// <summary>Two pass-through operators under <c>compose</c>, against
    /// <see cref="PassthroughBaseline"/>: the delta is the per-layer
    /// indirection tax — one extra OfferAsync/Dispatch hop plus the
    /// compose wiring's closure-captured lambda per link.</summary>
    [Benchmark]
    public async Task ComposeTwoDeep()
    {
        var op = compose(
            new PassthroughStreamOperator<int>(),
            new PassthroughStreamOperator<int>());
        op.Configure(NoopSink);
        for (int i = 0; i < Offers; i++)
            await op.OfferAsync(i);
    }
}

/// <summary>Minimal user-shaped operator: forwards every message unchanged.
/// Measures the base-class floor — virtual dispatch into OfferAsync plus the
/// Dispatch delegate invocation — that every real operator inherits.</summary>
internal sealed class PassthroughStreamOperator<T> : StreamOperator<T>
{
    public override Task OfferAsync(T message) => Dispatch(message);
}
