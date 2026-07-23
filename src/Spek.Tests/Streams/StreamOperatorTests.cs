using Spek.Streams;
using Spek.Testing;
using Xunit;

namespace Spek.Tests.Streams;

// Runtime coverage for Spek.Streams — the operator library behind the
// stream-shaped handler chain syntax (`on X => debounce(500) => { ... }`).
// The compiler-side scaffolding (parse + emit) is covered by
// Spek.Tests.Emit.StreamOperatorTests; these tests drive the operators
// themselves through their public contract: Configure / OfferAsync /
// Dispatch / StopAsync.
//
// Timing note: DebounceOperator and ThrottleOperator take their time
// source from Configure's optional TimeProvider (the seam the generated
// actor code fills with the actor's clock), so the timing-sensitive
// tests here run on a ManualTimeProvider: offers and Advance calls are
// the whole schedule, timers fire synchronously inside Advance, and the
// assertions are exact — no margins, no polling. One real-clock smoke
// test per timer-based operator stays behind to prove the default
// (system-clock) path end-to-end.

/// <summary>
/// Records every dispatched message so tests can assert exactly what an
/// operator emitted, in order. The <see cref="ReceiveAsync"/> method has the
/// <c>Func&lt;T, Task&gt;</c> shape <see cref="StreamOperator{T}.Configure"/>
/// expects.
/// </summary>
internal sealed class RecordingSink<T>
{
    private readonly List<T> _items = new();
    private readonly object _gate = new();

    public Task ReceiveAsync(T message)
    {
        lock (_gate) { _items.Add(message); }
        return Task.CompletedTask;
    }

    public IReadOnlyList<T> Items
    {
        get { lock (_gate) { return _items.ToArray(); } }
    }
}

/// <summary>
/// Minimal user-defined operator (the README's extension point):
/// forwards every message unchanged and does not override
/// <see cref="StreamOperator{T}.StopAsync"/>, so it also exercises the
/// base-class defaults.
/// </summary>
internal sealed class PassThroughOperator<T> : StreamOperator<T>
{
    public override Task OfferAsync(T message) => Dispatch(message);
}

/// <summary>
/// Appends a tag to each string it forwards, so a compose chain's
/// execution order is visible in the emitted value ("x" through tags
/// "1" then "2" arrives as "x12"). Optionally records its tag into a
/// shared log when stopped, so StopAsync propagation order is
/// observable too.
/// </summary>
internal sealed class TaggingOperator : StreamOperator<string>
{
    private readonly string _tag;
    private readonly List<string>? _stopLog;

    public TaggingOperator(string tag, List<string>? stopLog = null)
    {
        _tag = tag;
        _stopLog = stopLog;
    }

    public override Task OfferAsync(string message) => Dispatch(message + _tag);

    public override Task StopAsync()
    {
        _stopLog?.Add(_tag);
        return Task.CompletedTask;
    }
}

/// <summary>
/// Contract of the abstract base: one-shot configuration, guarded
/// dispatch, and the no-op StopAsync default.
/// </summary>
public sealed class StreamOperatorContractTests
{
    [Fact]
    public void Configure_NullDispatch_Throws()
    {
        var op = new PassThroughOperator<string>();
        Assert.Throws<ArgumentNullException>(() => op.Configure(null!));
    }

    [Fact]
    public void Configure_CalledTwice_Throws()
    {
        var op = new PassThroughOperator<string>();
        op.Configure(_ => Task.CompletedTask);

        var ex = Assert.Throws<InvalidOperationException>(
            () => op.Configure(_ => Task.CompletedTask));
        Assert.Contains("already configured", ex.Message);
    }

    [Fact]
    public async Task Offer_BeforeConfigure_ThrowsOnDispatchAsync()
    {
        // A pass-through dispatches immediately, so offering before the
        // runtime has wired the chain surfaces the guard.
        var op = new PassThroughOperator<string>();

        var ex = await Assert.ThrowsAsync<InvalidOperationException>(
            () => op.OfferAsync("orphan"));
        Assert.Contains("not been configured", ex.Message);
    }

    [Fact]
    public void StopAsync_DefaultImplementation_CompletesSynchronously()
    {
        // PassThroughOperator does not override StopAsync — this pins the
        // base-class no-op (actors call it unconditionally on stop).
        var op = new PassThroughOperator<int>();
        Assert.True(op.StopAsync().IsCompletedSuccessfully);
    }
}

/// <summary>
/// DebounceOperator: trailing-edge debounce. OfferAsync records the
/// latest message, re-arms the quiet-window timer, and returns without
/// waiting; the emit happens on a timer callback once the source has
/// been quiet for the interval. On the manual clock that callback runs
/// synchronously inside <see cref="ManualTimeProvider.Advance"/>, so
/// each test asserts immediately after moving time — exactly.
/// </summary>
public sealed class DebounceStreamOperatorTests
{
    /// <summary>Polls until the sink holds <paramref name="count"/> items
    /// or the (generous, load-tolerant) timeout expires. Only the
    /// real-clock smoke test needs this; virtual-time tests assert
    /// synchronously after Advance.</summary>
    private static async Task WaitForEmitsAsync(
        RecordingSink<string> sink, int count)
    {
        var deadline = DateTime.UtcNow + TimeSpan.FromSeconds(10);
        while (sink.Items.Count < count)
        {
            Assert.True(DateTime.UtcNow < deadline,
                $"expected {count} emits, saw {sink.Items.Count}: [{string.Join(", ", sink.Items)}]");
            await Task.Delay(10);
        }
    }

    [Fact]
    public void NegativeInterval_Throws()
    {
        Assert.Throws<ArgumentOutOfRangeException>(
            () => { _ = new DebounceOperator<string>(-1); });
        Assert.Throws<ArgumentOutOfRangeException>(
            () => { _ = new DebounceOperator<string>(TimeSpan.FromMilliseconds(-1)); });
    }

    [Fact]
    public void Offer_ReturnsWithoutWaitingOutTheWindow()
    {
        // The mailbox-stall half of the old defect: an offer must never
        // block on the quiet window. It completes synchronously and the
        // emit has not happened yet.
        var sink = new RecordingSink<string>();
        var op = new DebounceOperator<string>(TimeSpan.FromMilliseconds(250));
        op.Configure(sink.ReceiveAsync);

        var pending = op.OfferAsync("only");

        Assert.True(pending.IsCompletedSuccessfully);
        Assert.Empty(sink.Items);
    }

    [Fact]
    public async Task LoneOffer_EmitsAfterQuietWindowAsync()
    {
        // The deliberate REAL-CLOCK smoke test: no clock passed to
        // Configure, so the default system clock arms a real timer and
        // the emit lands on a timer thread — proving the production
        // path end-to-end. Every other timing case runs on the manual
        // clock below.
        var sink = new RecordingSink<string>();
        var op = new DebounceOperator<string>(TimeSpan.FromMilliseconds(50));
        op.Configure(sink.ReceiveAsync);

        await op.OfferAsync("only");
        await WaitForEmitsAsync(sink, 1);

        Assert.Equal(new[] { "only" }, sink.Items);
    }

    [Fact]
    public async Task SerializedBurst_OnlyLatestEmitsAsync()
    {
        // The supersession half of the old defect, and exactly how the
        // emitted actor code drives the chain: the mailbox awaits each
        // OfferAsync before offering the next. A burst must still
        // collapse to ONE emit — the latest message.
        var clock = new ManualTimeProvider();
        var sink = new RecordingSink<string>();
        var op = new DebounceOperator<string>(200);
        op.Configure(sink.ReceiveAsync, clock);

        await op.OfferAsync("a");
        await op.OfferAsync("b");
        await op.OfferAsync("c");
        Assert.Empty(sink.Items);           // nothing fires until time moves

        clock.Advance(TimeSpan.FromMilliseconds(200));

        Assert.Equal(new[] { "c" }, sink.Items);
    }

    [Fact]
    public async Task OfferDuringQuietWindow_ResetsWindowAndSupersedesAsync()
    {
        // Each new offer inside the quiet window resets it. On the
        // manual clock the reset is assertable exactly: after two
        // supersessions, one tick short of the final window is silent
        // and the tick that completes it emits the final message only.
        var clock = new ManualTimeProvider();
        var sink = new RecordingSink<string>();
        var op = new DebounceOperator<string>(400);
        op.Configure(sink.ReceiveAsync, clock);

        await op.OfferAsync("stale-1");
        clock.Advance(TimeSpan.FromMilliseconds(399));
        await op.OfferAsync("stale-2");     // resets the window
        clock.Advance(TimeSpan.FromMilliseconds(399));
        await op.OfferAsync("final");       // resets it again
        clock.Advance(TimeSpan.FromMilliseconds(399));
        Assert.Empty(sink.Items);           // still one tick short

        clock.Advance(TimeSpan.FromMilliseconds(1));

        Assert.Equal(new[] { "final" }, sink.Items);
    }

    [Fact]
    public async Task SeparateQuietPeriods_EachOfferEmitsInOrderAsync()
    {
        // Offers separated by more than the window each get their own
        // emit, source order preserved.
        var clock = new ManualTimeProvider();
        var sink = new RecordingSink<string>();
        var op = new DebounceOperator<string>(20);
        op.Configure(sink.ReceiveAsync, clock);

        await op.OfferAsync("a");
        clock.Advance(TimeSpan.FromMilliseconds(20));
        Assert.Equal(new[] { "a" }, sink.Items);

        await op.OfferAsync("b");
        clock.Advance(TimeSpan.FromMilliseconds(20));

        Assert.Equal(new[] { "a", "b" }, sink.Items);
    }

    [Fact]
    public async Task ZeroInterval_EmitsEveryMessageInlineAsync()
    {
        // No quiet window: dispatch is synchronous, no waiting needed.
        var sink = new RecordingSink<string>();
        var op = new DebounceOperator<string>(0);
        op.Configure(sink.ReceiveAsync);

        await op.OfferAsync("a");
        await op.OfferAsync("b");
        await op.OfferAsync("c");

        Assert.Equal(new[] { "a", "b", "c" }, sink.Items);
    }

    [Fact]
    public async Task Stop_CancelsThePendingEmitAsync()
    {
        // The actor is stopping; a message emitted now could only
        // dead-letter. Stop must cancel the armed timer and drop the
        // held message — even hours of clock later, nothing emits.
        var clock = new ManualTimeProvider();
        var sink = new RecordingSink<string>();
        var op = new DebounceOperator<string>(100);
        op.Configure(sink.ReceiveAsync, clock);

        await op.OfferAsync("doomed");
        await op.StopAsync();

        clock.Advance(TimeSpan.FromHours(1));
        Assert.Empty(sink.Items);

        // Offers after stop are ignored, not faulted.
        await op.OfferAsync("late");
        clock.Advance(TimeSpan.FromHours(1));
        Assert.Empty(sink.Items);
    }
}

/// <summary>
/// ThrottleOperator: leading-edge throttle. The first message opens a
/// window and emits; everything else inside the window is dropped; the
/// first message after the window expires emits and opens the next
/// window. OfferAsync never delays, so most of these tests are fully
/// deterministic.
/// </summary>
public sealed class ThrottleStreamOperatorTests
{
    [Fact]
    public void NegativeInterval_Throws()
    {
        Assert.Throws<ArgumentOutOfRangeException>(
            () => { _ = new ThrottleOperator<string>(-1); });
        Assert.Throws<ArgumentOutOfRangeException>(
            () => { _ = new ThrottleOperator<string>(TimeSpan.FromMilliseconds(-1)); });
    }

    [Fact]
    public async Task FirstMessage_PassesImmediatelyAsync()
    {
        // Leading edge: no waiting for the window to fill before the
        // first emit.
        var sink = new RecordingSink<string>();
        var op = new ThrottleOperator<string>(TimeSpan.FromMinutes(1));
        op.Configure(sink.ReceiveAsync);

        await op.OfferAsync("first");

        Assert.Equal(new[] { "first" }, sink.Items);
    }

    [Fact]
    public async Task MessagesInsideWindow_AreDroppedAsync()
    {
        var sink = new RecordingSink<string>();
        var op = new ThrottleOperator<string>(TimeSpan.FromMinutes(1));
        op.Configure(sink.ReceiveAsync);

        await op.OfferAsync("kept");
        await op.OfferAsync("dropped-1");
        await op.OfferAsync("dropped-2");

        Assert.Equal(new[] { "kept" }, sink.Items);
    }

    [Fact]
    public async Task Burst_EmitsExactlyOneAsync()
    {
        var sink = new RecordingSink<int>();
        var op = new ThrottleOperator<int>(TimeSpan.FromMinutes(1));
        op.Configure(sink.ReceiveAsync);

        for (var i = 0; i < 100; i++)
            await op.OfferAsync(i);

        Assert.Equal(new[] { 0 }, sink.Items);
    }

    [Fact]
    public async Task MessageAfterWindowExpires_PassesAsync()
    {
        // The deliberate REAL-CLOCK smoke test: no clock passed to
        // Configure, so the window math reads the system clock's real
        // timestamps. The drop needs the second offer to land within
        // 250ms of the first (adjacent statements — generous), and the
        // late offer waits 700ms, comfortably past the 250ms window.
        // The exact boundary is pinned on the manual clock below.
        var sink = new RecordingSink<string>();
        var op = new ThrottleOperator<string>(TimeSpan.FromMilliseconds(250));
        op.Configure(sink.ReceiveAsync);

        await op.OfferAsync("first");
        await op.OfferAsync("inside-window");
        await Task.Delay(700);
        await op.OfferAsync("late");

        Assert.Equal(new[] { "first", "late" }, sink.Items);
    }

    [Fact]
    public async Task WindowBoundary_IsExact_UnderVirtualTimeAsync()
    {
        // The window math rides the configured clock's timestamps, so
        // virtual time pins the boundary exactly: one tick inside the
        // window still drops, and elapsed == interval reopens it
        // (leading edge: `elapsed >= interval` passes).
        var clock = new ManualTimeProvider();
        var sink = new RecordingSink<string>();
        var op = new ThrottleOperator<string>(TimeSpan.FromMilliseconds(250));
        op.Configure(sink.ReceiveAsync, clock);

        await op.OfferAsync("opens-window");
        clock.Advance(TimeSpan.FromMilliseconds(249));
        await op.OfferAsync("one-tick-early");      // 249ms elapsed — drop
        clock.Advance(TimeSpan.FromMilliseconds(1));
        await op.OfferAsync("exactly-on-boundary"); // 250ms elapsed — pass

        Assert.Equal(new[] { "opens-window", "exactly-on-boundary" }, sink.Items);
    }

    [Fact]
    public async Task ZeroInterval_PassesEveryMessageAsync()
    {
        // With a zero window every message satisfies `elapsed >= interval`.
        var sink = new RecordingSink<string>();
        var op = new ThrottleOperator<string>(0);
        op.Configure(sink.ReceiveAsync);

        await op.OfferAsync("a");
        await op.OfferAsync("b");
        await op.OfferAsync("c");

        Assert.Equal(new[] { "a", "b", "c" }, sink.Items);
    }
}

/// <summary>
/// DistinctOperator: distinct-until-changed (adjacent dedup keyed by a
/// selector), NOT set-based distinct — a key seen before re-emits as
/// soon as a different key came between. Fully synchronous, fully
/// deterministic.
/// </summary>
public sealed class DistinctStreamOperatorTests
{
    private sealed record Reading(string Sensor, int Value);

    [Fact]
    public void NullArguments_Throw()
    {
        Assert.Throws<ArgumentNullException>(
            () => { _ = new DistinctOperator<string, string>(null!); });
        Assert.Throws<ArgumentNullException>(
            () => { _ = new DistinctOperator<string, string>(s => s, null!); });
    }

    [Fact]
    public async Task FirstMessage_AlwaysEmitsAsync()
    {
        var sink = new RecordingSink<string>();
        var op = new DistinctOperator<string, string>(s => s);
        op.Configure(sink.ReceiveAsync);

        await op.OfferAsync("a");

        Assert.Equal(new[] { "a" }, sink.Items);
    }

    [Fact]
    public async Task AdjacentDuplicateKeys_AreSuppressedAsync()
    {
        var sink = new RecordingSink<string>();
        var op = new DistinctOperator<string, string>(s => s);
        op.Configure(sink.ReceiveAsync);

        await op.OfferAsync("a");
        await op.OfferAsync("a");
        await op.OfferAsync("a");

        Assert.Equal(new[] { "a" }, sink.Items);
    }

    [Fact]
    public async Task NonAdjacentDuplicates_ReEmitAsync()
    {
        // Pins the semantics: this is distinct-UNTIL-CHANGED, not a
        // seen-set. "a" after "b" emits again because only the previous
        // emit's key is remembered.
        var sink = new RecordingSink<string>();
        var op = new DistinctOperator<string, string>(s => s);
        op.Configure(sink.ReceiveAsync);

        await op.OfferAsync("a");
        await op.OfferAsync("b");
        await op.OfferAsync("a");

        Assert.Equal(new[] { "a", "b", "a" }, sink.Items);
    }

    [Fact]
    public async Task KeySelector_DedupesByKey_NotByMessageAsync()
    {
        // Same key, different payloads: the FIRST message of a key-run
        // is the one that emits; later same-key messages are dropped
        // even though they differ.
        var sink = new RecordingSink<Reading>();
        var op = new DistinctOperator<Reading, string>(r => r.Sensor);
        op.Configure(sink.ReceiveAsync);

        await op.OfferAsync(new Reading("temp", 21));
        await op.OfferAsync(new Reading("temp", 22));
        await op.OfferAsync(new Reading("humidity", 40));
        await op.OfferAsync(new Reading("temp", 23));

        Assert.Equal(
            new[]
            {
                new Reading("temp", 21),
                new Reading("humidity", 40),
                new Reading("temp", 23),
            },
            sink.Items);
    }

    [Fact]
    public async Task CustomComparer_IsRespectedAsync()
    {
        var sink = new RecordingSink<string>();
        var op = new DistinctOperator<string, string>(
            s => s, StringComparer.OrdinalIgnoreCase);
        op.Configure(sink.ReceiveAsync);

        await op.OfferAsync("HELLO");
        await op.OfferAsync("hello");
        await op.OfferAsync("world");

        Assert.Equal(new[] { "HELLO", "world" }, sink.Items);
    }

    [Fact]
    public async Task NullKeys_AreComparableAsync()
    {
        var sink = new RecordingSink<string?>();
        var op = new DistinctOperator<string?, string?>(s => s);
        op.Configure(sink.ReceiveAsync);

        await op.OfferAsync(null);      // first message always emits
        await op.OfferAsync(null);      // adjacent null key — suppressed
        await op.OfferAsync("x");       // key changed — emits
        await op.OfferAsync(null);      // changed back to null — emits

        Assert.Equal(new string?[] { null, "x", null }, sink.Items);
    }
}

/// <summary>
/// ComposeOperator: wraps several operators as one chain link. Inner
/// operators are lazily wired on the first offer and run in declaration
/// order; StopAsync fans out to every inner operator (the async
/// StopAsync state machine the coverage report flagged).
/// </summary>
public sealed class ComposeStreamOperatorTests
{
    [Fact]
    public void NullOrEmptyInner_Throws()
    {
        Assert.Throws<ArgumentNullException>(
            () => { _ = new ComposeOperator<string>(null!); });
        Assert.Throws<ArgumentException>(
            () => { _ = new ComposeOperator<string>(); });
    }

    [Fact]
    public async Task SingleInner_PassesThroughAsync()
    {
        var sink = new RecordingSink<string>();
        var op = new ComposeOperator<string>(new PassThroughOperator<string>());
        op.Configure(sink.ReceiveAsync);

        await op.OfferAsync("solo");

        Assert.Equal(new[] { "solo" }, sink.Items);
    }

    [Fact]
    public async Task InnerOperators_RunInDeclarationOrderAsync()
    {
        // Each tagger appends its tag when the message passes through
        // it — "x12" proves the first-declared operator ran first
        // (reversed wiring would produce "x21").
        var sink = new RecordingSink<string>();
        var op = new ComposeOperator<string>(
            new TaggingOperator("1"),
            new TaggingOperator("2"));
        op.Configure(sink.ReceiveAsync);

        await op.OfferAsync("x");

        Assert.Equal(new[] { "x12" }, sink.Items);
    }

    [Fact]
    public async Task RepeatOffers_DoNotRewireTheChainAsync()
    {
        // The lazy wiring must run exactly once — a second offer that
        // re-Configure()'d the inner operators would throw "already
        // configured".
        var sink = new RecordingSink<string>();
        var op = new ComposeOperator<string>(
            new TaggingOperator("!"),
            new PassThroughOperator<string>());
        op.Configure(sink.ReceiveAsync);

        await op.OfferAsync("a");
        await op.OfferAsync("b");

        Assert.Equal(new[] { "a!", "b!" }, sink.Items);
    }

    [Fact]
    public async Task InnerFiltering_AppliesThroughTheChainAsync()
    {
        // distinct-then-tag: the duplicate is filtered by the first
        // inner operator and never reaches the second.
        var sink = new RecordingSink<string>();
        var op = new ComposeOperator<string>(
            new DistinctOperator<string, string>(s => s),
            new TaggingOperator("+"));
        op.Configure(sink.ReceiveAsync);

        await op.OfferAsync("a");
        await op.OfferAsync("a");
        await op.OfferAsync("b");

        Assert.Equal(new[] { "a+", "b+" }, sink.Items);
    }

    [Fact]
    public async Task StopAsync_PropagatesToAllInner_InDeclarationOrderAsync()
    {
        var stopLog = new List<string>();
        var op = new ComposeOperator<string>(
            new TaggingOperator("first", stopLog),
            new TaggingOperator("second", stopLog),
            new TaggingOperator("third", stopLog));
        op.Configure(new RecordingSink<string>().ReceiveAsync);
        await op.OfferAsync("warm-up");   // wire the chain first

        await op.StopAsync();

        Assert.Equal(new[] { "first", "second", "third" }, stopLog);
    }

    [Fact]
    public async Task StopAsync_BeforeAnyOffer_StillPropagatesAsync()
    {
        // An actor can stop before its stream handler ever received a
        // message — the inner chain is unwired, but stop must still
        // reach every inner operator.
        var stopLog = new List<string>();
        var op = new ComposeOperator<string>(
            new TaggingOperator("a", stopLog),
            new TaggingOperator("b", stopLog));

        await op.StopAsync();

        Assert.Equal(new[] { "a", "b" }, stopLog);
    }

    [Fact]
    public async Task Offer_WithoutOuterConfigure_ThrowsOnEmitAsync()
    {
        // The compose wires its inner chain lazily, but its own dispatch
        // is still guarded: emitting without the runtime having called
        // Configure surfaces the base-class error.
        var op = new ComposeOperator<string>(new PassThroughOperator<string>());

        await Assert.ThrowsAsync<InvalidOperationException>(
            () => op.OfferAsync("orphan"));
    }

    [Fact]
    public async Task Clock_PropagatesToInnerOperatorsAsync()
    {
        // The composed operator's Configure clock must reach its inner
        // operators during lazy wiring — a debounce inside a compose
        // fires on the manual clock's Advance, not a real timer.
        var clock = new ManualTimeProvider();
        var sink = new RecordingSink<string>();
        var op = new ComposeOperator<string>(
            new TaggingOperator("!"),
            new DebounceOperator<string>(100));
        op.Configure(sink.ReceiveAsync, clock);

        await op.OfferAsync("a");
        await op.OfferAsync("b");
        Assert.Empty(sink.Items);           // held by the inner debounce

        clock.Advance(TimeSpan.FromMilliseconds(100));

        Assert.Equal(new[] { "b!" }, sink.Items);
    }
}

/// <summary>
/// StreamOperators: the lowercase factory surface the emitted C# calls
/// (`debounce&lt;Tick&gt;(500)`, `compose&lt;T&gt;(...)`, and the
/// compiler-only `typed` identity helper).
/// </summary>
public sealed class StreamOperatorsFactoryTests
{
    [Fact]
    public void debounce_ReturnsDebounceOperator()
    {
        Assert.IsType<DebounceOperator<string>>(
            StreamOperators.debounce<string>(10));
        Assert.IsType<DebounceOperator<string>>(
            StreamOperators.debounce<string>(TimeSpan.FromMilliseconds(10)));
    }

    [Fact]
    public void throttle_ReturnsThrottleOperator()
    {
        Assert.IsType<ThrottleOperator<string>>(
            StreamOperators.throttle<string>(10));
        Assert.IsType<ThrottleOperator<string>>(
            StreamOperators.throttle<string>(TimeSpan.FromMilliseconds(10)));
    }

    [Fact]
    public void distinct_ReturnsDistinctOperator()
    {
        Assert.IsType<DistinctOperator<string, int>>(
            StreamOperators.distinct<string, int>(by: s => s.Length));
    }

    [Fact]
    public void compose_ReturnsComposeOperator()
    {
        Assert.IsType<ComposeOperator<string>>(
            StreamOperators.compose<string>(new PassThroughOperator<string>()));
    }

    [Fact]
    public void compose_WithNoOperators_Throws()
    {
        Assert.Throws<ArgumentException>(
            () => { _ = StreamOperators.compose<string>(); });
    }

    [Fact]
    public void typed_ReturnsTheSameInstance()
    {
        var op = new PassThroughOperator<string>();
        Assert.Same(op, StreamOperators.typed<string>(op));
    }

    [Fact]
    public async Task FactoryBuiltChain_WorksEndToEndAsync()
    {
        // The shape the emitter produces for a two-step chain:
        // compose<T>(distinct(...), throttle(...)). throttle(0) passes
        // everything, so the observable filtering is distinct's.
        var sink = new RecordingSink<string>();
        var chain = StreamOperators.compose<string>(
            StreamOperators.distinct<string, string>(by: s => s),
            StreamOperators.throttle<string>(0));
        chain.Configure(sink.ReceiveAsync);

        await chain.OfferAsync("a");
        await chain.OfferAsync("a");
        await chain.OfferAsync("b");
        await chain.OfferAsync("a");

        Assert.Equal(new[] { "a", "b", "a" }, sink.Items);
    }
}
