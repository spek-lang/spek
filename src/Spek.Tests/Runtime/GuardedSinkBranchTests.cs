using Spek.Observability;
using Spek.Runtime;
using Xunit;

namespace Spek.Tests.Runtime;

/// <summary>
/// The hardening boundary around user-supplied sinks (GuardedSinks.cs).
/// The runtime wraps every dead-letter sink handed to the
/// <see cref="ActorSystem"/> constructor and every metric sink registered
/// via <see cref="ActorSystem.UseMetricSink"/>, so a throwing user sink
/// can never convert a failure REPORT into a new failure. Pinned here,
/// through the public surface only (the wrappers are internal):
/// <list type="bullet">
///   <item>A throwing dead-letter sink: one stderr note, the call does not
///         escape, subsequent dead letters still reach the sink, and an
///         actor whose dispatch path reports through it keeps serving
///         (no supervision trip).</item>
///   <item>A throwing metric sink: Counter / Gauge / Histogram each get the
///         same treatment, and a full dispatch (which fires all three on the
///         hot path) neither crashes nor stops the actor.</item>
///   <item>Pass-through fidelity for every method of both wrappers,
///         including the <see cref="IMetricSink.Enabled"/> flag in both
///         directions.</item>
/// </list>
/// </summary>
public class GuardedSinkBranchTests
{
    private static readonly TimeSpan AskTimeout = TimeSpan.FromSeconds(15);

    // ─── messages ────────────────────────────────────────────────────────────
    public record Ping();
    public record Pong();
    public record Unknown();

    private sealed class PingPong : ActorBase
    {
        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            _currentSender = sender;
            switch (message)
            {
                case Ping: sender.Tell(new Pong()); break;
                default:   Unhandled(message); break;   // → dead-letter sink
            }
            return Task.CompletedTask;
        }
    }

    // ─── sink doubles ────────────────────────────────────────────────────────

    private sealed class ThrowingDeadLetterSink : IDeadLetterSink
    {
        public int Calls;
        public void DeadLetter(object message, string reason, Exception? cause)
        {
            Interlocked.Increment(ref Calls);
            throw new InvalidOperationException("dead-letter sink boom");
        }
    }

    /// <summary>Throws on the first call only, records everything after —
    /// proves one bad report doesn't poison the channel.</summary>
    private sealed class FlakyDeadLetterSink : IDeadLetterSink
    {
        private int _calls;
        private readonly RecordingDeadLetterSink _rest = new();
        public IReadOnlyList<DeadLetterRecord> RecordsAfterFirst => _rest.Records;

        public void DeadLetter(object message, string reason, Exception? cause)
        {
            if (Interlocked.Increment(ref _calls) == 1)
                throw new InvalidOperationException("first report boom");
            _rest.DeadLetter(message, reason, cause);
        }
    }

    private sealed class ThrowingMetricSink : IMetricSink
    {
        // Enabled stays the interface default (true) so the runtime's
        // Enabled-gated hot paths (mailbox-depth gauge) actually call in.
        public int Calls;
        public void Counter(string name, long delta = 1,
            IReadOnlyList<KeyValuePair<string, object?>>? tags = null)
        { Interlocked.Increment(ref Calls); throw new InvalidOperationException("counter boom"); }
        public void Gauge(string name, double value,
            IReadOnlyList<KeyValuePair<string, object?>>? tags = null)
        { Interlocked.Increment(ref Calls); throw new InvalidOperationException("gauge boom"); }
        public void Histogram(string name, double value,
            IReadOnlyList<KeyValuePair<string, object?>>? tags = null)
        { Interlocked.Increment(ref Calls); throw new InvalidOperationException("histogram boom"); }
    }

    private sealed record MetricCall(
        string Method, string Name, double Value,
        IReadOnlyList<KeyValuePair<string, object?>>? Tags);

    private sealed class RecordingMetricSink : IMetricSink
    {
        private readonly List<MetricCall> _calls = new();
        private readonly object _gate = new();
        public bool EnabledFlag = true;

        public bool Enabled => EnabledFlag;
        public IReadOnlyList<MetricCall> Calls
        {
            get { lock (_gate) return _calls.ToArray(); }
        }

        public void Counter(string name, long delta = 1,
            IReadOnlyList<KeyValuePair<string, object?>>? tags = null)
        { lock (_gate) _calls.Add(new MetricCall("Counter", name, delta, tags)); }
        public void Gauge(string name, double value,
            IReadOnlyList<KeyValuePair<string, object?>>? tags = null)
        { lock (_gate) _calls.Add(new MetricCall("Gauge", name, value, tags)); }
        public void Histogram(string name, double value,
            IReadOnlyList<KeyValuePair<string, object?>>? tags = null)
        { lock (_gate) _calls.Add(new MetricCall("Histogram", name, value, tags)); }
    }

    // ─── GuardedDeadLetterSink: throwing branch ──────────────────────────────

    [Fact]
    public void DeadLetterSink_Throws_CallDoesNotEscape_AndStderrNoteIsWritten()
    {
        var sink = new ThrowingDeadLetterSink();
        using var system = new ActorSystem("guarded-dls", deadLetterSink: sink);

        // DeliverIncoming to an unknown path dead-letters SYNCHRONOUSLY on
        // this thread — a deterministic capture window for stderr. Console
        // redirection is process-global, so retry a few times in case another
        // parallel test swaps Console.Error inside our window.
        var found = false;
        for (var attempt = 0; attempt < 5 && !found; attempt++)
        {
            var stderr = CaptureStderr(() =>
                system.DeliverIncoming("no/such/root", new Ping(), sender: null));
            found = stderr.Contains($"dead-letter sink '{nameof(ThrowingDeadLetterSink)}' threw")
                    && stderr.Contains(nameof(InvalidOperationException));
        }

        Assert.True(found,
            "expected a '[spek] dead-letter sink ... threw' note on stderr");
        Assert.True(sink.Calls >= 1);   // the user sink WAS invoked; the guard ate the throw
    }

    [Fact]
    public void DeadLetterSink_ThrowsOnce_SubsequentDeadLettersStillDelivered()
    {
        var sink = new FlakyDeadLetterSink();
        using var system = new ActorSystem("guarded-dls-flaky", deadLetterSink: sink);

        // First report throws inside the sink; the guard swallows it.
        system.DeliverIncoming("missing-a", new Ping(), sender: null);
        // Second report must still reach the sink — the channel is not poisoned.
        system.DeliverIncoming("missing-b", new Unknown(), sender: null);

        var record = Assert.Single(sink.RecordsAfterFirst);
        Assert.IsType<Unknown>(record.Message);
        Assert.Contains("missing-b", record.Reason);
    }

    [Fact]
    public async Task DeadLetterSink_Throws_OnTheDispatchPath_DoesNotTripSupervisionAsync()
    {
        var sink = new ThrowingDeadLetterSink();
        using var system = new ActorSystem("guarded-dls-dispatch", deadLetterSink: sink);
        var actor = system.Spawn<PingPong>();

        // Unhandled routes to the throwing sink from INSIDE the dispatch
        // path. Unguarded, that throw would be mis-attributed as a handler
        // failure and stop this root actor.
        actor.Tell(new Unknown());

        // FIFO mailbox: by the time the ask completes, the Unknown dispatch
        // (and its throwing sink call) has already run.
        var reply = await actor.AskAsync<Pong>(new Ping(), AskTimeout);
        Assert.IsType<Pong>(reply);
        Assert.False(actor.IsStopped);
        Assert.True(sink.Calls >= 1);
    }

    // ─── GuardedDeadLetterSink: pass-through fidelity ────────────────────────

    [Fact]
    public void DeadLetterSink_PassThrough_ForwardsMessageReasonAndCauseVerbatim()
    {
        var inner = new RecordingDeadLetterSink();
        using var system = new ActorSystem("guarded-dls-passthrough", deadLetterSink: inner);

        // ActorSystem.DeadLetterSink IS the guarded wrapper — calling it
        // directly exercises the happy path end to end.
        var message = new Ping();
        var cause = new TimeoutException("original cause");
        system.DeadLetterSink.DeadLetter(message, "custom reason", cause);

        var record = Assert.Single(inner.Records);
        Assert.Same(message, record.Message);
        Assert.Equal("custom reason", record.Reason);
        Assert.Same(cause, record.Cause);
    }

    // ─── GuardedMetricSink: pass-through fidelity, every method ──────────────

    [Fact]
    public void MetricSink_PassThrough_ForwardsAllThreeMethods_AndEnabledBothWays()
    {
        var inner = new RecordingMetricSink();
        using var system = new ActorSystem("guarded-metrics-passthrough");
        system.UseMetricSink(inner);

        // system.Metrics is the guarded wrapper.
        var tags = new[] { new KeyValuePair<string, object?>("k", "v") };
        system.Metrics.Counter("spek.test.counter", 5, tags);
        system.Metrics.Gauge("spek.test.gauge", 1.5, tags);
        system.Metrics.Histogram("spek.test.histogram", 2.5, tags);

        Assert.Collection(inner.Calls,
            c =>
            {
                Assert.Equal(("Counter", "spek.test.counter", 5d), (c.Method, c.Name, c.Value));
                Assert.Same(tags, c.Tags);
            },
            c =>
            {
                Assert.Equal(("Gauge", "spek.test.gauge", 1.5), (c.Method, c.Name, c.Value));
                Assert.Same(tags, c.Tags);
            },
            c =>
            {
                Assert.Equal(("Histogram", "spek.test.histogram", 2.5), (c.Method, c.Name, c.Value));
                Assert.Same(tags, c.Tags);
            });

        // Enabled is a pure pass-through — both directions.
        Assert.True(system.Metrics.Enabled);
        inner.EnabledFlag = false;
        Assert.False(system.Metrics.Enabled);
    }

    [Fact]
    public void MetricSink_PassThrough_DefaultDeltaAndNullTags()
    {
        var inner = new RecordingMetricSink();
        using var system = new ActorSystem("guarded-metrics-defaults");
        system.UseMetricSink(inner);

        system.Metrics.Counter("spek.test.default");   // delta defaults to 1, tags to null

        var call = Assert.Single(inner.Calls);
        Assert.Equal(("Counter", "spek.test.default", 1d), (call.Method, call.Name, call.Value));
        Assert.Null(call.Tags);
    }

    [Fact]
    public void UseMetricSink_Null_ThrowsArgumentNull()
    {
        using var system = new ActorSystem("guarded-metrics-null");
        Assert.Throws<ArgumentNullException>(() => system.UseMetricSink(null!));
    }

    // ─── GuardedMetricSink: throwing branch, every method ────────────────────

    [Fact]
    public void MetricSink_ThrowsOnEveryMethod_NothingEscapes_AndStderrNotesAreWritten()
    {
        var sink = new ThrowingMetricSink();
        using var system = new ActorSystem("guarded-metrics-throwing");
        system.UseMetricSink(sink);

        // Same retry rationale as the dead-letter stderr test: the calls are
        // synchronous, but Console.Error is process-global under the
        // parallel suite.
        var found = false;
        for (var attempt = 0; attempt < 5 && !found; attempt++)
        {
            var stderr = CaptureStderr(() =>
            {
                // Must not throw — that IS the guarantee under test.
                system.Metrics.Counter("spek.test.counter");
                system.Metrics.Gauge("spek.test.gauge", 1.0);
                system.Metrics.Histogram("spek.test.histogram", 2.0);
            });
            found = CountOccurrences(stderr,
                $"metric sink '{nameof(ThrowingMetricSink)}' threw") >= 3;
        }

        Assert.True(found,
            "expected one '[spek] metric sink ... threw' note per method on stderr");
        Assert.True(sink.Calls >= 3);
    }

    [Fact]
    public async Task MetricSink_Throws_OnTheHotPath_ActorKeepsServing_NoSupervisionTripAsync()
    {
        // A real dispatch fires the mailbox-depth gauge (enqueue), the
        // dispatch counter (inside the handler try) and the handler-duration
        // histogram (after the handler). All three throw here; unguarded,
        // the in-try ones would be mis-attributed as handler failures and
        // stop the actor.
        var deadLetters = new RecordingDeadLetterSink();
        var metrics = new ThrowingMetricSink();
        using var system = new ActorSystem("guarded-metrics-hotpath", deadLetterSink: deadLetters);
        system.UseMetricSink(metrics);

        var actor = system.Spawn<PingPong>();
        var reply = await actor.AskAsync<Pong>(new Ping(), AskTimeout);

        Assert.IsType<Pong>(reply);
        Assert.False(actor.IsStopped);
        Assert.True(metrics.Calls >= 3, $"expected >=3 metric calls, saw {metrics.Calls}");
        // No supervision activity: nothing was dead-lettered as a handler failure.
        Assert.DoesNotContain(deadLetters.Records, r => r.Reason.Contains("handler threw"));
    }

    // ─── helpers ─────────────────────────────────────────────────────────────

    private static string CaptureStderr(Action act)
    {
        var original = Console.Error;
        var captured = new StringWriter();
        Console.SetError(captured);
        try { act(); }
        finally { Console.SetError(original); }
        return captured.ToString();
    }

    private static int CountOccurrences(string haystack, string needle)
    {
        var count = 0;
        for (var i = haystack.IndexOf(needle, StringComparison.Ordinal);
             i >= 0;
             i = haystack.IndexOf(needle, i + needle.Length, StringComparison.Ordinal))
            count++;
        return count;
    }
}
