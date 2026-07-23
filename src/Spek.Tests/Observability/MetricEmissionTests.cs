using Spek;
using Spek.Observability;
using Spek.Runtime;
using Xunit;

namespace Spek.Tests.Observability;

/// <summary>
/// Runtime emits the well-known metrics
/// (<see cref="SpekMetricNames"/>) when an <see cref="IMetricSink"/>
/// is registered on the <see cref="ActorSystem"/>. The default
/// <see cref="NullMetricSink"/> path is exercised by every other
/// runtime test in the suite (no observable change), so this file
/// focuses on the active-sink path.
/// </summary>
public sealed class MetricEmissionTests
{
    public sealed record Tick();

    /// <summary>
    /// Static signal so the test can wait for handler completion
    /// without reaching for the internal `Slot` accessor on ActorRef.
    /// One signal per test; the per-test reset is handled by
    /// <see cref="TestSignal.Reset"/> at the top of each fact.
    /// </summary>
    private static class TestSignal
    {
        public static TaskCompletionSource Done { get; private set; } = new();
        public static void Reset() => Done = new();
    }

    public sealed class TickActor : ActorBase
    {
        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            if (message is Tick) TestSignal.Done.TrySetResult();
            return Task.CompletedTask;
        }
    }

    private sealed class CapturingSink : IMetricSink
    {
        public readonly List<(string Name, long Delta)> Counters = new();
        public readonly List<(string Name, double Value)> Gauges = new();
        public readonly List<(string Name, double Value)> Histograms = new();

        public void Counter(string name, long delta = 1, IReadOnlyList<KeyValuePair<string, object?>>? tags = null)
        {
            lock (Counters) Counters.Add((name, delta));
        }
        public void Gauge(string name, double value, IReadOnlyList<KeyValuePair<string, object?>>? tags = null)
        {
            lock (Gauges) Gauges.Add((name, value));
        }
        public void Histogram(string name, double value, IReadOnlyList<KeyValuePair<string, object?>>? tags = null)
        {
            lock (Histograms) Histograms.Add((name, value));
        }
    }

    [Fact]
    public async Task Enqueue_EmitsMailboxDepthGaugeAsync()
    {
        TestSignal.Reset();
        var sink = new CapturingSink();
        using var system = new ActorSystem("metric-test").UseMetricSink(sink);

        var actor = system.Spawn<TickActor>();
        actor.Tell(new Tick());

        await TestSignal.Done.Task.WaitAsync(TimeSpan.FromSeconds(2));

        Assert.Contains(sink.Gauges, g => g.Name == SpekMetricNames.MailboxDepth);
    }

    [Fact]
    public async Task Dispatch_EmitsCounterAndHistogramAsync()
    {
        TestSignal.Reset();
        var sink = new CapturingSink();
        using var system = new ActorSystem("metric-test").UseMetricSink(sink);

        var actor = system.Spawn<TickActor>();
        actor.Tell(new Tick());

        await TestSignal.Done.Task.WaitAsync(TimeSpan.FromSeconds(2));
        // Give the dispatch loop a moment to record post-await metrics.
        await Task.Delay(50);

        Assert.Contains(sink.Counters, c => c.Name == SpekMetricNames.MailboxDispatch);
        Assert.Contains(sink.Histograms, h => h.Name == SpekMetricNames.HandlerDurationMs);
    }

    [Fact]
    public void DefaultSink_IsNullSink_NoExceptions()
    {
        // Sanity: a system without a registered sink uses the null
        // sink, which absorbs every call. Spawning + telling must
        // not throw.
        using var system = new ActorSystem("no-sink-test");
        Assert.IsType<NullMetricSink>(system.Metrics);

        var actor = system.Spawn<TickActor>();
        actor.Tell(new Tick());   // null sink absorbs the gauge call
    }

    [Fact]
    public void NullSink_NotEnabled_RealSinkEnabled()
    {
        // The discriminator the hot path checks to skip building per-message
        // tag arrays. Null sink disabled; a real sink inherits Enabled=true.
        Assert.False(NullMetricSink.Instance.Enabled);
        Assert.True(((IMetricSink)new CapturingSink()).Enabled);
    }

    [Fact]
    public void TraceContext_NoActivity_ReturnsNullContext()
    {
        // Sanity check at the abstraction level — null trace context
        // is reachable and exposes the documented zero-value surface.
        var ctx = NullTraceContext.Instance;
        Assert.False(ctx.IsActive);
        Assert.Empty(ctx.TraceId);
        Assert.Empty(ctx.SpanId);
        Assert.Empty(ctx.Baggage);
    }
}
