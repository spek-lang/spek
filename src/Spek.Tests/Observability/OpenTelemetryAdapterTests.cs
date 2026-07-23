using System.Diagnostics;
using System.Diagnostics.Metrics;
using Spek;
using Spek.Observability;
using Spek.Observability.OpenTelemetry;
using Spek.Runtime;
using Xunit;

namespace Spek.Tests.Observability;

/// <summary>
/// Verifies the OpenTelemetry adapter genuinely
/// emits to the standard .NET telemetry primitives. We attach a
/// <see cref="MeterListener"/> and an <see cref="ActivityListener"/>
/// directly (no OTel SDK) and assert that the runtime's metric +
/// trace calls reach them.
/// </summary>
public sealed class OpenTelemetryAdapterTests
{
    private sealed class PingActor : ActorBase
    {
        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            // Acknowledge to the asker so AskAsync completes.
            sender.Tell("pong", _selfRef!);
            return Task.CompletedTask;
        }
    }

    [Fact]
    public async Task MeterListener_ObservesMailboxDispatchCounterAsync()
    {
        // ─── Arrange: hook a listener onto the Spek.Runtime meter ──────
        long observed = 0;
        using var listener = new MeterListener
        {
            InstrumentPublished = (instrument, l) =>
            {
                if (instrument.Meter.Name == MeterMetricSink.MeterName &&
                    instrument.Name == SpekMetricNames.MailboxDispatch)
                {
                    l.EnableMeasurementEvents(instrument);
                }
            },
        };
        listener.SetMeasurementEventCallback<long>((_, value, _, _) =>
            Interlocked.Add(ref observed, value));
        listener.Start();

        var system = new ActorSystem("otel-meter-test").UseOpenTelemetryMetrics();
        var actor = system.Spawn<PingActor>();

        // ─── Act: send a few messages, collect dispatch counters ───────
        for (int i = 0; i < 5; i++)
            actor.Tell("ping", actor);

        // Give the dispatcher a moment to drain.
        await WaitForAsync(() => Interlocked.Read(ref observed) >= 5, timeoutMs: 2000);

        // ─── Assert ────────────────────────────────────────────────────
        Assert.True(observed >= 5,
            $"Expected at least 5 dispatch events, observed {observed}.");
    }

    [Fact]
    public async Task ActivityListener_ObservesDispatchSpansForEachMessageAsync()
    {
        // ─── Arrange: hook an ActivityListener onto Spek.Runtime ───────
        int spanCount = 0;
        using var listener = new ActivityListener
        {
            ShouldListenTo = source => source.Name == "Spek.Runtime",
            // Both must return AllData(WithRecording) for the runtime's
            // StartActivity to actually create an Activity. Without this,
            // StartActivity returns null and the runtime emits no span.
            Sample = (ref ActivityCreationOptions<ActivityContext> _) =>
                ActivitySamplingResult.AllDataAndRecorded,
            ActivityStopped = activity =>
            {
                if (activity.Source.Name == "Spek.Runtime")
                    Interlocked.Increment(ref spanCount);
            },
        };
        ActivitySource.AddActivityListener(listener);

        var system = new ActorSystem("otel-trace-test");
        var actor = system.Spawn<PingActor>();

        // ─── Act ──────────────────────────────────────────────────────
        for (int i = 0; i < 3; i++)
            actor.Tell("ping", actor);

        await WaitForAsync(() => Volatile.Read(ref spanCount) >= 3, timeoutMs: 2000);

        // ─── Assert ────────────────────────────────────────────────────
        Assert.True(spanCount >= 3,
            $"Expected at least 3 dispatch spans, observed {spanCount}.");
    }

    [Fact]
    public async Task TraceContext_PropagatesAcrossTellBoundaryAsync()
    {
        // Verify that an Activity active when Tell() is called becomes
        // the parent of the receiving handler's dispatch span — that's
        // the headline trace-propagation guarantee.
        //
        // ActivityListeners are process-wide, so tests running in
        // parallel can both fire on Spek.Runtime spans. We isolate by
        // filtering captured spans to ones whose ParentSpanId matches
        // the specific parent we created in this test method.
        var capturedActivities = new System.Collections.Concurrent.ConcurrentBag<Activity>();
        using var listener = new ActivityListener
        {
            ShouldListenTo = source => source.Name == "Spek.Runtime" || source.Name == "Test.Caller",
            Sample = (ref ActivityCreationOptions<ActivityContext> _) =>
                ActivitySamplingResult.AllDataAndRecorded,
            ActivityStopped = activity =>
            {
                if (activity.Source.Name == "Spek.Runtime")
                    capturedActivities.Add(activity);
            },
        };
        ActivitySource.AddActivityListener(listener);

        var system = new ActorSystem("otel-propagation-test");
        var actor = system.Spawn<PingActor>();

        // ─── Act: open a parent activity, Tell from inside it ──────────
        using var callerSource = new ActivitySource("Test.Caller");
        ActivityContext parentContext;
        using (var parent = callerSource.StartActivity("test.parent"))
        {
            Assert.NotNull(parent);
            parentContext = parent!.Context;
            actor.Tell("ping", actor);
        }

        // ─── Assert: find the runtime span whose parent is OUR parent ──
        await WaitForAsync(
            () => capturedActivities.Any(a => a.ParentSpanId == parentContext.SpanId),
            timeoutMs: 2000);

        var match = capturedActivities.FirstOrDefault(a =>
            a.ParentSpanId == parentContext.SpanId);
        Assert.NotNull(match);
        Assert.Equal(parentContext.TraceId, match!.TraceId);
    }

    [Fact]
    public void MeterMetricSink_HistogramEmits_TaggedValues()
    {
        var observed = new List<(double Value, string? RegionTag)>();
        using var listener = new MeterListener
        {
            InstrumentPublished = (instrument, l) =>
            {
                if (instrument.Meter.Name == MeterMetricSink.MeterName &&
                    instrument.Name == SpekMetricNames.RegionLockWaitMs)
                {
                    l.EnableMeasurementEvents(instrument);
                }
            },
        };
        listener.SetMeasurementEventCallback<double>((_, value, tags, _) =>
        {
            string? region = null;
            for (int i = 0; i < tags.Length; i++)
                if (tags[i].Key == "region") region = tags[i].Value?.ToString();
            lock (observed) observed.Add((value, region));
        });
        listener.Start();

        using var sink = new MeterMetricSink();
        sink.Histogram(SpekMetricNames.RegionLockWaitMs, 12.5,
            new[] { new KeyValuePair<string, object?>("region", "MarketCache") });
        sink.Histogram(SpekMetricNames.RegionLockWaitMs, 0.3,
            new[] { new KeyValuePair<string, object?>("region", "MarketCache") });

        Assert.Contains(observed, o => o.Value == 12.5 && o.RegionTag == "MarketCache");
        Assert.Contains(observed, o => o.Value == 0.3  && o.RegionTag == "MarketCache");
    }

    private static async Task WaitForAsync(Func<bool> condition, int timeoutMs)
    {
        var sw = Stopwatch.StartNew();
        while (sw.ElapsedMilliseconds < timeoutMs)
        {
            if (condition()) return;
            await Task.Delay(20);
        }
    }
}
