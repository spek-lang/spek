using System.Diagnostics;
using Spek.Observability;
using Spek.Runtime;
using Spek.Testing;
using Xunit;

namespace Spek.Tests.Runtime;

/// <summary>
/// The trace-context propagation path: the runtime wraps a message in its
/// internal traced envelope only while an <see cref="ActivityListener"/> is
/// attached to the <c>"Spek.Runtime"</c> <see cref="ActivitySource"/>, and
/// each handler dispatch then opens a <c>spek.actor.{Type}.handler</c> span
/// parented on the sender's context. Pinned here:
/// <list type="bullet">
///   <item>Parent/child <see cref="Activity"/> linkage across TWO mailbox
///         hops — test → actor A → actor B — the actor-to-actor propagation
///         the OpenTelemetry adapter tests don't cover.</item>
///   <item><c>ActorBase.TraceContext</c> (the <c>self.TraceContext</c>
///         surface) inside a traced handler: active, carries the caller's
///         trace id, exposes the handler span's id, round-trips baggage,
///         skips null-valued baggage entries, and chains fluently.</item>
///   <item>With no Activity current the same property returns the
///         <see cref="NullTraceContext"/> singleton — empty ids, empty
///         baggage, no-op <c>WithBaggage</c>.</item>
/// </list>
/// The zero-listener fast path additionally promises "no wrapper
/// allocation", but the envelope type is internal and unwrapped before any
/// observable surface (inbox observers and mailbox snapshots both see the
/// unwrapped message), so that half is not assertable from here — the perf
/// suite guards it.
/// </summary>
public class TraceContextTests
{
    // ─── messages ────────────────────────────────────────────────────────────
    public record Ping();
    public record Hop();
    public record Done();
    public record Inspect();
    public record TraceReport(
        bool HadActivity,
        bool IsActive,
        string TraceId,
        string SpanId,
        bool FluentReturnsSameInstance,
        bool IsNullSingleton,
        IReadOnlyDictionary<string, string> Baggage);

    // ─── helper actors ───────────────────────────────────────────────────────

    /// <summary>First hop: forwards to the next actor from inside its handler,
    /// so the enqueue happens under the handler's dispatch span.</summary>
    private sealed class Forwarder : ActorBase
    {
        private readonly ActorRef _next;
        public Forwarder(ActorRef next) => _next = next;

        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            _currentSender = sender;
            if (message is Ping) _next.Tell(new Hop());
            return Task.CompletedTask;
        }
    }

    /// <summary>Second hop: signals the probe when the message lands.</summary>
    private sealed class Receiver : ActorBase
    {
        private readonly ActorRef _done;
        public Receiver(ActorRef done) => _done = done;

        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            _currentSender = sender;
            if (message is Hop) _done.Tell(new Done());
            return Task.CompletedTask;
        }
    }

    /// <summary>Reads <c>self.TraceContext</c> inside the handler and reports
    /// everything the test needs to assert on.</summary>
    private sealed class TraceInspector : ActorBase
    {
        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            _currentSender = sender;
            if (message is not Inspect) return Task.CompletedTask;

            var hadActivity = Activity.Current is not null;
            var ctx = TraceContext;

            // Fluent contract: WithBaggage returns the context it mutated
            // (and the null context returns itself as a no-op).
            var fluentSame = ReferenceEquals(ctx, ctx.WithBaggage("spek.test.key", "spek-test-value"));

            // Null-valued baggage entries must be skipped by the Baggage
            // snapshot (only reachable through the raw Activity API).
            Activity.Current?.SetBaggage("spek.test.null-key", null);

            // A fresh property read builds a fresh context over the same
            // Activity — the baggage written above must be visible in it.
            var report = new TraceReport(
                HadActivity:               hadActivity,
                IsActive:                  ctx.IsActive,
                TraceId:                   ctx.TraceId,
                SpanId:                    ctx.SpanId,
                FluentReturnsSameInstance: fluentSame,
                IsNullSingleton:           ReferenceEquals(ctx, NullTraceContext.Instance),
                Baggage:                   TraceContext.Baggage);
            sender.Tell(report);
            return Task.CompletedTask;
        }
    }

    // ─── listener plumbing ───────────────────────────────────────────────────

    /// <summary>Listener on the runtime's ActivitySource that records every
    /// stopped span. Listeners are process-wide, so assertions downstream
    /// always filter by the test's own trace id.</summary>
    private static ActivityListener ListenToSpekRuntime(
        System.Collections.Concurrent.ConcurrentBag<Activity> captured)
    {
        var listener = new ActivityListener
        {
            ShouldListenTo = source => source.Name == "Spek.Runtime",
            Sample = (ref ActivityCreationOptions<ActivityContext> _) =>
                ActivitySamplingResult.AllDataAndRecorded,
            ActivityStopped = activity =>
            {
                if (activity.Source.Name == "Spek.Runtime")
                    captured.Add(activity);
            },
        };
        ActivitySource.AddActivityListener(listener);
        return listener;
    }

    // ─── the traced dispatch path: two mailbox hops ──────────────────────────

    [Fact]
    public async Task TracedDispatch_ParentChildLinkage_AcrossTwoMailboxHopsAsync()
    {
        var captured = new System.Collections.Concurrent.ConcurrentBag<Activity>();
        using var listener = ListenToSpekRuntime(captured);

        using var system = new TestActorSystem("trace-hops");
        var probe = system.CreateProbe();
        var receiver = system.Spawn<Receiver>(probe.Ref);
        var forwarder = system.Spawn<Forwarder>(receiver);

        // Root span on the test thread: the Tell below happens inside it, so
        // the enqueue captures its context into the traced envelope.
        var root = new Activity("trace-hops-root").Start();
        try
        {
            forwarder.Tell(new Ping());
        }
        finally
        {
            root.Stop();
        }

        // The message reached the end of the chain...
        probe.ExpectMsg<Done>(TimeSpan.FromSeconds(30));

        // ...and both handler spans have been stopped and captured. Filter by
        // OUR trace id — other tests' listeners/spans may be live in parallel.
        await WaitUntilAsync(() =>
            captured.Count(a => a.TraceId == root.TraceId
                                && a.OperationName.Contains(nameof(Forwarder))) >= 1
            && captured.Count(a => a.TraceId == root.TraceId
                                   && a.OperationName.Contains(nameof(Receiver))) >= 1);

        var forwarderSpan = captured.Single(a =>
            a.TraceId == root.TraceId && a.OperationName.Contains(nameof(Forwarder)));
        var receiverSpan = captured.Single(a =>
            a.TraceId == root.TraceId && a.OperationName.Contains(nameof(Receiver)));

        // Span naming contract.
        Assert.Equal($"spek.actor.{nameof(Forwarder)}.handler", forwarderSpan.OperationName);
        Assert.Equal($"spek.actor.{nameof(Receiver)}.handler", receiverSpan.OperationName);

        // Hop 1: test root → Forwarder handler.
        Assert.Equal(root.SpanId, forwarderSpan.ParentSpanId);

        // Hop 2: Forwarder handler → Receiver handler. This is the
        // actor-to-actor propagation across the second mailbox: the Tell
        // inside Forwarder's handler ran under Forwarder's dispatch span.
        Assert.Equal(forwarderSpan.SpanId, receiverSpan.ParentSpanId);

        // One trace end to end.
        Assert.Equal(root.TraceId, forwarderSpan.TraceId);
        Assert.Equal(root.TraceId, receiverSpan.TraceId);
    }

    // ─── self.TraceContext inside a traced handler ───────────────────────────

    [Fact]
    public void TraceContext_InsideTracedHandler_IsActive_WithCallersTraceId_AndBaggageRoundTrip()
    {
        var captured = new System.Collections.Concurrent.ConcurrentBag<Activity>();
        using var listener = ListenToSpekRuntime(captured);

        using var system = new TestActorSystem("trace-context");
        var probe = system.CreateProbe();
        var inspector = system.Spawn<TraceInspector>();

        var root = new Activity("trace-context-root").Start();
        try
        {
            inspector.Tell(new Inspect(), probe.Ref);
        }
        finally
        {
            root.Stop();
        }

        var report = probe.ExpectMsg<TraceReport>(TimeSpan.FromSeconds(30));

        Assert.True(report.HadActivity, "dispatch should have opened a handler span");
        Assert.True(report.IsActive);
        Assert.False(report.IsNullSingleton);

        // The handler's context carries the CALLER's trace id (32 hex chars)
        // but its own span id (16 hex chars, distinct from the root's).
        Assert.Equal(root.TraceId.ToString(), report.TraceId);
        Assert.Equal(16, report.SpanId.Length);
        Assert.NotEqual(root.SpanId.ToString(), report.SpanId);

        // WithBaggage mutates the live Activity and chains fluently; a fresh
        // TraceContext read over the same Activity sees the entry. The
        // null-valued entry set through the raw Activity API is skipped.
        Assert.True(report.FluentReturnsSameInstance);
        Assert.Equal("spek-test-value", report.Baggage["spek.test.key"]);
        Assert.False(report.Baggage.ContainsKey("spek.test.null-key"));
    }

    // ─── self.TraceContext with no Activity: the null context ────────────────

    [Fact]
    public void TraceContext_WithoutActivity_IsTheNullSingleton_WithEmptyIds()
    {
        // This test registers NO listener. Activity listeners are
        // process-wide though, so a test elsewhere in the parallel suite may
        // have one attached to Spek.Runtime while we run — in that window the
        // dispatch legitimately opens a handler span. Pin the CONTRACT rather
        // than the environment: IsActive mirrors Activity presence exactly,
        // and only the no-Activity case must be the null singleton.
        using var system = new TestActorSystem("trace-null");
        var probe = system.CreateProbe();
        var inspector = system.Spawn<TraceInspector>();

        inspector.Tell(new Inspect(), probe.Ref);
        var report = probe.ExpectMsg<TraceReport>(TimeSpan.FromSeconds(30));

        Assert.Equal(report.HadActivity, report.IsActive);

        if (!report.HadActivity)
        {
            // The common case: no listener anywhere → NullTraceContext.
            Assert.True(report.IsNullSingleton);
            Assert.Equal(string.Empty, report.TraceId);
            Assert.Equal(string.Empty, report.SpanId);
            Assert.True(report.FluentReturnsSameInstance);   // WithBaggage is a no-op on itself
            Assert.Empty(report.Baggage);
        }
        else
        {
            // A parallel test's listener was live; the context is real.
            Assert.False(report.IsNullSingleton);
            Assert.NotEqual(string.Empty, report.TraceId);
        }
    }

    // ─── helpers ─────────────────────────────────────────────────────────────

    // 30s ceiling: ActivityStopped callbacks fire when the handler span
    // disposes, which can trail the probe's Done under parallel-suite load.
    // Returns as soon as the predicate holds (ms in the common case).
    private static async Task WaitUntilAsync(Func<bool> predicate, int timeoutMs = 30_000)
    {
        var deadline = DateTime.UtcNow.AddMilliseconds(timeoutMs);
        while (DateTime.UtcNow < deadline)
        {
            if (predicate()) return;
            await Task.Delay(25);
        }
        throw new TimeoutException($"Condition not met within {timeoutMs}ms.");
    }
}
