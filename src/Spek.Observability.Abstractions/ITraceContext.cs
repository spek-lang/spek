namespace Spek.Observability;

/// <summary>
/// Spek surface for the W3C trace context carried through
/// every actor message. <c>self.TraceContext</c> inside a handler
/// returns the current value; trace context flows automatically
/// through <c>Tell</c> / <c>AskAsync</c> so receiving handlers see
/// the same trace ID as the sender.
///
/// <para>
/// The implementation rides on <see cref="System.Diagnostics.Activity"/>
/// — the modern .NET primitive everyone uses, and the one
/// OpenTelemetry maps onto — but the Spek surface stays
/// implementation-agnostic so a future runtime could swap in
/// something else without breaking handler code.
/// </para>
///
/// <para>
/// All trace IDs are W3C-format hex strings; baggage entries
/// follow the W3C baggage spec.
/// </para>
/// </summary>
public interface ITraceContext
{
    /// <summary>
    /// W3C trace ID — 32-hex-char string identifying the entire
    /// distributed operation across actors and nodes. Empty when
    /// no trace is active (e.g. handler invoked outside a
    /// traced ingress).
    /// </summary>
    string TraceId { get; }

    /// <summary>
    /// W3C span ID — 16-hex-char string identifying this specific
    /// operation. Each handler invocation produces a new span ID;
    /// the trace ID is preserved.
    /// </summary>
    string SpanId { get; }

    /// <summary>
    /// True when a trace is active. Useful for branching on
    /// "is anything observing me?" without having to inspect
    /// the trace ID.
    /// </summary>
    bool IsActive { get; }

    /// <summary>
    /// Read-only snapshot of the W3C baggage entries — arbitrary
    /// key/value strings that propagate alongside the trace
    /// context. Use sparingly; baggage rides on every Tell.
    /// </summary>
    IReadOnlyDictionary<string, string> Baggage { get; }

    /// <summary>
    /// Add a baggage entry to the current context. Propagates to
    /// every downstream Tell. Returns the modified context (the
    /// underlying <see cref="System.Diagnostics.Activity"/> is
    /// itself mutable; the return is for fluent chaining).
    /// </summary>
    ITraceContext WithBaggage(string key, string value);
}

/// <summary>
/// Empty trace context returned when no observability
/// package is registered or no trace is active.
/// </summary>
public sealed class NullTraceContext : ITraceContext
{
    /// <summary>Shared singleton. The context is empty and immutable,
    /// so one instance serves everywhere.</summary>
    public static readonly NullTraceContext Instance = new();

    /// <summary>Always empty — no trace is active on this context.</summary>
    public string TraceId => string.Empty;
    /// <summary>Always empty — no span is active on this context.</summary>
    public string SpanId  => string.Empty;
    /// <summary>Always false, so handlers branching on "is anything
    /// observing me?" skip their tracing work.</summary>
    public bool IsActive  => false;
    /// <summary>Always empty; entries cannot be added (see
    /// <see cref="WithBaggage"/>).</summary>
    public IReadOnlyDictionary<string, string> Baggage { get; }
        = new Dictionary<string, string>();
    /// <summary>No-op by design — with no active trace there is nothing
    /// to attach baggage to. Returns the same empty context.</summary>
    public ITraceContext WithBaggage(string key, string value) => this;
}
