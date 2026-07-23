namespace Spek.Observability;

/// <summary>
/// Pluggable surface for runtime + user-code metric
/// emission. The Spek runtime calls into a registered sink for
/// every per-actor metric (mailbox depth, dispatch counts,
/// handler durations, restart counts, region lock contention,
/// etc.). User code reaches it through <c>self.Metrics</c>.
///
/// <para>
/// A <see cref="NullMetricSink"/> is the default when no
/// observability package is registered. Always-on instrumentation
/// means the runtime always calls
/// the sink; the null sink makes the call a no-op.
/// </para>
/// </summary>
public interface IMetricSink
{
    /// <summary>
    /// False on a sink that discards everything (<see cref="NullMetricSink"/>),
    /// so the runtime can skip building per-message tag arrays / reflecting on
    /// the actor type in the hot path when no metrics are collected. Real sinks
    /// inherit the default <c>true</c>; "always-on instrumentation" still holds
    /// — this only elides work whose result would be thrown away.
    /// </summary>
    bool Enabled => true;

    /// <summary>
    /// Increment a monotonically-increasing counter. Tags are
    /// emitted as dimensions in OTel/Prometheus.
    /// </summary>
    void Counter(string name, long delta = 1, IReadOnlyList<KeyValuePair<string, object?>>? tags = null);

    /// <summary>
    /// Set a gauge value. Use for instantaneous quantities
    /// (mailbox depth, queue length, region lock holders).
    /// </summary>
    void Gauge(string name, double value, IReadOnlyList<KeyValuePair<string, object?>>? tags = null);

    /// <summary>
    /// Record a value into a histogram. Use for distributions
    /// (handler durations, message sizes, lock acquisition times).
    /// </summary>
    void Histogram(string name, double value, IReadOnlyList<KeyValuePair<string, object?>>? tags = null);
}

/// <summary>
/// No-op metric sink, used when no observability package
/// is registered with the actor system. All calls are inlined
/// away by the JIT once it sees the empty body.
/// </summary>
public sealed class NullMetricSink : IMetricSink
{
    /// <summary>Shared singleton. The sink is stateless, so one
    /// instance serves every actor system in the process.</summary>
    public static readonly NullMetricSink Instance = new();

    /// <summary>Always false — lets the runtime skip hot-path tag
    /// building for metrics nothing will record.</summary>
    public bool Enabled => false;

    /// <summary>No-op by design; the counter increment is discarded.</summary>
    public void Counter(string name, long delta = 1, IReadOnlyList<KeyValuePair<string, object?>>? tags = null) { }
    /// <summary>No-op by design; the gauge value is discarded.</summary>
    public void Gauge(string name, double value, IReadOnlyList<KeyValuePair<string, object?>>? tags = null) { }
    /// <summary>No-op by design; the histogram sample is discarded.</summary>
    public void Histogram(string name, double value, IReadOnlyList<KeyValuePair<string, object?>>? tags = null) { }
}
