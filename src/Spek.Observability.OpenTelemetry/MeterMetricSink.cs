using System.Collections.Concurrent;
using System.Diagnostics;
using System.Diagnostics.Metrics;

namespace Spek.Observability.OpenTelemetry;

/// <summary>
/// Implementation of <see cref="IMetricSink"/> on top of
/// <see cref="Meter"/> from <c>System.Diagnostics.Metrics</c>. The
/// runtime's metric calls become <c>Counter&lt;long&gt;</c>,
/// <c>Histogram&lt;double&gt;</c>, and observable-gauge instruments
/// on the Spek meter. Any OpenTelemetry SDK pipeline (or
/// <c>dotnet-counters</c>) that listens to the <c>"Spek.Runtime"</c>
/// meter sees the metrics with no additional plumbing.
///
/// <para>
/// Instruments are created lazily on first call and cached by name.
/// Concurrent calls during creation are safe — the underlying
/// <see cref="ConcurrentDictionary{TKey,TValue}"/> guards against
/// double-instantiation; the worst case is one wasted instrument
/// allocation, which is harmless.
/// </para>
/// </summary>
public sealed class MeterMetricSink : IMetricSink, IDisposable
{
    /// <summary>
    /// Meter name. Stable across releases — listeners filter on
    /// this to scope what they collect.
    /// </summary>
    public const string MeterName = "Spek.Runtime";

    private readonly Meter _meter;
    private readonly ConcurrentDictionary<string, Counter<long>>      _counters    = new();
    private readonly ConcurrentDictionary<string, Histogram<double>>  _histograms  = new();
    private readonly ConcurrentDictionary<string, double>             _gaugeValues = new();
    private readonly ConcurrentDictionary<string, ObservableGauge<double>> _gauges = new();

    public MeterMetricSink()
    {
        _meter = new Meter(MeterName);
    }

    public void Counter(string name, long delta = 1,
        IReadOnlyList<KeyValuePair<string, object?>>? tags = null)
    {
        var counter = _counters.GetOrAdd(name, n => _meter.CreateCounter<long>(n));
        if (tags is { Count: > 0 })
            counter.Add(delta, ToTagList(tags));
        else
            counter.Add(delta);
    }

    public void Gauge(string name, double value,
        IReadOnlyList<KeyValuePair<string, object?>>? tags = null)
    {
        // Observable gauges in System.Diagnostics.Metrics callback
        // when the listener polls. We store the latest value and
        // return it from the callback — gauges are last-write-wins.
        // Tags on gauges aren't supported in this MVP; if needed,
        // we'd switch to per-tag-set gauge dictionaries.
        _gaugeValues[name] = value;
        _gauges.GetOrAdd(name, n => _meter.CreateObservableGauge<double>(
            n, () => _gaugeValues.TryGetValue(n, out var v) ? v : 0));
    }

    public void Histogram(string name, double value,
        IReadOnlyList<KeyValuePair<string, object?>>? tags = null)
    {
        var hist = _histograms.GetOrAdd(name, n => _meter.CreateHistogram<double>(n));
        if (tags is { Count: > 0 })
            hist.Record(value, ToTagList(tags));
        else
            hist.Record(value);
    }

    private static TagList ToTagList(IReadOnlyList<KeyValuePair<string, object?>> tags)
    {
        var tl = new TagList();
        for (int i = 0; i < tags.Count; i++)
            tl.Add(tags[i]);
        return tl;
    }

    public void Dispose() => _meter.Dispose();
}
