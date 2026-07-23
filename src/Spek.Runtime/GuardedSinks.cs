using Spek.Observability;

namespace Spek.Runtime;

/// <summary>
/// Hardening boundary for user-supplied observability sinks. Sinks are
/// called from every fragile place the runtime has — dispatch loops,
/// supervision catch blocks, timer callbacks — so a sink that throws
/// would otherwise convert a *report* of a failure into a new failure:
/// mis-attributed to the handler (tripping supervision), or escaping a
/// timer callback (killing the process), or wedging a guard path. The
/// wrappers make sink calls total: a throwing sink gets one stderr line
/// and the runtime moves on.
/// </summary>
internal sealed class GuardedDeadLetterSink(IDeadLetterSink inner) : IDeadLetterSink
{
    /// <summary>The user's sink, for identity/inspection.</summary>
    internal IDeadLetterSink Inner => inner;

    public void DeadLetter(object message, string reason, Exception? cause)
    {
        try { inner.DeadLetter(message, reason, cause); }
        catch (Exception ex)
        {
            Console.Error.WriteLine(
                $"[spek] dead-letter sink '{inner.GetType().Name}' threw " +
                $"({ex.GetType().Name}: {ex.Message}) while reporting: {reason}");
        }
    }
}

/// <summary>See <see cref="GuardedDeadLetterSink"/> — the same boundary for
/// metric sinks, which run on the dispatch hot path inside the handler try.</summary>
internal sealed class GuardedMetricSink(IMetricSink inner) : IMetricSink
{
    internal IMetricSink Inner => inner;

    public bool Enabled => inner.Enabled;

    public void Counter(string name, long delta = 1,
        IReadOnlyList<KeyValuePair<string, object?>>? tags = null)
    {
        try { inner.Counter(name, delta, tags); } catch (Exception ex) { Report(ex); }
    }

    public void Gauge(string name, double value,
        IReadOnlyList<KeyValuePair<string, object?>>? tags = null)
    {
        try { inner.Gauge(name, value, tags); } catch (Exception ex) { Report(ex); }
    }

    public void Histogram(string name, double value,
        IReadOnlyList<KeyValuePair<string, object?>>? tags = null)
    {
        try { inner.Histogram(name, value, tags); } catch (Exception ex) { Report(ex); }
    }

    private void Report(Exception ex) =>
        Console.Error.WriteLine(
            $"[spek] metric sink '{inner.GetType().Name}' threw: " +
            $"{ex.GetType().Name}: {ex.Message}");
}
