namespace Spek.Observability;

/// <summary>
/// Minimal structured-log surface. The Spek runtime calls
/// into a registered logger for lifecycle events (actor spawn /
/// stop / restart, supervision decisions, dead-letter drops);
/// user code reaches it through <c>self.Log</c>.
///
/// <para>
/// Deliberately tiny — six methods total — because Spek doesn't
/// need to reinvent logging. Real applications will register a
/// logger that adapts to Microsoft.Extensions.Logging's <c>ILogger&lt;T&gt;</c>
/// or whatever else they're using; the abstraction exists so the
/// runtime doesn't bake in a specific logging dependency.
/// </para>
/// </summary>
public interface IStructuredLogger
{
    /// <summary>
    /// Whether events at <paramref name="level"/> would actually be
    /// recorded. Callers check this before assembling an expensive
    /// property list so disabled levels cost nothing; implementations
    /// must answer cheaply — this runs on the dispatch hot path.
    /// </summary>
    bool IsEnabled(StructuredLogLevel level);

    /// <summary>
    /// Record one structured event. <paramref name="eventName"/> is a
    /// stable identifier for the event kind — something backends can
    /// match and aggregate on, not a pre-formatted sentence — and
    /// <paramref name="properties"/> carries the event's data as
    /// key/value pairs so each field stays individually indexable.
    /// Implementations must be thread-safe (the runtime logs from
    /// arbitrary dispatcher threads) and should never throw: a
    /// logging failure must not take down the handler that logged.
    /// </summary>
    void Log(
        StructuredLogLevel level,
        string eventName,
        IReadOnlyList<KeyValuePair<string, object?>>? properties = null,
        Exception? exception = null);
}

/// <summary>
/// Log severity levels. Mirrors
/// <see cref="Microsoft.Extensions.Logging.LogLevel"/> values
/// (without taking the dependency at the abstractions layer).
/// </summary>
public enum StructuredLogLevel
{
    /// <summary>Finest-grained detail — per-message dispatch level.
    /// Too chatty for anything but targeted debugging.</summary>
    Trace       = 0,
    /// <summary>Diagnostic detail useful while developing; off in
    /// production by default.</summary>
    Debug       = 1,
    /// <summary>Routine lifecycle events (spawn, stop, passivate) —
    /// the normal narrative of a healthy system.</summary>
    Information = 2,
    /// <summary>Unexpected but survived — the system recovered on its
    /// own (an actor restart, a dead-lettered message).</summary>
    Warning     = 3,
    /// <summary>An operation failed; something was lost or needs
    /// attention.</summary>
    Error       = 4,
    /// <summary>The system itself is in trouble and needs immediate
    /// attention.</summary>
    Critical    = 5,
}

/// <summary>No-op default; every call is inlined away.</summary>
public sealed class NullStructuredLogger : IStructuredLogger
{
    /// <summary>Shared singleton. The logger is stateless, so one
    /// instance serves every actor system in the process.</summary>
    public static readonly NullStructuredLogger Instance = new();
    /// <summary>Always false, so callers skip building property lists
    /// for events nobody will record.</summary>
    public bool IsEnabled(StructuredLogLevel level) => false;
    /// <summary>No-op by design; the event is discarded.</summary>
    public void Log(StructuredLogLevel level, string eventName,
        IReadOnlyList<KeyValuePair<string, object?>>? properties = null,
        Exception? exception = null) { }
}
