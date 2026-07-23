namespace Spek.Observability;

/// <summary>
/// Well-known metric names emitted by the Spek runtime.
/// Centralised here so adapters (OpenTelemetry, Prometheus,
/// custom sinks) can apply consistent naming and so user code
/// can reliably reference them in dashboards and alerts.
///
/// <para>
/// Naming follows OpenTelemetry conventions — lowercase with
/// dot separators, units suffixed for histograms.
/// </para>
/// </summary>
public static class SpekMetricNames
{
    /// <summary>Mailbox depth (gauge): pending messages in an
    /// actor's queue.</summary>
    public const string MailboxDepth = "spek.mailbox.depth";

    /// <summary>Dispatched messages (counter): how many messages
    /// have been pulled off the mailbox and handed to a handler.</summary>
    public const string MailboxDispatch = "spek.mailbox.dispatch";

    /// <summary>Handler duration (histogram, ms): how long each
    /// handler took to run, including lock acquisition.</summary>
    public const string HandlerDurationMs = "spek.actor.handler.duration.ms";

    /// <summary>Actor restart (counter): supervision-driven
    /// restart events. Tagged with strategy and exception type.</summary>
    public const string ActorRestart = "spek.actor.restart";

    /// <summary>Actor stop (counter): voluntary or supervised
    /// stops. Tagged with cause.</summary>
    public const string ActorStop = "spek.actor.stop";

    /// <summary>Region writer-lock acquisition latency (histogram,
    /// ms): time spent waiting to acquire the writer lock on a
    /// shared region.</summary>
    public const string RegionLockWaitMs = "spek.region.lock.wait.ms";

    /// <summary>Dead-letter drop (counter): messages routed to
    /// the dead-letter sink because no actor was alive to
    /// receive them.</summary>
    public const string DeadLetter = "spek.deadletter";
}
