namespace Spek;

/// <summary>
/// Mutable per-actor configuration for the reader/writer concurrency
/// model. Each <see cref="ActorRef"/> exposes a
/// dedicated <c>Readers</c> property; an actor self-configures via
/// direct property assignment in <c>init()</c> (or any handler that
/// holds <c>self</c>):
/// <code>
/// init() {
///     self.Readers.Strategy = ReaderStrategy.Fair;
///     self.Readers.Max = 8;
/// }
/// </code>
/// The slot reads these values atomically on each dispatch decision,
/// so mid-run changes take effect for the next message — no need to
/// stop and re-spawn.
/// </summary>
public sealed class ReaderPolicy
{
    private int _strategy = (int)ReaderStrategy.WriterPreferring;
    private int _max = int.MaxValue;

    /// <summary>
    /// Scheduling discipline when a writer is queued behind in-flight
    /// readers.
    /// <list type="bullet">
    ///   <item><see cref="ReaderStrategy.WriterPreferring"/> (default) —
    ///         no new readers join the in-flight set once a writer is
    ///         queued. Writers stay live; reader-message order can be
    ///         relaxed when readers don't depend on each other.</item>
    ///   <item><see cref="ReaderStrategy.Fair"/> — strict mailbox order.
    ///         Readers continue joining the in-flight set even when a
    ///         writer is queued; the writer waits until *all* preceding
    ///         readers (mailbox order) complete.</item>
    /// </list>
    /// </summary>
    public ReaderStrategy Strategy
    {
        get => (ReaderStrategy)Volatile.Read(ref _strategy);
        set => Volatile.Write(ref _strategy, (int)value);
    }

    /// <summary>
    /// Maximum number of in-flight reader handlers for this actor.
    /// Defaults to <see cref="int.MaxValue"/> (only the .NET thread pool
    /// limits concurrency). Set lower to bound resource use — useful for
    /// actors that hand off to expensive downstream calls (DB, remote
    /// services).
    /// </summary>
    public int Max
    {
        get => Volatile.Read(ref _max);
        set
        {
            if (value < 1)
                throw new ArgumentOutOfRangeException(nameof(value),
                    "Readers.Max must be at least 1.");
            Volatile.Write(ref _max, value);
        }
    }
}

/// <summary>Reader-scheduling fairness policies.</summary>
public enum ReaderStrategy
{
    /// <summary>Default. Queued writer prevents new readers from
    /// joining the in-flight set; writers stay live.</summary>
    WriterPreferring,

    /// <summary>Strict mailbox FIFO; readers behind a writer don't
    /// jump ahead, but readers that arrived before a writer all
    /// run before the writer.</summary>
    Fair,
}
