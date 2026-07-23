using System.Threading.Tasks.Sources;

namespace Spek.Runtime;

/// <summary>
/// The slotless reply target behind <see cref="ActorRef.AskAsync{T}(object)"/>.
/// Since perf r7 the cell is an <see cref="IValueTaskSource{T}"/> and the
/// ask returns a <c>ValueTask</c> — no <c>Task&lt;T&gt;</c> allocation. The
/// token exists so a future pooled cell can reject stale sender refs; the
/// current cell is fresh per ask (see the class remarks for why).
/// </summary>
/// <summary>
/// Test-kit invariant counters (perf r8 lesson): "every ask delivered
/// exactly once" should be an assertion, not a console-print archaeology
/// session. Cheap Interlocked increments, aggregated across all reply
/// cells; reset per test via <see cref="Reset"/>.
/// </summary>
internal static class ReplyDiagnostics
{
    internal static long DeliveredCount;
    internal static long DupDroppedCount;
    internal static long FailedCount;

    internal static long Delivered => Interlocked.Read(ref DeliveredCount);
    internal static long DupDropped => Interlocked.Read(ref DupDroppedCount);
    internal static long Failed => Interlocked.Read(ref FailedCount);

    internal static void Reset()
    {
        Interlocked.Exchange(ref DeliveredCount, 0);
        Interlocked.Exchange(ref DupDroppedCount, 0);
        Interlocked.Exchange(ref FailedCount, 0);
    }
}

/// <summary>
/// Per-<see cref="ActorSystem"/> ask-reply accounting. The process-global
/// <see cref="ReplyDiagnostics"/> statics are shared by every system in the
/// process, so under parallel test collections one test's asks bleed into
/// another's counters — reset-then-read is only sound for tests that opt out
/// of parallelism. A scope hangs off one system and counts only asks issued
/// against that system's actors, which is what makes the simulator's audit
/// invariant ("every ask completed exactly once") sound whatever else the
/// process is running. Counters only; never consulted by dispatch.
/// </summary>
internal sealed class ReplyDiagnosticsScope
{
    private long _issued, _delivered, _dupDropped, _failed;

    internal void OnIssued() => Interlocked.Increment(ref _issued);
    internal void OnDelivered() => Interlocked.Increment(ref _delivered);
    internal void OnDupDropped() => Interlocked.Increment(ref _dupDropped);
    internal void OnFailed() => Interlocked.Increment(ref _failed);

    /// <summary>Asks issued against this system (cell created AND enqueued —
    /// the slotless fail-fast ask never reaches a system, so it never counts).</summary>
    internal long Issued => Interlocked.Read(ref _issued);

    /// <summary>Replies delivered first (each completes exactly one ask).</summary>
    internal long Delivered => Interlocked.Read(ref _delivered);

    /// <summary>Completions dropped because the ask already completed — a
    /// nonzero value is a reply-routing bug (the r8 class).</summary>
    internal long DupDropped => Interlocked.Read(ref _dupDropped);

    /// <summary>Asks failed first (handler crash surfaced as AskException,
    /// deadline expiry, system disposal).</summary>
    internal long Failed => Interlocked.Read(ref _failed);
}

internal abstract class ReplyCell
{
    /// <summary>Delivers the reply for the given generation; stale tokens no-op.</summary>
    public abstract void Deliver(short token, object message);

    /// <summary>Fails the given generation's ask; stale tokens no-op.</summary>
    public abstract void Fail(short token, Exception ex);

    /// <summary>True once the generation completed (stale generations count as completed).</summary>
    public abstract bool IsCompleted(short token);
}

internal sealed class PooledReplyCell<T> : ReplyCell, IValueTaskSource<T>
{
    // Deliberately NOT pooled (r7 finding): pooling requires serializing the
    // version check with the set against reset — a per-completion lock that
    // cost more time than the pooled bytes were worth on a latency-shaped
    // path. A fresh cell per ask has no reuse hazard, so completion is one
    // Interlocked gate; the ValueTask shape still retires the Task<T>.
    private ManualResetValueTaskSourceCore<T> _core = new() { RunContinuationsAsynchronously = true };
    private int _completed;
    // The issuing system's per-system diagnostics, when the ask had a system
    // to charge (null for the slotless fail-fast ask). Global statics are
    // incremented regardless; the scope adds the system-local view.
    private readonly ReplyDiagnosticsScope? _scope;

    private PooledReplyCell(ReplyDiagnosticsScope? scope) => _scope = scope;

    public static PooledReplyCell<T> Rent(ReplyDiagnosticsScope? scope) => new(scope);

    /// <summary>The cell's token; capture at rent time.</summary>
    public short Token => _core.Version;

    public override void Deliver(short token, object message)
    {
        if (Interlocked.Exchange(ref _completed, 1) != 0)
        {
            Interlocked.Increment(ref ReplyDiagnostics.DupDroppedCount);
            _scope?.OnDupDropped();
            return;
        }
        Interlocked.Increment(ref ReplyDiagnostics.DeliveredCount);
        _scope?.OnDelivered();
        if (message is T result)
            _core.SetResult(result);
        else
            _core.SetException(new InvalidCastException(
                $"Expected {typeof(T).Name} but got {message.GetType().Name}"));
    }

    public override void Fail(short token, Exception ex)
    {
        if (Interlocked.Exchange(ref _completed, 1) != 0)
        {
            Interlocked.Increment(ref ReplyDiagnostics.DupDroppedCount);
            _scope?.OnDupDropped();
            return;
        }
        Interlocked.Increment(ref ReplyDiagnostics.FailedCount);
        _scope?.OnFailed();
        _core.SetException(ex);
    }

    public override bool IsCompleted(short token)
        => _core.GetStatus(token) != ValueTaskSourceStatus.Pending;

    // ── IValueTaskSource<T> ──────────────────────────────────────────────

    public T GetResult(short token) => _core.GetResult(token);

    public ValueTaskSourceStatus GetStatus(short token) => _core.GetStatus(token);

    public void OnCompleted(
        Action<object?> continuation, object? state, short token,
        ValueTaskSourceOnCompletedFlags flags)
        => _core.OnCompleted(continuation, state, token, flags);
}
