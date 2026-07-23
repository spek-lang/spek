using System.Collections.Concurrent;

namespace Spek.Runtime;

/// <summary>
/// One shared deadline timer for every timeout-carrying ask in an
/// <see cref="ActorSystem"/> (perf r11). The old shape —
/// <c>AskAsync(msg).AsTask().WaitAsync(timeout)</c> — allocated a bridging
/// <c>Task&lt;T&gt;</c> plus a WaitAsync timer per ask, doubling the ask's
/// allocation; this sweeper prices a timeout at one queue entry instead.
///
/// Mechanics: arming enqueues (cell, token, deadline) and starts a single
/// periodic timer; each sweep dequeues the queue's current length once,
/// dropping entries whose ask already completed, failing expired ones with
/// <see cref="TimeoutException"/> through the cell's token-gated
/// <see cref="ReplyCell.Fail"/> (a reply that races the sweep wins — first
/// completion takes the cell, same as the WaitAsync race), and re-enqueuing
/// the rest. The timer parks when the queue drains.
///
/// Precision: deadlines fire up to one sweep period (25&#160;ms) late, never
/// early — ask timeouts guard against wedged targets, they are not a
/// high-resolution clock. Entries hold their cell alive at most one period
/// past completion; cells are unpooled (r7), so that only delays GC, never
/// misroutes a reply.
///
/// Disposal (with the owning system, after its drain): every entry still
/// pending fails immediately with a shutdown-flavored
/// <see cref="TimeoutException"/> — by then the system can produce no more
/// replies, so waiting out the remaining window could only delay the caller's
/// failure, and a fast fault beats a hang (the slotless-ask precedent).
/// Timer callbacks are total per the hardening contract: a sweep never lets
/// an exception escape into the timer.
/// </summary>
internal sealed class AskDeadlineSweeper : IDisposable
{
    private const int SweepPeriodMs = 25;

    private readonly ConcurrentQueue<Entry> _pending = new();
    private readonly Timer _timer;
    private int _timerRunning;   // 1 while the periodic sweep is scheduled
    private volatile bool _disposed;

    private readonly record struct Entry(ReplyCell Cell, short Token, long DeadlineMs);

    public AskDeadlineSweeper() =>
        _timer = new Timer(static s => ((AskDeadlineSweeper)s!).Sweep(),
            this, Timeout.Infinite, Timeout.Infinite);

    /// <summary>Registers a deadline for the given ask generation.</summary>
    public void Arm(ReplyCell cell, short token, TimeSpan timeout)
    {
        if (_disposed)
        {
            cell.Fail(token, new TimeoutException(
                "ask armed with a timeout on a disposed ActorSystem — no reply can ever arrive."));
            return;
        }

        var deadline = Environment.TickCount64 + (long)timeout.TotalMilliseconds;
        _pending.Enqueue(new Entry(cell, token, deadline));
        EnsureTimerRunning();
    }

    private void EnsureTimerRunning()
    {
        if (Interlocked.CompareExchange(ref _timerRunning, 1, 0) == 0)
            _timer.Change(SweepPeriodMs, SweepPeriodMs);
    }

    private void Sweep()
    {
        try
        {
            var now = Environment.TickCount64;
            var count = _pending.Count;
            for (int i = 0; i < count; i++)
            {
                if (!_pending.TryDequeue(out var e)) break;
                if (e.Cell.IsCompleted(e.Token)) continue;   // reply won
                if (now >= e.DeadlineMs)
                    e.Cell.Fail(e.Token, new TimeoutException(
                        "ask did not receive a reply within its timeout."));
                else
                    _pending.Enqueue(e);
            }

            if (_pending.IsEmpty)
            {
                _timer.Change(Timeout.Infinite, Timeout.Infinite);
                Volatile.Write(ref _timerRunning, 0);
                // An Arm can slip in between the emptiness check and the
                // park; re-check so its entry is never stranded timerless.
                if (!_pending.IsEmpty) EnsureTimerRunning();
            }
        }
        catch (Exception ex)
        {
            // Timer callbacks are total by contract — a throw here would
            // vanish into the timer and silently stop enforcing deadlines.
            Console.Error.WriteLine($"[spek] ask-deadline sweep threw: {ex}");
        }
    }

    public void Dispose()
    {
        _disposed = true;
        _timer.Dispose();
        while (_pending.TryDequeue(out var e))
        {
            if (e.Cell.IsCompleted(e.Token)) continue;
            e.Cell.Fail(e.Token, new TimeoutException(
                "ActorSystem disposed before the ask received a reply."));
        }
    }
}
