using System.Diagnostics;
using Spek.Observability;

namespace Spek.Runtime;

/// <summary>
/// Dead-letter payload for a slow-handler watchdog report. No user message
/// was dropped — the suspect message is still inside its handler — so the
/// report itself stands in as the entry's "message", carrying the wedged
/// actor's display name, the type name of the last message that entered
/// dispatch on the slot (null if none has), and how long the handler had
/// been running when the sweep caught it.
/// </summary>
/// <param name="ActorPath">The slot's display identity — the persistence key
/// when one exists, otherwise <c>TypeName</c> / <c>TypeName#N</c>.</param>
/// <param name="LastMessageType">Type name of the last message dispatched on
/// the slot — for a wedged writer, the message the handler is stuck on.</param>
/// <param name="ObservedRunningFor">Wall-clock time the handler (or reader
/// phase) had been running when the sweep reported it.</param>
public sealed record SlowHandlerReport(
    string ActorPath, string? LastMessageType, TimeSpan ObservedRunningFor);

/// <summary>
/// One per-system watchdog for handlers that never come back — detection
/// only, never enforcement. Each sweep scans every tracked slot's dispatch
/// and reader-phase stamps; a stamp older than
/// <see cref="ActorSystem.SlowHandlerThreshold"/> produces one dead-letter
/// entry (a <see cref="SlowHandlerReport"/>) plus a
/// <see cref="SpekMetricNames.SlowHandler"/> counter tick. The runtime
/// never cancels or kills the suspect handler: a watchdog that guesses
/// wrong about "stuck" would corrupt exactly the state it exists to
/// protect, so its whole job is making the wedge loud.
///
/// Reports fire once per occurrence: the sweep remembers the stamp it
/// reported per slot per arm, and only a NEW stamp — a new message, or a
/// new reader phase — reports again. A handler wedged forever is one
/// entry, not one per second.
///
/// Timing is deliberately real wall-clock (<see cref="Environment.TickCount64"/>
/// and a plain <see cref="Timer"/>), NOT the system's
/// <see cref="ActorSystem.Clock"/>: a wedge is a wall-clock phenomenon,
/// and a virtual-time test that advances the manual clock by hours must
/// not false-positive. This mirrors the Clock contract's own carve-out —
/// "internal scheduling ... deliberately stay[s] on real time". For the
/// same reason a sweep skips entirely while a debugger is attached: a
/// breakpoint isn't a wedge.
///
/// Structure mirrors <see cref="AskDeadlineSweeper"/>: created lazily (on
/// the first tracked slot), one periodic timer that parks while detection
/// is disabled (null threshold) and re-arms when it's set back, disposed
/// with the owning system. Timer callbacks are total per the hardening
/// contract: a sweep never lets an exception escape into the timer.
/// </summary>
internal sealed class HandlerWatchdog : IDisposable
{
    /// <summary>Production sweep cadence. Coarse on purpose — the default
    /// threshold is 30s, so second-scale detection latency is noise. Tests
    /// shorten it via <see cref="ActorSystem.SlowHandlerSweepPeriodMs"/>.</summary>
    internal const long DefaultSweepPeriodMs = 1_000;

    private readonly ActorSystem _system;
    private readonly long _sweepPeriodMs;
    private readonly Timer _timer;
    private int _timerRunning;   // 1 while the periodic sweep is scheduled
    private int _sweeping;       // reentrancy gate — see Sweep
    private volatile bool _disposed;

    public HandlerWatchdog(ActorSystem system)
    {
        _system = system;
        // Read once at creation (first spawn); the test hook sets it earlier.
        _sweepPeriodMs = Math.Max(1, system.SlowHandlerSweepPeriodMs);
        _timer = new Timer(static s => ((HandlerWatchdog)s!).Sweep(),
            this, Timeout.Infinite, Timeout.Infinite);
        Wake();
    }

    /// <summary>(Re)starts the periodic sweep. Called at construction and by
    /// the <see cref="ActorSystem.SlowHandlerThreshold"/> setter when a null
    /// threshold (which parks the sweep) is set back to a value.</summary>
    internal void Wake()
    {
        if (_disposed) return;
        if (Interlocked.CompareExchange(ref _timerRunning, 1, 0) == 0)
        {
            try { _timer.Change(_sweepPeriodMs, _sweepPeriodMs); }
            catch (ObjectDisposedException) { /* raced Dispose — nothing to run */ }
        }
    }

    private void Sweep()
    {
        // Reentrancy gate: sweeps are fast, but a test-shortened period must
        // never let two overlapping sweeps race the once-per-occurrence
        // bookkeeping into a double report.
        if (Interlocked.CompareExchange(ref _sweeping, 1, 0) != 0) return;
        try
        {
            if (_disposed) return;

            var thresholdMs = _system.SlowHandlerThresholdMs;
            if (thresholdMs < 0)
            {
                // Detection disabled: park the timer. A re-enable can slip in
                // between the read and the park, so re-check — an armed
                // threshold must never be stranded sweepless (the
                // AskDeadlineSweeper park pattern).
                _timer.Change(Timeout.Infinite, Timeout.Infinite);
                Volatile.Write(ref _timerRunning, 0);
                if (_system.SlowHandlerThresholdMs >= 0) Wake();
                return;
            }

            // A breakpoint isn't a wedge: under a debugger, handlers
            // legitimately sit frozen for minutes. Stamps keep aging, so a
            // handler still stuck after detach is reported then.
            if (Debugger.IsAttached) return;

            var now = Environment.TickCount64;
            foreach (var slot in _system.SlotsSnapshot())
            {
                CheckArm(slot, slot.DispatchStartMs,
                    ref slot.WatchdogReportedDispatchStamp, now, thresholdMs,
                    readerPhase: false);
                CheckArm(slot, slot.ReaderPhaseStartMs,
                    ref slot.WatchdogReportedReaderStamp, now, thresholdMs,
                    readerPhase: true);
            }
        }
        catch (ObjectDisposedException)
        {
            // Raced Dispose between the flag check and the timer touch —
            // the system is tearing down; there is nothing left to watch.
        }
        catch (Exception ex)
        {
            // Timer callbacks are total by contract — a throw here would
            // vanish into the timer and silently stop the watchdog.
            Console.Error.WriteLine($"[spek] slow-handler sweep threw: {ex}");
        }
        finally
        {
            Volatile.Write(ref _sweeping, 0);
        }
    }

    /// <summary>
    /// Checks one arm's stamp and reports if it is both older than the
    /// threshold and not the stamp already reported for this arm. The
    /// reported-stamp field lives on the slot (written only from inside the
    /// gated sweep) so bookkeeping dies with the slot instead of leaking in
    /// a watchdog-side map.
    /// </summary>
    private void CheckArm(
        ActorSlot slot, long stamp, ref long reported,
        long now, long thresholdMs, bool readerPhase)
    {
        if (stamp == 0) return;                 // idle
        var elapsedMs = now - stamp;
        if (elapsedMs < thresholdMs) return;    // running, but within budget
        if (stamp == reported) return;          // this occurrence was already reported
        reported = stamp;

        var elapsed = TimeSpan.FromMilliseconds(elapsedMs);
        // The two arms stall different things: a wedged writer blocks its own
        // mailbox; a wedged reader lets other readers keep dispatching but
        // starves every writer waiting for the phase to drain.
        var reason = readerPhase
            ? $"reader handler running for {elapsed.TotalSeconds:0.0}s " +
              $"(threshold {thresholdMs / 1000.0:0.###}s) on '{slot.DisplayName}' " +
              "— possible wedge; writers are stalled behind it"
            : $"handler running for {elapsed.TotalSeconds:0.0}s " +
              $"(threshold {thresholdMs / 1000.0:0.###}s) on '{slot.DisplayName}' " +
              "— possible wedge; the mailbox is stalled behind it";

        _system.DeadLetterSink.DeadLetter(
            new SlowHandlerReport(slot.DisplayName, slot.LastMessageTypeName, elapsed),
            reason, cause: null);
        _system.Metrics.Counter(SpekMetricNames.SlowHandler, tags: slot.ActorTypeTags());
    }

    public void Dispose()
    {
        _disposed = true;
        _timer.Dispose();
    }
}
