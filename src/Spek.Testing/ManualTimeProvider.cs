namespace Spek.Testing;

/// <summary>
/// A deterministic <see cref="TimeProvider"/> for tests: time stands still
/// until <see cref="Advance"/> moves it, and timers fire synchronously inside
/// the Advance call, in due order. The runtime's semantic time (passivation
/// idleness, restart windows, <c>self.Clock</c> reads) flows through the
/// system clock, so a test that advances this provider controls all of it.
/// </summary>
public sealed class ManualTimeProvider : TimeProvider
{
    private readonly object _gate = new();
    private readonly List<ManualTimer> _timers = [];
    private DateTimeOffset _now;

    /// <summary>Creates the provider; time starts at <paramref name="start"/> (default: 2000-01-01 UTC).</summary>
    public ManualTimeProvider(DateTimeOffset? start = null)
        => _now = start ?? new DateTimeOffset(2000, 1, 1, 0, 0, 0, TimeSpan.Zero);

    /// <inheritdoc />
    public override DateTimeOffset GetUtcNow() { lock (_gate) return _now; }

    /// <inheritdoc />
    public override long GetTimestamp() { lock (_gate) return _now.UtcTicks; }

    /// <inheritdoc />
    public override long TimestampFrequency => TimeSpan.TicksPerSecond;

    /// <summary>
    /// Moves time forward and fires every timer that comes due, in due order,
    /// synchronously on the calling thread. Periodic timers fire repeatedly
    /// within one Advance when the delta spans multiple periods.
    /// </summary>
    public void Advance(TimeSpan delta)
    {
        if (delta < TimeSpan.Zero)
            throw new ArgumentOutOfRangeException(nameof(delta), "time only moves forward");

        DateTimeOffset target;
        lock (_gate) target = _now + delta;

        while (true)
        {
            ManualTimer? next = null;
            lock (_gate)
            {
                foreach (var t in _timers)
                    if (t.NextDue is { } due && due <= target
                        && (next?.NextDue is not { } best || due < best))
                    {
                        next = t;
                    }

                if (next is null) { _now = target; return; }
                _now = next.NextDue!.Value;
                next.Reschedule(_now);
            }
            next.Fire();
        }
    }

    /// <inheritdoc />
    public override ITimer CreateTimer(
        TimerCallback callback, object? state, TimeSpan dueTime, TimeSpan period)
    {
        var timer = new ManualTimer(this, callback, state);
        timer.Change(dueTime, period);
        lock (_gate) _timers.Add(timer);
        return timer;
    }

    private void Remove(ManualTimer t) { lock (_gate) _timers.Remove(t); }

    /// <summary>
    /// Audit snapshot of every timer still armed on this clock (a due time is
    /// set and has not fired). One-shot entries are parked work — a chaos
    /// delay, a defer re-admission, a debounce quiet-window — that only an
    /// <see cref="Advance"/> can release; periodic entries are runtime
    /// infrastructure (passivation checks) that stays legitimately armed for
    /// a slot's whole lifetime. Callbacks are opaque, so the snapshot can
    /// count and timestamp but not attribute.
    /// </summary>
    internal IReadOnlyList<(DateTimeOffset Due, bool Periodic)> ArmedTimersSnapshot()
    {
        lock (_gate)
            return _timers
                .Where(t => t.NextDue is not null)
                .Select(t => (t.NextDue!.Value, t.Periodic))
                .ToArray();
    }

    private sealed class ManualTimer(
        ManualTimeProvider owner, TimerCallback callback, object? state) : ITimer
    {
        private TimeSpan _period = Timeout.InfiniteTimeSpan;
        internal DateTimeOffset? NextDue;

        /// <summary>True for repeating timers. Read under the owner's gate
        /// (the audit snapshot holds it; Change writes under it).</summary>
        internal bool Periodic => _period != Timeout.InfiniteTimeSpan;

        public bool Change(TimeSpan dueTime, TimeSpan period)
        {
            lock (owner._gate)
            {
                _period = period;
                NextDue = dueTime == Timeout.InfiniteTimeSpan
                    ? null
                    : owner._now + dueTime;
            }
            return true;
        }

        internal void Reschedule(DateTimeOffset firedAt)
            => NextDue = _period == Timeout.InfiniteTimeSpan ? null : firedAt + _period;

        internal void Fire() => callback(state);

        public void Dispose() { NextDue = null; owner.Remove(this); }
        public ValueTask DisposeAsync() { Dispose(); return ValueTask.CompletedTask; }
    }
}
