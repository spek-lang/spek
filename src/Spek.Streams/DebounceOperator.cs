namespace Spek.Streams;

/// <summary>
/// Holds the most-recent message until the source has been quiet
/// for the configured interval, then emits it. New messages
/// supersede the pending one and reset the timer.
///
/// Typical use: a UI text-input "search-as-you-type" that should
/// fire after the user stops typing, or a FileSystemWatcher that
/// fires several events per save and should produce one
/// downstream effect.
///
/// <see cref="OfferAsync"/> never waits: it records the message,
/// re-arms the quiet-window timer, and returns. The emit happens on
/// a timer callback once the window elapses — in the actor wiring
/// that callback is a self-Tell, so the handler body still runs
/// through the mailbox under the actor lock, and the mailbox is
/// never stalled by the debounce interval. (An earlier version
/// awaited the interval inside <see cref="OfferAsync"/>, which both
/// blocked the mailbox one interval per message and defeated
/// supersession entirely, because serialized offers never overlap.)
///
/// A zero interval means "no quiet window": each message dispatches
/// inline, synchronously.
///
/// <see cref="StopAsync"/> cancels a pending emit: the actor is
/// stopping, so a message emitted now could only dead-letter.
/// </summary>
public sealed class DebounceOperator<T> : StreamOperator<T>
{
    private readonly TimeSpan _interval;
    private readonly object _gate = new();
    private ITimer? _timer;
    private T? _latest;
    private bool _pending;
    private bool _stopped;

    public DebounceOperator(int milliseconds)
    {
        if (milliseconds < 0)
            throw new ArgumentOutOfRangeException(nameof(milliseconds),
                "Debounce interval must be non-negative.");
        _interval = TimeSpan.FromMilliseconds(milliseconds);
    }

    public DebounceOperator(TimeSpan interval)
    {
        if (interval < TimeSpan.Zero)
            throw new ArgumentOutOfRangeException(nameof(interval),
                "Debounce interval must be non-negative.");
        _interval = interval;
    }

    public override Task OfferAsync(T message)
    {
        if (_interval == TimeSpan.Zero)
            return Dispatch(message);   // no quiet window — emit inline

        lock (_gate)
        {
            if (_stopped) return Task.CompletedTask;
            _latest = message;
            _pending = true;
            // The quiet-window timer comes from the configured clock, so
            // under a virtual-time test the emit fires inside an explicit
            // Advance rather than on a wall-clock timer thread.
            _timer ??= Clock.CreateTimer(static s => ((DebounceOperator<T>)s!).Fire(),
                this, Timeout.InfiniteTimeSpan, Timeout.InfiniteTimeSpan);
            _timer.Change(_interval, Timeout.InfiniteTimeSpan);
        }
        return Task.CompletedTask;
    }

    private void Fire()
    {
        T message;
        lock (_gate)
        {
            if (!_pending || _stopped) return;
            _pending = false;
            message = _latest!;
            _latest = default;
        }

        try
        {
            // In the actor wiring this is a self-Tell (returns completed);
            // a custom downstream that faults here is off any await path,
            // so be loud rather than let the exception vanish unobserved.
            var task = Dispatch(message);
            if (!task.IsCompletedSuccessfully)
                task.ContinueWith(
                    static t => Console.Error.WriteLine(
                        $"[spek] debounce dispatch threw: {t.Exception!.InnerException}"),
                    TaskContinuationOptions.OnlyOnFaulted);
        }
        catch (Exception ex)
        {
            Console.Error.WriteLine($"[spek] debounce dispatch threw: {ex}");
        }
    }

    public override Task StopAsync()
    {
        lock (_gate)
        {
            _stopped = true;
            _pending = false;
            _latest = default;
            _timer?.Dispose();
            _timer = null;
        }
        return Task.CompletedTask;
    }
}
