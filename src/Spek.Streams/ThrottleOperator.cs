namespace Spek.Streams;

/// <summary>
/// Emits at most one message per configured interval. Messages
/// that arrive while a previous emit is still within the window
/// are dropped (leading-edge throttle: the first message in each
/// window passes; the rest do not).
///
/// Typical use: a UI MouseMove handler that should run at most
/// every 16ms (~60fps), or a metric ingest that wants to cap the
/// downstream rate during a burst.
/// </summary>
public sealed class ThrottleOperator<T> : StreamOperator<T>
{
    private readonly TimeSpan _interval;
    private long _windowStartTicks;   // 0 = never emitted

    public ThrottleOperator(int milliseconds)
    {
        if (milliseconds < 0)
            throw new ArgumentOutOfRangeException(nameof(milliseconds),
                "Throttle interval must be non-negative.");
        _interval = TimeSpan.FromMilliseconds(milliseconds);
    }

    public ThrottleOperator(TimeSpan interval)
    {
        if (interval < TimeSpan.Zero)
            throw new ArgumentOutOfRangeException(nameof(interval),
                "Throttle interval must be non-negative.");
        _interval = interval;
    }

    public override async Task OfferAsync(T message)
    {
        var nowTicks = DateTimeOffset.UtcNow.UtcTicks;
        var startedAt = Interlocked.Read(ref _windowStartTicks);
        var elapsed   = TimeSpan.FromTicks(nowTicks - startedAt);

        if (startedAt == 0 || elapsed >= _interval)
        {
            // Take this message — open a new window.
            if (Interlocked.CompareExchange(ref _windowStartTicks, nowTicks, startedAt) == startedAt)
            {
                await Dispatch(message).ConfigureAwait(false);
            }
            // If CAS failed, another concurrent caller already opened
            // the window for this slot; drop this message quietly.
        }
        // Otherwise inside the window — drop.
    }
}
