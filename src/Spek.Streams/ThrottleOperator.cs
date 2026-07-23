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

    // Timestamp (in Clock.GetTimestamp units) of the current window's
    // open, 0 = never emitted. The sentinel is safe on both clock
    // families: the system clock's timestamp is time-since-boot (never
    // 0 at runtime) and the manual test clock's is DateTimeOffset
    // UtcTicks (its epoch defaults to year 2000).
    private long _windowStartTimestamp;

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
        // Window math rides the configured clock: GetTimestamp is the
        // monotonic source on the system clock (no wall-clock jump can
        // reopen or pin a window) and is virtualized by the test kit's
        // manual clock, so a virtual-time test Advances the window
        // instead of sleeping through it.
        var clock     = Clock;
        var nowTs     = clock.GetTimestamp();
        var startedAt = Interlocked.Read(ref _windowStartTimestamp);

        if (startedAt == 0 || clock.GetElapsedTime(startedAt, nowTs) >= _interval)
        {
            // Take this message — open a new window.
            if (Interlocked.CompareExchange(ref _windowStartTimestamp, nowTs, startedAt) == startedAt)
            {
                await Dispatch(message).ConfigureAwait(false);
            }
            // If CAS failed, another concurrent caller already opened
            // the window for this slot; drop this message quietly.
        }
        // Otherwise inside the window — drop.
    }
}
