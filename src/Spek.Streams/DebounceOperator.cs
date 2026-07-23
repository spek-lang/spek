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
/// </summary>
public sealed class DebounceOperator<T> : StreamOperator<T>
{
    private readonly TimeSpan _interval;
    private int _generation;
    private T? _latest;

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

    public override async Task OfferAsync(T message)
    {
        _latest = message;
        var generation = Interlocked.Increment(ref _generation);

        await Task.Delay(_interval).ConfigureAwait(false);

        if (Volatile.Read(ref _generation) == generation)
        {
            // No newer message arrived during the wait — emit.
            await Dispatch(_latest!).ConfigureAwait(false);
        }
        // Otherwise: a newer message superseded us; drop quietly.
    }
}
