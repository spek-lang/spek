namespace Spek.Streams;

/// <summary>
/// Emits a message only when the key extracted by the configured
/// selector differs from the previous emit's key. The first
/// message always emits.
///
/// Typical use: deduplicate a stream where logically-equal events
/// arrive multiple times (a `UserCreated` event redelivered by an
/// at-least-once transport, a config-change notification fired
/// repeatedly with the same content).
/// </summary>
public sealed class DistinctOperator<T, TKey> : StreamOperator<T>
{
    private readonly Func<T, TKey> _keySelector;
    private readonly IEqualityComparer<TKey> _comparer;
    private TKey? _lastKey;
    private bool _hasEmitted;
    private readonly object _gate = new();

    public DistinctOperator(Func<T, TKey> keySelector)
        : this(keySelector, EqualityComparer<TKey>.Default) { }

    public DistinctOperator(Func<T, TKey> keySelector, IEqualityComparer<TKey> comparer)
    {
        _keySelector = keySelector ?? throw new ArgumentNullException(nameof(keySelector));
        _comparer    = comparer ?? throw new ArgumentNullException(nameof(comparer));
    }

    public override async Task OfferAsync(T message)
    {
        var key = _keySelector(message);

        bool shouldEmit;
        lock (_gate)
        {
            if (!_hasEmitted)
            {
                _hasEmitted = true;
                _lastKey = key;
                shouldEmit = true;
            }
            else if (!_comparer.Equals(_lastKey, key))
            {
                _lastKey = key;
                shouldEmit = true;
            }
            else
            {
                shouldEmit = false;
            }
        }

        if (shouldEmit)
            await Dispatch(message).ConfigureAwait(false);
    }
}
