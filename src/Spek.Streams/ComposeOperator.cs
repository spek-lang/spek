namespace Spek.Streams;

/// <summary>
/// Combines several stream operators into one logical operator with
/// shared configuration. The operators run in declaration order:
/// the first operator's <see cref="StreamOperator{T}.Dispatch"/> is
/// wired to the second operator's <see cref="StreamOperator{T}.OfferAsync"/>,
/// the second's dispatch wires to the third, and so on. The
/// composed operator's own dispatch is wired to the final inner
/// operator's dispatch.
///
/// Use when several transforms naturally belong together as one
/// logical step (e.g. "deduplicate then debounce") so the chain
/// reads as a single unit at the call site.
/// </summary>
public sealed class ComposeOperator<T> : StreamOperator<T>
{
    private readonly StreamOperator<T>[] _inner;
    private bool _wired;

    public ComposeOperator(params StreamOperator<T>[] inner)
    {
        if (inner is null) throw new ArgumentNullException(nameof(inner));
        if (inner.Length == 0)
            throw new ArgumentException(
                "Compose requires at least one inner operator.", nameof(inner));
        _inner = inner;
    }

    public override Task OfferAsync(T message)
    {
        // Lazy-wire the inner chain on first message — by this point
        // our own Dispatch has been Configure()'d by the runtime.
        if (!_wired)
        {
            for (int i = 0; i < _inner.Length - 1; i++)
            {
                int next = i + 1;
                _inner[i].Configure(msg => _inner[next].OfferAsync(msg));
            }
            _inner[^1].Configure(msg => Dispatch(msg));
            _wired = true;
        }
        return _inner[0].OfferAsync(message);
    }

    public override async Task StopAsync()
    {
        foreach (var op in _inner)
            await op.StopAsync().ConfigureAwait(false);
    }
}
