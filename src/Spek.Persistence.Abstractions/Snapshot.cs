// Snapshot lives in the persistence-abstractions package because it's a
// persistence concept (the per-actor state-bag passed to `on Restore`).
// The emitter auto-imports `using Spek.Persistence;` so .spek source can
// write `on Restore(Snapshot s)` without ceremony.
using System.Text.Json;

namespace Spek.Persistence;

/// <summary>Immutable snapshot of all actor fields, used by persist and passivate.</summary>
public sealed class Snapshot
{
    private readonly IReadOnlyDictionary<string, object?> _fields;

    /// <summary>
    /// Wraps an already-captured field map — field name to value, one
    /// entry per actor field. The runtime builds these when an actor
    /// persists or passivates; stores rebuild them on load, where
    /// values may resurface as <see cref="JsonElement"/>s (see
    /// <see cref="Get{T}"/>).
    /// </summary>
    public Snapshot(IReadOnlyDictionary<string, object?> fields) => _fields = fields;

    /// <summary>The raw field map. Persistence providers iterate this to
    /// serialize; tests / inspectors use it for assertions.</summary>
    public IReadOnlyDictionary<string, object?> Fields => _fields;

    /// <summary>
    /// Reads the field stored under <paramref name="key"/>. Two storage
    /// shapes are tolerated transparently:
    /// <list type="bullet">
    ///   <item><b>Live object</b> — produced by
    ///         <c>InMemorySnapshotStore</c>: a direct cast to
    ///         <typeparamref name="T"/> handles primitives, records,
    ///         user-defined types.</item>
    ///   <item><b><see cref="JsonElement"/></b> — produced by
    ///         JSON-backed stores (<c>Spek.Persistence.File</c>,
    ///         <c>Spek.Persistence.Sqlite</c>): deserialised into
    ///         <typeparamref name="T"/> via
    ///         <see cref="JsonSerializer.Deserialize{T}(JsonElement, JsonSerializerOptions)"/>.</item>
    /// </list>
    /// </summary>
    public T Get<T>(string key)
    {
        if (!_fields.TryGetValue(key, out var value))
            throw new KeyNotFoundException($"Snapshot does not contain field '{key}'.");

        if (value is null) return default!;
        if (value is T typed) return typed;
        if (value is JsonElement element) return element.Deserialize<T>()!;

        // Fall back to a typed conversion — covers numeric promotions
        // (int boxed as long, double as decimal, etc.) that come back
        // from JSON's number parsing.
        return (T)Convert.ChangeType(value, typeof(T))!;
    }
}
