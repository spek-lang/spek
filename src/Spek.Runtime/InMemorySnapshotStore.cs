using System.Collections.Concurrent;
using Spek.Persistence;

namespace Spek.Runtime;

/// <summary>
/// Thread-safe in-memory <see cref="ISnapshotStore"/>. Primary use is tests and
/// quick-start scenarios where you want persistence semantics without a backing
/// file or database. State is lost when the process exits.
/// </summary>
public sealed class InMemorySnapshotStore : ISnapshotStore
{
    private readonly ConcurrentDictionary<string, Snapshot> _snapshots = new();

    /// <inheritdoc />
    public Task SaveAsync(string key, Snapshot snapshot)
    {
        _snapshots[key] = snapshot;
        return Task.CompletedTask;
    }

    /// <inheritdoc />
    public Task<Snapshot?> LoadAsync(string key)
        => Task.FromResult(_snapshots.TryGetValue(key, out var s) ? s : null);
}
