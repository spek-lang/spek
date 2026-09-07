namespace Spek.Persistence;

/// <summary>
/// Backing store for actor snapshots. A persistence-enabled <c>ActorSystem</c>
/// (in <c>Spek.Runtime</c>, which this abstractions layer doesn't reference)
/// is constructed with one of these; it reads on actor spawn (to restore state)
/// and writes when an actor calls <c>persist</c>.
///
/// Keys identify a persistent instance across restarts. The key is the
/// caller-supplied persistence key from <c>SpawnPersistent</c> (regions
/// use their <c>Name</c>).
/// </summary>
public interface ISnapshotStore
{
    /// <summary>Writes (or overwrites) the snapshot for <paramref name="key"/>.</summary>
    Task SaveAsync(string key, Snapshot snapshot);

    /// <summary>
    /// Loads the snapshot for <paramref name="key"/>, or <c>null</c> if nothing
    /// has been persisted for that key yet.
    /// </summary>
    Task<Snapshot?> LoadAsync(string key);
}
