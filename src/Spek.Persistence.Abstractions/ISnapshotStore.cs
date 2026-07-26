namespace Spek.Persistence;

/// <summary>
/// Backing store for actor snapshots. A persistence-enabled <c>ActorSystem</c>
/// (in <c>Spek.Runtime</c>, which this abstractions layer doesn't reference)
/// is constructed with one of these; it reads on actor spawn (to restore state)
/// and writes when an actor calls <c>persist</c>.
///
/// Keys identify a persistent instance across restarts. The current key
/// format is <c>"actorType/persistenceId"</c>; the emitter generates a
/// stable id from the actor's declared persistence-id member (if any) or the
/// runtime-assigned spawn id as a fallback. Persistent-identity semantics
/// will firm up as the runtime's persistence story matures.
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
