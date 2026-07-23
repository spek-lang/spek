using Spek.Persistence;

namespace Spek;

/// <summary>
/// Capability marker for shared regions whose state
/// survives process restart. The compiler emits region classes
/// inheriting from this when the user writes
/// <c>shared X : Persisted { ... }</c>.
///
/// On first reader/writer access, the runtime calls
/// <see cref="TryRestore"/> which fetches the registered store
/// (per-region override or the system default), loads the snapshot
/// at <see cref="SharedRegion.Name"/>, and overlays whichever fields
/// are present. Missing fields keep their compiler-emitted defaults
/// (the "graceful additive" rule). After every writer-handler exit,
/// the region kicks off a save through the same store; saves are
/// serialised per region via a one-slot semaphore so concurrent
/// writers don't clobber each other.
///
/// This ships with no throttle / coalesce — every writer-exit
/// triggers an immediate save. Throughput optimisations (debounce,
/// rate-limit) are out of scope here; the broader stream-policy design is
/// in the shared-regions chapter of the language guide.
/// </summary>
public abstract class PersistedRegion : SharedRegion
{
    private readonly SemaphoreSlim _saveLock = new(initialCount: 1, maxCount: 1);

    /// <summary>
    /// Subclass override (compiler-generated) — captures every region
    /// field into a <see cref="Snapshot"/>-friendly dictionary keyed
    /// by Spek field name. The runtime calls this on every save.
    /// </summary>
    protected abstract IReadOnlyDictionary<string, object?> CaptureFields();

    /// <summary>
    /// Subclass override (compiler-generated) — overwrites every
    /// region field whose name appears as a key in
    /// <paramref name="snapshot"/>. Missing keys leave the field at
    /// its compiler-emitted default. Type mismatches surface as the
    /// underlying <see cref="Snapshot.Get{T}"/> exception.
    /// </summary>
    protected abstract void RestoreFields(Snapshot snapshot);

    /// <summary>
    /// Resolves the snapshot store for this region. Per-region
    /// registration (<see cref="Spek.Runtime.ActorSystem.RegisterPersistenceProvider{T}"/>)
    /// wins; falls back to the system's default
    /// <see cref="Spek.Runtime.ActorSystem.SnapshotStore"/>.
    /// Returns <c>null</c> only when the region was instantiated
    /// outside an <c>ActorSystem</c> (test scaffolding) — in which
    /// case persistence silently no-ops.
    /// </summary>
    private ISnapshotStore? ResolveStore()
    {
        if (System is null) return null;
        return System.GetRegionStore(GetType()) ?? System.SnapshotStore;
    }

    /// <summary>
    /// Loads this region's snapshot from its resolved store and overlays the
    /// present fields onto the region (via <see cref="RestoreFields"/>). Runs at
    /// most once per region instance, inside the init lock. Returns <c>true</c>
    /// if a snapshot was found and applied (so <c>init { }</c> is skipped),
    /// <c>false</c> if there was no store or no snapshot.
    /// </summary>
    protected override bool TryRestore()
    {
        var store = ResolveStore();
        if (store is null) return false;

        // Synchronous wait is safe here — we're inside the
        // EnsureInitializedAsync init lock, no other reader/writer
        // can proceed until we return. Restore happens at most once
        // per region instance.
        var snapshot = store.LoadAsync(Name).GetAwaiter().GetResult();
        if (snapshot is null) return false;

        // Schema-change runtime warning. If the snapshot
        // has keys that no longer correspond to fields on this region,
        // log them once. The next save will drop them.
        WarnOnDroppedKeys(snapshot);

        RestoreFields(snapshot);
        return true;
    }

    /// <summary>
    /// Releases the writer lock (via the base implementation) and then kicks off
    /// a fire-and-forget save of the region's current state. Saves are serialised
    /// per region, so concurrent writers queue rather than clobber — the latest
    /// captured state wins.
    /// </summary>
    public override void ExitWriter()
    {
        base.ExitWriter();
        // Fire-and-forget save. Serialised against other in-flight
        // saves on this region via _saveLock; concurrent writers
        // queue but don't drop writes (the latest captured state
        // always wins).
        _ = SaveAsync();
    }

    /// <summary>
    /// Synchronous final save for the shutdown path. Captures the region's
    /// current state and persists it, waiting for completion (and for any in-flight
    /// fire-and-forget save to finish, via the same <see cref="_saveLock"/>). Called
    /// by <see cref="Spek.Runtime.ActorSystem.Dispose"/> so a graceful shutdown is
    /// durable: the last writer's state can't be lost to a save racing teardown.
    /// Blocking is acceptable here — it runs once, at teardown, off the dispatch path.
    /// </summary>
    internal void FlushSave() => SaveAsync().GetAwaiter().GetResult();

    private async Task SaveAsync()
    {
        var store = ResolveStore();
        if (store is null) return;

        await _saveLock.WaitAsync().ConfigureAwait(false);
        try
        {
            var snapshot = new Snapshot(CaptureFields());
            await store.SaveAsync(Name, snapshot).ConfigureAwait(false);
        }
        catch (Exception ex)
        {
            // Surface to the system's dead-letter sink so the failure
            // is observable; persistence faults shouldn't bring down
            // the actor system.
            System?.DeadLetterSink?.DeadLetter(
                this, $"PersistedRegion '{Name}' save failed", cause: ex);
        }
        finally
        {
            _saveLock.Release();
        }
    }

    private void WarnOnDroppedKeys(Snapshot snapshot)
    {
        // Capture current field names by running CaptureFields once
        // (cheap — just a dictionary build, no I/O). Compare against
        // snapshot keys; surface anything in the snapshot that's not
        // in the current schema.
        IReadOnlyDictionary<string, object?> currentFields;
        try
        {
            currentFields = CaptureFields();
        }
        catch
        {
            // CaptureFields could throw if subclass state is mid-init;
            // a snapshot-key warning is best-effort, not load-blocking.
            return;
        }

        var droppedKeys = new List<string>();
        foreach (var key in snapshot.Fields.Keys)
        {
            if (!currentFields.ContainsKey(key))
                droppedKeys.Add(key);
        }
        if (droppedKeys.Count == 0) return;

        // Console-log for now; future work could route through a
        // proper diagnostics channel. The dead-letter sink isn't quite
        // right (this isn't a dropped message), so a stderr line is
        // the simplest first-shipped behaviour.
        Console.Error.WriteLine(
            $"WARN: PersistedRegion '{Name}' snapshot has {droppedKeys.Count} " +
            $"key(s) with no matching field: [{string.Join(", ", droppedKeys)}]. " +
            $"These will be lost on the next save.");
    }
}
