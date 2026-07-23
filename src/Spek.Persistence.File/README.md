# Spek.Persistence.File

Local-disk `ISnapshotStore` for Spek actors. Each snapshot serializes
to a single JSON file under a configurable base directory; writes go
through a temp file + atomic rename so a crash mid-write can never
corrupt an existing snapshot.

```csharp
using Spek.Persistence.File;
using Spek.Runtime;

var store = new FileSnapshotStore(baseDirectory: "/var/lib/myapp/spek");

using var system = new ActorSystem("desktop", snapshotStore: store);
var account = system.SpawnPersistent<Account>("user-42");
```

## Layout

Snapshots live at `{baseDir}/{sanitizedKey}.snapshot.json`. Keys with
slashes (`actor/persistence-id`) become subdirectories; other unsafe
filename characters are percent-escaped.

## When to use this

- Desktop apps where "ship a Postgres alongside" is absurd.
- CLI tools that survive across runs.
- Single-node services where a separate database is overkill.
- Edge / appliance scenarios.

For multi-node clusters, swap to a shared store
(`Spek.Persistence.Sqlite` for single-node embedded, or a future
managed-DB adapter).

## Atomicity

Writes use **write-temp + fsync + rename**. POSIX guarantees the
rename is atomic with respect to other observers; on Windows the
underlying `MoveFile` provides the same semantics on the same
volume. A crash mid-write leaves either the old snapshot intact or
the new one in place, never a partial file.
