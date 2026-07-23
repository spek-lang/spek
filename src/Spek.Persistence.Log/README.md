# Spek.Persistence.Log

Append-only segmented `ISnapshotStore` for Spek actors. Each call to
`SaveAsync` appends a new immutable segment file rather than
overwriting the prior snapshot. Modeled on Cassandra's SSTable shape:
many immutable segments per key, latest wins on read, older segments
available for history replay and event-sourcing patterns.

```csharp
using Spek.Persistence.Log;
using Spek.Runtime;

var store = new LogSnapshotStore(
    baseDirectory: "/var/lib/myapp/spek-log",
    maxSegments:   50);   // keep last 50 per key; older auto-deleted

using var system = new ActorSystem("svc", snapshotStore: store);
var account = system.SpawnPersistent<Account>("user-42");
```

## Layout

```
{baseDir}/{sanitizedKey}/0000000001.snapshot.json
{baseDir}/{sanitizedKey}/0000000002.snapshot.json
{baseDir}/{sanitizedKey}/0000000003.snapshot.json   ← latest, returned by LoadAsync
```

Sequence numbers are zero-padded so lexicographic sort matches
chronological order. There's no `mtime` reliance, so clock skew doesn't matter.

## Compatibility with existing actors

`LogSnapshotStore` implements `ISnapshotStore` exactly, so any actor
that uses `ISnapshotStore` works unchanged: `LoadAsync` returns the
latest segment. Actors that need history replay (event sourcing) use
the additional `ReadHistoryAsync(key)` API directly on
`LogSnapshotStore`.

## When to use this vs `Spek.Persistence.File` / `.Sqlite`

| Concern | `.File` | `.Sqlite` | `.Log` |
|---|---|---|---|
| Storage shape | one file per key, overwritten | one DB row per key, upserted | many files per key, append-only |
| History | gone after each save | gone after each upsert | preserved, replayable |
| Best for | simple actors, point-in-time state | services with many actors, query-friendly | event sourcing, audit trails, time-travel debugging, Cassandra-class storage actors |
| Compaction | not applicable | sqlite VACUUM | `CompactAsync(key, keepLast)` |

## Atomicity

Same write-temp + rename pattern as `Spek.Persistence.File`. Each
new segment is written to `*.tmp`, fsynced, then atomically renamed.
Crash mid-write leaves the prior segments intact and discards the
partial write.
