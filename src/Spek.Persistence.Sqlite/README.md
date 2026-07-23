# Spek.Persistence.Sqlite

Embedded SQLite `ISnapshotStore` for Spek actors. Snapshots live in a
single SQLite database file (durable, crash-safe, cross-platform) with
no separate database process required.

```csharp
using Spek.Persistence.Sqlite;
using Spek.Runtime;

var store = new SqliteSnapshotStore("/var/lib/myapp/spek.db");

using var system = new ActorSystem("svc", snapshotStore: store);
var account = system.SpawnPersistent<Account>("user-42");
```

## Schema

```sql
CREATE TABLE IF NOT EXISTS snapshots (
  key        TEXT PRIMARY KEY,
  payload    BLOB NOT NULL,
  updated_at INTEGER NOT NULL
) WITHOUT ROWID;
```

`payload` holds the JSON-encoded snapshot dictionary; `updated_at` is
a UTC unix-millisecond timestamp updated on every write.

## When to use this vs Spek.Persistence.File

| Concern | File | Sqlite |
|---|---|---|
| Setup | zero (just a directory) | zero (single file, auto-created) |
| Atomicity | per-snapshot via temp+rename | full ACID via WAL mode |
| Concurrent writes | per-key lock | DB-level via SQLite |
| Inspection | `cat actor.snapshot.json` | `sqlite3 spek.db` |
| Best for | desktop apps, simple CLI tools | services, anything multi-writer |
