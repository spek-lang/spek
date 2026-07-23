# Spek.Persistence.Abstractions

The contracts that concrete Spek persistence providers implement.

## Surface

- **`Snapshot`** — immutable per-actor state-bag passed to
  `on Restore(Snapshot s)` handlers in `.spek` source.
- **`ISnapshotStore`** — pluggable contract for persistence
  providers. Default `InMemorySnapshotStore` lives in
  `Spek.Runtime`; durable providers (`Spek.Persistence.File`,
  `Spek.Persistence.Sqlite`) ship as separate packages and
  reference this one.

## Why this is its own package

Following Microsoft's pattern: `Microsoft.Extensions.Logging.Abstractions`
contains just `ILogger` so library authors can take a dependency on
the contract without pulling the default implementation. Same shape
here: third-party persistence-provider authors reference
`Spek.Persistence.Abstractions` and ship their own
`ISnapshotStore` implementations without dragging the in-memory
default along.
