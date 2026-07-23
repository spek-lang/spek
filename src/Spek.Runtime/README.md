# Spek.Runtime

Runtime for the [Spek](https://github.com/spek-lang/spek) actor language.

This package is a dependency of any assembly compiled from `.spek` source. You
don't write `Spek.Runtime` directly. You write Spek, run it through `spekc`, and
the generated C# uses these primitives.

## Namespace layout

User-facing fundamentals live in the bare `Spek` namespace; engine internals
stay in `Spek.Runtime`. Persistence types live in `Spek.Persistence`
(see `Spek.Persistence.Abstractions`).

## What's inside

### `namespace Spek` (user-facing)

- `ActorBase` — base class for generated actor classes. Override lifecycle
  hooks (`OnPreStart`, `OnPostStop`, `OnRestore`, `OnPassivate`, `OnFailure`,
  `OnChildFailure`) to customise behavior.
- `ActorRef` — stable reference to an actor. The underlying instance can be
  replaced on Restart or unloaded on passivation without invalidating refs.
  Also exposes `AttachIngressPolicy(...)` for `Spek.Resilience` policies.
- `IRemoteEndpoint` — adapter contract for remote refs (implemented by the
  cluster layer).
- `FailureDirective` — `Resume`, `Restart`, `Escalate`, `Stop`.
- `Outcome<T,E>`, `Outcome<T>` — typed success/failure reply payloads.
- `LamportClock`, `VectorClock` — logical clocks for distributed causality.

### `namespace Spek.Runtime` (engine)

- `ActorSystem` — root of an actor hierarchy; spawn top-level actors,
  configure the snapshot store and dead-letter sink, register a cluster
  adapter.
- `IDeadLetterSink` (+ `ConsoleDeadLetterSink`, `RecordingDeadLetterSink`)
  — observe unhandled and dropped messages.
- `IClusterAdapter` — the runtime's hook for the cluster layer.
- `InMemorySnapshotStore` — default in-memory implementation of
  `ISnapshotStore` (the contract itself lives in
  `Spek.Persistence.Abstractions`).

## Status

Public **0.x preview**. Packages are versioned `0.<era>.<build>`; the whole 0.x
line is the preview, with no `-alpha` or `-preview` suffix. The package is
pre-1.0, so the API surface can change without notice and is not frozen until
`1.0`.

## License

Apache-2.0.
