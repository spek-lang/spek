# Spek.Cluster.Abstractions

The contracts that concrete Spek cluster transports and membership
providers implement.

## Surface

- **`ISpekTransport`** — pluggable wire-adapter contract; modeled
  after `IConnectionListenerFactory` from
  `Microsoft.AspNetCore.Connections.Abstractions`.
- **`IClusterMembership`** — pluggable membership-strategy contract.
- **`IPlacementStrategy`** — maps located-actor logical keys to the
  cluster node that owns them.
- **`ISpekSerializer`** — pluggable wire-payload serialization.
- **Value records:** `NodeIdentity`, `ClusterMember`, `ClusterEvent`
  (with the joining/up/leaving/exiting/unreachable/down state
  transitions), `NodeState`, `RemoteEnvelope`.

## Why this is its own package

Following Microsoft's pattern: third-party authors building
alternative transports (`MyCompany.Spek.Cluster.RabbitMq` etc.) or
membership providers (`MyCompany.Spek.Cluster.HashicorpConsul`)
reference `Spek.Cluster.Abstractions` for the contracts without
pulling in the reference `Cluster.Bind` / `StaticSeedClusterMembership`
/ `ConsistentHashPlacement` implementations from `Spek.Cluster`.
