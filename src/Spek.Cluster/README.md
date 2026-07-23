# Spek.Cluster

The composition root for distributed Spek actor clusters. It wires an
`ActorSystem` to a transport, a membership provider, and a placement
strategy.

The contracts (`ISpekTransport`, `ISpekSerializer`,
`IClusterMembership`, `IPlacementStrategy`, `NodeIdentity`,
`ClusterMember`, `ClusterEvent`, `NodeState`, `RemoteEnvelope`) live
in **`Spek.Cluster.Abstractions`** so transport packages and third
parties can depend on the contracts without pulling in the bootstrap.

## What ships in this package

- **`Cluster`** — the composition root. `Cluster.Bind(system,
  transport, membership, placement)` registers a transport with an
  `ActorSystem` and exposes `ResolveRemote(...)` /
  located-actor activation.
- **`StaticSeedClusterMembership`** — the simplest membership
  implementation: a fixed peer list supplied at bootstrap. Good for
  tests and small fleets.
- **`ConsistentHashPlacement`** — the default
  `IPlacementStrategy`. Routes a given actor key to a peer using
  consistent hashing so peer-set churn moves a minimum number of
  actors.

## Usage

```csharp
using Spek.Cluster;
using Spek.Cluster.Abstractions;
using Spek.Runtime;

var system    = new ActorSystem("node-a");
var transport = /* concrete transport, e.g. TcpClusterTransport */;
var members   = new StaticSeedClusterMembership(seeds);
var cluster   = Cluster.Bind(system, transport, members);

var remote = cluster.ResolveRemote("node-b", "actor/path");
remote.Tell(new MyMessage());
```

Pick a transport from a sibling package: `Spek.Cluster.Tcp` (TCP +
length-prefixed framing), `Spek.Cluster.Memory` (in-process pair, for
tests). Each transport references **only** `Spek.Cluster.Abstractions`,
and this package is added on top by the bootstrap host.
