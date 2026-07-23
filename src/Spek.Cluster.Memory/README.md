# Spek.Cluster.Memory

In-process Spek cluster transport. It bridges multiple `ActorSystem`
instances inside a single CLR process via direct dispatch, which makes it a good
fit for unit and integration tests: no wire serialization, no sockets.

It implements the Bedrock-aligned `ISpekTransport` contract, the same interface
the production TCP transport targets, so test code exercises the same code
paths real clusters do.

This package depends on `Spek.Cluster.Abstractions` only. Add a
reference to the `Spek.Cluster` bootstrap package alongside it when
you want `Cluster.Bind`, the default membership/placement
implementations, etc.

## Quick start

```csharp
using Spek;                          // ActorRef
using Spek.Cluster;                  // Cluster.Bind
using Spek.Cluster.Abstractions;     // NodeIdentity, ISpekTransport
using Spek.Cluster.Memory;           // InMemoryClusterFabric
using Spek.Runtime;                  // ActorSystem

// Shared in-process fabric — wires multiple systems together.
using var fabric = new InMemoryClusterFabric();

using var systemA = new ActorSystem("a");
using var systemB = new ActorSystem("b");

var transportA = fabric.CreateTransport("node-a");
var transportB = fabric.CreateTransport("node-b");

var clusterA = Cluster.Bind(systemA, transportA);
var clusterB = Cluster.Bind(systemB, transportB);

clusterA.RegisterPeer("node-b", transportB.LocalNode);
clusterB.RegisterPeer("node-a", transportA.LocalNode);

// Spawn a named-root actor on B. Get a remote ref from A. Tell.
systemB.SpawnNamed<MyActor>("worker");
var remoteWorker = clusterA.ResolveRemote("node-b", "worker");
remoteWorker.Tell(new SomeMessage());
```

## When to use this vs `Spek.Cluster.Tcp`

- Tests: always. No socket setup, no port allocation, deterministic.
- Local development: run multi-system layouts inside a single process while you
  iterate, then switch to TCP for real deployments.
- Production: never. The "fabric" is a single dictionary, so both systems must
  live in the same process.
