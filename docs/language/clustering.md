---
title: Clustering
layout: default
parent: Language
nav_order: 22
permalink: /language/clustering/
description: "Distributed Spek: actors that span machines. Bedrock-aligned ISpekTransport, location-transparent ActorRef, at-most-once delivery."
---

# Clustering

Spek brings the actor model across the network. Everything below
is opt-in. Single-node Spek programs continue to work exactly as
before. Distributed Spek adds four projects on top:

| Project | Purpose |
|---|---|
| `Spek.Cluster.Abstractions` | Bedrock-aligned contracts: `ISpekTransport`, `NodeIdentity`, `RemoteEnvelope`, `IClusterMembership` |
| `Spek.Cluster` | The bootstrap layer: `Cluster`, consistent-hash placement, static-seed membership |
| `Spek.Cluster.Tcp` | Default transport: Spek-binary protocol over TCP |
| `Spek.Cluster.Memory` | In-process transport for tests |

{: .note }
> **Where this comes from.** Spreading actors across nodes is Akka Cluster and
> Orleans territory. Consistent-hash placement and location-transparent
> `ActorRef`s come from that playbook. The single-node programming model is
> unchanged: the same actor runs local or remote.

## Quick start

A two-process service. Each process knows its peer's identity up front
(static configuration).

**Process A, the auth service:**

```csharp
using Spek.Cluster;
using Spek.Cluster.Tcp;
using Spek.Runtime;

var optionsA = new TcpClusterOptions
{
    Label          = "auth-svc",
    ListenEndpoint = new IPEndPoint(IPAddress.Any, 5050),
    LoopbackOnly   = false,
};

await using var transportA = new TcpClusterTransport(optionsA);
using var systemA = new ActorSystem("auth");
var clusterA = Cluster.Bind(systemA, transportA);

clusterA.RegisterPeer("user-svc",
    new NodeIdentity(KnownIds.UserService, "user-svc"));
await transportA.ConnectToPeerAsync(
    new IPEndPoint(KnownEndpoints.UserService, 5050),
    new NodeIdentity(KnownIds.UserService, "user-svc"));

// Spawn an actor wire-addressable as "auth/login":
systemA.SpawnNamed<LoginActor>("auth/login");
```

**Process B, the user service, calling auth from a handler:**

```csharp
// Inside a handler on Process B:
var loginActor = clusterB.ResolveRemote("auth-svc", "auth/login");
loginActor.Tell(new LoginRequest(username, password));
```

That's it. From the message-sending code's perspective, `loginActor`
is just an `ActorRef`. Whether it's local or remote is transparent. The only difference is the latency and the wire format the runtime
uses underneath.

## Location transparency

`ActorRef` doesn't tell callers whether the target is local or remote.
This is deliberate. The same code works in unit tests
(`Spek.Cluster.Memory` transport) and in production (`Spek.Cluster.Tcp`)
without changes.

```csharp
target.Tell(new SomeMessage());
```

The receiver's code is identical too. `sender.Tell` routes back across
the wire when the originator was remote:

```spek
behavior Idle
{
    on SomeMessage => { sender.Tell(new ResponseMessage()); }
}
```

The runtime carries enough envelope information that `sender.Tell(reply)`
on a receiving actor reaches the original sender even if it's three nodes
away.

## Named-root actors

Only **named-root actors** are wire-addressable. To make an actor
remotely reachable, spawn it with `SpawnNamed`:

```csharp
systemB.SpawnNamed<MyActor>("worker");      // wire-addressable as "worker"
systemB.Spawn<MyActor>();                    // local-only
```

Named-root paths use forward slashes for hierarchy (`"workers/alice"`,
`"services/auth/login"`). Paths are flat strings. The runtime treats them as opaque keys.

## Discovery and lifecycle

### Static configuration

Each process is told upfront who its peers are. No announcement, no
joining state, no membership view:

```csharp
clusterA.RegisterPeer("auth-svc", knownAuthIdentity);
clusterB.RegisterPeer("auth-svc", knownAuthIdentity);   // same identity, both sides know
```

If a peer's actual identity at handshake doesn't match the configured
expected identity, the connection is refused (defensive against
accidental cross-cluster connection).

This works for two-node and small fixed-topology deployments.

### Membership state machine

The `NodeState` state machine and `IClusterMembership`
abstraction track cluster membership. Each peer transitions through:

```
Joining → Up → Leaving → Exiting → Down
         └→ Unreachable ⇄ Up
         └→ Down
```

State changes raise typed `ClusterEvent` notifications subscribers
react to:

```csharp
using var sub = cluster.Membership.Subscribe(ev =>
{
    switch (ev)
    {
        case ClusterEvent.NodeJoining n:        Console.WriteLine($"joining: {n.Identity}"); break;
        case ClusterEvent.NodeUp n:             Console.WriteLine($"up:      {n.Identity}"); break;
        case ClusterEvent.NodeLeaving n:        Console.WriteLine($"leaving: {n.Identity}"); break;
        case ClusterEvent.NodeExiting n:        Console.WriteLine($"exiting: {n.Identity}"); break;
        case ClusterEvent.NodeUnreachable n:    Console.WriteLine($"unreach: {n.Identity}"); break;
        case ClusterEvent.NodeReachableAgain n: Console.WriteLine($"back:    {n.Identity}"); break;
        case ClusterEvent.NodeDown n:           Console.WriteLine($"down:    {n.Identity}"); break;
    }
});
```

Cluster view query at any point:

```csharp
foreach (var member in cluster.Membership.Members)
    Console.WriteLine($"{member.Identity} state={member.State} dc={member.Metadata?["datacenter"]}");
```

Graceful leave at shutdown:

```csharp
await cluster.LeaveAsync();   // local node moves Leaving → Exiting; peers notified
```

### Locality metadata

Each peer can announce free-form metadata at registration:

```csharp
clusterA.RegisterPeer("auth-svc",
    knownAuthIdentity,
    metadata: new Dictionary<string, string>
    {
        ["datacenter"] = "us-east-1",
        ["zone"]       = "us-east-1a",
        ["rack"]       = "r-42",
        ["region"]     = "us-east",
    });
```

The metadata is available via
`cluster.Membership.Members[i].Metadata` for user code that wants
custom routing.

The membership abstractions ship with a deterministic
`StaticSeedClusterMembership` reference implementation. Alternative
`IClusterMembership` implementations plug into the same interface, so
switching between static-seed and other discovery mechanisms is a
configuration change.

## Delivery guarantees

**At-most-once with per-(sender, recipient) ordering.** Same model
Akka, Erlang, and Orleans use. If you need stronger semantics, build
them in your protocol. The runtime gives you the primitive.

| Guarantee | Local | Remote (TCP) |
|---|---|---|
| At-most-once | yes | yes |
| Per-pair ordering | yes (mailbox FIFO) | yes (single TCP connection per peer) |
| Dead-letter on fail | yes | yes (`ISpekTransport.DeliveryFailed`) |
| At-least-once | not built in (application layer) | not built in (application layer) |
| Exactly-once | impossible without coordination | impossible without coordination |
| Cross-restart durability | only via `persist;` | only via `persist;` |
| Global ordering | no (per-pair only) | no (per-pair only) |

At-least-once is a protocol pattern, not a transport setting: a
request-id plus ack plus retry, with the receiver deduping by
request-id so processing stays idempotent. Every reliable distributed
system builds that recipe on top of an at-most-once primitive.
Timeouts come from `Ask`, which raises `TimeoutException` when no
reply arrives inside the window. Prefer it over `Tell` whenever the
caller needs to notice silence.

## Transports

`ISpekTransport` is the Bedrock-aligned plug-in point, and two transports
ship behind it.

### `Spek.Cluster.Tcp`: production default

The TCP transport speaks a Spek-native binary protocol with length-prefixed
framing and serializes payloads as JSON (System.Text.Json), which works on
every Spek-emitted record without code-generation annotations. Connections
verify peer identity at handshake, and each peer pair shares a single TCP
connection, opened lazily.

### `Spek.Cluster.Memory`: test transport

The in-memory transport is an in-process registry with direct dispatch: no
sockets, no serialization. It exists so tests exercise the same code paths a
real cluster uses without any setup overhead. Use it for unit tests, never in
production.

Both transports implement `ISpekTransport`. Swapping one for the other
touches only the `Cluster.Bind` call.

## Authentication and TLS

TLS / mTLS / cluster-shared-secret authentication is not built in.
Keep single-machine clusters on `LoopbackOnly = true`, span machines
only over a VPN or private subnet, and never expose the TCP transport
to the public internet.

## Located actors

**Located actors** are actors addressed by a logical key,
placed automatically across the cluster, activated on demand. You
don't `spawn` a located actor; you call `Locate<TActor>(key)`, get
back an `ActorRef`, and the runtime ensures exactly one instance
exists per (`TActor`, `key`) pair across the entire cluster. (Akka
calls this "cluster sharding" and Orleans calls them "grains"; Spek's
**located actor** name is independent and emphasizes the
location-transparency story already at the heart of `ActorRef`.)

### Quick start

```csharp
// 1. Register your actor type as cluster-locatable.
cluster.RegisterLocatedActor<UserActor>();

// 2. Locate an instance — placement is automatic.
var alice = cluster.Locate<UserActor>("alice");
var bob   = cluster.Locate<UserActor>("bob");

// 3. Tell normally. Whichever node alice is placed on auto-activates.
alice.Tell(new Greet("hello"));
bob.Tell(new Greet("hi"));
```

The actor receives its location key as the first constructor argument,
so declare your actor with `(string key)` as the first param.

### Placement is deterministic

The default `ConsistentHashPlacement` uses **rendezvous hashing**
(HRW) over the cluster's `Up` members. Every node calling
`Locate<UserActor>("alice")` arrives at the same owner without
coordination, the same property systems like Riak,
Couchbase, and Envoy use for the same reason.

```csharp
var owner = cluster.Placement.ResolveOwner(
    "MyApp.UserActor", "alice", cluster.Membership.Members);
```

### Custom placement strategies

`IPlacementStrategy` is the plug-in point. Implement it for
locality-aware placement:

```csharp
public sealed class PreferSameZonePlacement : IPlacementStrategy
{
    public NodeIdentity? ResolveOwner(string actorType, string actorKey,
                                       IReadOnlyList<ClusterMember> members)
    {
        var myZone = Environment.GetEnvironmentVariable("SPEK_ZONE");
        var sameZone = members.Where(m =>
            m.State == NodeState.Up &&
            m.Metadata?.GetValueOrDefault("zone") == myZone).ToList();
        return sameZone.Count > 0
            ? new ConsistentHashPlacement().ResolveOwner(actorType, actorKey, sameZone)
            : new ConsistentHashPlacement().ResolveOwner(actorType, actorKey, members);
    }
}

var cluster = Cluster.Bind(system, transport,
    placement: new PreferSameZonePlacement());
```

### Auto-activation across the wire

When a Tell arrives at a node for a location path the local node
hosts but hasn't activated yet, `Cluster.OnReceiveAsync`
auto-activates before delivery. The path format is
`{TypeFullName}/{key}`, where the prefix identifies the registered
located-actor type.

This means a located actor doesn't need to be pre-spawned anywhere:
- Caller calls `cluster.Locate<UserActor>("alice").Tell(msg)`
- Placement resolves to node B
- Node A's transport sends envelope with target `"MyApp.UserActor/alice"`
- Node B receives, sees no local activation, looks up registered type,
  spawns the actor, delivers the message
- Subsequent calls to "alice" route to the same node and (now) the
  same activation

### Limits

Located actors ship **placement + activation**. Several pieces
are not built in:

- **Rebalancing on membership change.** When a node leaves or joins,
  some located actors' owner shifts (deterministic, by hash).
  Their state stays on the old owner; rehydration on the new
  owner requires snapshot transfer.
- **Cross-node rehydration.** When a node fails, the located actors
  it hosted are not re-spawned on their new owner from the latest
  snapshot.
- **Idle passivation policies.** Spek's local passivation does not
  extend to located-actor activations.
- **Locality-aware placement.** There is no built-in
  locality-aware placement strategy. Custom strategies work via
  `IPlacementStrategy`.
- **Typed location refs.** There is no `LocatedRef<TActor, TKey>`
  with reply-type inference.

## Test verification

The claim that two processes can find each other is verified at two layers.
The bulk of the test suite runs in-process: two `TcpClusterTransport`
instances on different localhost ports, with real sockets, real frame
parsing, and real serialization. A smaller smoke-test fixture goes one layer
further and uses `Process.Start` to spawn two real .NET processes that
connect over TCP and exchange messages, which covers process-startup
ordering and loopback reachability. Verification across physical machines is
a manual exercise.
