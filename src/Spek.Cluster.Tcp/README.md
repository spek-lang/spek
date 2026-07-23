# Spek.Cluster.Tcp

Default Spek cluster transport. Spek-native binary protocol over TCP,
modeled after Akka Artery's TCP mode and Orleans 3.0's Bedrock-based silo wire.

> ⚠️ **Security: not production-ready.** The wire is **currently unencrypted
> and peers are not authenticated**. TLS 1.3 / mTLS and shared-secret enforcement
> are designed but **not implemented**. The only protection enforced today is
> `LoopbackOnly`. **Do not run this across an untrusted network.** Keep clusters
> on a trusted/isolated network or loopback.

It implements the Bedrock-aligned `ISpekTransport` contract, the same interface
the in-memory transport (for tests) targets.

## Wire format

Each message frames as:

```
[4-byte BE length-of-rest]
[1-byte version (0x01)]
[4-byte BE typeName length][typeName UTF-8]
[4-byte BE targetPath length][targetPath UTF-8]
[4-byte BE senderPath length][senderPath UTF-8]   (length 0 = no sender)
[16-byte SenderNode UUID]                          (zeros = no sender node)
[4-byte BE senderLabel length][senderLabel UTF-8]  (length 0 = no label)
[4-byte BE payload length][payload bytes]
```

Every field uses a fixed 4-byte big-endian length prefix (standard
length-prefixed binary framing). Payloads are JSON-serialized by default
(`JsonSpekSerializer`); a compact binary serializer (MemoryPack / MessagePack)
is a planned swap-in at host-bootstrap time for cross-language clusters.

## Authentication

These are the **designed** modes; only `LoopbackOnly` is enforced today (see the
security warning above):

| Mode | When | Status |
|---|---|---|
| **mTLS** | Production. Each node carries a cert signed by the cluster CA. | designed, not implemented |
| **Cluster shared-secret** | Dev / internal. PSK-style symmetric token (`ClusterSharedKey`). | **plumbed but not enforced**; setting it logs a warning |
| **Loopback-only** (`LoopbackOnly`) | Single-host. Refuses non-`127.0.0.1` connections. | enforced |

This package depends on `Spek.Cluster.Abstractions` only. Add the
`Spek.Cluster` bootstrap package alongside it for `Cluster.Bind` and
the default membership / placement implementations.

## Quick start

```csharp
using Spek;                          // ActorRef
using Spek.Cluster;                  // Cluster.Bind
using Spek.Cluster.Abstractions;     // NodeIdentity, ISpekTransport
using Spek.Cluster.Tcp;              // TcpClusterTransport
using Spek.Runtime;                  // ActorSystem

using var system = new ActorSystem("auth-svc");

var transport = new TcpClusterTransport(new TcpClusterOptions
{
    Label             = "auth-svc-2",
    ListenEndpoint    = new IPEndPoint(IPAddress.Loopback, 5050),
    LoopbackOnly      = true,   // the only protection enforced today
});

var cluster = Cluster.Bind(system, transport);
cluster.RegisterPeer("user-svc", new NodeIdentity(/* known UUID */, "user-svc"));

// ... spawn actors, get remote refs as needed
```

## Status

Shipped: wire framing, serialization, the identity handshake, lazy
single-connection-per-peer, and `DeliveryFailed` reporting. **Not implemented:
any wire encryption or peer authentication.** There is no TLS/mTLS, and
`ClusterSharedKey` is not validated. Treat the transport as trusted-network-only.
