namespace Spek.Cluster;

/// <summary>
/// Pluggable wire adapter for distributed Spek clusters. Modeled
/// after <c>Microsoft.AspNetCore.Connections.Abstractions</c>'s
/// <c>IConnectionListenerFactory</c> + <c>IConnectionListener</c>
/// pair so Bedrock-shaped transports (TCP / QUIC / named pipes /
/// in-memory) can plug in via the same contract Orleans 3.0 uses.
///
/// A transport instance represents one cluster-edge: outbound
/// connections to peer nodes plus an inbound listener on this node's
/// wire endpoint. Concrete implementations:
///
/// <list type="bullet">
///   <item><c>Spek.Cluster.Memory</c> — in-process pipe, free byproduct of
///         the abstraction; ideal for tests.</item>
///   <item><c>Spek.Cluster.Tcp</c> — Spek-native binary protocol on
///         Kestrel's <c>SocketTransport</c>. Default for production.</item>
///   <item><c>Spek.Cluster.Grpc</c> — opt-in adapter for HTTP/2-only
///         environments.</item>
/// </list>
/// </summary>
public interface ISpekTransport : IAsyncDisposable
{
    /// <summary>This node's identity as seen on the cluster wire.</summary>
    NodeIdentity LocalNode { get; }

    /// <summary>
    /// Send <paramref name="envelope"/> to the actor system identified by
    /// <paramref name="target"/>. The transport handles routing — for
    /// in-memory it's a direct dispatch, for TCP/QUIC it's a wire send.
    /// Fire-and-forget at the transport level: failures (unreachable
    /// node, serialization error) surface via the
    /// <see cref="DeliveryFailed"/> event rather than throwing.
    /// </summary>
    Task SendAsync(NodeIdentity target, RemoteEnvelope envelope, CancellationToken cancellationToken = default);

    /// <summary>
    /// Wires up the inbound handler. Called once during cluster
    /// bootstrap (<c>Cluster.Bind</c> in <c>Spek.Cluster</c>) — the
    /// cluster installs its dispatcher here, and the transport invokes
    /// it whenever a peer node sends us an envelope. Calling twice
    /// replaces the previous handler.
    /// </summary>
    void SetReceiveHandler(Func<RemoteEnvelope, Task> handler);

    /// <summary>
    /// Fires when a send fails irrecoverably (target node is gone,
    /// envelope serialization rejected, the transport itself is
    /// disposing). The <see cref="Spek.Runtime.ActorSystem"/> uses this to
    /// dead-letter the offending message and raise a
    /// <c>RemoteNodeUnreachable</c> notification.
    /// </summary>
    event Action<NodeIdentity, RemoteEnvelope, Exception>? DeliveryFailed;
}
