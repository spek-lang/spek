using System.Net;

namespace Spek.Cluster.Tcp;

/// <summary>
/// Configuration for <see cref="TcpClusterTransport"/>, listening on the standard
/// Spek port. Each setting is overridable at host bootstrap.
/// </summary>
/// <remarks>
/// <b>Security status:</b> the wire is <b>currently unencrypted and
/// peers are not authenticated</b>. TLS 1.3 / mTLS and shared-secret enforcement
/// are designed but <b>not yet implemented</b> (see the README). Today the only real network restriction is
/// <see cref="LoopbackOnly"/>. <b>Do not run the cluster transport across an
/// untrusted network yet.</b>
/// </remarks>
public sealed class TcpClusterOptions
{
    /// <summary>
    /// Optional human-readable label for this node. Surfaces in the
    /// <see cref="NodeIdentity.Label"/> field other peers see, and in
    /// observability tools. Defaults to <c>Environment.MachineName</c>.
    /// </summary>
    public string? Label { get; set; }

    /// <summary>
    /// The endpoint this node listens on for inbound peer connections.
    /// Default <c>0.0.0.0:5050</c>. Set to <c>IPAddress.Loopback</c>
    /// for single-host clusters.
    /// </summary>
    public IPEndPoint ListenEndpoint { get; set; } = new(IPAddress.Any, 5050);

    /// <summary>
    /// Cluster-shared symmetric secret (Erlang-cookie / Akka-cluster-cookie style).
    /// </summary>
    /// <remarks>
    /// <b>NOT YET ENFORCED.</b> The intended behavior is that peers must present
    /// this token at handshake, but the handshake currently exchanges only node
    /// identity — this value is <b>not validated</b>. Setting it does not
    /// authenticate peers (the transport logs a warning to make that obvious).
    /// Enforcement ships with mTLS in a later release. Until then, rely on
    /// <see cref="LoopbackOnly"/> and network-level isolation.
    /// </remarks>
    public string? ClusterSharedKey { get; set; }

    /// <summary>
    /// When true, accept inbound connections only from <c>127.0.0.1</c> /
    /// <c>::1</c>. Default <c>false</c>.
    /// </summary>
    /// <remarks>
    /// This is currently the <b>only enforced</b> network protection (mTLS and
    /// shared-secret auth are not yet implemented — see the class remarks). For a
    /// single-host dev cluster set this <c>true</c>; for anything multi-host, keep
    /// the transport on a trusted/isolated network until mTLS lands.
    /// </remarks>
    public bool LoopbackOnly { get; set; } = false;
}
