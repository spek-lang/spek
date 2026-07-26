using System.Net;

namespace Spek.Cluster.Tcp;

/// <summary>
/// Configuration for <see cref="TcpClusterTransport"/>, listening on the standard
/// Spek port. Each setting is overridable at host bootstrap.
/// </summary>
/// <remarks>
/// <b>Security status:</b> the wire is <b>unencrypted and peers are not
/// authenticated</b>. The transport has no TLS and no shared-secret
/// enforcement; the only network restriction is <see cref="LoopbackOnly"/>.
/// <b>Do not run the cluster transport across an untrusted network.</b>
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
    /// <b>NOT ENFORCED.</b> The handshake exchanges only node identity; this
    /// value is <b>not validated</b> and setting it does not authenticate
    /// peers (the transport logs a warning to make that obvious). Rely on
    /// <see cref="LoopbackOnly"/> and network-level isolation.
    /// </remarks>
    public string? ClusterSharedKey { get; set; }

    /// <summary>
    /// When true, accept inbound connections only from <c>127.0.0.1</c> /
    /// <c>::1</c>. Default <c>false</c>.
    /// </summary>
    /// <remarks>
    /// This is the <b>only enforced</b> network protection (the transport has
    /// no mTLS and no shared-secret enforcement; see the class remarks). For a
    /// single-host dev cluster set this <c>true</c>; for anything multi-host,
    /// keep the transport on a trusted or isolated network.
    /// </remarks>
    public bool LoopbackOnly { get; set; } = false;
}
