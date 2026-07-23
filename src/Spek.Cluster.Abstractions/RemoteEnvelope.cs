namespace Spek.Cluster;

/// <summary>
/// What crosses the wire (or in-process pipe) between
/// <see cref="Spek.Runtime.ActorSystem"/>s. Carries the payload plus
/// enough addressing for the receiving system to dispatch it to the
/// right local actor and (when the receiver replies) route the reply
/// back to the original sender across nodes.
/// </summary>
/// <param name="TargetPath">
/// Logical path identifying the recipient actor on the receiving
/// node. Currently a flat name (the actor's <c>NamedRoot</c> registration
/// label); a later release extends this to hierarchical paths once cluster
/// sharding lands.
/// </param>
/// <param name="Message">
/// The user-defined Spek <c>message</c> instance. In-memory transports
/// pass it as-is; wire transports serialize it via the configured
/// <see cref="ISpekSerializer"/>.
/// </param>
/// <param name="SenderPath">
/// If the originator was an actor (vs. a host adapter or external
/// caller), this is its path on its own node. Combined with
/// <see cref="SenderNode"/>, the receiver can <c>sender.Tell(reply)</c>
/// back across the wire.
/// </param>
/// <param name="SenderNode">
/// The originating node's identity. <c>null</c> means the sender was
/// local-only or anonymous (e.g. a hosting adapter triggering a
/// <c>Shutdown</c>).
/// </param>
public sealed record RemoteEnvelope(
    string TargetPath,
    object Message,
    string? SenderPath = null,
    NodeIdentity? SenderNode = null);
