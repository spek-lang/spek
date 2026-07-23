namespace Spek.Cluster;

/// <summary>
/// Identifies a node on the cluster wire. Each <see cref="Spek.Runtime.ActorSystem"/>
/// gets one when a transport is registered with it. The <see cref="Id"/> is
/// the wire-canonical identity (UUID, generated at startup);
/// <see cref="Label"/> is an optional human-friendly name surfaced in
/// observability tools and routing-by-name APIs.
///
/// Both are wire-serializable and round-trip across transports without
/// loss.
/// </summary>
public sealed record NodeIdentity(Guid Id, string? Label = null)
{
    /// <summary>The "no node" marker — used when a message originates
    /// locally with no associated remote sender (the default for
    /// fire-and-forget Tells from outside the actor system).</summary>
    public static readonly NodeIdentity Local = new(Guid.Empty, "local");

    public override string ToString() =>
        Label is null ? $"node:{Id:N}" : $"node:{Label}({Id:N})";
}
