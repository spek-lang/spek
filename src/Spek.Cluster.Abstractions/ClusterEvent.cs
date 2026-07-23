namespace Spek.Cluster;

/// <summary>
/// Notification raised when a cluster member's state changes. Subscribed
/// to by user code (and by Spek's own routing layer) via
/// <see cref="IClusterMembership.Subscribe"/>.
///
/// Discriminated-union style (separate record per event kind) keeps
/// matching ergonomic without runtime type-checks at the call site.
/// </summary>
public abstract record ClusterEvent(NodeIdentity Identity)
{
    /// <summary>A new node entered <see cref="NodeState.Joining"/>.</summary>
    public sealed record NodeJoining(NodeIdentity Id) : ClusterEvent(Id);

    /// <summary>A node transitioned to <see cref="NodeState.Up"/>.</summary>
    public sealed record NodeUp(NodeIdentity Id) : ClusterEvent(Id);

    /// <summary>A node announced graceful shutdown
    /// (<see cref="NodeState.Leaving"/>).</summary>
    public sealed record NodeLeaving(NodeIdentity Id) : ClusterEvent(Id);

    /// <summary>A node completed graceful shutdown
    /// (<see cref="NodeState.Exiting"/>).</summary>
    public sealed record NodeExiting(NodeIdentity Id) : ClusterEvent(Id);

    /// <summary>The failure detector marked a node as
    /// <see cref="NodeState.Unreachable"/>.</summary>
    public sealed record NodeUnreachable(NodeIdentity Id) : ClusterEvent(Id);

    /// <summary>A previously-unreachable node came back to
    /// <see cref="NodeState.Up"/>.</summary>
    public sealed record NodeReachableAgain(NodeIdentity Id) : ClusterEvent(Id);

    /// <summary>A node transitioned to
    /// <see cref="NodeState.Down"/> — permanently offline.</summary>
    public sealed record NodeDown(NodeIdentity Id) : ClusterEvent(Id);
}
