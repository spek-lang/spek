namespace Spek.Cluster;

/// <summary>
/// Maps a located-actor logical key to the cluster node that should
/// host it. Called every time the cluster needs to dispatch a message
/// to a logical-keyed actor.
///
/// Concrete implementations:
/// <list type="bullet">
///   <item><c>ConsistentHashPlacement</c> (default; in <c>Spek.Cluster</c>) — rendezvous
///         hashing across the cluster's <c>Up</c> members. Stable as
///         long as membership doesn't change; minimal disruption when
///         it does.</item>
///   <item>Future: <c>LocalityAwarePlacement</c> — prefer same DC /
///         zone / rack from node metadata. Custom user-defined
///         strategies plug in via the same interface.</item>
/// </list>
///
/// Determinism is required: the same <c>(actorType, key, members)</c>
/// triple MUST yield the same node, so two systems calling
/// <see cref="ResolveOwner"/> independently arrive at the same answer.
/// </summary>
public interface IPlacementStrategy
{
    /// <summary>
    /// Pick the owning node for a located actor.
    /// <paramref name="members"/> is the snapshot of cluster members
    /// the caller considers addressable — typically
    /// <c>Membership.Members</c> filtered to <see cref="NodeState.Up"/>.
    /// Returns <c>null</c> if no candidates (empty list) — caller
    /// should treat as transient and retry.
    /// </summary>
    NodeIdentity? ResolveOwner(string actorType, string actorKey,
                               IReadOnlyList<ClusterMember> members);
}
