namespace Spek.Cluster;

/// <summary>
/// A node in the cluster as observed by the local node. Combines the
/// peer's wire identity with its current <see cref="NodeState"/> and
/// any free-form metadata it announced at join time
/// (<c>datacenter</c>, <c>zone</c>, <c>rack</c>, <c>region</c>, etc. —
/// used by locality-aware placement strategies).
/// </summary>
public sealed record ClusterMember(
    NodeIdentity Identity,
    NodeState State,
    IReadOnlyDictionary<string, string>? Metadata = null);
