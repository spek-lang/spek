namespace Spek.Cluster;

/// <summary>
/// Pluggable cluster-membership contract. The default implementation
/// is <c>StaticSeedClusterMembership</c> - upfront-configured peers,
/// no dynamic discovery, but a real state machine + leave-on-shutdown
/// + cluster-view subscription.
///
/// Other membership strategies (gossip protocols, service-discovery
/// or registry adapters) plug in behind this same interface, so
/// switching membership strategies is config, not code.
/// </summary>
public interface IClusterMembership : IAsyncDisposable
{
    /// <summary>This node's identity in the cluster.</summary>
    NodeIdentity LocalNode { get; }

    /// <summary>Snapshot of every member the local node currently knows
    /// about, including this node itself.</summary>
    IReadOnlyList<ClusterMember> Members { get; }

    /// <summary>
    /// Looks up the live member by identity, or <c>null</c> if not in
    /// our view (never joined, or already removed).
    /// </summary>
    ClusterMember? FindMember(NodeIdentity identity);

    /// <summary>
    /// Subscribe to membership events. The returned token unsubscribes
    /// when disposed: pass it through a <c>using</c> at the call site
    /// for scoped lifetime, or hold it for the lifetime of the
    /// subscriber.
    /// </summary>
    IDisposable Subscribe(Action<ClusterEvent> handler);

    /// <summary>
    /// Announce that this node is gracefully leaving. Transitions
    /// local state to <see cref="NodeState.Leaving"/>; implementations
    /// may propagate the announcement to peers. Call this from your
    /// shutdown sequence so peers see a clean leave rather than a
    /// phantom unreachable.
    /// </summary>
    Task LeaveAsync(CancellationToken cancellationToken = default);
}
