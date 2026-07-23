using System.Collections.Concurrent;

namespace Spek.Cluster;

/// <summary>
/// Minimum-viable <see cref="IClusterMembership"/>. Peers are
/// registered upfront via <see cref="AddSeedMember"/>; this is enough
/// to exercise the state-machine + cluster-view + leave-on-shutdown
/// surface without a full SWIM gossip implementation. Real gossip
/// drops in via a different impl behind the same interface later.
///
/// Behavior:
/// <list type="bullet">
///   <item>Each <see cref="AddSeedMember"/> call adds a peer in
///         <see cref="NodeState.Joining"/>, immediately raises
///         <see cref="ClusterEvent.NodeJoining"/>.</item>
///   <item><see cref="MarkUp"/> transitions a peer to
///         <see cref="NodeState.Up"/> and raises
///         <see cref="ClusterEvent.NodeUp"/>. Typically called by the
///         transport layer once the handshake completes.</item>
///   <item><see cref="LeaveAsync"/> transitions the local node to
///         <see cref="NodeState.Leaving"/> → <see cref="NodeState.Exiting"/>,
///         emits the corresponding events, and completes the task it
///         returns so callers can sequence shutdown.</item>
///   <item><see cref="MarkUnreachable"/> / <see cref="MarkReachableAgain"/>
///         simulate failure-detector signals; in the production SWIM
///         impl these come from missed heartbeats.</item>
/// </list>
/// </summary>
public sealed class StaticSeedClusterMembership : IClusterMembership
{
    private readonly ConcurrentDictionary<Guid, ClusterMember> _members = new();
    private readonly ConcurrentBag<Subscription> _subscribers = new();
    private bool _disposed;
    private TaskCompletionSource? _leaveCompleted;

    public StaticSeedClusterMembership(NodeIdentity localNode,
        IReadOnlyDictionary<string, string>? localMetadata = null)
    {
        LocalNode = localNode;
        // The local node enters its own view in `Up` immediately — we're
        // by definition reachable from ourselves.
        _members[localNode.Id] = new ClusterMember(localNode, NodeState.Up, localMetadata);
    }

    public NodeIdentity LocalNode { get; }

    public IReadOnlyList<ClusterMember> Members => _members.Values.ToList();

    public ClusterMember? FindMember(NodeIdentity identity)
        => _members.GetValueOrDefault(identity.Id);

    public IDisposable Subscribe(Action<ClusterEvent> handler)
    {
        var sub = new Subscription(this, handler);
        _subscribers.Add(sub);
        return sub;
    }

    /// <summary>Add a peer to the cluster view in
    /// <see cref="NodeState.Joining"/>. Raises
    /// <see cref="ClusterEvent.NodeJoining"/>.</summary>
    public void AddSeedMember(NodeIdentity identity,
        IReadOnlyDictionary<string, string>? metadata = null)
    {
        var member = new ClusterMember(identity, NodeState.Joining, metadata);
        if (_members.TryAdd(identity.Id, member))
            Raise(new ClusterEvent.NodeJoining(identity));
    }

    /// <summary>Transition <paramref name="identity"/> to
    /// <see cref="NodeState.Up"/>. No-op if the node isn't in our view
    /// or is already <c>Up</c>. Raises <see cref="ClusterEvent.NodeUp"/>
    /// on transition.</summary>
    public void MarkUp(NodeIdentity identity)
    {
        if (Transition(identity, NodeState.Up, m => m.State != NodeState.Up))
            Raise(new ClusterEvent.NodeUp(identity));
    }

    /// <summary>Mark a peer <see cref="NodeState.Unreachable"/>. The
    /// failure detector calls this in production; the static-seed
    /// impl exposes it for tests.</summary>
    public void MarkUnreachable(NodeIdentity identity)
    {
        if (Transition(identity, NodeState.Unreachable,
                m => m.State == NodeState.Up))
            Raise(new ClusterEvent.NodeUnreachable(identity));
    }

    /// <summary>The previously-unreachable node has heartbeated again.</summary>
    public void MarkReachableAgain(NodeIdentity identity)
    {
        if (Transition(identity, NodeState.Up,
                m => m.State == NodeState.Unreachable))
            Raise(new ClusterEvent.NodeReachableAgain(identity));
    }

    /// <summary>Permanently remove a peer (failure-detector promotion
    /// from <c>Unreachable</c>, or end of graceful leave).</summary>
    public void MarkDown(NodeIdentity identity)
    {
        if (Transition(identity, NodeState.Down, _ => true))
            Raise(new ClusterEvent.NodeDown(identity));
    }

    public Task LeaveAsync(CancellationToken cancellationToken = default)
    {
        if (_leaveCompleted is not null) return _leaveCompleted.Task;
        _leaveCompleted = new TaskCompletionSource();

        // Local node moves Leaving → Exiting. Subscribers see both events.
        if (Transition(LocalNode, NodeState.Leaving, m => m.State == NodeState.Up))
            Raise(new ClusterEvent.NodeLeaving(LocalNode));

        if (Transition(LocalNode, NodeState.Exiting, m => m.State == NodeState.Leaving))
            Raise(new ClusterEvent.NodeExiting(LocalNode));

        // In the real SWIM impl this would wait for peers to ack the
        // leave gossip. For the static-seed impl there are no peers
        // observing us; signal complete immediately.
        _leaveCompleted.TrySetResult();
        return _leaveCompleted.Task;
    }

    /// <summary>
    /// Atomic state transition. Returns true if the state changed.
    /// Predicate gates the transition (so we don't, e.g., move a Down
    /// node back to Up via a racy Tell).
    /// </summary>
    private bool Transition(NodeIdentity identity, NodeState newState,
                            Func<ClusterMember, bool> guard)
    {
        if (!_members.TryGetValue(identity.Id, out var current)) return false;
        if (!guard(current)) return false;
        var updated = current with { State = newState };
        return _members.TryUpdate(identity.Id, updated, current);
    }

    private void Raise(ClusterEvent ev)
    {
        if (_disposed) return;
        foreach (var sub in _subscribers)
            if (!sub.IsDisposed) sub.Handler(ev);
    }

    public ValueTask DisposeAsync()
    {
        _disposed = true;
        _members.Clear();
        return default;
    }

    private sealed class Subscription : IDisposable
    {
        private readonly StaticSeedClusterMembership _owner;
        public Action<ClusterEvent> Handler { get; }
        public bool IsDisposed { get; private set; }

        public Subscription(StaticSeedClusterMembership owner,
                            Action<ClusterEvent> handler)
        {
            _owner = owner;
            Handler = handler;
        }

        public void Dispose() => IsDisposed = true;
    }
}
