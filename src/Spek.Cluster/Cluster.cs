using System.Collections.Concurrent;
using Spek.Runtime;

namespace Spek.Cluster;

/// <summary>
/// Wires an <see cref="ActorSystem"/> together with an
/// <see cref="ISpekTransport"/> and a peer registry. This is the
/// primary entry point for distributed Spek — call
/// <see cref="Bind"/> once during host bootstrap, register peers as
/// they're discovered (or up front for static deployments), and use
/// <see cref="ResolveRemote"/> to obtain remote actor refs.
///
/// Implements <see cref="IClusterAdapter"/> so the runtime can route
/// local-side resolution through here.
/// </summary>
public sealed class Cluster : IClusterAdapter, IAsyncDisposable
{
    private readonly ActorSystem _system;
    private readonly ISpekTransport _transport;
    private readonly IClusterMembership _membership;
    private readonly IPlacementStrategy _placement;
    private readonly Dictionary<string, NodeIdentity> _peerByLabel =
        new(StringComparer.Ordinal);
    private readonly Dictionary<Guid, NodeIdentity> _peerById = new();
    private readonly Dictionary<string, LocatedActorRegistration> _locatedActorTypes =
        new(StringComparer.Ordinal);
    private readonly Lock _peersLock = new();

    // Remote-sender refs, one per (origin node, sender path), reused across
    // inbound envelopes (perf r16) — the receive path used to allocate a fresh
    // RemoteEndpoint + ActorRef for every envelope that carried a sender.
    // Keyed by the node's wire-canonical UUID; the label plays no part in
    // routing. Unbounded by design: sender paths are named roots, explicitly
    // registered on the origin node via SpawnNamed, so the population is one
    // entry per (peer, named root actually used as a sender) — small and
    // fixed for any real topology, and entries are ~100 B each.
    private readonly ConcurrentDictionary<(Guid NodeId, string Path), ActorRef>
        _remoteSenderRefs = new();

    private Cluster(ActorSystem system, ISpekTransport transport,
                    IClusterMembership membership, IPlacementStrategy placement)
    {
        _system = system;
        _transport = transport;
        _membership = membership;
        _placement = placement;
    }

    /// <summary>The placement strategy in effect — defaults to
    /// <see cref="ConsistentHashPlacement"/>.</summary>
    public IPlacementStrategy Placement => _placement;

    /// <summary>This node's identity on the cluster.</summary>
    public NodeIdentity LocalNode => _transport.LocalNode;

    /// <summary>The transport this cluster is using.</summary>
    public ISpekTransport Transport => _transport;

    /// <summary>The membership view backing this cluster — subscribe
    /// to events, query the live member list, mark nodes up/down.</summary>
    public IClusterMembership Membership => _membership;

    /// <summary>
    /// Binds <paramref name="transport"/> to <paramref name="system"/>,
    /// installs the inbound-receive handler, and registers the cluster
    /// as the system's <see cref="IClusterAdapter"/>. Returns the
    /// configured cluster object — keep a reference for peer
    /// registration and resolution.
    ///
    /// If <paramref name="membership"/> is null, a default
    /// <see cref="StaticSeedClusterMembership"/> is constructed bound
    /// to the transport's local node identity. For future gossip-based
    /// clusters, pass an explicit membership impl.
    /// </summary>
    public static Cluster Bind(
        ActorSystem system,
        ISpekTransport transport,
        IClusterMembership? membership = null,
        IPlacementStrategy? placement = null)
    {
        membership ??= new StaticSeedClusterMembership(transport.LocalNode);
        placement  ??= new ConsistentHashPlacement();
        var cluster = new Cluster(system, transport, membership, placement);
        system.RegisterClusterAdapter(cluster);
        transport.SetReceiveHandler(cluster.OnReceiveAsync);
        return cluster;
    }

    /// <summary>
    /// Register a peer node. <paramref name="label"/> is the name local
    /// callers use in <see cref="ResolveRemote"/>; <paramref name="identity"/>
    /// is the wire-canonical UUID + optional remote label that identifies
    /// the target across the network. Also adds the peer to the
    /// membership view in <see cref="NodeState.Joining"/>; call
    /// <see cref="MarkPeerUp"/> once the transport handshake completes
    /// to advance to <see cref="NodeState.Up"/>.
    /// </summary>
    public void RegisterPeer(string label, NodeIdentity identity,
        IReadOnlyDictionary<string, string>? metadata = null)
    {
        ArgumentException.ThrowIfNullOrEmpty(label);
        ArgumentNullException.ThrowIfNull(identity);
        lock (_peersLock)
        {
            _peerByLabel[label] = identity;
            _peerById[identity.Id] = identity;
        }
        if (_membership is StaticSeedClusterMembership ssm)
            ssm.AddSeedMember(identity, metadata);
    }

    /// <summary>
    /// Mark a registered peer as <see cref="NodeState.Up"/> — typically
    /// called once the transport-level handshake completes successfully.
    /// In a future SWIM-based impl this transition is automatic; for
    /// the static-seed impl it's user-driven so tests can sequence
    /// the state machine deterministically.
    /// </summary>
    public void MarkPeerUp(NodeIdentity identity)
    {
        if (_membership is StaticSeedClusterMembership ssm)
            ssm.MarkUp(identity);
    }

    /// <summary>
    /// Mark a registered peer as <see cref="NodeState.Unreachable"/>.
    /// In production the failure detector calls this; the static-seed
    /// impl exposes it for tests.
    /// </summary>
    public void MarkPeerUnreachable(NodeIdentity identity)
    {
        if (_membership is StaticSeedClusterMembership ssm)
            ssm.MarkUnreachable(identity);
    }

    /// <summary>
    /// Announce that this node is gracefully leaving. Hosting adapters
    /// call this in their shutdown sequence so peers see a clean
    /// leave rather than a phantom unreachable.
    /// </summary>
    public Task LeaveAsync(CancellationToken cancellationToken = default)
        => _membership.LeaveAsync(cancellationToken);

    // ─── Located actors ─────────────────────────────────────────────────────

    /// <summary>
    /// Register an actor type as <b>located</b> — eligible for
    /// cluster-wide auto-placement and on-demand activation via
    /// <see cref="Locate{TActor}"/>. The location key is passed as
    /// the first constructor argument when the actor is activated
    /// locally; if your actor takes additional ctor args, supply
    /// them via <paramref name="extraArgsFactory"/> (called once
    /// per activation).
    /// </summary>
    public void RegisterLocatedActor<TActor>(Func<string, object[]>? extraArgsFactory = null)
        where TActor : ActorBase
    {
        var typeName = typeof(TActor).FullName ?? typeof(TActor).Name;
        lock (_peersLock)
        {
            _locatedActorTypes[typeName] = new LocatedActorRegistration(
                typeof(TActor),
                extraArgsFactory ?? (_ => Array.Empty<object>()));
        }
    }

    /// <summary>
    /// Returns a cluster-wide <see cref="ActorRef"/> for the
    /// (<typeparamref name="TActor"/>, <paramref name="key"/>) pair.
    /// The placement strategy decides which cluster node owns this
    /// location; if it's the local node, the actor is auto-activated;
    /// if it's a remote node, the returned ref dispatches across the
    /// wire — exactly the same code path as a manually-resolved
    /// remote ref.
    ///
    /// Idempotent — repeated calls with the same key return refs
    /// pointing at the same logical actor (the runtime activates at
    /// most one instance per (Type, Key) per cluster).
    /// </summary>
    public ActorRef Locate<TActor>(string key)
        where TActor : ActorBase
    {
        ArgumentException.ThrowIfNullOrEmpty(key);
        var typeName = typeof(TActor).FullName ?? typeof(TActor).Name;

        // Pick the owning node. Filter to Up members only — placement
        // shouldn't pin a location to a node that can't accept traffic.
        var members = _membership.Members
            .Where(m => m.State == NodeState.Up)
            .ToList();
        var owner = _placement.ResolveOwner(typeName, key, members)
            ?? throw new InvalidOperationException(
                $"No live cluster members available to host located actor '{typeName}/{key}'.");

        var locationPath = $"{typeName}/{key}";

        if (owner.Id == LocalNode.Id)
        {
            // Local activation. Spawn-or-find via SpawnNamed; the
            // method auto-registers the actor as a named root.
            return EnsureLocalActivation(typeof(TActor), locationPath, key);
        }

        // Remote — return a ref pointing at the location path on the
        // owning node. The receiver auto-activates on first inbound
        // envelope (see OnReceiveAsync below).
        return new ActorRef(new RemoteEndpoint(_transport, owner, locationPath, this));
    }

    /// <summary>
    /// Spawn-or-find a local activation for a location path. If the
    /// named root already exists, returns it; otherwise spawns the
    /// actor and registers it under the location path.
    /// </summary>
    private ActorRef EnsureLocalActivation(Type actorType, string locationPath, string locationKey)
    {
        var existing = _system.ResolveNamed(locationPath);
        if (existing is not null) return existing;

        // Look up registration — supplies extra ctor args beyond the
        // location key. Default is "just the key as the only arg."
        LocatedActorRegistration? reg;
        var typeName = actorType.FullName ?? actorType.Name;
        lock (_peersLock) reg = _locatedActorTypes.GetValueOrDefault(typeName);

        var args = new List<object> { locationKey };
        if (reg is not null)
            args.AddRange(reg.ExtraArgsFactory(locationKey));

        // Use the runtime's named-root spawn — registers under the path
        // for inbound resolution.
        var actor = _system.SpawnNamed(actorType, locationPath, args.ToArray());
        return actor;
    }

    /// <summary>
    /// Snapshot of located-actor types currently registered with this
    /// cluster. Used by <see cref="OnReceiveAsync"/> to auto-activate
    /// inbound traffic addressed to a location path.
    /// </summary>
    private LocatedActorRegistration? LookupLocatedActorType(string typeName)
    {
        lock (_peersLock) return _locatedActorTypes.GetValueOrDefault(typeName);
    }

    private sealed record LocatedActorRegistration(
        Type ActorType,
        Func<string, object[]> ExtraArgsFactory);

    /// <summary>
    /// Resolves the remote actor named <paramref name="path"/> on the peer
    /// labeled <paramref name="nodeLabel"/>. Returns an
    /// <see cref="ActorRef"/> whose <c>Tell</c> routes through the
    /// transport.
    /// </summary>
    public ActorRef ResolveRemote(string nodeLabel, string path)
    {
        NodeIdentity node;
        lock (_peersLock)
        {
            if (!_peerByLabel.TryGetValue(nodeLabel, out var found))
                throw new InvalidOperationException(
                    $"No peer registered with label '{nodeLabel}'. Call RegisterPeer() first.");
            node = found;
        }
        return new ActorRef(new RemoteEndpoint(_transport, node, path, this));
    }

    private Task OnReceiveAsync(RemoteEnvelope envelope)
    {
        // If the envelope carries sender info, resolve a remote-sender ref
        // that round-trips back to the originator. Otherwise the
        // recipient's `_currentSender` will be NoSender and any
        // `sender.Tell(reply)` dead-letters — same semantics as a Tell
        // from outside the actor system locally.
        //
        // Resolution is cached per (node id, path): repeated envelopes from
        // the same originator hand the recipient the SAME ActorRef instance
        // (reference identity — pinned by ClusterMemoryTransportTests
        // .RepeatedEnvelopesFromSameSender_ReceiverSeesOneCachedRef), so
        // per-message traffic doesn't allocate an endpoint + ref pair,
        // and recipients that stash senders in sets/dictionaries see one
        // logical sender as one ref. The static factory keeps the miss path
        // closure-free.
        ActorRef? sender = null;
        if (envelope.SenderNode is { } sn && envelope.SenderPath is { } sp)
        {
            sender = _remoteSenderRefs.GetOrAdd(
                (sn.Id, sp),
                static ((Guid, string) _, (Cluster Cluster, NodeIdentity Node, string Path) a) =>
                    new ActorRef(new RemoteEndpoint(
                        a.Cluster._transport, a.Node, a.Path, a.Cluster)),
                (this, sn, sp));
        }

        // Located-actor auto-activation. If the target path
        // matches a registered located-actor type's prefix and isn't
        // yet bound locally, spawn-and-register it before delivery.
        // Format: `{TypeFullName}/{key}` — the prefix-before-`/` is
        // the type name to look up.
        var slash = envelope.TargetPath.IndexOf('/');
        if (slash > 0 && _system.ResolveNamed(envelope.TargetPath) is null)
        {
            var typeName    = envelope.TargetPath[..slash];
            var locationKey = envelope.TargetPath[(slash + 1)..];
            if (LookupLocatedActorType(typeName) is { } reg)
            {
                EnsureLocalActivation(reg.ActorType, envelope.TargetPath, locationKey);
            }
        }

        _system.DeliverIncoming(envelope.TargetPath, envelope.Message, sender);
        return Task.CompletedTask;
    }

    /// <summary>
    /// Reverse lookup — given a local <see cref="ActorRef"/>, find its
    /// named-root path so a remote receiver can reply through it. Only
    /// named roots are currently wire-addressable; anonymous spawned actors
    /// are local-only.
    /// </summary>
    internal string? LocalPathOfNamedRoot(ActorRef? actor) =>
        // On the send path for every Tell-with-sender. The runtime keeps a
        // ref → path map alongside its named-root registry (perf r16), so
        // this is a dictionary hit under the system lock, not a scan.
        actor is null ? null : _system.PathOfNamedRoot(actor);

    public async ValueTask DisposeAsync()
    {
        await _membership.DisposeAsync().ConfigureAwait(false);
        await _transport.DisposeAsync().ConfigureAwait(false);
    }
}

/// <summary>
/// Internal <see cref="IRemoteEndpoint"/> impl that bridges
/// <see cref="ActorRef.Tell(object)"/> to the cluster's transport.
/// One instance per (target node, target path) pair; users normally
/// don't construct these directly — use <see cref="Cluster.ResolveRemote"/>.
/// </summary>
internal sealed class RemoteEndpoint : IRemoteEndpoint
{
    private readonly ISpekTransport _transport;
    private readonly NodeIdentity _targetNode;
    private readonly string _targetPath;
    private readonly Cluster _cluster;
    private bool _isDead;

    public RemoteEndpoint(ISpekTransport transport, NodeIdentity targetNode,
                          string targetPath, Cluster cluster)
    {
        _transport = transport;
        _targetNode = targetNode;
        _targetPath = targetPath;
        _cluster = cluster;
    }

    public bool IsKnownDead => _isDead;
    public NodeIdentity TargetNode => _targetNode;
    public string TargetPath => _targetPath;

    public void Dispatch(ActorRef self, object message, ActorRef? sender)
    {
        // Build sender info for the envelope. Three cases:
        //  1. sender is itself a remote ref → propagate its (node, path)
        //     so the wire-form points back at the original originator
        //  2. sender is a local named-root → use our local node + the
        //     ref's named-root path so the remote can reply back
        //  3. sender is anonymous / NoSender → no sender info, replies
        //     dead-letter (same as Tell from outside actor scope locally)
        string? senderPath = null;
        NodeIdentity? senderNode = null;

        if (sender?.RemoteEndpoint is RemoteEndpoint rep)
        {
            senderPath = rep._targetPath;
            senderNode = rep._targetNode;
        }
        else if (sender is { Slot: not null })
        {
            senderPath = _cluster.LocalPathOfNamedRoot(sender);
            if (senderPath is not null) senderNode = _cluster.LocalNode;
        }

        var env = new RemoteEnvelope(_targetPath, message, senderPath, senderNode);

        try
        {
            _ = _transport.SendAsync(_targetNode, env);
        }
        catch
        {
            _isDead = true;
        }
    }
}
