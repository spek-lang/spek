using SpekClusterNs = Spek.Cluster;
using Spek.Cluster;
using Spek.Cluster.Memory;
using Spek.Runtime;
using Xunit;

namespace Spek.Tests.ClusterIntegration.Failover;

/// <summary>
/// Coverage for located-actor failover / re-placement after a topology
/// change. When the node that currently owns a location key goes
/// Down / Unreachable, <see cref="SpekClusterNs.Cluster.Locate{TActor}"/>
/// must re-resolve the key to a surviving Up member — placement filters
/// <see cref="NodeState.Up"/> only. And when *no* member is Up, Locate
/// must surface the documented <see cref="InvalidOperationException"/>
/// rather than silently picking a dead node.
///
/// Mirrors the harness/style of LocatedActorPlacementTests: an
/// in-memory fabric, one ActorSystem per node, peers registered + marked
/// Up via the cluster's static-seed membership API.
/// </summary>
public class ClusterFailoverTests
{
    public sealed record Increment();
    public sealed record GetCount();
    public sealed record CountReply(string Key, int Count);

    /// <summary>
    /// Counter actor keyed by its location key (first ctor arg). Lets a
    /// re-placed activation prove it actually handles messages on the
    /// surviving node.
    /// </summary>
    private sealed class CounterActor : ActorBase
    {
        private readonly string _key;
        private int _count;

        public CounterActor(string key) => _key = key;

        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            switch (message)
            {
                case Increment: _count++; break;
                case GetCount:  _currentSender.Tell(new CountReply(_key, _count)); break;
            }
            return Task.CompletedTask;
        }
    }

    /// <summary>
    /// Finds a location key whose consistent-hash owner is
    /// <paramref name="desired"/>, given the candidate member set. The
    /// placement is deterministic, so once found the key reliably maps
    /// to that node until membership changes.
    /// </summary>
    private static string FindKeyOwnedBy(
        SpekClusterNs.IPlacementStrategy placement,
        string typeName,
        NodeIdentity desired,
        IReadOnlyList<SpekClusterNs.ClusterMember> upMembers)
    {
        for (int i = 0; i < 10_000; i++)
        {
            var key   = $"key-{i}";
            var owner = placement.ResolveOwner(typeName, key, upMembers);
            if (owner is not null && owner.Id == desired.Id)
                return key;
        }
        throw new Xunit.Sdk.XunitException(
            $"Could not find a key owned by {desired} within 10000 candidates.");
    }

    private static List<SpekClusterNs.ClusterMember> UpMembers(SpekClusterNs.Cluster cluster) =>
        cluster.Membership.Members
            .Where(m => m.State == NodeState.Up)
            .ToList();

    /// <summary>
    /// Scenario 1: the owner of a key goes Unreachable in a 2-node
    /// fabric. Locate must re-resolve the key to the surviving Up node.
    /// </summary>
    [Fact]
    public async Task Locate_OwnerGoesUnreachable_ReplacesOntoSurvivingNodeAsync()
    {
        using var fabric = new InMemoryClusterFabric();
        using var systemA = new ActorSystem("a");
        using var systemB = new ActorSystem("b");
        var transportA = fabric.CreateTransport("a");
        var transportB = fabric.CreateTransport("b");
        var clusterA   = SpekClusterNs.Cluster.Bind(systemA, transportA);
        var clusterB   = SpekClusterNs.Cluster.Bind(systemB, transportB);

        // From A's vantage point: register B as a peer and bring it Up so
        // both nodes are candidates for placement.
        clusterA.RegisterPeer("b", transportB.LocalNode);
        clusterA.MarkPeerUp(transportB.LocalNode);
        clusterA.RegisterLocatedActor<CounterActor>();

        var typeName = typeof(CounterActor).FullName!;
        var nodeA = transportA.LocalNode;
        var nodeB = transportB.LocalNode;

        // Sanity: with both Up, A sees exactly two Up members.
        var bothUp = UpMembers(clusterA);
        Assert.Equal(2, bothUp.Count);

        // Pick a key that B currently owns. Resolving it now should point
        // at B, and Locate (which filters Up members) agrees.
        var key = FindKeyOwnedBy(clusterA.Placement, typeName, nodeB, bothUp);
        var ownerBeforeFailover = clusterA.Placement.ResolveOwner(typeName, key, UpMembers(clusterA))!;
        Assert.Equal(nodeB.Id, ownerBeforeFailover.Id);

        // Locate while B is Up resolves to B → a remote ref (owner != local
        // node A), not a local activation.
        var refBeforeFailover = clusterA.Locate<CounterActor>(key);
        Assert.NotNull(refBeforeFailover.RemoteEndpoint);
        Assert.Equal(nodeB.Id,
            Assert.IsType<RemoteEndpoint>(refBeforeFailover.RemoteEndpoint).TargetNode.Id);

        // ── Topology change: B's failure detector fires; B goes Unreachable.
        clusterA.MarkPeerUnreachable(nodeB);

        // B is no longer Up; only A remains a candidate.
        var afterMembers = UpMembers(clusterA);
        var soleSurvivor = Assert.Single(afterMembers);
        Assert.Equal(nodeA.Id, soleSurvivor.Identity.Id);

        // Re-placement: the same key now resolves to the surviving node A.
        var ownerAfterFailover = clusterA.Placement.ResolveOwner(typeName, key, afterMembers)!;
        Assert.Equal(nodeA.Id, ownerAfterFailover.Id);

        // And Locate, which performs exactly that Up-filtered resolution,
        // now hands back a *local* activation on A (no remote endpoint).
        var refAfterFailover = clusterA.Locate<CounterActor>(key);
        Assert.Null(refAfterFailover.RemoteEndpoint);

        // The re-placed local activation is a live actor that handles
        // messages on the surviving node.
        var probe = new ReplyProbe();
        var collector = systemA.SpawnNamed<ReplyCapture>("collector-1", probe);
        refAfterFailover.Tell(new Increment());
        refAfterFailover.Tell(new Increment());
        refAfterFailover.Tell(new GetCount(), sender: collector);

        var winner = await Task.WhenAny(probe.Completion, Task.Delay(TimeSpan.FromSeconds(3)));
        Assert.True(ReferenceEquals(winner, probe.Completion),
            "GetCount reply did not arrive within 3s after re-placement.");
        var reply = Assert.IsType<CountReply>(await probe.Completion);
        Assert.Equal(key, reply.Key);
        Assert.Equal(2, reply.Count);
    }

    /// <summary>
    /// Same shape, but the owner is promoted all the way to Down via the
    /// static-seed membership's MarkDown (failure-detector promotion).
    /// Down is also excluded from placement.
    /// </summary>
    [Fact]
    public void Locate_OwnerGoesDown_ReplacesOntoSurvivingNode()
    {
        using var fabric = new InMemoryClusterFabric();
        using var systemA = new ActorSystem("a");
        using var systemB = new ActorSystem("b");
        var transportA = fabric.CreateTransport("a");
        var transportB = fabric.CreateTransport("b");
        var clusterA   = SpekClusterNs.Cluster.Bind(systemA, transportA);
        var clusterB   = SpekClusterNs.Cluster.Bind(systemB, transportB);

        clusterA.RegisterPeer("b", transportB.LocalNode);
        clusterA.MarkPeerUp(transportB.LocalNode);
        clusterA.RegisterLocatedActor<CounterActor>();

        var typeName = typeof(CounterActor).FullName!;
        var nodeA = transportA.LocalNode;
        var nodeB = transportB.LocalNode;

        var bothUp = UpMembers(clusterA);
        Assert.Equal(2, bothUp.Count);

        var key = FindKeyOwnedBy(clusterA.Placement, typeName, nodeB, bothUp);
        Assert.Equal(nodeB.Id,
            clusterA.Placement.ResolveOwner(typeName, key, bothUp)!.Id);

        // Promote B straight to Down. MarkDown lives on the static-seed
        // membership impl (the cluster facade exposes only Unreachable),
        // so reach it through Membership.
        var ssm = Assert.IsType<StaticSeedClusterMembership>(clusterA.Membership);
        ssm.MarkDown(nodeB);

        var afterMembers = UpMembers(clusterA);
        var soleSurvivor = Assert.Single(afterMembers);
        Assert.Equal(nodeA.Id, soleSurvivor.Identity.Id);

        // Down node is filtered out → key re-places onto A.
        Assert.Equal(nodeA.Id,
            clusterA.Placement.ResolveOwner(typeName, key, afterMembers)!.Id);

        var refAfterFailover = clusterA.Locate<CounterActor>(key);
        Assert.Null(refAfterFailover.RemoteEndpoint); // local activation on A
    }

    /// <summary>
    /// Scenario 2: every member is non-Up. Locate must throw the
    /// documented InvalidOperationException with the "No live cluster
    /// members" message rather than pinning to a dead node.
    /// </summary>
    [Fact]
    public void Locate_NoLiveMembers_Throws()
    {
        using var fabric = new InMemoryClusterFabric();
        using var systemA = new ActorSystem("a");
        using var systemB = new ActorSystem("b");
        var transportA = fabric.CreateTransport("a");
        var transportB = fabric.CreateTransport("b");
        var clusterA   = SpekClusterNs.Cluster.Bind(systemA, transportA);
        var clusterB   = SpekClusterNs.Cluster.Bind(systemB, transportB);

        clusterA.RegisterPeer("b", transportB.LocalNode);
        clusterA.MarkPeerUp(transportB.LocalNode);
        clusterA.RegisterLocatedActor<CounterActor>();

        var nodeA = transportA.LocalNode;
        var nodeB = transportB.LocalNode;

        Assert.Equal(2, UpMembers(clusterA).Count);

        // Knock both members out of Up. The local node starts Up and is
        // only ever moved out of Up via LeaveAsync (Leaving/Exiting) — so
        // mark the local node Down directly through the membership impl,
        // and the peer Unreachable through the facade.
        var ssm = Assert.IsType<StaticSeedClusterMembership>(clusterA.Membership);
        clusterA.MarkPeerUnreachable(nodeB);
        ssm.MarkDown(nodeA);

        // No member is Up now.
        Assert.Empty(UpMembers(clusterA));

        var ex = Assert.Throws<InvalidOperationException>(
            () => clusterA.Locate<CounterActor>("orphan-key"));
        Assert.Contains("No live cluster members", ex.Message);
    }

    // ── Reply capture: spawned in the same ActorSystem as the target, sets a
    //    TaskCompletionSource the moment a message arrives. Mirrors the model
    //    files' reply-capture pattern.
    private sealed class ReplyProbe
    {
        public readonly TaskCompletionSource<object> Tcs =
            new(TaskCreationOptions.RunContinuationsAsynchronously);
        public Task<object> Completion => Tcs.Task;
    }

    private sealed class ReplyCapture : ActorBase
    {
        private readonly ReplyProbe _probe;
        public ReplyCapture(ReplyProbe probe) => _probe = probe;
        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            _probe.Tcs.TrySetResult(message);
            return Task.CompletedTask;
        }
    }
}

