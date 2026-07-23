using SpekClusterNs = Spek.Cluster;
using Spek.Cluster.Memory;
using Spek.Runtime;
using Xunit;

namespace Spek.Tests.ClusterIntegration.Membership;

/// <summary>
/// Coverage for cluster membership: state-machine transitions,
/// event subscription, locality metadata, and leave-on-shutdown
/// integration. Drives <see cref="SpekClusterNs.StaticSeedClusterMembership"/>
/// directly (deterministic, no real gossip — full SWIM gossip is a
/// follow-up that lives behind the same
/// <see cref="SpekClusterNs.IClusterMembership"/> interface).
/// </summary>
public class ClusterMembershipTests
{
    [Fact]
    public void NewMembership_LocalNodeImmediatelyUp()
    {
        var local = new SpekClusterNs.NodeIdentity(Guid.NewGuid(), "self");
        var m = new SpekClusterNs.StaticSeedClusterMembership(local);

        var members = m.Members;
        Assert.Single(members);
        Assert.Equal(SpekClusterNs.NodeState.Up, members[0].State);
        Assert.Equal(local.Id, members[0].Identity.Id);
    }

    [Fact]
    public void AddSeedMember_StartsInJoiningStateAndRaisesEvent()
    {
        var local = new SpekClusterNs.NodeIdentity(Guid.NewGuid(), "self");
        var peer  = new SpekClusterNs.NodeIdentity(Guid.NewGuid(), "peer");
        var m = new SpekClusterNs.StaticSeedClusterMembership(local);

        var events = new List<SpekClusterNs.ClusterEvent>();
        using var sub = m.Subscribe(events.Add);

        m.AddSeedMember(peer);

        Assert.Equal(SpekClusterNs.NodeState.Joining, m.FindMember(peer)!.State);
        Assert.Single(events);
        Assert.IsType<SpekClusterNs.ClusterEvent.NodeJoining>(events[0]);
    }

    [Fact]
    public void MarkUp_TransitionsJoiningToUp_AndRaisesEvent()
    {
        var local = new SpekClusterNs.NodeIdentity(Guid.NewGuid(), "self");
        var peer  = new SpekClusterNs.NodeIdentity(Guid.NewGuid(), "peer");
        var m = new SpekClusterNs.StaticSeedClusterMembership(local);
        m.AddSeedMember(peer);

        var events = new List<SpekClusterNs.ClusterEvent>();
        using var sub = m.Subscribe(events.Add);

        m.MarkUp(peer);

        Assert.Equal(SpekClusterNs.NodeState.Up, m.FindMember(peer)!.State);
        Assert.Single(events);
        Assert.IsType<SpekClusterNs.ClusterEvent.NodeUp>(events[0]);
    }

    [Fact]
    public void Unreachable_ThenReachableAgain_RoundTrip()
    {
        var local = new SpekClusterNs.NodeIdentity(Guid.NewGuid(), "self");
        var peer  = new SpekClusterNs.NodeIdentity(Guid.NewGuid(), "peer");
        var m = new SpekClusterNs.StaticSeedClusterMembership(local);
        m.AddSeedMember(peer);
        m.MarkUp(peer);

        var events = new List<SpekClusterNs.ClusterEvent>();
        using var sub = m.Subscribe(events.Add);

        m.MarkUnreachable(peer);
        Assert.Equal(SpekClusterNs.NodeState.Unreachable, m.FindMember(peer)!.State);

        m.MarkReachableAgain(peer);
        Assert.Equal(SpekClusterNs.NodeState.Up, m.FindMember(peer)!.State);

        Assert.IsType<SpekClusterNs.ClusterEvent.NodeUnreachable>(events[0]);
        Assert.IsType<SpekClusterNs.ClusterEvent.NodeReachableAgain>(events[1]);
    }

    [Fact]
    public async Task LeaveAsync_TransitionsLocalToLeavingThenExitingAsync()
    {
        var local = new SpekClusterNs.NodeIdentity(Guid.NewGuid(), "self");
        var m = new SpekClusterNs.StaticSeedClusterMembership(local);

        var events = new List<SpekClusterNs.ClusterEvent>();
        using var sub = m.Subscribe(events.Add);

        await m.LeaveAsync();

        Assert.Equal(SpekClusterNs.NodeState.Exiting, m.FindMember(local)!.State);
        Assert.Equal(2, events.Count);
        Assert.IsType<SpekClusterNs.ClusterEvent.NodeLeaving>(events[0]);
        Assert.IsType<SpekClusterNs.ClusterEvent.NodeExiting>(events[1]);
    }

    [Fact]
    public void NodeMetadata_PreservedAcrossStateTransitions()
    {
        var local = new SpekClusterNs.NodeIdentity(Guid.NewGuid(), "self");
        var peer  = new SpekClusterNs.NodeIdentity(Guid.NewGuid(), "peer");
        var meta = new Dictionary<string, string>
        {
            ["datacenter"] = "us-east-1",
            ["zone"]       = "us-east-1a",
            ["rack"]       = "r-42",
        };

        var m = new SpekClusterNs.StaticSeedClusterMembership(local);
        m.AddSeedMember(peer, meta);
        m.MarkUp(peer);

        var member = m.FindMember(peer)!;
        Assert.Equal(SpekClusterNs.NodeState.Up, member.State);
        Assert.NotNull(member.Metadata);
        Assert.Equal("us-east-1", member.Metadata!["datacenter"]);
        Assert.Equal("us-east-1a", member.Metadata["zone"]);
        Assert.Equal("r-42", member.Metadata["rack"]);
    }

    [Fact]
    public void Subscription_DisposedSubscribers_StopReceivingEvents()
    {
        var local = new SpekClusterNs.NodeIdentity(Guid.NewGuid(), "self");
        var peer  = new SpekClusterNs.NodeIdentity(Guid.NewGuid(), "peer");
        var m = new SpekClusterNs.StaticSeedClusterMembership(local);

        var events = new List<SpekClusterNs.ClusterEvent>();
        var sub = m.Subscribe(events.Add);

        m.AddSeedMember(peer);
        sub.Dispose();
        m.MarkUp(peer);    // should NOT fire to disposed sub

        Assert.Single(events);
    }

    /// <summary>
    /// End-to-end: register a peer via Cluster.RegisterPeer, observe the
    /// NodeJoining event, mark up via Cluster.MarkPeerUp, observe the
    /// NodeUp event. Confirms the bridge between the existing
    /// peer-registry API and the new membership view.
    /// </summary>
    [Fact]
    public void Cluster_RegisterPeer_PropagatesToMembership()
    {
        using var fabric = new InMemoryClusterFabric();
        using var system = new ActorSystem("a");
        var transport = fabric.CreateTransport("a");
        var cluster   = SpekClusterNs.Cluster.Bind(system, transport);

        var events = new List<SpekClusterNs.ClusterEvent>();
        using var sub = cluster.Membership.Subscribe(events.Add);

        var peer = new SpekClusterNs.NodeIdentity(Guid.NewGuid(), "peer");
        cluster.RegisterPeer("peer", peer);
        Assert.Equal(SpekClusterNs.NodeState.Joining,
                     cluster.Membership.FindMember(peer)!.State);

        cluster.MarkPeerUp(peer);
        Assert.Equal(SpekClusterNs.NodeState.Up,
                     cluster.Membership.FindMember(peer)!.State);

        Assert.Equal(2, events.Count);
        Assert.IsType<SpekClusterNs.ClusterEvent.NodeJoining>(events[0]);
        Assert.IsType<SpekClusterNs.ClusterEvent.NodeUp>(events[1]);
    }

    /// <summary>
    /// End-to-end leave: cluster.LeaveAsync() transitions the local
    /// node through Leaving → Exiting and fires both events. Test
    /// for the integration the hosting adapters will tap to wire
    /// graceful Shutdown into clean cluster departure.
    /// </summary>
    [Fact]
    public async Task Cluster_LeaveAsync_FiresLeavingThenExitingAsync()
    {
        using var fabric = new InMemoryClusterFabric();
        using var system = new ActorSystem("a");
        var transport = fabric.CreateTransport("a");
        var cluster   = SpekClusterNs.Cluster.Bind(system, transport);

        var events = new List<SpekClusterNs.ClusterEvent>();
        using var sub = cluster.Membership.Subscribe(events.Add);

        await cluster.LeaveAsync();

        Assert.Equal(2, events.Count);
        Assert.IsType<SpekClusterNs.ClusterEvent.NodeLeaving>(events[0]);
        Assert.IsType<SpekClusterNs.ClusterEvent.NodeExiting>(events[1]);
    }
}
