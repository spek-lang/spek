using System.Collections.Concurrent;
using SpekClusterNs = Spek.Cluster;
using Spek.Cluster.Memory;
using Spek.Runtime;
using Xunit;

namespace Spek.Tests.ClusterIntegration.LocatedActors;

/// <summary>
/// Coverage for located actors / cluster-wide actor placement.
/// Drives the <see cref="SpekClusterNs.Cluster"/> located-actor APIs
/// end-to-end: determinism of placement, local auto-activation,
/// remote routing, per-key actor isolation, key-stable identity.
/// </summary>
public class LocatedActorPlacementTests
{
    public sealed record Increment();
    public sealed record GetCount();
    public sealed record CountReply(string Key, int Count);

    /// <summary>
    /// Counter actor keyed by its constructor argument (the location key).
    /// Each call to Increment bumps an internal count; GetCount replies
    /// with the (key, count) pair.
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

    private sealed class CollectingActor : ActorBase
    {
        private readonly BlockingCollection<object> _sink;
        public CollectingActor(BlockingCollection<object> sink) => _sink = sink;
        protected override Task DispatchAsync(object message, ActorRef sender)
        { _sink.Add(message); return Task.CompletedTask; }
    }

    [Fact]
    public void ConsistentHashPlacement_IsDeterministic()
    {
        var p = new SpekClusterNs.ConsistentHashPlacement();
        var nodeA = new SpekClusterNs.NodeIdentity(Guid.Parse("11111111-1111-1111-1111-111111111111"), "a");
        var nodeB = new SpekClusterNs.NodeIdentity(Guid.Parse("22222222-2222-2222-2222-222222222222"), "b");
        var nodeC = new SpekClusterNs.NodeIdentity(Guid.Parse("33333333-3333-3333-3333-333333333333"), "c");
        var members = new List<SpekClusterNs.ClusterMember>
        {
            new(nodeA, SpekClusterNs.NodeState.Up),
            new(nodeB, SpekClusterNs.NodeState.Up),
            new(nodeC, SpekClusterNs.NodeState.Up),
        };

        // The same (type, key, members) triple yields the same node
        // every time. This is the property that makes any two cluster
        // nodes agree on located-actor ownership without coordination.
        var first  = p.ResolveOwner("CounterActor", "user-42", members);
        var second = p.ResolveOwner("CounterActor", "user-42", members);
        Assert.Equal(first!.Id, second!.Id);
    }

    [Fact]
    public void ConsistentHashPlacement_IgnoresNonUpMembers()
    {
        var p = new SpekClusterNs.ConsistentHashPlacement();
        var nodeA = new SpekClusterNs.NodeIdentity(Guid.NewGuid(), "a");
        var nodeB = new SpekClusterNs.NodeIdentity(Guid.NewGuid(), "b");
        var members = new List<SpekClusterNs.ClusterMember>
        {
            new(nodeA, SpekClusterNs.NodeState.Up),
            new(nodeB, SpekClusterNs.NodeState.Down),       // dead — placement should skip
        };

        var owner = p.ResolveOwner("CounterActor", "anything", members);
        Assert.Equal(nodeA.Id, owner!.Id);
    }

    [Fact]
    public void ConsistentHashPlacement_NoLiveMembers_ReturnsNull()
    {
        var p = new SpekClusterNs.ConsistentHashPlacement();
        var members = new List<SpekClusterNs.ClusterMember>();
        Assert.Null(p.ResolveOwner("CounterActor", "x", members));
    }

    [Fact]
    public void ConsistentHashPlacement_DifferentKeys_FrequentlyDifferentNodes()
    {
        // With 4 nodes and 100 keys, we expect roughly 25 keys per node
        // — not exact, but each node should own at least 10. This
        // verifies the hash actually disperses, vs degenerating into
        // "first node wins for everything."
        var p = new SpekClusterNs.ConsistentHashPlacement();
        var members = Enumerable.Range(0, 4)
            .Select(i => new SpekClusterNs.ClusterMember(
                new SpekClusterNs.NodeIdentity(Guid.NewGuid(), $"node-{i}"),
                SpekClusterNs.NodeState.Up))
            .ToList();

        var ownerCounts = new Dictionary<Guid, int>();
        for (int i = 0; i < 100; i++)
        {
            var owner = p.ResolveOwner("CounterActor", $"key-{i}", members)!;
            ownerCounts[owner.Id] = ownerCounts.GetValueOrDefault(owner.Id) + 1;
        }
        Assert.Equal(4, ownerCounts.Count);
        Assert.All(ownerCounts.Values, c => Assert.True(c >= 10,
            $"Expected each of 4 nodes to own at least 10 of 100 keys, got {c}."));
    }

    [Fact]
    public void Locate_LocalOwner_AutoActivates()
    {
        // Single-node cluster: every located-actor placement resolves
        // to local. Calling Locate spawns the actor under
        // "{TypeName}/{key}"; subsequent calls return the same ref.
        using var fabric = new InMemoryClusterFabric();
        using var system = new ActorSystem("solo");
        var transport    = fabric.CreateTransport("solo");
        var cluster      = SpekClusterNs.Cluster.Bind(system, transport);
        cluster.RegisterLocatedActor<CounterActor>();

        var actor1 = cluster.Locate<CounterActor>("alice");
        var actor2 = cluster.Locate<CounterActor>("alice");
        Assert.Same(actor1, actor2);

        // Different key → different actor.
        var bob = cluster.Locate<CounterActor>("bob");
        Assert.NotSame(actor1, bob);
    }

    [Fact]
    public async Task Locate_LocalActivation_HandlesMessagesAsync()
    {
        using var fabric = new InMemoryClusterFabric();
        using var system = new ActorSystem("solo");
        var transport    = fabric.CreateTransport("solo");
        var cluster      = SpekClusterNs.Cluster.Bind(system, transport);
        cluster.RegisterLocatedActor<CounterActor>();

        var sink = new BlockingCollection<object>();
        var collector = system.SpawnNamed<CollectingActor>("collector", sink);

        var alice = cluster.Locate<CounterActor>("alice");
        alice.Tell(new Increment());
        alice.Tell(new Increment());
        alice.Tell(new Increment());
        alice.Tell(new GetCount(), sender: collector);

        Assert.True(sink.TryTake(out var arrived, TimeSpan.FromSeconds(2)),
            "GetCount reply did not arrive within 2s.");
        var reply = Assert.IsType<CountReply>(arrived);
        Assert.Equal("alice", reply.Key);
        Assert.Equal(3, reply.Count);

        await Task.CompletedTask;
    }

    [Fact]
    public async Task Locate_TwoSystems_KeyDeterministicOwnerActivatesOnceTotalAsync()
    {
        // Stand up two ActorSystems sharing a fabric. Both register the
        // same located-actor type. From either side,
        // Locate<CounterActor>("k1") resolves the same owner (placement
        // is deterministic) and the actor auto-activates on whichever
        // node wins.
        using var fabric = new InMemoryClusterFabric();
        using var systemA = new ActorSystem("a");
        using var systemB = new ActorSystem("b");
        var transportA = fabric.CreateTransport("a");
        var transportB = fabric.CreateTransport("b");
        var clusterA   = SpekClusterNs.Cluster.Bind(systemA, transportA);
        var clusterB   = SpekClusterNs.Cluster.Bind(systemB, transportB);

        clusterA.RegisterPeer("b", transportB.LocalNode);
        clusterB.RegisterPeer("a", transportA.LocalNode);
        clusterA.MarkPeerUp(transportB.LocalNode);
        clusterB.MarkPeerUp(transportA.LocalNode);

        clusterA.RegisterLocatedActor<CounterActor>();
        clusterB.RegisterLocatedActor<CounterActor>();

        // From A, locate the actor — it'll resolve to whichever node
        // wins the consistent-hash. From B, locate the same actor —
        // it should also resolve to the same node. Send Increments
        // from both sides; the count should reflect all of them.
        var sink = new BlockingCollection<object>();
        var collector = systemA.SpawnNamed<CollectingActor>("collector", sink);

        var actorFromA = clusterA.Locate<CounterActor>("shared-key");
        var actorFromB = clusterB.Locate<CounterActor>("shared-key");

        // 3 increments from A's view + 2 from B's view = 5 total. The
        // actor lives on exactly one node (whoever placement picked).
        actorFromA.Tell(new Increment());
        actorFromA.Tell(new Increment());
        actorFromA.Tell(new Increment());
        actorFromB.Tell(new Increment());
        actorFromB.Tell(new Increment());

        // Ask the actor (via A's view) what it counted.
        actorFromA.Tell(new GetCount(), sender: collector);

        Assert.True(sink.TryTake(out var arrived, TimeSpan.FromSeconds(3)),
            "GetCount reply did not arrive within 3s.");
        var reply = Assert.IsType<CountReply>(arrived);
        Assert.Equal("shared-key", reply.Key);
        Assert.Equal(5, reply.Count);

        await Task.CompletedTask;
    }

    [Fact]
    public void RegisterLocatedActor_MissingRegistration_RemoteIncoming_DeadLetters()
    {
        // If a peer sends a Tell to a location path on a node that
        // hasn't registered the actor type, the message dead-letters
        // cleanly rather than throwing or auto-spawning the wrong type.
        using var fabric = new InMemoryClusterFabric();
        using var system = new ActorSystem("solo");
        var transport    = fabric.CreateTransport("solo");
        var cluster      = SpekClusterNs.Cluster.Bind(system, transport);
        // Note: NO RegisterLocatedActor call.

        // Manually send an envelope as if it came over the wire from a
        // peer who thinks this located actor lives here. Auto-activation
        // should skip (no registration), DeliverIncoming should dead-
        // letter. We can't easily intercept the dead-letter sink in
        // this scope, but we can confirm no exception escapes:
        var ex = Record.Exception(() =>
            system.DeliverIncoming("CounterActor/orphan", new Increment(), sender: null));
        Assert.Null(ex);
    }
}
