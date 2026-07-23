using System.Collections.Concurrent;
using SpekClusterNs = Spek.Cluster;       // alias to dodge namespace collision below
using Spek.Cluster.Memory;
using Spek.Runtime;
using Xunit;

namespace Spek.Tests.ClusterIntegration;     // not "Spek.Tests.Cluster" — avoids `Cluster.Bind` ambiguity

/// <summary>
/// End-to-end tests for the Bedrock-aligned cluster layer. Two
/// <see cref="ActorSystem"/>s in the same process bridged through a
/// shared <see cref="InMemoryClusterFabric"/> exercise the same code
/// paths a real TCP cluster would: <see cref="ActorRef"/> Tell routes
/// through <see cref="SpekClusterNs.ISpekTransport.SendAsync"/>; the
/// receiver's <c>_currentSender.Tell(reply)</c> goes back across the
/// wire because the envelope carries the originator's named-root path
/// + node.
/// </summary>
public class ClusterMemoryTransportTests
{
    public sealed record Greet(string Name);
    public sealed record GreetReply(string Greeting);

    /// <summary>Echo-style actor: replies to Greet via _currentSender.Tell.</summary>
    private sealed class GreeterActor : ActorBase
    {
        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            if (message is Greet g)
                _currentSender.Tell(new GreetReply($"hello {g.Name}"));
            return Task.CompletedTask;
        }
    }

    /// <summary>Sink — captures replies into a shared queue passed by the test.</summary>
    private sealed class CollectingActor : ActorBase
    {
        private readonly BlockingCollection<object> _sink;

        public CollectingActor(BlockingCollection<object> sink) => _sink = sink;

        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            _sink.Add(message);
            return Task.CompletedTask;
        }
    }

    /// <summary>Captures the sender ref of every message it receives and
    /// replies through it — observes remote-sender ref identity across
    /// envelopes while proving the ref still routes.</summary>
    private sealed class SenderCapturingActor : ActorBase
    {
        private readonly BlockingCollection<ActorRef> _senders;

        public SenderCapturingActor(BlockingCollection<ActorRef> senders) => _senders = senders;

        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            _currentSender = sender;
            _senders.Add(sender);
            if (message is Greet g)
                _currentSender.Tell(new GreetReply($"hello {g.Name}"));
            return Task.CompletedTask;
        }
    }

    [Fact]
    public async Task TwoSystems_TellAcrossWire_DeliversMessageAsync()
    {
        // Spawn the greeter on B; from A, get a remote ref to it; Tell.
        // Greeter's reply Tell uses NoSender (no round-trip required for
        // this test). Sanity-check that delivery doesn't throw and the
        // greeter actor stays alive.
        using var fabric = new InMemoryClusterFabric();
        using var systemA = new ActorSystem("a");
        using var systemB = new ActorSystem("b");

        var transportA = fabric.CreateTransport("node-a");
        var transportB = fabric.CreateTransport("node-b");

        var clusterA = SpekClusterNs.Cluster.Bind(systemA, transportA);
        var clusterB = SpekClusterNs.Cluster.Bind(systemB, transportB);

        clusterA.RegisterPeer("node-b", transportB.LocalNode);
        clusterB.RegisterPeer("node-a", transportA.LocalNode);

        var greeter = systemB.SpawnNamed<GreeterActor>("greeter");

        var remoteGreeter = clusterA.ResolveRemote("node-b", "greeter");
        Assert.True(remoteGreeter.IsRemote);

        remoteGreeter.Tell(new Greet("world"));

        // Give the dispatch loops a moment.
        await Task.Delay(100);

        Assert.False(greeter.IsStopped);
    }

    [Fact]
    public async Task TwoSystems_SenderRoundTrip_ReplyArrivesAtOriginatorAsync()
    {
        // Full location-transparency test. A named-root collector on A
        // Tells the greeter on B with itself as the sender; the greeter's
        // _currentSender.Tell(reply) routes back across the wire to A
        // and lands in the collector's mailbox.
        using var fabric = new InMemoryClusterFabric();
        using var systemA = new ActorSystem("a");
        using var systemB = new ActorSystem("b");

        var transportA = fabric.CreateTransport("node-a");
        var transportB = fabric.CreateTransport("node-b");

        var clusterA = SpekClusterNs.Cluster.Bind(systemA, transportA);
        var clusterB = SpekClusterNs.Cluster.Bind(systemB, transportB);

        clusterA.RegisterPeer("node-b", transportB.LocalNode);
        clusterB.RegisterPeer("node-a", transportA.LocalNode);

        systemB.SpawnNamed<GreeterActor>("greeter");

        var sink = new BlockingCollection<object>();
        var collector = systemA.SpawnNamed<CollectingActor>("collector", sink);

        var remoteGreeter = clusterA.ResolveRemote("node-b", "greeter");
        remoteGreeter.Tell(new Greet("location-transparency"), sender: collector);

        // Wait for the round-trip with a 2s timeout cap so wiring bugs
        // don't hang the test runner.
        Assert.True(sink.TryTake(out var arrived, TimeSpan.FromSeconds(2)),
            "Reply did not arrive at the originating collector within 2s.");

        var reply = Assert.IsType<GreetReply>(arrived);
        Assert.Equal("hello location-transparency", reply.Greeting);

        await Task.CompletedTask;
    }

    [Fact]
    public void RepeatedEnvelopesFromSameSender_ReceiverSeesOneCachedRef()
    {
        // Pins the perf-r16 receive-side contract: every inbound envelope
        // carrying the same (origin node, sender path) resolves to the SAME
        // ActorRef instance — reference identity, since ActorRef has no value
        // equality — instead of a fresh RemoteEndpoint + ref per message.
        // Recipients that stash senders (dedupe sets, per-sender state keyed
        // by ref) therefore see one logical remote sender as one ref. The
        // replies assert the cached ref still routes on every use, not just
        // the first.
        using var fabric = new InMemoryClusterFabric();
        using var systemA = new ActorSystem("a");
        using var systemB = new ActorSystem("b");

        var transportA = fabric.CreateTransport("node-a");
        var transportB = fabric.CreateTransport("node-b");

        var clusterA = SpekClusterNs.Cluster.Bind(systemA, transportA);
        var clusterB = SpekClusterNs.Cluster.Bind(systemB, transportB);

        clusterA.RegisterPeer("node-b", transportB.LocalNode);
        clusterB.RegisterPeer("node-a", transportA.LocalNode);

        var senders = new BlockingCollection<ActorRef>();
        systemB.SpawnNamed<SenderCapturingActor>("capturer", senders);

        var replies = new BlockingCollection<object>();
        var collector = systemA.SpawnNamed<CollectingActor>("collector", replies);

        var remoteCapturer = clusterA.ResolveRemote("node-b", "capturer");
        remoteCapturer.Tell(new Greet("first"), sender: collector);
        remoteCapturer.Tell(new Greet("second"), sender: collector);

        Assert.True(senders.TryTake(out var seen1, TimeSpan.FromSeconds(2)),
            "First envelope did not arrive within 2s.");
        Assert.True(senders.TryTake(out var seen2, TimeSpan.FromSeconds(2)),
            "Second envelope did not arrive within 2s.");

        Assert.NotNull(seen1);
        Assert.Same(seen1, seen2);
        Assert.True(seen1!.IsRemote);

        // The shared ref keeps working per message — both replies land.
        Assert.True(replies.TryTake(out _, TimeSpan.FromSeconds(2)),
            "First reply did not route back through the cached sender ref.");
        Assert.True(replies.TryTake(out _, TimeSpan.FromSeconds(2)),
            "Second reply did not route back through the cached sender ref.");
    }

    [Fact]
    public void EnvelopesFromDistinctSenders_ReceiverSeesDistinctRefs()
    {
        // Companion to the caching test: the cache key is (node, path), so
        // two different named-root senders on the same node must NOT collapse
        // into one ref.
        using var fabric = new InMemoryClusterFabric();
        using var systemA = new ActorSystem("a");
        using var systemB = new ActorSystem("b");

        var transportA = fabric.CreateTransport("node-a");
        var transportB = fabric.CreateTransport("node-b");

        var clusterA = SpekClusterNs.Cluster.Bind(systemA, transportA);
        var clusterB = SpekClusterNs.Cluster.Bind(systemB, transportB);

        clusterA.RegisterPeer("node-b", transportB.LocalNode);
        clusterB.RegisterPeer("node-a", transportA.LocalNode);

        var senders = new BlockingCollection<ActorRef>();
        systemB.SpawnNamed<SenderCapturingActor>("capturer", senders);

        var sinkOne = new BlockingCollection<object>();
        var sinkTwo = new BlockingCollection<object>();
        var collectorOne = systemA.SpawnNamed<CollectingActor>("collector-one", sinkOne);
        var collectorTwo = systemA.SpawnNamed<CollectingActor>("collector-two", sinkTwo);

        var remoteCapturer = clusterA.ResolveRemote("node-b", "capturer");
        remoteCapturer.Tell(new Greet("one"), sender: collectorOne);

        Assert.True(senders.TryTake(out var seenOne, TimeSpan.FromSeconds(2)),
            "Envelope from collector-one did not arrive within 2s.");
        Assert.True(sinkOne.TryTake(out _, TimeSpan.FromSeconds(2)),
            "Reply to collector-one did not arrive within 2s.");

        remoteCapturer.Tell(new Greet("two"), sender: collectorTwo);

        Assert.True(senders.TryTake(out var seenTwo, TimeSpan.FromSeconds(2)),
            "Envelope from collector-two did not arrive within 2s.");
        Assert.NotSame(seenOne, seenTwo);

        // And each reply reached its own originator, so the two cached refs
        // route independently.
        Assert.True(sinkTwo.TryTake(out _, TimeSpan.FromSeconds(2)),
            "Reply to collector-two did not arrive within 2s.");
        Assert.Empty(sinkOne);
    }

    [Fact]
    public void RemoteRef_ReportsIsRemoteTrue()
    {
        using var fabric = new InMemoryClusterFabric();
        using var system = new ActorSystem("only");

        var transport = fabric.CreateTransport("only-node");
        var cluster   = SpekClusterNs.Cluster.Bind(system, transport);
        cluster.RegisterPeer("self", transport.LocalNode);

        var someRemote = cluster.ResolveRemote("self", "nonexistent");
        Assert.True(someRemote.IsRemote);
        Assert.NotNull(someRemote.RemoteEndpoint);
    }

    [Fact]
    public void ResolveRemote_UnknownPeer_Throws()
    {
        using var fabric = new InMemoryClusterFabric();
        using var system = new ActorSystem("only");

        var transport = fabric.CreateTransport("only");
        var cluster   = SpekClusterNs.Cluster.Bind(system, transport);

        Assert.Throws<InvalidOperationException>(() =>
            cluster.ResolveRemote("not-registered", "anything"));
    }
}
