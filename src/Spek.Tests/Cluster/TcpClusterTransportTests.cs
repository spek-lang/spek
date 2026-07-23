using System.Collections.Concurrent;
using System.Net;
using SpekClusterNs = Spek.Cluster;
using Spek.Cluster.Tcp;
using Spek.Runtime;
using Xunit;

namespace Spek.Tests.ClusterIntegration.Tcp;

/// <summary>
/// Layer-1 integration tests for <see cref="TcpClusterTransport"/>:
/// two transports in the same process, each on its own loopback port,
/// exchanging real TCP frames. Catches wire-format bugs, serialization
/// bugs, handshake bugs, and the sender round-trip path.
///
/// Cross-process verification (Layer 2) lives in a separate fixture
/// that uses <c>Process.Start</c> to spawn child binaries — out of
/// scope for these tests.
/// </summary>
public class TcpClusterTransportTests
{
    public sealed record Greet(string Name);
    public sealed record GreetReply(string Greeting);

    private sealed class GreeterActor : ActorBase
    {
        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            if (message is Greet g)
                _currentSender.Tell(new GreetReply($"hello {g.Name}"));
            return Task.CompletedTask;
        }
    }

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

    /// <summary>
    /// Stand up two ActorSystems each with its own TcpClusterTransport
    /// on a loopback port. Wire them as cluster peers. Round-trip a
    /// Greet → GreetReply across the wire.
    /// </summary>
    [Fact]
    public async Task TwoTransports_LoopbackTcp_RoundtripsSenderReplyAsync()
    {
        // Port 0 → OS picks a free port. Bound endpoint exposed via
        // BoundEndpoint so the peer can connect to the right port.
        var optsA = new TcpClusterOptions
        {
            Label          = "node-a",
            ListenEndpoint = new IPEndPoint(IPAddress.Loopback, 0),
            LoopbackOnly   = true,
        };
        var optsB = new TcpClusterOptions
        {
            Label          = "node-b",
            ListenEndpoint = new IPEndPoint(IPAddress.Loopback, 0),
            LoopbackOnly   = true,
        };

        await using var transportA = new TcpClusterTransport(optsA);
        await using var transportB = new TcpClusterTransport(optsB);

        // A connects to B; B connects to A. (TCP is point-to-point,
        // each side opens its own outbound socket. Production code
        // would do this lazily on first Tell.)
        await transportA.ConnectToPeerAsync(transportB.BoundEndpoint, transportB.LocalNode);
        await transportB.ConnectToPeerAsync(transportA.BoundEndpoint, transportA.LocalNode);

        using var systemA = new ActorSystem("a");
        using var systemB = new ActorSystem("b");
        var clusterA = SpekClusterNs.Cluster.Bind(systemA, transportA);
        var clusterB = SpekClusterNs.Cluster.Bind(systemB, transportB);

        clusterA.RegisterPeer("node-b", transportB.LocalNode);
        clusterB.RegisterPeer("node-a", transportA.LocalNode);

        systemB.SpawnNamed<GreeterActor>("greeter");

        var sink = new BlockingCollection<object>();
        var collector = systemA.SpawnNamed<CollectingActor>("collector", sink);

        var remoteGreeter = clusterA.ResolveRemote("node-b", "greeter");
        remoteGreeter.Tell(new Greet("over-the-wire"), sender: collector);

        Assert.True(sink.TryTake(out var arrived, TimeSpan.FromSeconds(5)),
            "Reply did not arrive across the TCP wire within 5s.");

        var reply = Assert.IsType<GreetReply>(arrived);
        Assert.Equal("hello over-the-wire", reply.Greeting);
    }

    [Fact]
    public async Task ConnectToPeer_IdentityMismatch_ThrowsAndDisconnectsAsync()
    {
        // Stand up B with a real identity; tell A to connect expecting a
        // different identity. The handshake should detect mismatch and
        // refuse the connection.
        var optsA = new TcpClusterOptions { ListenEndpoint = new IPEndPoint(IPAddress.Loopback, 0), LoopbackOnly = true };
        var optsB = new TcpClusterOptions { ListenEndpoint = new IPEndPoint(IPAddress.Loopback, 0), LoopbackOnly = true };

        await using var transportA = new TcpClusterTransport(optsA);
        await using var transportB = new TcpClusterTransport(optsB);

        // Synthesize a wrong identity for B.
        var bogusB = new SpekClusterNs.NodeIdentity(Guid.NewGuid(), "expected-but-wrong");

        var ex = await Assert.ThrowsAsync<InvalidOperationException>(async () =>
            await transportA.ConnectToPeerAsync(transportB.BoundEndpoint, bogusB));
        Assert.Contains("reported identity", ex.Message);
    }
}
