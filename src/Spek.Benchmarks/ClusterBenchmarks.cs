using System.Buffers;
using System.Net;
using BenchmarkDotNet.Attributes;
using Spek;
using Spek.Cluster.Memory;
using Spek.Cluster.Tcp;
using Spek.Runtime;
using SpekCluster = Spek.Cluster.Cluster;   // the type shares its namespace's name

namespace Spek.Benchmarks;

/// <summary>
/// The boundary costs — what a message pays the moment it leaves the local
/// mailbox world and crosses between <see cref="ActorSystem"/>s. Three rungs:
/// a plain in-process ask (the baseline), the same request/reply over the
/// in-memory cluster transport (envelope + fabric dispatch + remote-sender
/// reconstruction, no bytes), and over the TCP loopback transport (all of the
/// above plus JSON serialization, binary framing, and a real socket hop each
/// way). The serialize-only rows isolate the message→wire-bytes step by
/// calling <see cref="JsonSpekSerializer"/> directly.
///
/// Remote asks aren't a runtime primitive yet — <c>AskAsync</c> on a remote
/// ref throws <see cref="NotSupportedException"/> — so the transport ask arms
/// use the wire request/reply idiom instead: Tell with a named-root sender,
/// reply routed back across the transport into a <see cref="ReplySinkActor"/>
/// (one extra mailbox hop and a TaskCompletionSource per round trip; the
/// baseline's pooled reply cell has neither, so read the ratio as the price
/// of the whole boundary, not of the transport alone).
///
/// Nodes are stood up once in GlobalSetup — this class measures the
/// steady-state per-message boundary tax, not node bootstrap.
/// </summary>
[MemoryDiagnoser]
public class ClusterBenchmarks
{
    /// <summary>Sequential round trips per invocation for the ask-shaped
    /// arms — same count as <see cref="MessagingBenchmarks.AskRoundTrip"/>,
    /// so the rows read side by side.</summary>
    private const int RoundTrips = 1_000;

    [Params(100_000)]
    public int Messages;

    // ── In-memory cluster (two systems, one fabric) ──────────────────────
    private InMemoryClusterFabric _memFabric = null!;
    private ActorSystem _memSystemA = null!;
    private ActorSystem _memSystemB = null!;
    private SpekCluster _memClusterA = null!;
    private SpekCluster _memClusterB = null!;
    private ActorRef _localEcho = null!;
    private ActorRef _memAsker = null!;
    private ActorRef _memRemoteEcho = null!;
    private ActorRef _memRemoteCounter = null!;
    private ReplySink _memReplies = null!;

    [GlobalSetup(Targets = new[]
        { nameof(LocalAskBaseline), nameof(MemoryTransportAsk), nameof(MemoryTransportTell) })]
    public void SetupMemory()
    {
        _memFabric  = new InMemoryClusterFabric();
        _memSystemA = new ActorSystem("bench-a");
        _memSystemB = new ActorSystem("bench-b");

        var transportA = _memFabric.CreateTransport("node-a");
        var transportB = _memFabric.CreateTransport("node-b");
        _memClusterA = SpekCluster.Bind(_memSystemA, transportA);
        _memClusterB = SpekCluster.Bind(_memSystemB, transportB);
        _memClusterA.RegisterPeer("node-b", transportB.LocalNode);
        _memClusterB.RegisterPeer("node-a", transportA.LocalNode);

        _memSystemB.SpawnNamed<EchoActor>("echo");
        _memSystemB.SpawnNamed<CounterActor>("counter");

        _memReplies = new ReplySink();
        _memAsker   = _memSystemA.SpawnNamed<ReplySinkActor>("asker", _memReplies);
        _localEcho  = _memSystemA.Spawn<EchoActor>();

        _memRemoteEcho    = _memClusterA.ResolveRemote("node-b", "echo");
        _memRemoteCounter = _memClusterA.ResolveRemote("node-b", "counter");
    }

    [GlobalCleanup(Targets = new[]
        { nameof(LocalAskBaseline), nameof(MemoryTransportAsk), nameof(MemoryTransportTell) })]
    public async Task CleanupMemory()
    {
        _memSystemA.Dispose();
        _memSystemB.Dispose();
        await _memClusterA.DisposeAsync();
        await _memClusterB.DisposeAsync();
        _memFabric.Dispose();
    }

    /// <summary>The reference row: a plain in-process ask against a local
    /// actor on the same node the transport arms send from. Send → handler →
    /// pooled-reply-cell resume; no envelope, no transport, no second
    /// mailbox.</summary>
    [Benchmark(Baseline = true)]
    public async Task LocalAskBaseline()
    {
        for (int i = 0; i < RoundTrips; i++)
            await _localEcho.AskAsync<Pong>(new Ping());
    }

    /// <summary>
    /// The same request/reply crossing the in-memory transport between two
    /// in-process nodes. Each round trip pays, twice (request and reply): a
    /// named-root path lookup on the send side (O(1) map since r16), a
    /// RemoteEnvelope, the fabric's transport lookup, and the receive side's
    /// cached remote-sender ref resolution (per-envelope RemoteEndpoint +
    /// ActorRef allocation retired in r16) — plus the target's mailbox
    /// hop on each node. No bytes are produced; this is the envelope-and-
    /// dispatch machinery with serialization at zero.
    /// </summary>
    [Benchmark]
    public async Task MemoryTransportAsk()
    {
        for (int i = 0; i < RoundTrips; i++)
        {
            var reply = _memReplies.Arm();
            _memRemoteEcho.Tell(new Ping(), _memAsker);
            await reply;
        }
    }

    /// <summary>Fire-and-forget throughput across the in-memory transport:
    /// N sender-less tells into a remote counter, then drain node B to idle.
    /// The fabric delivers synchronously, so each Tell runs the full
    /// envelope → transport → dispatch chain before returning — the per-
    /// message boundary tax without any reply traffic.</summary>
    [Benchmark]
    public int MemoryTransportTell()
    {
        for (int i = 0; i < Messages; i++)
            _memRemoteCounter.Tell(new Ping());
        _memSystemB.AwaitTermination();
        return Messages;
    }

    // ── TCP loopback cluster (two systems, two real sockets) ─────────────
    private TcpClusterTransport _tcpTransportA = null!;
    private TcpClusterTransport _tcpTransportB = null!;
    private ActorSystem _tcpSystemA = null!;
    private ActorSystem _tcpSystemB = null!;
    private SpekCluster _tcpClusterA = null!;
    private SpekCluster _tcpClusterB = null!;
    private ActorRef _tcpAsker = null!;
    private ActorRef _tcpRemoteEcho = null!;
    private ReplySink _tcpReplies = null!;

    [GlobalSetup(Target = nameof(TcpLoopbackAsk))]
    public async Task SetupTcp()
    {
        // Port 0 → the OS picks a free port; BoundEndpoint reveals it. Keeps
        // parallel/repeated benchmark processes from fighting over a port.
        _tcpTransportA = new TcpClusterTransport(new TcpClusterOptions
        {
            Label          = "node-a",
            ListenEndpoint = new IPEndPoint(IPAddress.Loopback, 0),
            LoopbackOnly   = true,
        });
        _tcpTransportB = new TcpClusterTransport(new TcpClusterOptions
        {
            Label          = "node-b",
            ListenEndpoint = new IPEndPoint(IPAddress.Loopback, 0),
            LoopbackOnly   = true,
        });

        await _tcpTransportA.ConnectToPeerAsync(_tcpTransportB.BoundEndpoint, _tcpTransportB.LocalNode);
        await _tcpTransportB.ConnectToPeerAsync(_tcpTransportA.BoundEndpoint, _tcpTransportA.LocalNode);

        _tcpSystemA  = new ActorSystem("bench-a");
        _tcpSystemB  = new ActorSystem("bench-b");
        _tcpClusterA = SpekCluster.Bind(_tcpSystemA, _tcpTransportA);
        _tcpClusterB = SpekCluster.Bind(_tcpSystemB, _tcpTransportB);
        _tcpClusterA.RegisterPeer("node-b", _tcpTransportB.LocalNode);
        _tcpClusterB.RegisterPeer("node-a", _tcpTransportA.LocalNode);

        _tcpSystemB.SpawnNamed<EchoActor>("echo");
        _tcpReplies = new ReplySink();
        _tcpAsker   = _tcpSystemA.SpawnNamed<ReplySinkActor>("asker", _tcpReplies);

        _tcpRemoteEcho = _tcpClusterA.ResolveRemote("node-b", "echo");
    }

    [GlobalCleanup(Target = nameof(TcpLoopbackAsk))]
    public async Task CleanupTcp()
    {
        _tcpSystemA.Dispose();
        _tcpSystemB.Dispose();
        await _tcpClusterA.DisposeAsync();
        await _tcpClusterB.DisposeAsync();
    }

    /// <summary>
    /// The same request/reply over <c>Spek.Cluster.Tcp</c> on 127.0.0.1: on
    /// top of everything <see cref="MemoryTransportAsk"/> pays, each leg adds
    /// JSON payload serialization, binary frame encode, a socket write +
    /// flush, kernel loopback, frame decode, and payload deserialization
    /// (including the type-name → Type resolution). Round trips are
    /// sequential, so this is boundary latency, not socket throughput.
    /// </summary>
    [Benchmark]
    public async Task TcpLoopbackAsk()
    {
        for (int i = 0; i < RoundTrips; i++)
        {
            var reply = _tcpReplies.Arm();
            _tcpRemoteEcho.Tell(new Ping(), _tcpAsker);
            await reply;
        }
    }

    // ── Serialization step in isolation ──────────────────────────────────
    private readonly JsonSpekSerializer _serializer = new();
    private readonly ArrayBufferWriter<byte> _wireBuffer = new(4096);
    private static readonly SmallWireMessage SmallMessage = new("bench");
    private static readonly EightFieldWireMessage EightFieldMessage = new(
        Id: 42_424_242L,
        Name: "bench-entity",
        Region: "us-east-1",
        Count: 1_337,
        Score: 99.75,
        Active: true,
        CreatedAt: DateTimeOffset.FromUnixTimeSeconds(1_750_000_000),
        Notes: "eight-field wire payload for the serializer benchmark");

    /// <summary>Just the message → wire-bytes step for a one-field record:
    /// one <c>Utf8JsonWriter</c> construction plus reflection-driven JSON
    /// serialization per call, into a reused buffer. This is the send-side
    /// serializer slice of every <see cref="TcpLoopbackAsk"/> leg.</summary>
    [Benchmark]
    public int EnvelopeSerializeOnly_Small()
    {
        _wireBuffer.ResetWrittenCount();
        _serializer.Serialize(SmallMessage, _wireBuffer);
        return _wireBuffer.WrittenCount;
    }

    /// <summary>The same step for the eight-field record — how the
    /// serializer cost scales with payload width.</summary>
    [Benchmark]
    public int EnvelopeSerializeOnly_EightField()
    {
        _wireBuffer.ResetWrittenCount();
        _serializer.Serialize(EightFieldMessage, _wireBuffer);
        return _wireBuffer.WrittenCount;
    }
}
