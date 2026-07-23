using System.Collections.Concurrent;
using System.Net;
using Spek.Cluster;
using Spek.Cluster.Tcp;
using Xunit;

namespace Spek.Tests.Cluster;

/// <summary>
/// Wire integrity under concurrent senders (the r15 gate). PipeWriter is
/// single-writer; before the per-connection send gate, two threads sending
/// to the same peer could interleave GetSpan/Advance mid-frame and corrupt
/// the stream — a bug no sequential test can see. This drives many senders
/// through one connection and requires every frame to arrive parseable and
/// payload-intact.
/// </summary>
public class TcpConcurrentSendTests
{
    public sealed record Tagged(int Sender, int Seq, string Padding);

    [Fact]
    public async Task ManyConcurrentSenders_OneConnection_EveryFrameArrivesIntactAsync()
    {
        await using var a = new TcpClusterTransport(new TcpClusterOptions
        {
            Label = "node-a",
            ListenEndpoint = new IPEndPoint(IPAddress.Loopback, 0),
            LoopbackOnly = true,
        });
        await using var b = new TcpClusterTransport(new TcpClusterOptions
        {
            Label = "node-b",
            ListenEndpoint = new IPEndPoint(IPAddress.Loopback, 0),
            LoopbackOnly = true,
        });

        await a.ConnectToPeerAsync(b.BoundEndpoint, b.LocalNode);

        var received = new ConcurrentBag<Tagged>();
        b.SetReceiveHandler(envelope =>
        {
            received.Add((Tagged)envelope.Message);
            return Task.CompletedTask;
        });

        const int Senders = 16;
        const int PerSender = 50;
        // Variable-length padding makes frames of different sizes, so an
        // interleaved write can't accidentally line up on frame boundaries.
        var senders = Enumerable.Range(0, Senders).Select(s => Task.Run(async () =>
        {
            for (var i = 0; i < PerSender; i++)
            {
                var envelope = new RemoteEnvelope(
                    TargetPath: "sink",
                    Message: new Tagged(s, i, new string('x', 1 + (s * 31 + i * 7) % 512)),
                    SenderPath: null,
                    SenderNode: null);
                await a.SendAsync(b.LocalNode, envelope);
            }
        })).ToArray();

        await Task.WhenAll(senders);

        // Every frame must arrive whole: right count, every (sender, seq)
        // pair exactly once, padding uncorrupted. A torn frame shows up as
        // a deserialization failure (receive loop dies), a missing pair, or
        // mangled padding.
        var deadline = DateTime.UtcNow + TimeSpan.FromSeconds(20);
        while (received.Count < Senders * PerSender && DateTime.UtcNow < deadline)
            await Task.Delay(20);

        Assert.Equal(Senders * PerSender, received.Count);
        var seen = new HashSet<(int, int)>();
        foreach (var m in received)
        {
            Assert.True(seen.Add((m.Sender, m.Seq)), $"duplicate ({m.Sender},{m.Seq})");
            Assert.Equal(new string('x', 1 + (m.Sender * 31 + m.Seq * 7) % 512), m.Padding);
        }
    }
}
