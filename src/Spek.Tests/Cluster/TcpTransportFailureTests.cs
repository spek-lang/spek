using System.Buffers;
using System.IO.Pipelines;
using System.Net;
using System.Net.Sockets;
using Spek.Cluster;
using Spek.Cluster.Tcp;
using Xunit;

namespace Spek.Tests.ClusterIntegration.Tcp;

/// <summary>
/// Failure-mode tests for the production TCP wire path
/// (<see cref="TcpClusterTransport"/> + <see cref="TcpFrame"/>) — the
/// parts the happy-path integration tests in
/// <c>TcpClusterTransportTests</c> never exercise:
///
/// <list type="bullet">
///   <item>send-to-never-connected-peer surfacing via the
///         <see cref="ISpekTransport.DeliveryFailed"/> event rather
///         than a throw (the contract's fire-and-forget guarantee);</item>
///   <item><see cref="TcpFrame.TryRead"/> behaviour under fragmentation,
///         version mismatch, and the null-sender-node sentinel;</item>
///   <item>half-open / truncated inbound handshakes being swallowed
///         without poisoning the listener.</item>
/// </list>
///
/// All sockets are real loopback sockets bound to an OS-assigned
/// ephemeral port (<c>port: 0</c>); every transport is disposed in a
/// <c>finally</c> and every wait is bounded so a wiring bug cannot hang
/// the runner.
/// </summary>
public class TcpTransportFailureTests
{
    public sealed record Ping(string Note);

    private static TcpClusterOptions LoopbackOpts(string label) => new()
    {
        Label          = label,
        ListenEndpoint = new IPEndPoint(IPAddress.Loopback, 0),
        LoopbackOnly   = true,
    };

    // Pins the documented security status: ClusterSharedKey is NOT yet
    // enforced — setting it must fail LOUD (a stderr warning), not silently imply
    // peer authentication. When mTLS / shared-secret enforcement ships,
    // this test flips to assert the key is actually validated at handshake.
    [Fact]
    public async Task ClusterSharedKey_IsNotYetEnforced_AndWarnsLoudlyWhenSet()
    {
        var opts = LoopbackOpts("with-key");
        opts.ClusterSharedKey = "a-secret-that-does-nothing-yet";

        var original = Console.Error;
        var captured = new StringWriter();
        Console.SetError(captured);
        TcpClusterTransport? transport = null;
        try
        {
            transport = new TcpClusterTransport(opts);   // warning fires in the ctor
        }
        finally
        {
            Console.SetError(original);
            if (transport is not null) await transport.DisposeAsync();
        }

        Assert.Contains("NOT YET ENFORCED", captured.ToString(), StringComparison.OrdinalIgnoreCase);
    }

    // ----------------------------------------------------------------
    // Scenario 1 — DeliveryFailed fires for an unconnected peer, and
    // SendAsync itself never throws (ISpekTransport's documented
    // "failures surface via DeliveryFailed rather than throwing").
    // ----------------------------------------------------------------

    [Fact]
    public async Task SendAsync_ToNeverConnectedPeer_RaisesDeliveryFailed_AndDoesNotThrow()
    {
        var transport = new TcpClusterTransport(LoopbackOpts("lonely"));
        try
        {
            NodeIdentity?    failedTarget = null;
            RemoteEnvelope?  failedEnvelope = null;
            Exception?       failedException = null;
            var fired = new TaskCompletionSource<bool>(TaskCreationOptions.RunContinuationsAsynchronously);

            transport.DeliveryFailed += (target, envelope, ex) =>
            {
                failedTarget    = target;
                failedEnvelope  = envelope;
                failedException = ex;
                fired.TrySetResult(true);
            };

            // A peer we have never called ConnectToPeerAsync for — there
            // is no outbound connection registered under its Id.
            var ghost    = new NodeIdentity(Guid.NewGuid(), "ghost");
            var envelope = new RemoteEnvelope("some/actor", new Ping("hi"));

            // SendAsync must complete without throwing...
            var sendTask = transport.SendAsync(ghost, envelope);
            await sendTask;            // would rethrow had it faulted
            Assert.True(sendTask.IsCompletedSuccessfully);

            // ...and DeliveryFailed must have fired synchronously inside it.
            var raised = await Task.WhenAny(fired.Task, Task.Delay(TimeSpan.FromSeconds(5)));
            Assert.True(ReferenceEquals(raised, fired.Task) && fired.Task.Result,
                "DeliveryFailed did not fire for a send to an unconnected peer.");

            Assert.Equal(ghost.Id, failedTarget!.Id);
            Assert.Same(envelope, failedEnvelope);
            Assert.NotNull(failedException);
        }
        finally
        {
            await transport.DisposeAsync();
        }
    }

    // ----------------------------------------------------------------
    // Scenario 2 — TcpFrame round-trip / fragmentation / version / null
    // sender node.  Uses TcpFrame directly (InternalsVisibleTo grants
    // Spek.Tests access to the internal static class).
    // ----------------------------------------------------------------

    /// <summary>
    /// Serialize one envelope to its on-the-wire bytes by driving the
    /// real <see cref="TcpFrame.Write"/> into a <see cref="Pipe"/> and
    /// pulling the buffered bytes back out — exactly the bytes a peer
    /// socket would receive.
    /// </summary>
    private static byte[] FrameBytes(RemoteEnvelope envelope)
    {
        var pipe = new Pipe();
        TcpFrame.Write(pipe.Writer, envelope, envelope.Message.GetType().FullName!, new JsonSpekSerializer());
        pipe.Writer.FlushAsync().AsTask().GetAwaiter().GetResult();
        pipe.Writer.Complete();

        if (!pipe.Reader.TryRead(out var result))
            throw new InvalidOperationException("Pipe produced no buffered frame bytes.");
        var bytes = result.Buffer.ToArray();
        pipe.Reader.AdvanceTo(result.Buffer.End);
        pipe.Reader.Complete();
        return bytes;
    }

    private static bool TryReadFrame(
        ReadOnlySequence<byte> seq,
        out ReadOnlySequence<byte> remaining,
        out string typeName,
        out string targetPath,
        out string? senderPath,
        out Guid senderNodeId,
        out string? senderLabel,
        out byte[] payloadBytes)
    {
        var buffer = seq;
        var ok = TcpFrame.TryRead(
            ref buffer,
            out typeName,
            out targetPath,
            out senderPath,
            out senderNodeId,
            out senderLabel,
            out var payload);
        remaining    = buffer;
        payloadBytes = ok ? payload.ToArray() : Array.Empty<byte>();
        return ok;
    }

    [Fact]
    public void TryRead_FragmentedFrame_OneByteShort_ReturnsFalseAndConsumesNothing_ThenSucceeds()
    {
        var sender   = new NodeIdentity(Guid.NewGuid(), "sender-label");
        var envelope = new RemoteEnvelope("target/path", new Ping("fragment-me"), "sender/path", sender);
        var full     = FrameBytes(envelope);
        Assert.True(full.Length > 1);

        // Feed all but the final byte: a partial frame.
        var shortSeq = new ReadOnlySequence<byte>(full, 0, full.Length - 1);
        var ok = TryReadFrame(shortSeq, out var remaining, out _, out _, out _, out _, out _, out _);

        Assert.False(ok);
        // The buffer must be left untouched (caller keeps accumulating).
        Assert.Equal(shortSeq.Length, remaining.Length);

        // Now feed the complete frame: it must parse, and every field
        // must round-trip.
        var fullSeq = new ReadOnlySequence<byte>(full);
        var ok2 = TryReadFrame(fullSeq, out var leftover, out var typeName, out var targetPath,
                               out var senderPath, out var senderNodeId, out var senderLabel,
                               out var payload);

        Assert.True(ok2);
        Assert.Equal(typeof(Ping).FullName, typeName);
        Assert.Equal("target/path", targetPath);
        Assert.Equal("sender/path", senderPath);
        Assert.Equal(sender.Id, senderNodeId);
        Assert.Equal("sender-label", senderLabel);
        // Exactly one frame's worth was consumed.
        Assert.Equal(0, leftover.Length);

        // Payload deserializes back to the original message.
        var roundTripped = new JsonSpekSerializer().Deserialize(typeName, new ReadOnlySequence<byte>(payload));
        var ping = Assert.IsType<Ping>(roundTripped);
        Assert.Equal("fragment-me", ping.Note);
    }

    [Fact]
    public void TryRead_VersionMismatch_ThrowsInvalidDataException()
    {
        var envelope = new RemoteEnvelope("t", new Ping("v"));
        var full     = FrameBytes(envelope);

        // The frame layout is [4-byte BE inner length][1-byte version]...
        // so the version byte sits at index 4. Corrupt it to an
        // unsupported value while keeping the length prefix intact, so
        // TryRead reaches the version check (rather than bailing early
        // on an incomplete buffer).
        var corrupt = (byte[])full.Clone();
        Assert.Equal(0x01, corrupt[4]);     // sanity: real version is 0x01
        corrupt[4] = 0x02;

        var seq = new ReadOnlySequence<byte>(corrupt);
        var ex = Assert.Throws<InvalidDataException>(() =>
            TryReadFrame(seq, out _, out _, out _, out _, out _, out _, out _));
        Assert.Contains("0x02", ex.Message);
    }

    [Fact]
    public void TryRead_NullSenderNode_RoundTripsToGuidEmpty()
    {
        // No SenderNode => Write stamps an all-zero 16-byte UUID; TryRead
        // must surface that as Guid.Empty (and senderLabel as null).
        var envelope = new RemoteEnvelope("target", new Ping("anon"), SenderPath: null, SenderNode: null);
        var full     = FrameBytes(envelope);

        var seq = new ReadOnlySequence<byte>(full);
        var ok = TryReadFrame(seq, out _, out var typeName, out var targetPath,
                              out var senderPath, out var senderNodeId, out var senderLabel,
                              out var payload);

        Assert.True(ok);
        Assert.Equal(typeof(Ping).FullName, typeName);
        Assert.Equal("target", targetPath);
        Assert.Null(senderPath);
        Assert.Equal(Guid.Empty, senderNodeId);
        Assert.Null(senderLabel);

        var ping = Assert.IsType<Ping>(
            new JsonSpekSerializer().Deserialize(typeName, new ReadOnlySequence<byte>(payload)));
        Assert.Equal("anon", ping.Note);
    }

    // ----------------------------------------------------------------
    // Scenario 3 — Half-open / truncated inbound handshake: the accept
    // loop must swallow a peer that connects, sends fewer than the
    // 20-byte handshake, and closes — and must still accept a
    // subsequent well-formed ConnectToPeerAsync.
    // ----------------------------------------------------------------

    [Fact]
    public async Task TruncatedInboundHandshake_IsSwallowed_AndLaterValidConnectStillSucceeds()
    {
        // The receiving transport whose accept loop we are stressing.
        var receiver = new TcpClusterTransport(LoopbackOpts("receiver"));
        // A second transport that performs a *legitimate* connect later.
        var dialer   = new TcpClusterTransport(LoopbackOpts("dialer"));
        try
        {
            // 1) Open a raw socket, send a partial handshake (< 20 bytes),
            //    then slam it shut. HandleInboundConnectionAsync ->
            //    ReadHandshakeAsync sees IsCompleted with too few bytes and
            //    throws InvalidOperationException, which the catch-all in
            //    HandleInboundConnectionAsync swallows.
            using (var raw = new TcpClient())
            {
                await raw.ConnectAsync(receiver.BoundEndpoint);
                var ns = raw.GetStream();
                // 8 bytes — well under the 16-byte UUID + 4-byte len = 20.
                await ns.WriteAsync(new byte[] { 1, 2, 3, 4, 5, 6, 7, 8 });
                await ns.FlushAsync();
                // closing scope disposes the client -> half-open close.
            }

            // Give the receiver's connection task a beat to run its
            // doomed handshake read and swallow the error. (Bounded; we
            // don't assert on this delay — the real assertion is that the
            // valid connect below still works.)
            await Task.Delay(150);

            // 2) The accept loop must still be alive: a proper connect
            //    from the dialer to the receiver must complete the mutual
            //    handshake and register an outbound connection.
            using var connectCts = new CancellationTokenSource(TimeSpan.FromSeconds(10));
            await dialer.ConnectToPeerAsync(receiver.BoundEndpoint, receiver.LocalNode, connectCts.Token);

            // 3) And a real send over that connection must not raise
            //    DeliveryFailed (proves the post-handshake frame loop on
            //    the receiver is healthy, not just the accept loop).
            var failed = new TaskCompletionSource<Exception>(TaskCreationOptions.RunContinuationsAsynchronously);
            dialer.DeliveryFailed += (_, _, ex) => failed.TrySetResult(ex);

            await dialer.SendAsync(receiver.LocalNode, new RemoteEnvelope("anywhere", new Ping("post-recovery")));

            // If DeliveryFailed fires within a short window the connection
            // was not actually healthy. Bounded wait, then assert clean.
            var settled = await Task.WhenAny(failed.Task, Task.Delay(TimeSpan.FromMilliseconds(500)));
            Assert.False(ReferenceEquals(settled, failed.Task),
                "A send over the recovered connection raised DeliveryFailed: " +
                (failed.Task.IsCompleted ? failed.Task.Result.ToString() : "<none>"));
        }
        finally
        {
            await dialer.DisposeAsync();
            await receiver.DisposeAsync();
        }
    }
}

