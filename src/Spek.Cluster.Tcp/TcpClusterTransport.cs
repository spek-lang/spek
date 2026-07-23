using System.Buffers;
using System.Buffers.Binary;
using System.Collections.Concurrent;
using System.IO.Pipelines;
using System.Net;
using System.Net.Sockets;
using System.Text;

namespace Spek.Cluster.Tcp;

/// <summary>
/// Default Spek cluster transport — Spek-native binary protocol over TCP.
/// Bedrock-aligned in shape (it implements the same
/// <see cref="ISpekTransport"/> contract the in-memory transport does)
/// but uses raw <see cref="Socket"/> + <see cref="System.IO.Pipelines"/>
/// rather than going through <c>Microsoft.AspNetCore.Connections.Abstractions</c>'s
/// <c>IConnectionListener</c> indirection. The same wire format would
/// drop into a Kestrel-hosted listener without changes.
///
/// Ships today:
/// <list type="bullet">
///   <item>Length-prefixed binary framing.</item>
///   <item>JSON serialization of payloads (<see cref="JsonSpekSerializer"/>).</item>
///   <item>Mutual identity handshake at connection setup.</item>
///   <item>Single connection per peer pair, lazy connection.</item>
/// </list>
///
/// Not yet implemented:
/// <list type="bullet">
///   <item>TLS / mTLS via <c>SslStream</c> middleware.</item>
///   <item>Cluster-shared-secret authentication.</item>
///   <item>Connection retry / reconnect on transient failure.</item>
///   <item>Multiple concurrent connections per peer (keepalive pool).</item>
///   <item>SWIM heartbeats / failure detection.</item>
/// </list>
/// </summary>
public sealed class TcpClusterTransport : ISpekTransport
{
    private readonly TcpClusterOptions _options;
    private readonly ISpekSerializer _serializer;
    private readonly TcpListener _listener;
    private readonly ConcurrentDictionary<Guid, OutboundConnection> _outbound = new();
    private readonly CancellationTokenSource _shutdown = new();
    private Func<RemoteEnvelope, Task>? _handler;

    public TcpClusterTransport(TcpClusterOptions options, ISpekSerializer? serializer = null)
    {
        _options    = options    ?? throw new ArgumentNullException(nameof(options));
        _serializer = serializer ?? new JsonSpekSerializer();

        // Fail loud, not silent: ClusterSharedKey looks like peer authentication
        // but isn't enforced yet (the handshake doesn't check it, and the wire is
        // unencrypted). Don't let an operator believe they're secured. mTLS +
        // shared-secret enforcement are planned but not built yet.
        if (options.ClusterSharedKey is not null)
        {
            Console.Error.WriteLine(
                "WARN: TcpClusterOptions.ClusterSharedKey is set but NOT YET ENFORCED — " +
                "peers are not authenticated and the cluster wire is unencrypted. " +
                "Use LoopbackOnly and/or network isolation until mTLS ships.");
        }

        LocalNode  = new NodeIdentity(
            Guid.NewGuid(),
            options.Label ?? Environment.MachineName);

        _listener  = new TcpListener(options.ListenEndpoint);
        _listener.Start();

        // Spawn the accept loop on the thread pool so the constructor
        // returns immediately. The loop is cooperatively cancellable
        // via _shutdown.
        _ = Task.Run(AcceptLoopAsync);
    }

    public NodeIdentity LocalNode { get; }

    /// <summary>The local endpoint we're actually bound to. Useful for
    /// tests that pass <c>port: 0</c> to let the OS pick a free port —
    /// after construction this property reveals the chosen port.</summary>
    public IPEndPoint BoundEndpoint => (IPEndPoint)_listener.LocalEndpoint;

    public event Action<NodeIdentity, RemoteEnvelope, Exception>? DeliveryFailed;

    public async Task SendAsync(NodeIdentity target, RemoteEnvelope envelope, CancellationToken cancellationToken = default)
    {
        if (_shutdown.IsCancellationRequested) return;

        // Find or open the outbound connection to this peer.
        if (!_outbound.TryGetValue(target.Id, out var conn))
        {
            DeliveryFailed?.Invoke(target, envelope, new InvalidOperationException(
                $"No outbound connection registered for node {target}. Call ConnectToPeer() first."));
            return;
        }

        // One frame at a time per connection: PipeWriter is not safe for
        // concurrent writers, and before this gate two actors sending to the
        // same peer from different threads could interleave GetSpan/Advance
        // and corrupt the wire. The gate also makes the connection's frame
        // scratch buffer safely reusable (perf r15). Flush faults are routed
        // to DeliveryFailed like every other send failure, per the
        // ISpekTransport contract.
        await conn.SendGate.WaitAsync(cancellationToken).ConfigureAwait(false);
        try
        {
            TcpFrame.Write(conn.Writer, envelope, envelope.Message.GetType().FullName!,
                _serializer, conn.FrameScratch);
            await conn.Writer.FlushAsync(cancellationToken).ConfigureAwait(false);
        }
        catch (Exception ex)
        {
            DeliveryFailed?.Invoke(target, envelope, ex);
        }
        finally
        {
            conn.SendGate.Release();
        }
    }

    public void SetReceiveHandler(Func<RemoteEnvelope, Task> handler) =>
        _handler = handler ?? throw new ArgumentNullException(nameof(handler));

    /// <summary>
    /// Open an outbound connection to a peer at <paramref name="endpoint"/>.
    /// Performs the identity handshake; if the peer's reported
    /// <see cref="NodeIdentity"/> doesn't match
    /// <paramref name="expectedIdentity"/>, the connection is dropped
    /// (defensive against accidental cross-cluster connections).
    ///
    /// Idempotent — calling twice with the same target is a no-op.
    /// </summary>
    public async Task ConnectToPeerAsync(IPEndPoint endpoint, NodeIdentity expectedIdentity, CancellationToken cancellationToken = default)
    {
        if (_outbound.ContainsKey(expectedIdentity.Id)) return;

        var client = new TcpClient();
        await client.ConnectAsync(endpoint, cancellationToken).ConfigureAwait(false);

        var stream = client.GetStream();
        var pipe = PipeReader.Create(stream);
        var writer = PipeWriter.Create(stream);

        // Handshake: send our identity, read theirs, verify.
        await SendHandshakeAsync(writer, cancellationToken).ConfigureAwait(false);
        var their = await ReadHandshakeAsync(pipe, cancellationToken).ConfigureAwait(false);

        if (their.Id != expectedIdentity.Id)
        {
            client.Dispose();
            throw new InvalidOperationException(
                $"Peer at {endpoint} reported identity {their} but caller expected {expectedIdentity}. " +
                $"Likely cross-cluster misconfiguration.");
        }

        var conn = new OutboundConnection(client, writer, pipe);
        _outbound[expectedIdentity.Id] = conn;
    }

    /// <summary>Inbound accept loop — runs for the transport's lifetime.</summary>
    private async Task AcceptLoopAsync()
    {
        while (!_shutdown.IsCancellationRequested)
        {
            TcpClient client;
            try
            {
                client = await _listener.AcceptTcpClientAsync(_shutdown.Token).ConfigureAwait(false);
            }
            catch (OperationCanceledException) { break; }
            catch (ObjectDisposedException)    { break; }

            // Optional loopback-only enforcement.
            if (_options.LoopbackOnly &&
                client.Client.RemoteEndPoint is IPEndPoint rep &&
                !IPAddress.IsLoopback(rep.Address))
            {
                client.Dispose();
                continue;
            }

            // Run each connection's read loop on the thread pool so the
            // accept loop stays free to take more connections.
            _ = Task.Run(() => HandleInboundConnectionAsync(client));
        }
    }

    private async Task HandleInboundConnectionAsync(TcpClient client)
    {
        try
        {
            using (client)
            {
                var stream = client.GetStream();
                var reader = PipeReader.Create(stream);
                var writer = PipeWriter.Create(stream);

                // Handshake: read peer's identity, send ours.
                _ = await ReadHandshakeAsync(reader, _shutdown.Token).ConfigureAwait(false);
                await SendHandshakeAsync(writer, _shutdown.Token).ConfigureAwait(false);

                // Frame loop: keep pulling complete frames out of the
                // reader and dispatching to the configured handler.
                while (!_shutdown.IsCancellationRequested)
                {
                    var result = await reader.ReadAsync(_shutdown.Token).ConfigureAwait(false);
                    var buffer = result.Buffer;

                    while (TcpFrame.TryRead(ref buffer,
                                            out var typeName,
                                            out var targetPath,
                                            out var senderPath,
                                            out var senderNodeId,
                                            out var senderLabel,
                                            out var payload))
                    {
                        var message = _serializer.Deserialize(typeName, payload);

                        NodeIdentity? senderNode = senderNodeId == Guid.Empty
                            ? null
                            : new NodeIdentity(senderNodeId, senderLabel);

                        var envelope = new RemoteEnvelope(targetPath, message, senderPath, senderNode);
                        if (_handler is not null) await _handler(envelope).ConfigureAwait(false);
                    }

                    reader.AdvanceTo(buffer.Start, buffer.End);
                    if (result.IsCompleted) break;
                }
            }
        }
        catch (OperationCanceledException) { /* shutdown */ }
        catch (Exception)
        {
            // Swallow — transient peer errors shouldn't bring the
            // transport down. A later release will surface these via
            // the failure detector.
        }
    }

    /// <summary>
    /// Handshake frame: <c>[16-byte UUID][4-byte BE label-len][label UTF-8]</c>.
    /// Simpler than the message frame; no version byte (the message
    /// frame's version covers protocol changes here too).
    /// </summary>
    private async Task SendHandshakeAsync(PipeWriter writer, CancellationToken ct)
    {
        var label = LocalNode.Label ?? "";
        var labelBytes = Encoding.UTF8.GetBytes(label);
        var span = writer.GetSpan(16 + 4 + labelBytes.Length);

        LocalNode.Id.TryWriteBytes(span);
        BinaryPrimitives.WriteInt32BigEndian(span[16..], labelBytes.Length);
        labelBytes.AsSpan().CopyTo(span[20..]);
        writer.Advance(16 + 4 + labelBytes.Length);

        await writer.FlushAsync(ct).ConfigureAwait(false);
    }

    private async Task<NodeIdentity> ReadHandshakeAsync(PipeReader reader, CancellationToken ct)
    {
        while (true)
        {
            var result = await reader.ReadAsync(ct).ConfigureAwait(false);
            var buf = result.Buffer;

            if (buf.Length < 20)
            {
                if (result.IsCompleted)
                    throw new InvalidOperationException("Peer closed connection before completing handshake.");
                reader.AdvanceTo(buf.Start, buf.End);
                continue;
            }

            var sr = new SequenceReader<byte>(buf);
            Span<byte> uuid = stackalloc byte[16];
            sr.TryCopyTo(uuid);
            sr.Advance(16);
            sr.TryReadBigEndian(out int labelLen);

            if (sr.Remaining < labelLen)
            {
                reader.AdvanceTo(buf.Start, buf.End);
                continue;
            }

            Span<byte> labelBuf = labelLen <= 256 ? stackalloc byte[labelLen] : new byte[labelLen];
            if (labelLen > 0)
            {
                sr.TryCopyTo(labelBuf);
                sr.Advance(labelLen);
            }

            var consumed = buf.GetPosition(20 + labelLen);
            reader.AdvanceTo(consumed);

            var id    = new Guid(uuid);
            var label = labelLen == 0 ? null : Encoding.UTF8.GetString(labelBuf);
            return new NodeIdentity(id, label);
        }
    }

    public async ValueTask DisposeAsync()
    {
        _shutdown.Cancel();
        _listener.Stop();
        foreach (var conn in _outbound.Values) conn.Dispose();
        _outbound.Clear();
        _shutdown.Dispose();
        await Task.CompletedTask;
    }

    private sealed class OutboundConnection : IDisposable
    {
        public TcpClient Client { get; }
        public PipeWriter Writer { get; }
        public PipeReader Reader { get; }

        /// <summary>Serializes frame writes — PipeWriter is single-writer.</summary>
        public SemaphoreSlim SendGate { get; } = new(1, 1);

        /// <summary>Payload staging buffer, reused across frames under
        /// <see cref="SendGate"/> (perf r15).</summary>
        public ArrayBufferWriter<byte> FrameScratch { get; } = new(4096);

        public OutboundConnection(TcpClient client, PipeWriter writer, PipeReader reader)
        {
            Client = client;
            Writer = writer;
            Reader = reader;
        }

        public void Dispose()
        {
            try { Writer.Complete(); } catch { /* ignore */ }
            try { Reader.Complete(); } catch { /* ignore */ }
            Client.Dispose();
            SendGate.Dispose();
        }
    }
}
