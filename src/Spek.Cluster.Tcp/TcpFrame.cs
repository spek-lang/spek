using System.Buffers;
using System.Buffers.Binary;
using System.IO.Pipelines;
using System.Text;
using System.Text.Json;

namespace Spek.Cluster.Tcp;

/// <summary>
/// Wire-frame layout for <see cref="TcpClusterTransport"/>:
///
/// <code>
/// [4-byte BE length-of-rest]
/// [1-byte version (0x01)]
/// [4-byte BE typeName length][typeName UTF-8]
/// [4-byte BE targetPath length][targetPath UTF-8]
/// [4-byte BE senderPath length][senderPath UTF-8]   (length 0 = no sender)
/// [16-byte SenderNode UUID]                          (zeros = no sender node)
/// [4-byte BE senderLabel length][senderLabel UTF-8]  (length 0 = no label)
/// [4-byte BE payload length][payload bytes]
/// </code>
///
/// The 4-byte BE length prefix at the start lets a reader pull a complete
/// frame out of the underlying stream before any other parsing — standard
/// length-prefixed binary framing.
/// </summary>
internal static class TcpFrame
{
    private const byte Version = 0x01;

    /// <summary>
    /// Serialize an envelope into the frame format and write it to the pipe.
    /// <paramref name="payloadScratch"/> stages the payload so its length is
    /// known before the prefix is written; the caller owns it (per
    /// connection, reused under the connection's send gate — perf r15: a
    /// fresh buffer per frame was a large slice of the boundary's
    /// allocation). Strings encode straight into the frame span via
    /// GetByteCount/GetBytes(Span) — no intermediate arrays; the wire bytes
    /// are unchanged.
    /// </summary>
    public static void Write(
        PipeWriter writer,
        RemoteEnvelope envelope,
        string messageTypeName,
        ISpekSerializer serializer,
        ArrayBufferWriter<byte> payloadScratch)
    {
        payloadScratch.ResetWrittenCount();
        serializer.Serialize(envelope.Message, payloadScratch);
        var payload = payloadScratch.WrittenSpan;

        var senderPath  = envelope.SenderPath;
        var senderLabel = envelope.SenderNode?.Label;

        int typeLen        = Encoding.UTF8.GetByteCount(messageTypeName);
        int targetPathLen  = Encoding.UTF8.GetByteCount(envelope.TargetPath);
        int senderPathLen  = senderPath is null ? 0 : Encoding.UTF8.GetByteCount(senderPath);
        int senderLabelLen = senderLabel is null ? 0 : Encoding.UTF8.GetByteCount(senderLabel);

        // Calculate total length (excluding the 4-byte length prefix).
        int innerLength =
            1                                       // version
            + 4 + typeLen
            + 4 + targetPathLen
            + 4 + senderPathLen
            + 16
            + 4 + senderLabelLen
            + 4 + payload.Length;

        var span = writer.GetSpan(4 + innerLength);
        BinaryPrimitives.WriteInt32BigEndian(span, innerLength);
        int o = 4;
        span[o++] = Version;

        WriteLenString(span, ref o, messageTypeName, typeLen);
        WriteLenString(span, ref o, envelope.TargetPath, targetPathLen);
        WriteLenString(span, ref o, senderPath, senderPathLen);

        if (envelope.SenderNode is { } node)
            node.Id.TryWriteBytes(span.Slice(o, 16));   // same layout as ToByteArray
        else
            span.Slice(o, 16).Clear();                  // all zeros = no sender node
        o += 16;

        WriteLenString(span, ref o, senderLabel, senderLabelLen);

        BinaryPrimitives.WriteInt32BigEndian(span[o..], payload.Length);
        o += 4;
        payload.CopyTo(span[o..]);
        o += payload.Length;

        writer.Advance(4 + innerLength);
    }

    private static void WriteLenString(Span<byte> dest, ref int offset, string? text, int byteLen)
    {
        BinaryPrimitives.WriteInt32BigEndian(dest[offset..], byteLen);
        offset += 4;
        if (byteLen > 0)
            offset += Encoding.UTF8.GetBytes(text!, dest[offset..]);
    }

    /// <summary>
    /// Tries to read one complete frame from the buffer. Returns
    /// <c>true</c> + the parsed parts if a full frame is available;
    /// otherwise <c>false</c> and the buffer is left untouched (caller
    /// should keep accumulating).
    /// </summary>
    public static bool TryRead(
        ref ReadOnlySequence<byte> buffer,
        out string typeName,
        out string targetPath,
        out string? senderPath,
        out Guid senderNodeId,
        out string? senderLabel,
        out ReadOnlySequence<byte> payload)
    {
        typeName = ""; targetPath = ""; senderPath = null;
        senderNodeId = Guid.Empty; senderLabel = null;
        payload = default;

        if (buffer.Length < 4) return false;

        var reader = new SequenceReader<byte>(buffer);
        if (!reader.TryReadBigEndian(out int innerLen)) return false;
        if (buffer.Length < 4 + innerLen) return false;

        if (!reader.TryRead(out byte version))     return false;
        if (version != Version)
            throw new InvalidDataException(
                $"Unsupported frame version 0x{version:X2} (this build speaks 0x{Version:X2}).");

        typeName    = ReadLenString(ref reader);
        targetPath  = ReadLenString(ref reader);
        senderPath  = ReadLenStringOrNull(ref reader);

        Span<byte> uuid = stackalloc byte[16];
        if (!reader.TryCopyTo(uuid)) return false;
        reader.Advance(16);
        senderNodeId = new Guid(uuid);

        senderLabel = ReadLenStringOrNull(ref reader);

        if (!reader.TryReadBigEndian(out int payloadLen)) return false;
        if (reader.Remaining < payloadLen) return false;
        payload = reader.UnreadSequence.Slice(0, payloadLen);

        // Advance the outer buffer past the consumed frame.
        buffer = buffer.Slice(4 + innerLen);
        return true;
    }

    private static string ReadLenString(ref SequenceReader<byte> reader)
    {
        if (!reader.TryReadBigEndian(out int len))
            throw new InvalidDataException("Truncated frame: missing length prefix.");
        if (len < 0)
            throw new InvalidDataException("Negative length prefix.");
        if (len == 0) return string.Empty;
        Span<byte> buf = len <= 256 ? stackalloc byte[len] : new byte[len];
        if (!reader.TryCopyTo(buf))
            throw new InvalidDataException("Truncated frame: declared string longer than remaining bytes.");
        reader.Advance(len);
        return Encoding.UTF8.GetString(buf);
    }

    private static string? ReadLenStringOrNull(ref SequenceReader<byte> reader)
    {
        var s = ReadLenString(ref reader);
        return s.Length == 0 ? null : s;
    }
}
