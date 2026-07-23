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
    /// </summary>
    public static void Write(
        PipeWriter writer,
        RemoteEnvelope envelope,
        string messageTypeName,
        ISpekSerializer serializer)
    {
        // Serialize payload to its own buffer first so we know its length.
        var payloadBuf = new ArrayBufferWriter<byte>();
        serializer.Serialize(envelope.Message, payloadBuf);
        var payload = payloadBuf.WrittenSpan;

        var typeBytes        = Encoding.UTF8.GetBytes(messageTypeName);
        var targetPathBytes  = Encoding.UTF8.GetBytes(envelope.TargetPath);
        var senderPathBytes  = envelope.SenderPath is null
            ? Array.Empty<byte>()
            : Encoding.UTF8.GetBytes(envelope.SenderPath);
        var senderLabelBytes = envelope.SenderNode?.Label is null
            ? Array.Empty<byte>()
            : Encoding.UTF8.GetBytes(envelope.SenderNode.Label);

        var senderUuidBytes = envelope.SenderNode is null
            ? new byte[16]                          // all zeros
            : envelope.SenderNode.Id.ToByteArray();

        // Calculate total length (excluding the 4-byte length prefix).
        int innerLength =
            1                                       // version
            + 4 + typeBytes.Length
            + 4 + targetPathBytes.Length
            + 4 + senderPathBytes.Length
            + 16
            + 4 + senderLabelBytes.Length
            + 4 + payload.Length;

        var span = writer.GetSpan(4 + innerLength);
        BinaryPrimitives.WriteInt32BigEndian(span, innerLength);
        int o = 4;
        span[o++] = Version;

        WriteLenString(span, ref o, typeBytes);
        WriteLenString(span, ref o, targetPathBytes);
        WriteLenString(span, ref o, senderPathBytes);

        senderUuidBytes.AsSpan().CopyTo(span[o..]);
        o += 16;

        WriteLenString(span, ref o, senderLabelBytes);
        WriteLenString(span, ref o, payload);

        writer.Advance(4 + innerLength);
    }

    private static void WriteLenString(Span<byte> dest, ref int offset, ReadOnlySpan<byte> data)
    {
        BinaryPrimitives.WriteInt32BigEndian(dest[offset..], data.Length);
        offset += 4;
        if (data.Length > 0)
        {
            data.CopyTo(dest[offset..]);
            offset += data.Length;
        }
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
