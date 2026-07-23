using System.Buffers;

namespace Spek.Cluster;

/// <summary>
/// Pluggable message serialization for wire transports. In-memory
/// transports skip serialization entirely (passing objects through);
/// wire transports (TCP, QUIC, gRPC) ask their configured serializer
/// to convert each <see cref="RemoteEnvelope.Message"/> to bytes on
/// send and back on receive.
///
/// The default serializer is <c>JsonSpekSerializer</c>
/// (shipped in <c>Spek.Cluster.Tcp</c>); cross-language clusters can
/// opt into a compact binary serializer (MemoryPack/MessagePack) once shipped.
/// </summary>
public interface ISpekSerializer
{
    /// <summary>Serialize the message + the (already-resolved) type
    /// identifier into the supplied buffer writer.</summary>
    void Serialize(object message, IBufferWriter<byte> writer);

    /// <summary>Deserialize a message of the named type from the
    /// supplied byte sequence. The type name is the type's
    /// fully-qualified .NET name as resolved on the receiver
    /// (across-process clusters need matching message assemblies).
    /// </summary>
    object Deserialize(string typeName, ReadOnlySequence<byte> bytes);
}
