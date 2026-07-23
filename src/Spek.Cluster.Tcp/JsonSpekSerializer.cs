using System.Buffers;
using System.Text.Json;

namespace Spek.Cluster.Tcp;

/// <summary>
/// Default <see cref="ISpekSerializer"/> implementation — uses
/// <c>System.Text.Json</c>, which works on any Spek-emitted record
/// without code-generation annotations. Slower than MemoryPack /
/// MessagePack, faster than Newtonsoft.Json. Ships as the default
/// because it's zero-friction.
///
/// MemoryPack swap-in is planned once the emitter learns to
/// stamp <c>[MemoryPackable]</c> on generated message records.
/// MessagePack lands later as a cross-language opt-in.
/// </summary>
public sealed class JsonSpekSerializer : ISpekSerializer
{
    private static readonly JsonSerializerOptions Options = new()
    {
        IncludeFields = true,                  // record positional ctor → fields
        PropertyNameCaseInsensitive = true,
    };

    public void Serialize(object message, IBufferWriter<byte> writer)
    {
        using var jw = new Utf8JsonWriter(writer);
        JsonSerializer.Serialize(jw, message, message.GetType(), Options);
    }

    public object Deserialize(string typeName, ReadOnlySequence<byte> bytes)
    {
        var type = Type.GetType(typeName, throwOnError: false)
                   ?? ResolveAcrossLoadedAssemblies(typeName)
                   ?? throw new InvalidOperationException(
                       $"Could not resolve message type '{typeName}'. " +
                       $"Ensure the receiving process references the assembly that " +
                       $"declares this message.");

        var reader = new Utf8JsonReader(bytes);
        return JsonSerializer.Deserialize(ref reader, type, Options)
               ?? throw new InvalidOperationException($"Deserialization of '{typeName}' returned null.");
    }

    /// <summary>
    /// Fallback for the common case where <c>Type.GetType</c> can't find
    /// the type because it's in an assembly the runtime hasn't probed yet.
    /// Linear scan of loaded assemblies — slow but only fires on cache
    /// miss; the AssemblyQualifiedName fast path covers most messages.
    /// </summary>
    private static Type? ResolveAcrossLoadedAssemblies(string typeName)
    {
        foreach (var asm in AppDomain.CurrentDomain.GetAssemblies())
        {
            var t = asm.GetType(typeName, throwOnError: false);
            if (t is not null) return t;
        }
        return null;
    }
}
