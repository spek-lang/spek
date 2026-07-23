using System.Buffers;
using System.Collections.Concurrent;
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

    // Wire-name → Type cache (perf r13). The wire carries the plain
    // FullName, and Type.GetType(fullName) probes only mscorlib and this
    // assembly — user message types miss EVERY time, so before the cache
    // each inbound message paid a linear scan of every loaded assembly.
    // A cluster's message vocabulary is small and fixed, so the cache
    // saturates within the first message of each type. Failed lookups are
    // deliberately not cached: the miss throw is the diagnostic, and a
    // late-loaded assembly should be findable on retry.
    private static readonly ConcurrentDictionary<string, Type> ResolvedTypes = new();

    public object Deserialize(string typeName, ReadOnlySequence<byte> bytes)
    {
        if (!ResolvedTypes.TryGetValue(typeName, out var type))
        {
            type = Type.GetType(typeName, throwOnError: false)
                   ?? ResolveAcrossLoadedAssemblies(typeName)
                   ?? throw new InvalidOperationException(
                       $"Could not resolve message type '{typeName}'. " +
                       $"Ensure the receiving process references the assembly that " +
                       $"declares this message.");
            ResolvedTypes.TryAdd(typeName, type);
        }

        var reader = new Utf8JsonReader(bytes);
        return JsonSerializer.Deserialize(ref reader, type, Options)
               ?? throw new InvalidOperationException($"Deserialization of '{typeName}' returned null.");
    }

    /// <summary>
    /// Fallback for the common case where <c>Type.GetType</c> can't find
    /// the type because it's in an assembly the runtime hasn't probed —
    /// with a bare FullName on the wire that is every user message type's
    /// first arrival. Linear scan of loaded assemblies; the cache above
    /// makes it a once-per-type cost.
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
