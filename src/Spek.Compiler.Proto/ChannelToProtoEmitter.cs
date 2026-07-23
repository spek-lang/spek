using System.Text;
using Spek.Compiler.AST;

namespace Spek.Compiler.Proto;

/// <summary>
/// Code-first companion to <see cref="ProtoChannelSynthesizer"/>.
/// Takes a parsed Spek <see cref="ChannelDecl"/> and the
/// <see cref="MessageDecl"/>s its inputs and emits reference,
/// produces a proto3 <c>.proto</c> file. Output is intended to be
/// consumed by <c>protoc</c> (and from there by <c>Grpc.Tools</c>
/// for C# stub generation, or by tooling in any other language
/// that wants to call the service).
///
/// <para>
/// The output is plain text proto3, including an auto-generated
/// header that points readers back at the canonical Spek source.
/// "Source: src/UserApi.spek" so a downstream developer who
/// receives the proto knows where the contract is actually
/// defined.
/// </para>
///
/// <para>
/// Only the proto3 subset that Spek messages can express is
/// emitted: scalar fields, message-typed fields, enum-typed
/// fields. Spek's <c>List&lt;T&gt;</c> and <c>Dictionary&lt;K,V&gt;</c>
/// don't appear on messages today (CE0010), so repeated and map
/// fields are not yet generated. Tag numbers are assigned by
/// declaration order starting at 1, matching the convention from
/// freshly-written proto files.
/// </para>
/// </summary>
public sealed class ChannelToProtoEmitter
{
    /// <summary>
    /// Diagnostics surfaced during emission — non-fatal warnings
    /// (Spek field type that maps awkwardly to proto) and fatal
    /// errors (unknown message type referenced by the channel).
    /// </summary>
    public IReadOnlyList<SynthesisDiagnostic> Diagnostics => _diagnostics;
    private readonly List<SynthesisDiagnostic> _diagnostics = new();

    /// <summary>
    /// Renders <paramref name="channel"/> as a proto3 source file.
    /// <paramref name="messages"/> must include every input and
    /// emit type referenced by the channel, plus any nested
    /// message types those reach. <paramref name="protoPackage"/>
    /// is written as the proto file's <c>package</c> directive
    /// (defaults to lowercased channel name when null).
    /// <paramref name="spekSourceLabel"/> goes into the
    /// auto-generated header so consumers know where the canonical
    /// Spek source lives.
    /// </summary>
    public string Emit(
        ChannelDecl channel,
        IReadOnlyDictionary<string, MessageDecl> messages,
        string spekSourceLabel,
        string? protoPackage = null)
    {
        ArgumentNullException.ThrowIfNull(channel);
        ArgumentNullException.ThrowIfNull(messages);
        ArgumentNullException.ThrowIfNull(spekSourceLabel);

        protoPackage ??= channel.Name.ToLowerInvariant();

        var sb = new StringBuilder();
        EmitHeader(sb, spekSourceLabel);
        sb.Append("syntax = \"proto3\";").AppendLine();
        sb.AppendLine();
        sb.Append("package ").Append(protoPackage).Append(';').AppendLine();
        sb.AppendLine();

        // Walk every input + emit type, depth-first across nested
        // message references, and emit each message exactly once.
        var emittedMessages = new HashSet<string>(StringComparer.Ordinal);
        var inputNames  = channel.Members.OfType<ChannelInput>()
            .Select(i => i.MessageType.Simple).ToList();
        var emitNames   = channel.Members.OfType<ChannelEmits>()
            .Where(e => !e.IsAny && e.MessageType is not null)
            .Select(e => e.MessageType!.Simple).ToList();

        foreach (var name in inputNames.Concat(emitNames))
            EmitMessageRecursive(sb, name, messages, emittedMessages);

        // Service definition.
        sb.Append("service ").Append(channel.Name).AppendLine(" {");
        // Each `on Foo;` becomes `rpc Foo(Foo) returns (oneof of emits);`.
        // For now we use the FIRST emit type as the response type;
        // multi-emit `oneof` synthesis is a follow-up. Channels with
        // a single emit produce a clean RPC; multi-emit channels
        // currently emit a TODO comment alongside the first emit.
        var emitTypes = channel.Members.OfType<ChannelEmits>()
            .Where(e => !e.IsAny && e.MessageType is not null)
            .Select(e => e.MessageType!.Simple)
            .ToList();
        if (emitTypes.Count == 0)
        {
            _diagnostics.Add(SynthesisDiagnostic.Warning(
                $"Channel '{channel.Name}' has no `emits` declarations; using google.protobuf.Empty as response."));
        }

        foreach (var input in channel.Members.OfType<ChannelInput>())
        {
            var inputName = input.MessageType.Simple;
            // Method name = input message name (matches the REST
            // and gRPC channel-as-contract convention).
            string responseType;
            if (emitTypes.Count == 0)
            {
                responseType = "google.protobuf.Empty";
            }
            else if (emitTypes.Count == 1)
            {
                responseType = emitTypes[0];
            }
            else
            {
                // Multi-emit channels need a oneof wrapper. For now
                // we emit a TODO marker — the synthesised proto won't
                // compile until the user adds the wrapper or we ship
                // the oneof generator in a follow-up.
                responseType = $"/* TODO oneof of {string.Join(", ", emitTypes)} */ {emitTypes[0]}";
                _diagnostics.Add(SynthesisDiagnostic.Warning(
                    $"Channel '{channel.Name}' emits multiple types ({string.Join(", ", emitTypes)}). " +
                    $"The first is picked as the response type; multi-emit `oneof` " +
                    $"generation is a follow-up."));
            }
            sb.Append("    rpc ").Append(inputName)
              .Append('(').Append(inputName).Append(") returns (")
              .Append(responseType).Append(");").AppendLine();
        }
        sb.AppendLine("}");

        return sb.ToString();
    }

    // ─── Message emission ──────────────────────────────────────────────

    private void EmitMessageRecursive(
        StringBuilder sb,
        string messageName,
        IReadOnlyDictionary<string, MessageDecl> messages,
        HashSet<string> emitted)
    {
        if (!emitted.Add(messageName)) return;
        if (!messages.TryGetValue(messageName, out var decl))
        {
            _diagnostics.Add(SynthesisDiagnostic.Error(
                $"Message '{messageName}' is referenced by the channel but not " +
                $"included in the message map; cannot emit proto."));
            return;
        }

        sb.Append("message ").Append(decl.Name).AppendLine(" {");
        for (int i = 0; i < decl.Fields.Count; i++)
        {
            var field = decl.Fields[i];
            var protoType = MapTypeToProto(field.Type, messages, emitted, sb, depth: 0);
            sb.Append("    ").Append(protoType).Append(' ').Append(field.Name)
              .Append(" = ").Append(i + 1).Append(';').AppendLine();
        }
        sb.AppendLine("}");
        sb.AppendLine();
    }

    /// <summary>
    /// Maps a Spek <see cref="TypeRef"/> to a proto type string.
    /// Recurses into nested message references so transitively-
    /// reached types are emitted.
    /// </summary>
    private string MapTypeToProto(
        TypeRef type,
        IReadOnlyDictionary<string, MessageDecl> messages,
        HashSet<string> emitted,
        StringBuilder sb,
        int depth)
    {
        var simple = type.Name.Simple;
        var protoScalar = simple switch
        {
            "bool"          => "bool",
            "string"        => "string",
            "int"           => "int32",
            "long"          => "int64",
            "uint"          => "uint32",
            "ulong"         => "uint64",
            "float"         => "float",
            "double"        => "double",
            "byte[]"        => "bytes",
            _               => null,
        };
        if (protoScalar is not null) return protoScalar;

        // Nested message reference — emit recursively (cap recursion
        // at a reasonable depth to avoid pathological proto graphs).
        if (depth > 16)
        {
            _diagnostics.Add(SynthesisDiagnostic.Warning(
                $"Recursion depth exceeded while emitting proto for type '{simple}'; emitted as bytes."));
            return "bytes";
        }
        if (messages.ContainsKey(simple))
        {
            EmitMessageRecursive(sb, simple, messages, emitted);
            return simple;
        }

        // Unknown type — surface as a warning and use bytes as the
        // safe fallback so the proto at least parses.
        _diagnostics.Add(SynthesisDiagnostic.Warning(
            $"Cannot map Spek type '{simple}' to a proto type; emitted as bytes."));
        return "bytes";
    }

    private static void EmitHeader(StringBuilder sb, string spekSourceLabel)
    {
        sb.AppendLine("// <auto-generated>");
        sb.Append("//   Source: ").AppendLine(spekSourceLabel);
        sb.AppendLine("//   This file was generated from a Spek channel definition by");
        sb.AppendLine("//   the Spek compiler. DO NOT EDIT — changes are overwritten on");
        sb.AppendLine("//   the next build. To modify the channel's contract, edit");
        sb.Append("//   ").Append(spekSourceLabel).AppendLine(" and rebuild.");
        sb.AppendLine("// </auto-generated>");
        sb.AppendLine();
    }
}
