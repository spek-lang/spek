using Spek.Compiler.AST;

namespace Spek.Compiler.Emit;

/// <summary>
/// Emits C# record declarations for Spek message types.
///
/// Mapping (per CONTEXT.md):
///   message Foo(T x)           → public record Foo(T x);
///   message Response&lt;T&gt;(T v)   → public record Response&lt;T&gt;(T v);
///   message Shutdown(string r = "normal") → public record Shutdown(string r = "normal");
/// </summary>
public sealed class MessageEmitter
{
    private readonly CSharpWriter _w;

    public MessageEmitter(CSharpWriter writer) => _w = writer;

    public void Emit(MessageDecl msg)
    {
        var typeParams = msg.TypeParameters.Count > 0
            ? $"<{string.Join(", ", msg.TypeParameters.Select(p => p.Name))}>"
            : "";

        var fields = string.Join(", ", msg.Fields.Select(EmitField));

        // `abstract message` → an abstract record (a family base you can't
        // instantiate, only handle). A variant chains to the base's primary
        // constructor: `: Base()` — empty because the abstract base carries no
        // fields (base-carries-fields is deferred; see CE0125).
        var abstractKw = msg.IsAbstract ? "abstract " : "";
        var baseClause = msg.BaseMessage is not null ? $" : {msg.BaseMessage}()" : "";

        _w.DocComment(msg.DocComment);
        _w.Line($"public {abstractKw}record {msg.Name}{typeParams}({fields}){baseClause};");
    }

    private static string EmitField(MessageField f)
    {
        var type = EmitTypeRef(f.Type);
        var defaultPart = f.DefaultValue is not null
            ? $" = {EmitDefaultValue(f.DefaultValue)}"
            : "";
        return $"{type} {f.Name}{defaultPart}";
    }

    private static string EmitTypeRef(TypeRef t)
    {
        var nullable = t.IsNullable ? "?" : "";
        if (t.TypeArgs.Count == 0)
            return t.Name.ToString() + nullable;

        var args = string.Join(", ", t.TypeArgs.Select(EmitTypeRef));
        return $"{t.Name}<{args}>{nullable}";
    }

    private static string EmitDefaultValue(Expr expr) => expr switch
    {
        StringLiteralExpr s   => s.Raw ?? $"\"{s.Value}\"",
        IntLiteralExpr i      => i.Raw ?? i.Value.ToString(),
        DecimalLiteralExpr d  => d.Raw ?? (d.HasSuffix ? $"{d.Value}m" : d.Value.ToString()),
        CharLiteralExpr c     => c.Raw,
        BoolLiteralExpr b     => b.Value ? "true" : "false",
        NullLiteralExpr       => "null",
        NameExpr n            => n.Name.ToString(),
        _                     => throw new NotSupportedException(
                                     $"Default value expression type {expr.GetType().Name} is not supported in message fields.")
    };
}
