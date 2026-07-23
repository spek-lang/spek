namespace Spek.Compiler.AST;

// ─── Shared small types ───────────────────────────────────────────────────────

public record QualifiedName(SourceSpan Span, IReadOnlyList<string> Parts) : AstNode(Span)
{
    public string Simple => Parts[^1];
    public override string ToString() => string.Join(".", Parts);
}

public record TypeRef(
    SourceSpan Span,
    QualifiedName Name,
    IReadOnlyList<TypeRef> TypeArgs,
    bool IsNullable = false,
    // Array rank: 0 = not an array, 1 = `T[]`, 2 = `T[][]` (jagged).
    int ArrayRank = 0
) : AstNode(Span)
{
    public override string ToString()
    {
        var baseName = TypeArgs.Count == 0
            ? Name.ToString()
            : $"{Name}<{string.Join(", ", TypeArgs)}>";
        if (IsNullable) baseName += "?";
        for (var i = 0; i < ArrayRank; i++) baseName += "[]";
        return baseName;
    }
}

public record TypeParameter(SourceSpan Span, string Name) : AstNode(Span);

// A generic type-parameter constraint clause: `where T : C1, C2`.
// Constraints are kept as already-rendered C# fragments (`class`, `new()`,
// `IComparable<T>`, `U`) and emitted verbatim — Roslyn enforces them.
public record WhereClause(
    SourceSpan Span,
    string TypeParam,
    IReadOnlyList<string> Constraints
) : AstNode(Span);

// Parameter-passing modifier. Mirrors C# exactly: `in` is a
// readonly reference, `ref` is an aliased mutable reference, `out` is
// a caller-bound output. `None` is by-value (the default). The
// modifier flows verbatim into the emitted C# signature; at call sites
// it's carried by RefArgExpr (see Expressions.cs).
public enum ParamModifier { None, In, Ref, Out }

public record Param(
    SourceSpan Span,
    TypeRef Type,
    string Name,
    ParamModifier Modifier = ParamModifier.None
) : AstNode(Span);

public enum Visibility { Private, Internal, Protected, Public }
