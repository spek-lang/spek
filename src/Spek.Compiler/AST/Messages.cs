namespace Spek.Compiler.AST;

// ─── Message declarations ────────────────────────────────────────────────────

public record MessageDecl(
    SourceSpan Span,
    string Name,
    IReadOnlyList<TypeParameter> TypeParameters,
    IReadOnlyList<MessageField> Fields,
    // `abstract message` — a polymorphic dispatch-contract base. Lowers to an
    // abstract C# record; can't be instantiated, only handled (`on Base`).
    bool IsAbstract = false,
    // `message D(...) : Base` — the family base this message extends. Null for a
    // standalone message. The base must be an (empty) `abstract message`.
    QualifiedName? BaseMessage = null
) : TopLevelDecl(Span);

public record MessageField(
    SourceSpan Span,
    TypeRef Type,
    string Name,
    Expr? DefaultValue
) : AstNode(Span);

// ─── Enum declarations ────────────────────────────────────────────────────────

/// <summary>
/// A Spek-native enum declaration. Emitted as a plain C# enum in the
/// generated output; members are value-comparable and immutable by
/// construction, which lets them flow through <c>message</c> field types
/// without tripping CE0010.
/// </summary>
public record EnumDecl(
    SourceSpan Span,
    Visibility Visibility,
    string Name,
    IReadOnlyList<EnumMember> Members
) : TopLevelDecl(Span);

public record EnumMember(SourceSpan Span, string Name) : AstNode(Span);
