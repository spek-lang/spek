using Spek.Compiler.AST;

namespace Spek.Compiler.Semantic;

/// <summary>Which reference-typed root a reader-handler local aliases, so a
/// write or mutating call reached through the local is judged by CE0087 as if
/// it had named the root directly.</summary>
public enum FieldAliasKind
{
    /// <summary>The local aliases a <c>use X handle;</c> shared-region attachment
    /// (<c>var c = cache;</c>). A write through it (<c>c.field = …</c>) is a
    /// region write.</summary>
    Region,
    /// <summary>The local aliases a confined-class actor field
    /// (<c>var h = counter;</c>). A mutating method on it (<c>h.Inc()</c>) or a
    /// member write (<c>h.n = …</c>) mutates the field's object.</summary>
    ConfinedClass,
}

/// <summary>A local bound to a reference-typed CE0087 root — a <c>use</c> region
/// handle or a confined-class actor field (with the class name). Value-typed
/// roots are intentionally not tracked: aliasing a primitive/struct field copies
/// it, so a write through the alias never reaches the field.</summary>
public readonly record struct FieldAlias(FieldAliasKind Kind, string Root, string? ClassName);

/// <summary>Coarse classification of what an expression evaluates to.</summary>
public enum ExprKind
{
    /// <summary>Could not be determined — rules that need type info should skip (fail open).</summary>
    Unknown,
    /// <summary>An <c>ActorRef</c> — opaque capability handle. Member access blocked except <c>Tell</c>.</summary>
    ActorRef,
    /// <summary>A <c>message</c>-declared type instance.</summary>
    Message,
    /// <summary>A primitive / string / immutable scalar the classifier knows about.</summary>
    Primitive,
}

/// <summary>
/// Walks an expression and, where it can, determines what kind of value it
/// produces. Intentionally lean — only enough to power CE0012 and CE0020 for
/// <c>Tell</c>. Anything it can't classify is reported as
/// <see cref="ExprKind.Unknown"/>; downstream rules should interpret that as
/// "insufficient information, don't fire."
///
/// The typer is aware of the current actor's fields (so <c>someField.Tell</c>
/// can be classified when the field is declared <c>ActorRef</c>) and of
/// locally-declared <c>var</c>s whose initializers it has already seen.
/// </summary>
public sealed class ExpressionTyper
{
    private readonly SymbolTable _symbols;
    private readonly ActorSymbols _actor;
    private readonly Dictionary<string, ExprKind> _locals = new();
    // Track the declared type of locals/params alongside their
    // ExprKind. Used by CE0103 (exhaustive switch) to determine when a
    // switch subject is an enum value, and to look up the enum's
    // variant set without re-deriving it from the arms.
    private readonly Dictionary<string, TypeRef> _localTypes = new();
    // CE0087 reachability: locals that alias a reference-typed root a reader
    // must not write through — a `use` region handle or a confined-class actor
    // field. Populated by the semantic walker as it sees `var h = field;`
    // declarations; a rebind (or a shadowing parameter) clears the entry so a
    // reused name never inherits a stale alias.
    private readonly Dictionary<string, FieldAlias> _aliases = new();

    public ExpressionTyper(SymbolTable symbols, ActorSymbols actor)
    {
        _symbols = symbols;
        _actor = actor;
    }

    /// <summary>Record the inferred kind of a <c>var</c>-declared local. Clears
    /// any prior CE0087 alias for the name (a redeclaration sheds it; the
    /// walker re-establishes it via <see cref="SetAlias"/> when the new
    /// initializer still aliases a field).</summary>
    public void RegisterLocal(string name, ExprKind kind)
    {
        _locals[name] = kind;
        _aliases.Remove(name);
    }

    /// <summary>Records (or, with <c>null</c>, clears) the reference-typed root a
    /// reader-handler local aliases for CE0087.</summary>
    public void SetAlias(string name, FieldAlias? alias)
    {
        if (alias is { } a) _aliases[name] = a;
        else _aliases.Remove(name);
    }

    /// <summary>The CE0087 alias this local resolves to, or <c>null</c>.</summary>
    public FieldAlias? GetAlias(string name) =>
        _aliases.TryGetValue(name, out var a) ? a : null;

    /// <summary>
    /// Record the declared type of a typed local
    /// (`MyEnum s = …;`). The type also flows through to the kind via
    /// the same classification rules as <see cref="RegisterParameter"/>.
    /// </summary>
    public void RegisterLocalWithType(string name, TypeRef type)
    {
        _locals[name] = ClassifyTypeRef(type);
        _localTypes[name] = type;
        _aliases.Remove(name);
    }

    /// <summary>True if <paramref name="name"/> resolves to an actor
    /// field (as opposed to a local / parameter / global). Used by
    /// CE0087 to flag mutation of actor state from a reader handler.</summary>
    public bool IsActorField(string name) =>
        !_locals.ContainsKey(name) && _actor.Fields.ContainsKey(name);

    /// <summary>
    /// Register a method / init parameter so references to it by name
    /// classify correctly (instead of defaulting to <see cref="ExprKind.Unknown"/>).
    /// </summary>
    public void RegisterParameter(string name, TypeRef type)
    {
        _locals[name] = ClassifyTypeRef(type);
        _localTypes[name] = type;
        // A parameter (incl. a lambda parameter) shadows any same-named field,
        // so it also sheds any CE0087 alias the name carried.
        _aliases.Remove(name);
    }

    /// <summary>
    /// Returns the declared type of <paramref name="expr"/> when
    /// it resolves to a typed local, parameter, or actor field; null
    /// otherwise. Used by CE0103 to find the enum type underlying a
    /// switch subject.
    /// </summary>
    public TypeRef? TryGetType(Expr expr)
    {
        return expr switch
        {
            NameExpr nm when nm.Name.Parts.Count == 1 => TryGetSimpleType(nm.Name.Simple),
            ParenExpr p => TryGetType(p.Inner),
            _ => null,
        };
    }

    private TypeRef? TryGetSimpleType(string name)
    {
        if (_localTypes.TryGetValue(name, out var t)) return t;
        if (_actor.Fields.TryGetValue(name, out var field)) return field.Type;
        return null;
    }

    /// <summary>The declared type of a typed local, parameter, or actor field
    /// by name — the by-name companion to <see cref="TryGetType"/>, for
    /// resolving the head of a multi-part name (<c>msg.field</c>) without
    /// synthesising an <see cref="Expr"/>. Null when the name isn't typed.</summary>
    public TypeRef? TypeOfName(string name) => TryGetSimpleType(name);

    /// <summary>
    /// Classifies a bare identifier (local var first, then actor field).
    /// Used to inspect the head of a multi-part <see cref="QualifiedName"/>
    /// without having to synthesise an <see cref="Expr"/>.
    /// </summary>
    public ExprKind ClassifySimple(string name)
    {
        if (_locals.TryGetValue(name, out var localKind))
            return localKind;
        if (_actor.Fields.TryGetValue(name, out var field))
            return ClassifyTypeRef(field.Type);
        return ExprKind.Unknown;
    }

    public ExprKind Classify(Expr expr)
    {
        switch (expr)
        {
            case SelfExpr:
            case SenderExpr:
                return ExprKind.ActorRef;

            case SpawnExpr:
                // spawn<T>() returns an ActorRef regardless of T.
                return ExprKind.ActorRef;

            case NewExpr n:
                return _symbols.ResolveMessage(n.Type) is not null
                    ? ExprKind.Message
                    : ExprKind.Unknown;

            case NameExpr nm when nm.Name.Parts.Count == 1:
            {
                var simple = nm.Name.Simple;
                if (_locals.TryGetValue(simple, out var localKind))
                    return localKind;
                if (_actor.Fields.TryGetValue(simple, out var field))
                    return ClassifyTypeRef(field.Type);
                return ExprKind.Unknown;
            }

            case ParenExpr p:
                return Classify(p.Inner);

            case AskExpr:
                // ask returns the declared reply type — unknown at this layer.
                return ExprKind.Unknown;

            case LiteralExpr:
                return ExprKind.Primitive;

            default:
                return ExprKind.Unknown;
        }
    }

    private ExprKind ClassifyTypeRef(TypeRef type)
    {
        var simple = type.Name.Simple;
        if (simple == "ActorRef") return ExprKind.ActorRef;
        if (_symbols.ResolveMessage(type.Name) is not null) return ExprKind.Message;

        // Anything else — defer to the classifier's simple-type set.
        return ImmutableTypeClassifier.Classify(type, _symbols, Array.Empty<TypeParameter>())
            == ImmutableTypeClassifier.Classification.Allowed
            ? ExprKind.Primitive
            : ExprKind.Unknown;
    }
}
