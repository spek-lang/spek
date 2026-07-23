namespace Spek.Compiler.AST;

// ─── File / top-level ────────────────────────────────────────────────────────

public record SpekFile(
    SourceSpan Span,
    NamespaceDecl? Namespace,
    IReadOnlyList<UsingDecl> Usings,
    IReadOnlyList<TopLevelDecl> Declarations
) : AstNode(Span);

public abstract record TopLevelDecl(SourceSpan Span) : AstNode(Span);

public record NamespaceDecl(SourceSpan Span, QualifiedName Name) : AstNode(Span);

/// <summary>
/// A <c>using</c> directive in Spek source. <see cref="IsInterop"/>
/// is set when the user wrote <c>interop using NS;</c> — the CE0080
/// hostile-namespace check is skipped, but the user accepts CE0086
/// (interop bypasses safety guarantees) in exchange. The emitter
/// ignores the flag — both forms produce the same C# <c>using NS;</c>.
/// </summary>
public record UsingDecl(SourceSpan Span, QualifiedName Name, bool IsInterop = false) : AstNode(Span);

// ─── Program entry point ─────────────────────────────────────────────────────

public record ProgramDecl(SourceSpan Span, string Name, BlockStmt Body) : TopLevelDecl(Span);

// ─── Modules and their methods ─────────────────────────────────────────
//
// A `module X { … }` is a stateless container of methods, equivalent
// in shape to a C# static class. Modules may nest (sub-namespacing
// inside a namespace, Erlang-style). They cannot contain mutable
// state, instance fields, or actor-shaped concerns — `actor` /
// `shared` / `channel` continue to be the homes for those.
//
// Method declarations live inside modules and lower to C# static
// methods on the emitted module's static class. Signatures are
// identical in shape to C# methods; visibility modifiers
// (`public` / `internal` / `protected` / `private`) work the same.

// A module's body is just methods (the same MethodDecl as actors/classes — Spek
// has ONE method concept) plus nested modules. The methods emit as C# `static`
// methods on the module's static class; `static` is never written in source.
public record ModuleDecl(
    SourceSpan Span,
    Visibility Visibility,
    string Name,
    IReadOnlyList<MethodDecl> Methods,
    IReadOnlyList<ModuleDecl> NestedModules
) : TopLevelDecl(Span);

// ─── Classes ──────────────────────────────────────────────────────────────────
//
// A `class X { ... }` is a mutable, single-owner instance type — the
// genuinely-new "mutable but not concurrent" kind from the type/ownership
// model. It holds fields, an optional `init(params)` constructor, and methods,
// and lowers to a plain C# instance class. There is no capability marker:
// mutability of the declaration is the signal, ownership is inferred
// (CE0085 / CE0087). Members reuse the actor-side records (FieldDecl /
// InitBlock / MethodDecl) — those derive from ActorMember, but ClassDecl
// references them concretely, so the shared base is only cosmetic.
public record ClassDecl(
    SourceSpan Span,
    Visibility Visibility,
    bool IsAbstract,                                 // `abstract class` — extendable, not sealed
    string Name,
    IReadOnlyList<TypeParameter> TypeParameters,
    IReadOnlyList<WhereClause> WhereClauses,        // Generic constraints
    IReadOnlyList<FieldDecl> Fields,
    InitBlock? Init,
    IReadOnlyList<MethodDecl> Methods,
    IReadOnlyList<PropertyDecl> Properties,          // Class properties
    // Base list after the `:` — an optional base class (first) followed by
    // implemented interfaces, in source order. Semantic analysis classifies
    // each name; the emitter passes them through to the C# base list verbatim.
    IReadOnlyList<QualifiedName> Bases
) : TopLevelDecl(Span);

// ─── Interfaces ─────────────────────────────────────────────────────────────
//
// An `interface X { ... }` is the class-side implementation contract: the
// method/property surface a `class` promises. It is the method-based sibling
// of `channel` (the actor's message-based contract) and lowers to the same
// thing — a C# `interface`. Deliberately pre-C#-8: signatures only, never a
// body and never a field. A body or field is carried into the AST anyway so
// the analyzer can reject it with a friendly CE0120 (the no-behavior rule: a
// contract declares shape, never behavior), rather than a raw parser error.
public record InterfaceDecl(
    SourceSpan Span,
    Visibility Visibility,
    string Name,
    IReadOnlyList<TypeParameter> TypeParameters,
    IReadOnlyList<WhereClause> WhereClauses,
    IReadOnlyList<QualifiedName> BaseInterfaces,
    IReadOnlyList<MethodSignature> Methods,
    IReadOnlyList<PropertyDecl> Properties,
    // Fields are illegal in an interface; carried only so the analyzer can
    // point CE0120 at them. Always empty in well-formed source.
    IReadOnlyList<FieldDecl> Fields
) : TopLevelDecl(Span);

// A method signature inside an `interface`. <see cref="Body"/> is null for the
// legal `;`-terminated form; it is non-null only when the user illegally wrote
// a default-method body, which the analyzer reports as CE0120.
public record MethodSignature(
    SourceSpan Span,
    Visibility Visibility,
    TypeRef? ReturnType,  // null = void
    string Name,
    IReadOnlyList<TypeParameter> TypeParameters,
    IReadOnlyList<WhereClause> WhereClauses,
    IReadOnlyList<Param> Parameters,
    BlockStmt? Body = null
) : AstNode(Span);

// ─── Shared regions ───────────────────────────────────────────────────────────
//
// A `shared X { ... }` declaration introduces per-`ActorSystem` state
// with a reader/writer lock separate from any actor's own lock. Actors
// attach a region with `use X foo;` inside their body. Phase 1 supports
// only the field list — init blocks, persistence clauses, and
// reader-only / writer-only modes are deferred to follow-up work.

public record SharedRegionDecl(
    SourceSpan Span,
    Visibility Visibility,
    string Name,
    IReadOnlyList<FieldDecl> Fields,
    BlockStmt? Init = null,
    // Capability marker. Null = transient (default,
    // in-memory only). Known markers: "Persisted".
    // Reserved for later: "Replicated", "EventSourced",
    // "ConflictFreeReplicated".
    string? BaseCapability = null,
    // `term { ... }` block on the region. Runs at
    // `ActorSystem` shutdown in reverse construction order;
    // triggers `IAsyncDisposable` emission on the region class.
    BlockStmt? Term = null
) : TopLevelDecl(Span);
