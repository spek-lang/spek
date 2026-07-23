namespace Spek.Compiler.AST;

// ─── Actor declarations ───────────────────────────────────────────────────────

public record ActorDecl(
    SourceSpan Span,
    Visibility Visibility,
    bool IsAbstract,
    string Name,
    IReadOnlyList<TypeParameter> TypeParameters,
    IReadOnlyList<WhereClause> WhereClauses,        // Generic constraints
    QualifiedName? BaseActor,
    IReadOnlyList<QualifiedName> ImplementedChannels,
    IReadOnlyList<ActorMember> Members
) : TopLevelDecl(Span);

// ─── Channel declarations ─────────────────────────────────────────────────────

/// <summary>
/// A channel names a set of inputs (messages the implementing actor
/// accepts) and emits (unprompted event messages the implementing actor
/// may produce). Every type referenced is a pre-existing <c>message</c>
/// decl — channels do not re-declare payloads.
///
/// Channels may inherit from other channels via
/// <see cref="BaseChannels"/>. Inheritance is add-only — a derived
/// channel adds inputs and emits to the base set without overriding
/// or hiding inherited members. Diamond patterns are linearized:
/// each input/emit appears once in the flattened set regardless of
/// how many ancestors contributed it.
/// </summary>
public record ChannelDecl(
    SourceSpan Span,
    Visibility Visibility,
    string Name,
    IReadOnlyList<QualifiedName> BaseChannels,
    IReadOnlyList<ChannelMember> Members
) : TopLevelDecl(Span);

public abstract record ChannelMember(SourceSpan Span) : AstNode(Span);
public record ChannelInput(SourceSpan Span, QualifiedName MessageType) : ChannelMember(Span);

/// <summary>
/// <c>emits X;</c> — this channel declares that implementing actors may
/// produce an <c>X</c> unprompted. <c>emits any;</c> is the advisory
/// escape hatch: anything can be emitted without a compile-time check.
/// </summary>
public record ChannelEmits(
    SourceSpan Span,
    QualifiedName? MessageType,   // null means `emits any` (advisory mode)
    bool IsAny
) : ChannelMember(Span);

public abstract record ActorMember(SourceSpan Span) : AstNode(Span);

// ─── Field ───────────────────────────────────────────────────────────────────

public record FieldDecl(
    SourceSpan Span,
    Visibility Visibility,
    TypeRef Type,
    string Name,
    Expr? Initializer,
    // Field lifecycle marker. See <see cref="FieldLifecycle"/>.
    FieldLifecycle Lifecycle = FieldLifecycle.Normal,
    // True when the user explicitly wrote a visibility
    // modifier (`public`, `private`, `internal`, `protected`); false
    // when the modifier was omitted. Lets the emitter apply
    // owner-specific defaults: actor fields default to private,
    // region fields default to public so attaching actors can read
    // them. CE0108 (region field visibility enforcement) builds on
    // this: an explicit `private` on a region field means the field
    // is internal to the region (set in init / used by other fields)
    // and hidden from attaching actors.
    bool IsExplicitVisibility = false
) : ActorMember(Span)
{
    /// <summary>Opts the field out of `: Persisted` capture/restore.</summary>
    public bool IsTransient => Lifecycle == FieldLifecycle.Transient;
    /// <summary>References compile with a CE0101 warning; data still roundtrips.</summary>
    public bool IsDeprecated => Lifecycle == FieldLifecycle.Deprecated;
    /// <summary>References are a CE0102 error; field is skipped from capture/restore.</summary>
    public bool IsRetired => Lifecycle == FieldLifecycle.Retired;
}

// Exhaustive lifecycle marker for a region or actor field.
//
// Inspired by gRPC's reserved/deprecated semantics: fields stay in
// the source forever as the schema evolves; their lifecycle marker
// signals to readers (humans and tools) where they sit in that
// evolution. The four states are mutually exclusive.
public enum FieldLifecycle
{
    /// <summary>The default. Field is captured, restored, and freely referenced.</summary>
    Normal,

    /// <summary>Field is in-memory only; never captured or restored.</summary>
    Transient,

    /// <summary>
    /// Field is on its way out. References still compile but emit
    /// a CE0101 warning so callers know to migrate off. The field is
    /// still captured and restored so existing snapshots round-trip
    /// without data loss.
    /// </summary>
    Deprecated,

    /// <summary>
    /// Field is no longer reachable from new code. References are
    /// a CE0102 error; the name remains reserved (a future field cannot
    /// reuse it without an explicit unblock). The emitter skips it from
    /// capture and restore so persistence stores can drop the key on
    /// the next save.
    /// </summary>
    Retired,
}

// ─── Shared-region attachment ─────────────────────────────────────────────────
//
// `use MarketCache cache;` inside an actor body. Attaches the named
// region (a top-level `shared` decl) to the actor under a local name.
// Within handlers, the local name reads/writes through the region's
// RW lock; reader handlers see only the region's reader lock, writer
// handlers acquire the writer lock.

public record UseDecl(
    SourceSpan Span,
    string RegionType,
    string LocalName
) : ActorMember(Span);

// ─── Init block ──────────────────────────────────────────────────────────────

public record InitBlock(
    SourceSpan Span,
    IReadOnlyList<Param> Parameters,
    BlockStmt Body,
    // `: base(args)` — arguments chained to a base-class constructor. Null when
    // the constructor has no base call (the common case). Empty list means
    // `: base()`. Only meaningful on a `class` that extends an abstract base.
    IReadOnlyList<Expr>? BaseArgs = null
) : ActorMember(Span);

// ─── Term block ────────────────────────────────────────────────────────────────
//
// `term { ... }` is the disposal counterpart to `init`. Runs at the
// end of the actor stop sequence — after `on PostStop`, before the
// actor reference becomes invalid. Triggers `IAsyncDisposable`
// emission on the generated actor class.
//
// Same scope rules as `init`: no `Tell`, `ask`, `become`, `persist`.
// This is for resource release (file handles, sockets, etc.), not
// for state mutation or messaging.
public record TermBlock(
    SourceSpan Span,
    BlockStmt Body
) : ActorMember(Span);

// ─── Behavior ────────────────────────────────────────────────────────────────

public record BehaviorDecl(
    SourceSpan Span,
    bool IsAbstract,
    bool IsOverride,
    string Name,
    IReadOnlyList<OnHandler> Handlers
) : ActorMember(Span);

/// <summary>
/// A single <c>on Foo => ...</c> arm inside a behavior. Each handler
/// may carry a visibility modifier — <see cref="Visibility.Public"/>
/// (default; reachable from any caller), <see cref="Visibility.Internal"/>
/// (same-assembly only), or <see cref="Visibility.Private"/> (reachable
/// only via <c>self.Tell</c>; the runtime dead-letters anything else
/// targeting it) — and an optional <see cref="HandlerMode"/>:
/// <see cref="HandlerMode.Reader"/> (no field mutation; future
/// runtime can run readers concurrently) or
/// <see cref="HandlerMode.Writer"/> (default; full mutation rights;
/// serialized).
/// </summary>
public record OnHandler(
    SourceSpan Span,
    MessagePattern Pattern,
    HandlerBody Body,
    Visibility Visibility = Visibility.Public,
    HandlerMode Mode = HandlerMode.Writer,
    // Zero or more stream operators chained via `=>` between
    // the message pattern and the body. Each entry is a factory call
    // like `debounce(500)` or `distinct(by: x => x.Id)` whose result
    // is a `Spek.Streams.StreamOperator<T>`. Empty list = no stream
    // shaping; behaves exactly as an unshaped handler.
    IReadOnlyList<Expr>? StreamOperators = null
) : AstNode(Span);

/// <summary>
/// Concurrency discipline an <see cref="OnHandler"/> opts into.
/// Default is <see cref="Writer"/> — the
/// "every handler serializes against every other" semantics.
/// </summary>
public enum HandlerMode { Writer, Reader }

public abstract record MessagePattern(SourceSpan Span) : AstNode(Span);
public record NamedBindPattern(SourceSpan Span, QualifiedName MessageType, string Binding) : MessagePattern(Span);
public record NoBindPattern(SourceSpan Span, QualifiedName MessageType) : MessagePattern(Span);
public record CatchAllPattern(SourceSpan Span, string Binding) : MessagePattern(Span);

/// <summary>
/// `on event Name(params) => body`. Models a C# event-handler
/// shape: the compiler synthesises a hidden message record from
/// <see cref="Parameters"/>, emits a bridge method named
/// <see cref="HandlerName"/> with the verbatim signature so user code
/// can attach it via <c>source.Event += HandlerName;</c>, and adds a
/// dispatch arm that unboxes the synthetic message back into the
/// declared parameter names. The body runs on the actor thread like
/// any other handler.
/// </summary>
public record EventPattern(
    SourceSpan Span,
    string HandlerName,
    IReadOnlyList<Param> Parameters
) : MessagePattern(Span);

public abstract record HandlerBody(SourceSpan Span) : AstNode(Span);
public record BlockHandlerBody(SourceSpan Span, BlockStmt Block) : HandlerBody(Span);
public record InlineHandlerBody(SourceSpan Span, Expr Expr) : HandlerBody(Span);

// ─── Lifecycle hooks ─────────────────────────────────────────────────────────

public record LifecycleHook(
    SourceSpan Span,
    LifecycleEvent Event,
    HandlerBody Body
) : ActorMember(Span);

public abstract record LifecycleEvent(SourceSpan Span) : AstNode(Span);
public record PreStartEvent(SourceSpan Span) : LifecycleEvent(Span);
public record PostStopEvent(SourceSpan Span) : LifecycleEvent(Span);
public record RestoreEvent(SourceSpan Span, TypeRef SnapshotType, string Binding) : LifecycleEvent(Span);

// ─── Passivate declaration ───────────────────────────────────────────────────

// Timeout is a TimeSpan-valued expression (e.g. System.TimeSpan.FromMinutes(10)).
public record PassivateDecl(SourceSpan Span, Expr Timeout) : ActorMember(Span);

// ─── Supervise declaration ───────────────────────────────────────────────────

public record SuperviseDecl(
    SourceSpan Span,
    Expr? Target,              // null = default (applies to all children)
    SuperviseStrategy Strategy
) : ActorMember(Span);

public abstract record SuperviseStrategy(SourceSpan Span) : AstNode(Span);
public record OneForOneStrategy(SourceSpan Span, IReadOnlyList<SuperviseOption> Options) : SuperviseStrategy(Span);
public record AllForOneStrategy(SourceSpan Span, IReadOnlyList<SuperviseOption> Options) : SuperviseStrategy(Span);

public abstract record SuperviseOption(SourceSpan Span) : AstNode(Span);
/// <summary>
/// A single <c>on Failure[(ExceptionType)]: Action</c> arm in a supervise
/// strategy. <see cref="ExceptionType"/> is null for the untyped catch-all
/// form (<c>on Failure: Action</c>); when set it narrows the arm to only
/// match causes that are <c>cause is ExceptionType</c>. Multiple arms in
/// a single strategy are matched top-to-bottom — the first matching arm
/// wins.
/// </summary>
public record OnFailureOption(
    SourceSpan Span,
    QualifiedName? ExceptionType,
    RestartAction Action
) : SuperviseOption(Span);
// maxRetries / withinTime are named arguments (IDENTIFIER COLON expression). Their
// values are expressions emitted verbatim — an int and a TimeSpan in practice, both
// type-checked by Roslyn. An unrecognized option name parses to UnknownSuperviseOption
// and is reported as CE0117 in semantics (never reaches emit).
public record MaxRetriesOption(SourceSpan Span, Expr Value) : SuperviseOption(Span);
public record WithinTimeOption(SourceSpan Span, Expr Window) : SuperviseOption(Span);
public record UnknownSuperviseOption(SourceSpan Span, string Name) : SuperviseOption(Span);

public enum RestartAction { Restart, Stop, Escalate, Resume }

// ─── Method declaration ──────────────────────────────────────────────────────

public record MethodDecl(
    SourceSpan Span,
    Visibility Visibility,
    TypeRef? ReturnType,  // null = void
    string Name,
    IReadOnlyList<TypeParameter> TypeParameters,   // Generic methods
    IReadOnlyList<WhereClause> WhereClauses,        // Generic constraints
    IReadOnlyList<Param> Parameters,
    // For an `abstract` method (no source body) this is a synthesized empty
    // block — the emitter gates on IsAbstract and emits a `;` instead, so the
    // block is never rendered. Keeping it non-null avoids nullable ripple
    // through every body-walking pass.
    BlockStmt Body,
    bool IsAbstract = false
) : ActorMember(Span);

// A property on a class: `public int X { get; set; } = 0;`.
// Accessors are get/set/init; Body is null for an auto-accessor or an
// expression for `get => expr`. Initializer is the optional `= expr`.
public record PropertyDecl(
    SourceSpan Span,
    Visibility Visibility,
    TypeRef Type,
    string Name,
    IReadOnlyList<PropertyAccessor> Accessors,
    Expr? Initializer
) : ActorMember(Span);

public record PropertyAccessor(
    SourceSpan Span,
    Visibility? Visibility,   // null = same as the property
    string Kind,              // "get" | "set" | "init"
    Expr? Body                // null = auto-accessor; else `=> Body`
) : AstNode(Span);
