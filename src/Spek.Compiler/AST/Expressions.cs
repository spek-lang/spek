namespace Spek.Compiler.AST;

// ─── Expressions ─────────────────────────────────────────────────────────────

public abstract record Expr(SourceSpan Span) : AstNode(Span);

// target.Ask(new MessageType(args))                     — reply type inferred from handler returns.
// target.Ask<ExplicitReply>(new MessageType(args))       — reply type declared at call site (disambiguates
//                                                          when multiple handlers return different types).
// Message is normally a `new Msg(...)` expression; reply inference and CE0020 read
// the constructed type from it. Modeled as an Expr so a pre-built message works too.
public record AskExpr(
    SourceSpan Span,
    Expr Target,
    Expr Message,
    TypeRef? ExplicitReplyType = null) : Expr(Span);

// a = b, a += b, etc.
public record AssignExpr(SourceSpan Span, Expr Left, AssignOp Op, Expr Right) : Expr(Span);

public enum AssignOp
{
    Assign, PlusAssign, MinusAssign, StarAssign, SlashAssign,
    ModAssign, AndAssign, OrAssign, XorAssign, CoalesceAssign
}

// a ? b : c
public record ConditionalExpr(SourceSpan Span, Expr Condition, Expr Then, Expr Else) : Expr(Span);

// Binary operations
public record BinaryExpr(SourceSpan Span, Expr Left, BinaryOp Op, Expr Right) : Expr(Span);

public enum BinaryOp
{
    Coalesce,
    Or, And,
    BitOr, BitXor, BitAnd,
    Eq, Neq,
    Lt, Lte, Gt, Gte,
    Shl, Shr,
    Add, Sub,
    Mul, Div, Mod
}

// Unary operations
public record UnaryExpr(SourceSpan Span, UnaryOp Op, Expr Operand) : Expr(Span);

// Type operations that pair an expression with a type:
//   Cast: (Type)operand   | Is: operand is Type [binding]   | As: operand as Type
// Binding is only meaningful for Is (the `x is Foo f` capture); null otherwise.
public enum TypeOpKind { Cast, Is, As }
public record TypeOpExpr(SourceSpan Span, TypeOpKind Kind, Expr Operand, TypeRef Type, string? Binding = null) : Expr(Span);
public enum UnaryOp { Not, Negate, BitNot }

// Member access: expr.name
// NullConditional => emit `?.` instead of `.`.
public record MemberAccessExpr(SourceSpan Span, Expr Target, string Member, bool NullConditional = false) : Expr(Span);

// Method call: expr.Method<TArgs>(args). NullConditional => `?.`.
public record MethodCallExpr(
    SourceSpan Span,
    Expr Target,
    string Method,
    IReadOnlyList<TypeRef> TypeArgs,
    IReadOnlyList<Expr> Args,
    bool NullConditional = false
) : Expr(Span);

// A call-site argument carrying an `in` / `ref` / `out`
// modifier: `Foo(ref x)`, `Foo(out var y)`, `Foo(in z)`. Only ever
// produced at argument positions (the parser's `arg` rule); the inner
// expression is the actual value. Modeled as an Expr so every
// arg-bearing node keeps its `IReadOnlyList<Expr> Args` shape — the
// emitter prefixes the C# keyword and emits the inner expression.
public record RefArgExpr(SourceSpan Span, ParamModifier Modifier, Expr Inner) : Expr(Span);

// Inline out-variable declaration at a call site: `Foo(out var x)`.
// Only produced at argument positions. The variable is introduced into the
// enclosing scope (C# out-var semantics); emit is `out var x`.
public record OutVarExpr(SourceSpan Span, string Name) : Expr(Span);

// Named argument at a call site: `Foo(width: 3)`. Only produced at
// argument positions (parser's `arg` rule). Modeled as an Expr wrapping the
// value, same as RefArgExpr; the emitter prefixes `Name: ` and emits Value.
public record NamedArgExpr(SourceSpan Span, string Name, Expr Value) : Expr(Span);

// Array creation. Three shapes, all one node:
//   ElementType null              → implicit `new[] { e1, e2 }`
//   ElementType set, Size set      → sized `new T[n]` (Elements empty) or `new T[n] { ... }`
//   ElementType set, Size null     → typed `new T[] { e1, e2 }`
public record ArrayExpr(
    SourceSpan Span,
    IReadOnlyList<Expr> Elements,
    TypeRef? ElementType = null,
    Expr? Size = null
) : Expr(Span);

// Tuple literal: `(a, b)` (always two or more elements — a single
// parenthesized expression stays a ParenExpr). Emitted verbatim; C# infers
// the ValueTuple type.
public record TupleExpr(SourceSpan Span, IReadOnlyList<Expr> Elements) : Expr(Span);

// Throw expression: `x ?? throw new Foo()`. Distinct from the throw
// STATEMENT (ThrowStmt); emitted verbatim as `throw <Value>`.
public record ThrowExpr(SourceSpan Span, Expr Value) : Expr(Span);

// Default expression: `default` (Type null) or `default(T)`. First-class
// now that `default` is a keyword (was previously a bare-call coincidence);
// `default(T)` also handles generic types the bare-call form couldn't.
public record DefaultExpr(SourceSpan Span, TypeRef? Type) : Expr(Span);

// Bare function invocation: `name(args)` with no receiver.
// Used for free-standing factory functions imported via `using` (e.g.
// `debounce(500)` after `using Spek.Streams`). The compiler emits the
// callee verbatim — Roslyn resolves the import.
public record InvocationExpr(
    SourceSpan Span,
    string Callee,
    IReadOnlyList<Expr> Args
) : Expr(Span);

// Index access: expr[index]. NullConditional => emit `?[` instead of `[`.
public record IndexExpr(SourceSpan Span, Expr Target, Expr Index, bool NullConditional = false) : Expr(Span);

// Switch expression: `value switch { pattern => result, ... }`.
// Postfix on a primary expression; arms run in declaration order; the
// first matching pattern (with its optional `when` guard true) wins.
public record SwitchExpr(
    SourceSpan Span,
    Expr Subject,
    IReadOnlyList<SwitchArm> Arms
) : Expr(Span);

public record SwitchArm(
    SourceSpan Span,
    SwitchPattern Pattern,
    Expr? When,
    Expr Result
) : AstNode(Span);

public abstract record SwitchPattern(SourceSpan Span) : AstNode(Span);

/// <summary>Discard pattern (`_`) — matches anything.</summary>
public record DiscardPattern(SourceSpan Span) : SwitchPattern(Span);

/// <summary>Type pattern (`Foo` or `Foo bar`) — matches when the
/// subject is assignable to the declared type; optionally binds the
/// matched value to <paramref name="Binding"/>.</summary>
public record TypePattern(
    SourceSpan Span,
    TypeRef Type,
    string? Binding
) : SwitchPattern(Span);

/// <summary>Constant pattern (`1`, `"x"`, `MyEnum.Value`) — matches
/// when the subject equals the constant value.</summary>
public record ConstPattern(SourceSpan Span, Expr Value) : SwitchPattern(Span);

/// <summary>Relational pattern (`&gt; 0`, `&lt;= 100`) — matches when the
/// subject compares to the operand using the given operator. Maps
/// directly to C# 9 relational patterns; Roslyn enforces that the
/// operand is a constant expression.</summary>
public record RelationalPattern(
    SourceSpan Span,
    RelationalPatternOp Op,
    Expr Value
) : SwitchPattern(Span);

public enum RelationalPatternOp { Lt, Lte, Gt, Gte, Eq, Neq }

/// <summary>Property pattern (`{ X: 0, Inner.Y: > 0 }`) — matches when
/// every listed property/path matches its sub-pattern. Empty form
/// (`{ }`) matches any non-null subject.</summary>
public record PropertyPattern(
    SourceSpan Span,
    IReadOnlyList<PropertySubpattern> Properties
) : SwitchPattern(Span);

public record PropertySubpattern(
    SourceSpan Span,
    QualifiedName Path,
    SwitchPattern Inner
) : AstNode(Span);

/// <summary>Negation pattern (`not pat`). Emits as C# `not pat`.</summary>
public record NotPattern(SourceSpan Span, SwitchPattern Inner) : SwitchPattern(Span);

/// <summary>Conjunction pattern (`pat1 and pat2`). Left-associative.
/// Emits as C# `pat1 and pat2`.</summary>
public record AndPattern(SourceSpan Span, SwitchPattern Left, SwitchPattern Right) : SwitchPattern(Span);

/// <summary>Disjunction pattern (`pat1 or pat2`). Left-associative;
/// lower precedence than <see cref="AndPattern"/>. Emits as C# `pat1 or pat2`.</summary>
public record OrPattern(SourceSpan Span, SwitchPattern Left, SwitchPattern Right) : SwitchPattern(Span);

// new Foo(args)
public record NewExpr(
    SourceSpan Span,
    QualifiedName Type,
    IReadOnlyList<TypeRef> TypeArgs,
    IReadOnlyList<Expr> Args,
    // Object / collection initializer: `new T { a, b }`. Null when the
    // form has no braces. Each element is an expression (a `Prop = v` object
    // initializer is just an AssignExpr); the emitter wraps them in `{ ... }`.
    IReadOnlyList<Expr>? Initializer = null
) : Expr(Span);

// spawn<Foo>(args)
public record SpawnExpr(
    SourceSpan Span,
    IReadOnlyList<TypeRef> TypeArgs,
    IReadOnlyList<Expr> Args
) : Expr(Span);

// self, sender
public record SelfExpr(SourceSpan Span) : Expr(Span);
public record SenderExpr(SourceSpan Span) : Expr(Span);

// Lambda expression. Emits verbatim as a C# lambda; type
// inference, capture, and target-typing are handled by Roslyn at
// the C# layer.
//
//   x => x.Id                       LambdaParam{ Name="x", Type=null }
//   (int x, int y) => x + y         two LambdaParams with explicit types
//   () => 42                        empty params list
//   x => { return x + 1; }          block body (Body is BlockStmt)
//   x => x + 1                      expression body (Body is Expr)
public record LambdaExpr(
    SourceSpan Span,
    IReadOnlyList<LambdaParam> Parameters,
    AstNode Body                                    // Expr or BlockStmt
) : Expr(Span);

public record LambdaParam(
    SourceSpan Span,
    TypeRef? Type,                                  // null = inferred
    string Name
) : AstNode(Span);

// Identifier reference
public record NameExpr(SourceSpan Span, QualifiedName Name) : Expr(Span);

// Parenthesised expression
public record ParenExpr(SourceSpan Span, Expr Inner) : Expr(Span);

// ─── Literals ────────────────────────────────────────────────────────────────

public abstract record LiteralExpr(SourceSpan Span) : Expr(Span);

// Numeric/string literals carry the verbatim source lexeme in `Raw` (set
// when parsed from source). The emitter prints `Raw` so the exact C# form —
// digit separators, hex/binary, type suffixes, exponents, string escapes —
// passes through untouched. `Raw` is null for nodes built directly in tests,
// which fall back to formatting the parsed value.
public record IntLiteralExpr(SourceSpan Span, long Value, string? Raw = null) : LiteralExpr(Span);
public record DecimalLiteralExpr(SourceSpan Span, decimal Value, bool HasSuffix, string? Raw = null) : LiteralExpr(Span);
public record StringLiteralExpr(SourceSpan Span, string Value, string? Raw = null) : LiteralExpr(Span);
// A char literal is always emitted from its verbatim lexeme (e.g. `'a'`, `'\n'`).
public record CharLiteralExpr(SourceSpan Span, string Raw) : LiteralExpr(Span);
// An interpolated string, parsed structurally so each hole is a real Spek
// expression (field/self rewriting + invisible-async apply inside it).
// Prefix is the opening delimiter (`$"`, `$@"`, or `@$"`); the closing `"`
// is implicit. Parts alternate literal text (verbatim) with holes.
public record InterpolatedStringExpr(SourceSpan Span, string Prefix, IReadOnlyList<InterpolationPart> Parts) : LiteralExpr(Span);

public abstract record InterpolationPart(SourceSpan Span) : AstNode(Span);
public record InterpolationText(SourceSpan Span, string Text) : InterpolationPart(Span);
// Suffix is the verbatim `:format` / `,alignment` tail (including its leading
// `:` or `,`), or empty.
public record InterpolationHole(SourceSpan Span, Expr Expr, string Suffix) : InterpolationPart(Span);
public record BoolLiteralExpr(SourceSpan Span, bool Value) : LiteralExpr(Span);
public record NullLiteralExpr(SourceSpan Span) : LiteralExpr(Span);
