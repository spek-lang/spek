namespace Spek.Compiler.AST;

// ─── Statements ───────────────────────────────────────────────────────────────

public abstract record Stmt(SourceSpan Span) : AstNode(Span);

public record BlockStmt(SourceSpan Span, IReadOnlyList<Stmt> Statements) : Stmt(Span);

public record BecomeStmt(SourceSpan Span, string BehaviorName) : Stmt(Span);

public record PersistStmt(SourceSpan Span) : Stmt(Span);

public record ReturnStmt(SourceSpan Span, Expr? Value) : Stmt(Span);

// `IsUsing` marks a `using var x = ...` declaration (deterministic
// disposal at scope end); emitted with a `using` prefix.
public record VarDeclStmt(SourceSpan Span, TypeRef? Type, string Name, Expr Initializer, bool IsUsing = false) : Stmt(Span);

public record IfStmt(
    SourceSpan Span,
    Expr Condition,
    BlockStmt Then,
    Stmt? Else
) : Stmt(Span);

public record ForStmt(
    SourceSpan Span,
    VarDeclStmt Init,
    Expr Condition,
    Expr Increment,
    BlockStmt Body
) : Stmt(Span);

public record WhileStmt(SourceSpan Span, Expr Condition, BlockStmt Body) : Stmt(Span);

// `foreach (Type? name in Collection) Body`. Type is null for `var`.
public record ForeachStmt(SourceSpan Span, TypeRef? Type, string Name, Expr Collection, BlockStmt Body) : Stmt(Span);

// `do Body while (Condition);`
public record DoWhileStmt(SourceSpan Span, BlockStmt Body, Expr Condition) : Stmt(Span);

// `break;` / `continue;` (leaf statements; valid only inside a loop —
// the emitted C# is the arbiter, so a stray one is a C# error, not a Spek CE).
public record BreakStmt(SourceSpan Span) : Stmt(Span);
public record ContinueStmt(SourceSpan Span) : Stmt(Span);

public record ExpressionStmt(SourceSpan Span, Expr Expr) : Stmt(Span);

/// <summary>
/// Local exception handling. Mirrors C#: a try block, zero or
/// more typed catch clauses (with optional <c>when</c> guards),
/// optional finally. An exception that escapes every catch clause
/// (or is uncaught) propagates to the slot's supervision flow.
/// </summary>
public record TryStmt(
    SourceSpan Span,
    BlockStmt Try,
    IReadOnlyList<CatchClause> Catches,
    BlockStmt? Finally
) : Stmt(Span);

public record CatchClause(
    SourceSpan Span,
    TypeRef? ExceptionType,   // null = catch-all
    string? Binding,           // null = no binding
    Expr? When,                // null = no guard
    BlockStmt Body
) : AstNode(Span);

/// <summary>
/// Explicit throw statement. <c>Value</c> is null for a bare
/// <c>throw;</c> (re-throw within a catch clause).
/// </summary>
public record ThrowStmt(SourceSpan Span, Expr? Value) : Stmt(Span);

// C-style switch statement. Each section has one or more case/default
// labels (CaseLabel.Pattern null = `default:`) and a statement body. Case-label
// patterns reuse the switch-expression SwitchPattern hierarchy.
public record SwitchStmt(SourceSpan Span, Expr Subject, IReadOnlyList<SwitchSection> Sections) : Stmt(Span);
public record SwitchSection(SourceSpan Span, IReadOnlyList<CaseLabel> Labels, IReadOnlyList<Stmt> Body) : AstNode(Span);
public record CaseLabel(SourceSpan Span, SwitchPattern? Pattern, Expr? Guard) : AstNode(Span);
