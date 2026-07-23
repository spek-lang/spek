using Spek.Compiler.AST;

namespace Spek.Compiler.Semantic;

/// <summary>
/// A compiler diagnostic — covers both syntax errors (from ANTLR) and semantic
/// errors (from <see cref="SemanticAnalyzer"/>). The <see cref="Code"/> is the
/// stable identifier shown to users and referenced in docs (e.g. "CE0011").
/// Syntax errors use "CE0001".
///
/// Diagnostics carry a <see cref="Severity"/>. Errors fail the build;
/// warnings surface to the developer but do not block compilation. Defaults
/// to <see cref="DiagnosticSeverity.Error"/> so existing call sites keep
/// their semantics without modification.
///
/// Diagnostics may also carry the full <see cref="Span"/> of the
/// offending source range (start AND end). <see cref="Line"/>/<see cref="Column"/>
/// remain the 1-based start position (kept for back-compat and for ANTLR
/// syntax errors, which only know a point). When <see cref="Span"/> is present,
/// the CLI underlines the exact range Rust-style and the language server lights
/// up a precise squiggle instead of guessing a single character.
/// </summary>
public sealed record Diagnostic(
    string Code,
    int Line,
    int Column,
    string Message,
    DiagnosticSeverity Severity = DiagnosticSeverity.Error,
    SourceSpan? Span = null)
{
    /// <summary>Builds a diagnostic from a source <see cref="SourceSpan"/>,
    /// deriving <see cref="Line"/>/<see cref="Column"/> from its start so the
    /// underline width comes from the span while point-based consumers keep
    /// working.</summary>
    public static Diagnostic At(
        string code, SourceSpan span, string message,
        DiagnosticSeverity severity = DiagnosticSeverity.Error)
        => new(code, span.StartLine, span.StartColumn, message, severity, span);

    public override string ToString() => $"{Code} ({Line}:{Column}) {Message}";
}

/// <summary>Distinguishes build-blocking errors from informational warnings.</summary>
public enum DiagnosticSeverity
{
    Error,
    Warning,
    // Editor-only severities (don't fail the build) for ReSharper-style
    // suggestions — "this works, but here's the idiomatic form". Information
    // shows as a normal info squiggle; Hint is the faintest (often just a
    // quick-fix lightbulb).
    Information,
    Hint,
}
