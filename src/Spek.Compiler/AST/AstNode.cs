namespace Spek.Compiler.AST;

/// <summary>Base class for every AST node.</summary>
public abstract record AstNode(SourceSpan Span)
{
    /// <summary>
    /// The <c>///</c> XML doc-comment block written immediately above this
    /// declaration, verbatim (each entry is one <c>/// …</c> line). Carried
    /// through transpilation and re-emitted above the generated C# declaration
    /// so a converted <c>.spek</c> keeps its public API documentation. Null
    /// when the declaration has no doc comment.
    /// </summary>
    public IReadOnlyList<string>? DocComment { get; init; }
}

/// <summary>
/// Source location range. Lines and columns are 1-based; <see cref="EndColumn"/>
/// is EXCLUSIVE (one past the last character), so a span's single-line width is
/// <c>EndColumn - StartColumn</c>. Every producer (parser spans, name-span
/// approximations) and consumer (caret renderer, LSP range mapping) follows
/// this convention.
/// </summary>
public record SourceSpan(int StartLine, int StartColumn, int EndLine, int EndColumn)
{
    public static readonly SourceSpan None = new(0, 0, 0, 0);
}
