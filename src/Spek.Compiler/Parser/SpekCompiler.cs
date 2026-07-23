using Antlr4.Runtime;
using Spek.Compiler.AST;
using Spek.Compiler.Grammar;
using Spek.Compiler.Semantic;

namespace Spek.Compiler.Parser;

public record CompilationResult(SpekFile? Tree, IReadOnlyList<Diagnostic> Diagnostics)
{
    /// <summary>
    /// True when no error-severity diagnostics were produced. Warning-
    /// severity diagnostics surface to callers via
    /// <see cref="Diagnostics"/> without blocking a successful parse.
    /// </summary>
    public bool Success => !Diagnostics.Any(d => d.Severity == DiagnosticSeverity.Error);
}

/// <summary>Entry point for parsing and semantically analysing Spek source text.</summary>
public static class SpekCompiler
{
    public static CompilationResult Parse(string source)
    {
        var (ast, diagnostics) = ParseToTree(source);
        if (ast is null) return new CompilationResult(null, diagnostics);

        // Semantic analysis, single-file scope. For cross-file resolution
        // use <see cref="SpekCompilation.Create"/> instead.
        diagnostics.AddRange(SemanticAnalyzer.Analyze(ast));
        return new CompilationResult(ast, diagnostics);
    }

    /// <summary>
    /// Parses <paramref name="source"/> to an AST without running semantic
    /// analysis. Used by <see cref="SpekCompilation"/> so the combined
    /// semantic pass runs once with a symbol table that unions all files.
    /// </summary>
    public static (SpekFile? Tree, List<Diagnostic> Diagnostics) ParseToTree(string source)
    {
        var diagnostics = new List<Diagnostic>();

        var input  = CharStreams.fromString(source);
        var lexer  = new SpekLexer(input);
        lexer.RemoveErrorListeners();
        lexer.AddErrorListener(new ErrorCollector(diagnostics));

        var tokens = new CommonTokenStream(lexer);
        var parser = new SpekParser(tokens);
        parser.RemoveErrorListeners();
        parser.AddErrorListener(new ErrorCollector(diagnostics));

        var tree = parser.file_();
        if (diagnostics.Count > 0) return (null, diagnostics);

        var ast = (SpekFile)new AstBuilder(tokens).Visit(tree);
        return (ast, diagnostics);
    }

    private sealed class ErrorCollector : IAntlrErrorListener<int>, IAntlrErrorListener<IToken>
    {
        private readonly List<Diagnostic> _diagnostics;
        public ErrorCollector(List<Diagnostic> diagnostics) => _diagnostics = diagnostics;

        public void SyntaxError(TextWriter output, IRecognizer recognizer,
            int offendingSymbol, int line, int charPositionInLine, string msg, RecognitionException e)
            => _diagnostics.Add(new Diagnostic("CE0001", line, charPositionInLine + 1, msg));

        public void SyntaxError(TextWriter output, IRecognizer recognizer,
            IToken offendingSymbol, int line, int charPositionInLine, string msg, RecognitionException e)
            => _diagnostics.Add(new Diagnostic("CE0001", line, charPositionInLine + 1, msg));
    }
}
