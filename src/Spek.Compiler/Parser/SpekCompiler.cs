using Antlr4.Runtime;
using Antlr4.Runtime.Atn;
using Antlr4.Runtime.Misc;
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
        // Two-stage ALL(*) parse. Stage 1 predicts in SLL mode — the
        // cacheable, context-free approximation, far cheaper in
        // adaptive-prediction transients than full LL — with a bail strategy
        // and no error listeners; the grammar is SLL-clean for valid Spek
        // source, so every clean file finishes here. Stage 2 (rare) re-parses
        // from scratch in full LL with the normal error collectors. A bail is
        // retried rather than trusted because an SLL conflict can resolve to
        // a different alternative than full-context prediction would choose,
        // so a bailed input may still be valid — and because stage 1 collects
        // nothing, the full-LL pass reproduces the pre-two-stage diagnostics
        // exactly (lexer and parser errors interleaved in token order) when
        // the source really is invalid.
        var lexer  = new SpekLexer(CharStreams.fromString(source));
        lexer.RemoveErrorListeners();
        var lexerErrors = new ErrorCounter();
        lexer.AddErrorListener(lexerErrors);

        var tokens = new CommonTokenStream(lexer);
        var parser = new SpekParser(tokens);
        parser.RemoveErrorListeners();
        parser.Interpreter.PredictionMode = PredictionMode.SLL;
        parser.ErrorHandler = new BailErrorStrategy();

        try
        {
            var tree = parser.file_();
            // A bad character doesn't bail — the lexer skips it and the
            // parse can still succeed — but it must surface as a diagnostic,
            // so a lexer error also falls through to the full-LL stage.
            if (lexerErrors.Count == 0)
                return ((SpekFile)new AstBuilder(tokens).Visit(tree), []);
        }
        catch (ParseCanceledException)
        {
            // SLL guessed wrong, or the source is invalid — stage 2 decides.
        }

        // Stage 2: full LL with fresh lexer/parser state (re-lexing re-fires
        // the lexer diagnostics in token order) and default error recovery.
        var diagnostics = new List<Diagnostic>();

        var input  = CharStreams.fromString(source);
        var lexer2 = new SpekLexer(input);
        lexer2.RemoveErrorListeners();
        lexer2.AddErrorListener(new ErrorCollector(diagnostics));

        var tokens2 = new CommonTokenStream(lexer2);
        var parser2 = new SpekParser(tokens2);
        parser2.RemoveErrorListeners();
        parser2.AddErrorListener(new ErrorCollector(diagnostics));

        var tree2 = parser2.file_();
        if (diagnostics.Count > 0) return (null, diagnostics);

        var ast = (SpekFile)new AstBuilder(tokens2).Visit(tree2);
        return (ast, diagnostics);
    }

    /// <summary>Stage-1 lexer listener: counts errors without rendering
    /// them — any hit reroutes the parse to the diagnostic-collecting
    /// full-LL stage.</summary>
    private sealed class ErrorCounter : IAntlrErrorListener<int>
    {
        public int Count { get; private set; }

        public void SyntaxError(TextWriter output, IRecognizer recognizer,
            int offendingSymbol, int line, int charPositionInLine, string msg, RecognitionException e)
            => Count++;
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
