using Spek.Compiler.AST;
using Spek.Compiler.Semantic;

namespace Spek.Compiler.Parser;

/// <summary>
/// Multi-file compilation unit. Parses every file, then runs semantic
/// analysis once with a combined <see cref="SymbolTable"/> so cross-file
/// references (a <c>message</c> declared in file A, referenced via
/// <c>ask</c> in file B) resolve without triggering false-positive
/// "undeclared message" diagnostics.
///
/// Namespaces are informational for now: resolution is flat-by-simple-name
/// across the whole compilation. Namespace-scoped resolution (with
/// <c>using</c>-aware lookup) is future work.
/// </summary>
public sealed record SpekCompilation(
    IReadOnlyList<SpekFile> Files,
    IReadOnlyList<Diagnostic> Diagnostics)
{
    /// <summary>
    /// True when no error-severity diagnostics were produced across
    /// any file in the compilation. Warning-severity diagnostics
    /// surface via <see cref="Diagnostics"/> without
    /// blocking a successful parse.
    /// </summary>
    public bool Success => !Diagnostics.Any(d => d.Severity == DiagnosticSeverity.Error);

    /// <summary>
    /// Parses and semantically analyses a set of Spek sources as a single
    /// compilation. Each tuple is (file-name-for-diagnostics, source-text).
    /// </summary>
    public static SpekCompilation Create(IEnumerable<(string Name, string Source)> sources)
    {
        var diagnostics = new List<Diagnostic>();
        var files = new List<SpekFile>();

        foreach (var (_, src) in sources)
        {
            var (tree, fileDiagnostics) = SpekCompiler.ParseToTree(src);
            diagnostics.AddRange(fileDiagnostics);
            if (tree is not null) files.Add(tree);
        }

        if (files.Count > 0)
        {
            var symbols = SymbolTable.BuildCombined(files);
            foreach (var file in files)
                diagnostics.AddRange(SemanticAnalyzer.Analyze(file, symbols));

            // Cross-file CE0013 — names that collide across files.
            diagnostics.AddRange(SemanticAnalyzer.CheckCrossFileDuplicates(files));
        }

        return new SpekCompilation(files, diagnostics);
    }
}
