using BenchmarkDotNet.Attributes;
using Spek.Compiler.AST;
using Spek.Compiler.Parser;
using Spek.Compiler.Semantic;

namespace Spek.Benchmarks.Toolchain;

/// <summary>
/// The error path: what a big file costs when it is broken. A developer
/// mid-edit hits this path far more often than the clean one — every
/// keystroke that leaves a construct half-typed reparses with ANTLR error
/// recovery engaged, and every rendered CE diagnostic re-scans the source
/// for its caret frame.
/// </summary>
[MemoryDiagnoser]
public class DiagnosticRenderBenchmarks
{
    private const int DocLines = 5000;

    private string _cleanSource = null!;
    private string _corruptedSource = null!;
    private Diagnostic _diagnostic = null!;

    [GlobalSetup]
    public void Setup()
    {
        _cleanSource = SyntheticSpekSource.Generate(DocLines);

        // Break one behavior declaration in the MIDDLE cell —
        // `behavior 123 Draining` — so the parser recovers roughly at line
        // 2,500 and finishes the remaining half of the file post-recovery.
        var midCell = SyntheticSpekSource.CellCount(DocLines) / 2;
        var cellStart = _cleanSource.IndexOf($"// ─── Cell {midCell}:", StringComparison.Ordinal);
        var target = _cleanSource.IndexOf("behavior Draining", cellStart, StringComparison.Ordinal);
        _corruptedSource = _cleanSource[..target]
            + "behavior 123 Draining"
            + _cleanSource[(target + "behavior Draining".Length)..];

        // Guards: the corrupted source must actually fail, the clean one pass.
        var corrupted = SpekCompiler.Parse(_corruptedSource);
        if (corrupted.Success)
            throw new InvalidOperationException("corrupted source unexpectedly parsed clean");
        if (!SpekCompiler.Parse(_cleanSource).Success)
            throw new InvalidOperationException("clean source failed to parse");

        // A CE0087-shaped diagnostic aimed at a mid-file reader handler, with
        // a real span so the renderer takes the caret-underline path.
        var line = LineOfFirstAfter(_cleanSource, cellStart, "reader on QueryState");
        _diagnostic = Diagnostic.At(
            "CE0087",
            new SourceSpan(line, 9, line, 15),
            "'reader on ...' handler may not mutate actor field 'accepted' (synthetic benchmark diagnostic).");
    }

    /// <summary>Baseline: parse + single-file analysis of the clean
    /// 5,000-line source — the same work CompilerBenchmarks.ParseAndAnalyze
    /// measures, repeated here so the error-path methods read as a ratio in
    /// one table.</summary>
    [Benchmark(Baseline = true)]
    public CompilationResult ParseClean() => SpekCompiler.Parse(_cleanSource);

    /// <summary>Parse of the same source with one syntax error mid-file.
    /// ANTLR error recovery itself (sync-token scanning at the failure
    /// point) adds little, and the error path then SKIPS work: once any
    /// syntax diagnostic exists, ParseToTree returns before the AstBuilder
    /// visit and semantic analysis never runs. Measured result: the broken
    /// file costs about HALF the clean one (rough ratio 0.45–0.55 across dry
    /// and short runs, ~0.6x allocations) — the ratio column is the number
    /// to watch, and a drift upward past 1.0 would mean recovery started
    /// thrashing.</summary>
    [Benchmark]
    public CompilationResult ParseWithMidFileSyntaxError() => SpekCompiler.Parse(_corruptedSource);

    /// <summary>DiagnosticRenderer.Render of one span-carrying diagnostic
    /// against the 5,000-line source. The renderer re-normalizes and splits
    /// the ENTIRE source into lines on every call (Replace + Split), so the
    /// cost — and one full string[] copy of the file — scales with document
    /// size, not diagnostic count, and repeats per diagnostic rendered.
    /// Rough short-run figure: ~100 µs / 355 KB per render on 5,000
    /// lines.</summary>
    [Benchmark]
    public string RenderDiagnostic() =>
        DiagnosticRenderer.Render(_cleanSource, "Synthetic.spek", _diagnostic);

    /// <summary>1-based line number of the first occurrence of
    /// <paramref name="anchor"/> at or after <paramref name="startIndex"/>.</summary>
    private static int LineOfFirstAfter(string source, int startIndex, string anchor)
    {
        var index = source.IndexOf(anchor, startIndex, StringComparison.Ordinal);
        if (index < 0) throw new InvalidOperationException($"anchor '{anchor}' not found");
        int line = 1;
        for (int i = 0; i < index; i++)
            if (source[i] == '\n') line++;
        return line;
    }
}
