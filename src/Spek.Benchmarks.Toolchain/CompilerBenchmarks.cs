using BenchmarkDotNet.Attributes;
using Spek.Compiler.AST;
using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Spek.Compiler.Semantic;

namespace Spek.Benchmarks.Toolchain;

/// <summary>
/// Batch-compile latency: what one `spekc compile` invocation pays per file,
/// split into the pipeline's stages so a regression names its own stage. The
/// staged methods mirror CliRunner exactly — ParseToTree, then
/// SymbolTable.BuildCombined, then SemanticAnalyzer.Analyze against the
/// combined table, then FileEmitter.Emit with #line mapping on (the CLI
/// default) — so ParseAndEmit is the honest full path a build pays for one
/// clean file, minus only disk I/O and diagnostic rendering (both zero-cost
/// on a clean compile).
/// </summary>
[MemoryDiagnoser]
public class CompilerBenchmarks
{
    /// <summary>Synthetic source size in lines (~24 bytes/line). 100 is a
    /// sample-sized file, 1,000 a large real-world module, 5,000 the
    /// pathological single file that exposes super-linear passes.</summary>
    [Params(100, 1000, 5000)]
    public int Lines;

    private string _source = null!;
    private SpekFile _tree = null!;
    private SymbolTable _symbols = null!;
    private readonly FileEmitter _emitter = new();

    [GlobalSetup]
    public void Setup()
    {
        _source = SyntheticSpekSource.Generate(Lines);

        // Guard: the benchmark must measure the clean-compile path. A
        // diagnostic here means the generator drifted from the grammar.
        var (tree, parseDiags) = SpekCompiler.ParseToTree(_source);
        if (tree is null || parseDiags.Count > 0)
            throw new InvalidOperationException(
                $"synthetic source failed to parse: {parseDiags.FirstOrDefault()}");
        _tree = tree;
        _symbols = SymbolTable.BuildCombined([_tree]);
        var semDiags = SemanticAnalyzer.Analyze(_tree, _symbols);
        if (semDiags.Count > 0)
            throw new InvalidOperationException(
                $"synthetic source is not diagnostic-clean: {semDiags[0]}");
    }

    /// <summary>ANTLR lex + parse + AST build, no semantic analysis — the
    /// floor under every other toolchain path. Since perf r14 valid source
    /// predicts in SLL mode with a full-LL retry only on bail (see
    /// SpekCompiler.ParseToTree), which cut the adaptive-prediction
    /// transients that used to dominate this path. Rough short-run figures
    /// (Apple M5; a proper baseline run supersedes these): ~0.35 / 4.7 /
    /// 49 ms at 100 / 1k / 5k lines at ~5.6 MB allocated per KLOC — time
    /// still grows super-linearly past 1k (10x for 5x the lines) while
    /// allocations stay linear, so the residual growth is GC pressure, not
    /// more garbage per line.</summary>
    [Benchmark]
    public SpekFile? ParseOnly() => SpekCompiler.ParseToTree(_source).Tree;

    /// <summary>Parse plus single-file semantic analysis
    /// (SpekCompiler.Parse) — the exact work DocumentCache.Update runs on
    /// every LSP didChange. The delta over ParseOnly is the analyzer walk
    /// (symbol table build + every CE rule). Rough short-run figures: ~1.5 /
    /// 9.2 / 90–194 ms at 100 / 1k / 5k. The 5k mean swings run-to-run
    /// because the ~46 MB allocated per op forces a Gen2 collection per
    /// parse — watch the Gen2 column with the mean.</summary>
    [Benchmark]
    public CompilationResult ParseAndAnalyze() => SpekCompiler.Parse(_source);

    /// <summary>The full single-file build path, staged exactly as CliRunner
    /// stages it: ParseToTree, combined symbol table, semantic analysis
    /// against that table, then FileEmitter.Emit with #line mapping (the CLI
    /// default). Emit includes the invisible-async pass — a Roslyn parse and
    /// CSharpCompilation over the emitted C# — which dominates: roughly
    /// 70–90% of this method's time in short-run passes (~59 / 340 / 790 ms
    /// at 100 / 1k / 5k, ~18 MB allocated per KLOC; rough, high variance —
    /// see EmitOnly). Excludes only disk I/O and diagnostic rendering, both
    /// zero on a clean compile.</summary>
    [Benchmark]
    public string ParseAndEmit()
    {
        var (tree, _) = SpekCompiler.ParseToTree(_source);
        var symbols = SymbolTable.BuildCombined([tree!]);
        if (SemanticAnalyzer.Analyze(tree!, symbols).Count > 0)
            throw new InvalidOperationException("expected a clean analysis");
        return _emitter.Emit(tree!, symbols, sourceFileName: "Synthetic.spek");
    }

    /// <summary>C# emission alone, against a pre-parsed tree and pre-built
    /// symbol table — the marginal cost of the backend. Dominated by the
    /// invisible-async pass: AsyncRewriter parses the provisional C# with
    /// Roslyn and builds a BCL-referenced CSharpCompilation to find
    /// Task-returning calls to auto-await. Two costs to know about: a
    /// process-cold floor of roughly 1.4 s for the first emit in a process
    /// (Roslyn JIT + reference loading — the whole dry-run mean at 100
    /// lines), and a warmed per-file cost of roughly ~36 / 310 / 550 ms at
    /// 100 / 1k / 5k in short-run passes (rough, high run-to-run variance).
    /// Marginal allocations are linear at ~8 MB per KLOC over a ~10 MB
    /// fixed floor.</summary>
    [Benchmark]
    public string EmitOnly() => _emitter.Emit(_tree, _symbols, sourceFileName: "Synthetic.spek");
}
