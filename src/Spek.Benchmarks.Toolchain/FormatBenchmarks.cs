using BenchmarkDotNet.Attributes;
using Spek.Compiler.Format;

namespace Spek.Benchmarks.Toolchain;

/// <summary>
/// Formatter latency. SpekFormatter.Format is a pure string-to-string
/// function in Spek.Compiler — the same core both `spekc format` (CliRunner)
/// and the LSP textDocument/formatting handler call — so we invoke it
/// directly and pay no file I/O; the CLI adds only ReadAllText/WriteAllText
/// around this call, and format-on-save in an editor pays exactly this.
/// </summary>
[MemoryDiagnoser]
public class FormatBenchmarks
{
    private const int DocLines = 1000;

    private string _source = null!;

    [GlobalSetup]
    public void Setup()
    {
        _source = SyntheticSpekSource.Generate(DocLines);
        // Guard: formatting must succeed, or the ops would measure a throw.
        _ = SpekFormatter.Format(_source);
    }

    /// <summary>Formats a 1,000-line document. The formatter re-lexes the
    /// source (comments ride along as hidden-channel tokens) and re-prints
    /// with canonical indentation, so the cost is one lexer pass plus string
    /// building — no parse, no semantic analysis. Rough short-run figure
    /// (Apple M5): ~1 ms / 2.1 MB per 1,000-line document, an order of
    /// magnitude under the reparse of the same document — comfortably cheap
    /// for format-on-save.</summary>
    [Benchmark]
    public string FormatDocument() => SpekFormatter.Format(_source);
}
