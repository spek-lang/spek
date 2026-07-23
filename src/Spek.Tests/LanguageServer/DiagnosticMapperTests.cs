using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using Spek.LanguageServer;
using Xunit;
using SpekDiag = Spek.Compiler.Semantic.Diagnostic;
using SpekSeverity = Spek.Compiler.Semantic.DiagnosticSeverity;
using SourceSpan = Spek.Compiler.AST.SourceSpan;

namespace Spek.Tests.LanguageServer;

public class DiagnosticMapperTests
{
    [Fact]
    public void ToLsp_ConvertsCodeLineAndMessage()
    {
        var d = new SpekDiag("CE0011", 5, 10, "'become Foo' — Foo is not declared");

        var lsp = DiagnosticMapper.ToLsp(d);

        Assert.Equal("CE0011", lsp.Code?.String);
        Assert.Equal("'become Foo' — Foo is not declared", lsp.Message);
        Assert.Equal("spek", lsp.Source);
        Assert.Equal(DiagnosticSeverity.Error, lsp.Severity);
    }

    [Fact]
    public void ToLsp_ConvertsOneBasedToZeroBased()
    {
        // Spek: line 5, col 10 (both 1-based) → LSP: line 4, col 9 (both 0-based).
        var d = new SpekDiag("CE0042", 5, 10, "ask used outside on");

        var lsp = DiagnosticMapper.ToLsp(d);

        Assert.Equal(4, lsp.Range.Start.Line);
        Assert.Equal(9, lsp.Range.Start.Character);
        // Range is single-character so squiggle lights up under the first token.
        Assert.Equal(4, lsp.Range.End.Line);
        Assert.Equal(10, lsp.Range.End.Character);
    }

    [Fact]
    public void ToLsp_LineOneColumnOne_MapsToZeroZero()
    {
        var d = new SpekDiag("CE0001", 1, 1, "parse error at start of file");

        var lsp = DiagnosticMapper.ToLsp(d);

        Assert.Equal(0, lsp.Range.Start.Line);
        Assert.Equal(0, lsp.Range.Start.Character);
    }

    [Fact]
    public void ToLsp_ClampsNegativePositions()
    {
        // Synthetic edge case — the analyzer shouldn't produce these, but
        // the mapper clamps rather than throws.
        var d = new SpekDiag("CE9999", 0, 0, "synthetic");

        var lsp = DiagnosticMapper.ToLsp(d);

        Assert.Equal(0, lsp.Range.Start.Line);
        Assert.Equal(0, lsp.Range.Start.Character);
    }

    [Fact]
    public void ToLsp_UsesSpanForRange_WhenPresent()
    {
        // A diagnostic carrying a real span underlines the whole range, not
        // just the single-character fallback.
        var d = SpekDiag.At("CE0010", new SourceSpan(5, 10, 5, 18), "mutable field type");

        var lsp = DiagnosticMapper.ToLsp(d);

        Assert.Equal(4, lsp.Range.Start.Line);
        Assert.Equal(9, lsp.Range.Start.Character);
        Assert.Equal(4, lsp.Range.End.Line);
        Assert.Equal(17, lsp.Range.End.Character);   // span end, not start+1
    }

    [Fact]
    public void ToLsp_MapsWarningSeverity()
    {
        // Previously every code mapped to Error; warnings (CE0107, CE0101, …)
        // must now surface as warning squiggles.
        var d = new SpekDiag("CE0107", 3, 9, "redundant Task<T>", SpekSeverity.Warning);

        var lsp = DiagnosticMapper.ToLsp(d);

        Assert.Equal(DiagnosticSeverity.Warning, lsp.Severity);
    }
}
