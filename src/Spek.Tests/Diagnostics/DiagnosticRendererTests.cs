using Spek.Compiler.AST;
using Spek.Compiler.Parser;
using Spek.Compiler.Semantic;
using Xunit;

namespace Spek.Tests.Diagnostics;

/// <summary>
/// Rust-style diagnostics. A <see cref="Diagnostic"/> carries the full
/// source <see cref="SourceSpan"/> (start AND end), and
/// <see cref="DiagnosticRenderer"/> turns it into a framed block with a
/// <c>--&gt;</c> location line and a caret underline pointing at the exact span.
/// </summary>
public sealed class DiagnosticRendererTests
{
    private static string CaretLine(string rendered) =>
        rendered.Split('\n').First(l => l.Contains('^'));

    [Fact]
    public void Header_Error_And_Location()
    {
        var outp = DiagnosticRenderer.Render("actor A\n{\n}", "a.spek",
            new Diagnostic("CE0042", 1, 7, "something is wrong"));
        Assert.StartsWith("error[CE0042]: something is wrong", outp);
        Assert.Contains("--> a.spek:1:7", outp);
    }

    [Fact]
    public void Header_Warning_Uses_Warning_Word()
    {
        var outp = DiagnosticRenderer.Render("module M {}", "m.spek",
            new Diagnostic("CE0107", 1, 1, "redundant", DiagnosticSeverity.Warning));
        Assert.StartsWith("warning[CE0107]: redundant", outp);
    }

    [Fact]
    public void Span_Underline_Has_Span_Width()
    {
        // "x = aaaa + bbbb" — underline cols 5..9 (the `aaaa`), width 4.
        var outp = DiagnosticRenderer.Render("x = aaaa + bbbb", "f.spek",
            Diagnostic.At("CE0001", new SourceSpan(1, 5, 1, 9), "msg"));
        Assert.Equal(4, CaretLine(outp).Count(c => c == '^'));
    }

    [Fact]
    public void Point_Diagnostic_Underlines_Whole_Word()
    {
        // No span (a point diagnostic) → the caret widens to cover the
        // identifier under the column rather than a single character.
        var outp = DiagnosticRenderer.Render("self.Ask(new GetBalance());", "f.spek",
            new Diagnostic("CE0020", 1, 14, "not a message"));   // col 14 = 'G'
        Assert.Equal("GetBalance".Length, CaretLine(outp).Count(c => c == '^'));
    }

    [Fact]
    public void Renders_The_Offending_Source_Line()
    {
        var src = "line one\nlet bad = here\nline three";
        var outp = DiagnosticRenderer.Render(src, "f.spek",
            new Diagnostic("CE0001", 2, 1, "x"));
        Assert.Contains("2 | let bad = here", outp);
    }

    [Fact]
    public void Real_Diagnostic_Carries_Span_And_Renders()
    {
        // CE0010: a message field of a known-mutable type. The analyzer now
        // builds it from the type's span, so the rendered caret hugs the type.
        const string src = "message Bad(List<int> items);";
        var parsed = SpekCompiler.Parse(src);

        var d = Assert.Single(parsed.Diagnostics, x => x.Code == "CE0010");
        Assert.NotNull(d.Span);

        var outp = DiagnosticRenderer.Render(src, "m.spek", d);
        Assert.Contains("error[CE0010]", outp);
        Assert.Contains("1 | message Bad(List<int> items);", outp);
        Assert.Contains('^', outp);
    }
}
