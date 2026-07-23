using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Spek.Tests.Emit;   // RoslynCompileHelper
using Xunit;

namespace Spek.Tests.Parser;

/// <summary>
/// Extended C# string forms beyond the plain <c>"..."</c>: verbatim
/// (<c>@"..."</c>), raw (<c>"""..."""</c>), and verbatim-interpolated
/// (<c>$@"..."</c> / <c>@$"..."</c>). Like all literals they emit verbatim,
/// so the exact C# form passes straight through. Each case must parse, emit
/// the form unchanged, and produce C# that compiles.
/// </summary>
public sealed class StringFormTests
{
    private static void AssertVerbatim(string spekLiteral, string prelude = "")
    {
        var src = $"program Main {{ {prelude}var probe = {spekLiteral}; System.Console.WriteLine(probe); }}";
        var parsed = SpekCompiler.Parse(src);
        Assert.True(parsed.Success,
            $"`{spekLiteral}` failed to parse: " +
            string.Join("; ", parsed.Diagnostics.Select(d => $"{d.Code} {d.Message}")));

        var code = new FileEmitter().Emit(parsed.Tree!);
        Assert.True(code.Contains($"= {spekLiteral};"),
            $"`{spekLiteral}` did not emit verbatim.\nEmitted: " +
            (code.Split('\n').FirstOrDefault(l => l.Contains("probe")) ?? "(probe line not found)"));

        var (ok, summary, _) = RoslynCompileHelper.TryCompile(code, "StringForm");
        Assert.True(ok, $"`{spekLiteral}` did not compile:\n{summary}");
    }

    // ── Verbatim strings: @"..." (backslashes literal, "" = escaped quote) ──
    [Fact]
    public void Verbatim_BackslashesAreLiteral() =>
        AssertVerbatim("@\"C:\\dir\\file.txt\"");          // @"C:\dir\file.txt"

    [Fact]
    public void Verbatim_DoubledQuoteIsEscape() =>
        AssertVerbatim("@\"a \"\"b\"\" c\"");              // @"a ""b"" c"

    // ── Raw strings: """...""" (no escape processing; may contain quotes) ──
    [Fact]
    public void Raw_NoEscapeProcessing() =>
        AssertVerbatim("\"\"\"raw \\ text\"\"\"");         // """raw \ text"""

    [Fact]
    public void Raw_MayContainQuotes() =>
        AssertVerbatim("\"\"\"He said \"hi\".\"\"\"");     // """He said "hi"."""

    // ── Verbatim-interpolated: $@"..." and @$"..." (hole is a local, so it
    //    round-trips verbatim; full interpolation semantics are covered in
    //    InterpolationTests, where holes are re-emitted as Spek expressions). ──
    [Fact]
    public void VerbatimInterpolated_DollarAt() =>
        AssertVerbatim("$@\"path={x}\"", prelude: "var x = 1; ");   // $@"path={x}"

    [Fact]
    public void VerbatimInterpolated_AtDollar() =>
        AssertVerbatim("@$\"path={x}\"", prelude: "var x = 1; ");   // @$"path={x}"
}
