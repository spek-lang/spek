using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Xunit;
using Xunit.Abstractions;

namespace Spek.Tests.Emit;

/// <summary>
/// Throw expressions in the null-coalescing position:
/// `x ?? throw new Foo()`. Parsed via the optional `?? throwExpr` tail on
/// `coalesceExpr`, modeled as ThrowExpr, emitted verbatim as `throw <expr>`.
/// </summary>
public sealed class ThrowExpressionTests(ITestOutputHelper output)
{
    private static string EmitCSharp(string source)
    {
        var parsed = SpekCompiler.Parse(source);
        Assert.True(parsed.Success,
            string.Join("\n", parsed.Diagnostics.Select(d => $"{d.Code} {d.Message}")));
        return new FileEmitter().Emit(parsed.Tree!);
    }

    [Fact]
    public void NullCoalescingThrow_EmitAndCompile()
    {
        const string src = """
            module M
            {
                public string Require(string? s)
                {
                    return s ?? throw new System.ArgumentNullException();
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("s ?? throw new System.ArgumentNullException()", code);

        var (ok, summary, _) = RoslynCompileHelper.TryCompile(code, "ThrowExpr");
        if (!ok) { output.WriteLine(code); output.WriteLine(summary); }
        Assert.True(ok, $"Emitted C# did not compile:\n{summary}");
    }

    [Fact]
    public void PlainCoalesce_StillWorks()
    {
        // Regression: a normal `??` chain (no throw) must be unaffected.
        const string src = """
            module M
            {
                public string Pick(string? a, string b) { return a ?? b; }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("a ?? b", code);
        Assert.DoesNotContain("throw", code);
    }
}
