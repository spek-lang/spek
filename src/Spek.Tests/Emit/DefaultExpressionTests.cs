using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Xunit;
using Xunit.Abstractions;

namespace Spek.Tests.Emit;

/// <summary>
/// `default` / `default(T)` as first-class expressions. Necessary once
/// `default` became a keyword (for the switch statement); also fixes the
/// generic form `default(List&lt;int&gt;)` that the old bare-call coincidence
/// couldn't parse.
/// </summary>
public sealed class DefaultExpressionTests(ITestOutputHelper output)
{
    private static string EmitCSharp(string source)
    {
        var parsed = SpekCompiler.Parse(source);
        Assert.True(parsed.Success,
            string.Join("\n", parsed.Diagnostics.Select(d => $"{d.Code} {d.Message}")));
        return new FileEmitter().Emit(parsed.Tree!);
    }

    private void AssertCompiles(string code, string name)
    {
        var (ok, summary, _) = RoslynCompileHelper.TryCompile(code, name);
        if (!ok) { output.WriteLine(code); output.WriteLine(summary); }
        Assert.True(ok, $"Emitted C# did not compile:\n{summary}");
    }

    [Fact]
    public void DefaultOfType_StillWorks_AfterKeyword()
    {
        // Regression: default(int) worked before as a bare-call; must keep working
        // now that `default` is a keyword (now via DefaultExpr).
        const string src = """
            module M
            {
                public int Z() { return default(int); }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("default(int)", code);
        AssertCompiles(code, "DefaultInt");
    }

    [Fact]
    public void DefaultOfGenericType_NowWorks()
    {
        // The incidental win: the generic-arg form the bare-call form couldn't parse.
        const string src = """
            module M
            {
                public object Z() { return default(System.Collections.Generic.List<int>); }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("default(System.Collections.Generic.List<int>)", code);
        AssertCompiles(code, "DefaultGeneric");
    }

    [Fact]
    public void BareDefault_EmitAndCompile()
    {
        const string src = """
            module M
            {
                public int Z() { int x = default; return x; }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("int x = default;", code);
        AssertCompiles(code, "BareDefault");
    }
}
