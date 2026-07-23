using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Xunit;
using Xunit.Abstractions;

namespace Spek.Tests.Emit;

/// <summary>
/// Language-completeness — control flow: <c>foreach</c>, <c>do/while</c>,
/// <c>break</c>, <c>continue</c>. These lower verbatim to the matching C#
/// constructs, so the round-trips prove the emitted C# actually compiles. The
/// last test proves the analyzer's statement walkers descend into the new loop
/// bodies (a brand-new statement kind that silently swallowed nested analysis
/// would be an unsoundness hole).
/// </summary>
public sealed class ControlFlowTests(ITestOutputHelper output)
{
    private static string EmitCSharp(string source)
    {
        var parsed = SpekCompiler.Parse(source);
        Assert.True(parsed.Success,
            "Source must parse + analyze; diagnostics: " +
            string.Join("\n", parsed.Diagnostics.Select(d => $"{d.Code} {d.Line}:{d.Column} {d.Message}")));
        return new FileEmitter().Emit(parsed.Tree!);
    }

    private void AssertCompiles(string source, string name, params string[] mustContain)
    {
        var code = EmitCSharp(source);
        foreach (var s in mustContain) Assert.Contains(s, code);
        var (ok, summary, _) = RoslynCompileHelper.TryCompile(code, name);
        if (!ok)
        {
            output.WriteLine("─── emitted C# ───");
            output.WriteLine(code);
            output.WriteLine("─── diagnostics ───");
            output.WriteLine(summary);
        }
        Assert.True(ok, $"Emitted C# did not compile:\n{summary}");
    }

    [Fact]
    public void Foreach_VarLoopVariable_RoundTrips()
    {
        const string src = """
            module L
            {
                public int Sum(System.Collections.Generic.List<int> xs)
                {
                    int total = 0;
                    foreach (var x in xs) { total = total + x; }
                    return total;
                }
            }
            """;
        AssertCompiles(src, "ForeachVar", "foreach (var x in xs)");
    }

    [Fact]
    public void Foreach_ExplicitTypeLoopVariable_RoundTrips()
    {
        const string src = """
            module L
            {
                public int Sum(System.Collections.Generic.List<int> xs)
                {
                    int total = 0;
                    foreach (int x in xs) { total = total + x; }
                    return total;
                }
            }
            """;
        AssertCompiles(src, "ForeachTyped", "foreach (int x in xs)");
    }

    [Fact]
    public void DoWhile_RoundTrips()
    {
        const string src = """
            module L
            {
                public int CountTo(int max)
                {
                    int n = 0;
                    do { n = n + 1; } while (n < max);
                    return n;
                }
            }
            """;
        AssertCompiles(src, "DoWhile", "do", "while ((n < max));");
    }

    [Fact]
    public void BreakAndContinue_InForeach_RoundTrip()
    {
        const string src = """
            module L
            {
                public int FirstBig(System.Collections.Generic.List<int> xs)
                {
                    int found = -1;
                    foreach (var x in xs)
                    {
                        if (x < 0) { continue; }
                        if (x > 10) { found = x; break; }
                    }
                    return found;
                }
            }
            """;
        AssertCompiles(src, "BreakContinue", "continue;", "break;");
    }

    [Fact]
    public void Analysis_DescendsIntoForeachBody()
    {
        // CE0011 (become target must be a declared behavior) lives in WalkStmt's
        // BecomeStmt case. If WalkStmt didn't recurse into the foreach body, the
        // bad `become` would go unnoticed. Its presence proves the descent.
        const string src = """
            message Tick();
            actor Counter
            {
                System.Collections.Generic.List<int> xs = new System.Collections.Generic.List<int>();
                writer on Tick t =>
                {
                    foreach (var i in xs)
                    {
                        become Missing;
                    }
                }
            }
            """;
        var result = SpekCompiler.Parse(src);
        Assert.Contains(result.Diagnostics, d => d.Code == "CE0011");
    }
}
