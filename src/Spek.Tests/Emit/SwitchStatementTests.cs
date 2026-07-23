using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Xunit;
using Xunit.Abstractions;

namespace Spek.Tests.Emit;

/// <summary>
/// C-style switch statement: value-less multi-way branch with case/default
/// labels and statement bodies. Case labels reuse the switch-expression pattern
/// rule (type patterns, `when` guards). Emitted verbatim; Roslyn enforces
/// break/exhaustiveness.
/// </summary>
public sealed class SwitchStatementTests(ITestOutputHelper output)
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
    public void SwitchStatement_CasesAndDefault_EmitAndCompile()
    {
        const string src = """
            module M
            {
                public string Classify(int code)
                {
                    switch (code)
                    {
                        case 200:
                            return "ok";
                        case 404:
                        case 500:
                            return "error";
                        default:
                            return "unknown";
                    }
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("switch (code)", code);
        Assert.Contains("case 200:", code);
        Assert.Contains("case 404:", code);   // shared-label fall-through
        Assert.Contains("default:", code);
        AssertCompiles(code, "SwitchStmt");
    }

    [Fact]
    public void SwitchStatement_TypePatternWithGuard_EmitAndCompile()
    {
        const string src = """
            module M
            {
                public string Describe(object o)
                {
                    switch (o)
                    {
                        case int n when n > 0:
                            return "positive";
                        case string s:
                            return s;
                        default:
                            return "other";
                    }
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("case int n when", code);   // pattern + guard
        Assert.Contains("case string s:", code);    // pattern binding used in body
        AssertCompiles(code, "SwitchPattern");
    }
}
