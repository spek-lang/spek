using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Xunit;
using Xunit.Abstractions;

namespace Spek.Tests.Emit;

/// <summary>
/// Language-completeness — null-conditional access: <c>?.</c> (member and
/// method) and <c>?[</c> (index). Implemented as a flag on the existing
/// MemberAccessExpr / MethodCallExpr / IndexExpr nodes (no new AST node, so all
/// analysis walkers already cover them); the emitter chooses <c>?.</c>/<c>?[</c>
/// vs <c>.</c>/<c>[</c>. The TernaryStillParses test guards that <c>?.</c> didn't
/// swallow the conditional <c>? :</c> operator.
/// </summary>
public sealed class NullConditionalTests(ITestOutputHelper output)
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
    public void NullConditionalMember_RoundTrips()
    {
        const string src = """
            module N { public int LenOrZero(string s) { return s?.Length ?? 0; } }
            """;
        AssertCompiles(src, "NullMember", "s?.Length");
    }

    [Fact]
    public void NullConditionalMethod_RoundTrips()
    {
        const string src = """
            module N { public string Up(string s) { return s?.ToUpper(); } }
            """;
        AssertCompiles(src, "NullMethod", "s?.ToUpper()");
    }

    [Fact]
    public void NullConditionalIndex_AndChaining_RoundTrips()
    {
        const string src = """
            module N
            {
                public string First(System.Collections.Generic.List<string> xs)
                {
                    return xs?[0]?.ToUpper();
                }
            }
            """;
        AssertCompiles(src, "NullIndexChain", "xs?[0]?.ToUpper()");
    }

    [Fact]
    public void Ternary_StillParses_AlongsideNullConditional()
    {
        const string src = """
            module N { public bool Pos(int x) { return x > 0 ? true : false; } }
            """;
        AssertCompiles(src, "Ternary", " ? ", " : ");
    }
}
