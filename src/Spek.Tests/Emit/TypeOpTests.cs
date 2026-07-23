using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Xunit;
using Xunit.Abstractions;

namespace Spek.Tests.Emit;

/// <summary>
/// Language-completeness — type operations: cast <c>(T)x</c>, type-test
/// <c>x is T</c> (with optional capture <c>x is T name</c>), and safe cast
/// <c>x as T</c>. All three share one AST node (<see cref="Spek.Compiler.AST.TypeOpExpr"/>)
/// and lower verbatim to C#.
///
/// Cast parsing note: <c>(Identifier)</c> immediately before a <c>-</c>/<c>~</c>/<c>!</c>
/// expression is read as a cast. If <c>Identifier</c> is a value, not a type, the
/// emitted C# fails to compile (CS0118) — a loud failure, never silent
/// misbehavior. The numeric-cast tests below pin the intended behavior.
/// </summary>
public sealed class TypeOpTests(ITestOutputHelper output)
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
    public void Is_TypeTest_RoundTrips()
    {
        const string src = """
            module T { public bool IsText(object o) { return o is string; } }
            """;
        AssertCompiles(src, "IsTest", "(o is string)");
    }

    [Fact]
    public void Is_WithCaptureBinding_RoundTrips()
    {
        const string src = """
            module T
            {
                public int Len(object o) { if (o is string s) { return s.Length; } return 0; }
            }
            """;
        AssertCompiles(src, "IsCapture", "o is string s");
    }

    [Fact]
    public void As_SafeCast_RoundTrips()
    {
        const string src = """
            module T
            {
                public int Len(object o) { string s = o as string; return s == null ? 0 : s.Length; }
            }
            """;
        AssertCompiles(src, "AsCast", "(o as string)");
    }

    [Fact]
    public void Cast_NumericNarrowing_RoundTrips()
    {
        const string src = """
            module T
            {
                public long Widen(int x)    { return (long)x; }
                public int  Narrow(double d) { return (int)d; }
            }
            """;
        AssertCompiles(src, "NumericCast", "((long)x)", "((int)d)");
    }

    [Fact]
    public void Cast_ReferenceType_RoundTrips()
    {
        const string src = """
            module T
            {
                public string Describe(object o) { return (string)o; }
            }
            """;
        AssertCompiles(src, "RefCast", "((string)o)");
    }
}
