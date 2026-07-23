using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Xunit;
using Xunit.Abstractions;

namespace Spek.Tests.Emit;

/// <summary>
/// C# named arguments at call sites: `Foo(width: 3)`. Parsed via the
/// `(IDENTIFIER COLON)?` prefix on the `arg` rule, modeled as a NamedArgExpr
/// wrapping the value, emitted verbatim as `name: value`.
/// </summary>
public sealed class NamedArgumentTests(ITestOutputHelper output)
{
    private static string EmitCSharp(string source)
    {
        var parsed = SpekCompiler.Parse(source);
        Assert.True(parsed.Success,
            string.Join("\n", parsed.Diagnostics.Select(d => $"{d.Code} {d.Message}")));
        return new FileEmitter().Emit(parsed.Tree!);
    }

    [Fact]
    public void NamedArgs_OnMethodCall_EmitAndCompile()
    {
        // System.Math.Round(double value, int digits) — known param names, so
        // the named form compiles regardless of Spek's own naming.
        const string src = """
            module M
            {
                public double R() { return System.Math.Round(value: 3.14, digits: 1); }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("value: 3.14", code);
        Assert.Contains("digits: 1", code);

        var (ok, summary, _) = RoslynCompileHelper.TryCompile(code, "NamedArgs");
        if (!ok) { output.WriteLine(code); output.WriteLine(summary); }
        Assert.True(ok, $"Emitted C# did not compile:\n{summary}");
    }

    [Fact]
    public void NamedArgs_MixedWithPositional_Parse()
    {
        // Leading positional + trailing named, the common shape.
        const string src = """
            module M
            {
                public double R() { return System.Math.Round(3.14159, digits: 2); }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("System.Math.Round(3.14159, digits: 2)", code);
    }

    [Fact]
    public void NamedArgs_DoNotBreakConditionalExpression()
    {
        // A ternary argument has a COLON but no `IDENTIFIER COLON` arg-prefix —
        // it must still parse as a positional conditional expression.
        const string src = """
            module M
            {
                public int Pick(bool b) { return System.Math.Max(b ? 1 : 2, 3); }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("b ? 1 : 2", code);
    }
}
