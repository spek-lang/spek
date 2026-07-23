using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Xunit;
using Xunit.Abstractions;

namespace Spek.Tests.Emit;

/// <summary>
/// Tuple literals `(a, b)`. Parsed via the
/// `LPAREN expression (COMMA expression)+ RPAREN` primary alternative (a
/// comma is required, so `(x)` stays a ParenExpr and `(T)x` stays a cast).
/// Modeled as TupleExpr, emitted verbatim (C# infers the ValueTuple type).
/// </summary>
public sealed class TupleLiteralTests(ITestOutputHelper output)
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
    public void Tuple_EmitAndCompile()
    {
        const string src = """
            module M
            {
                public int First()
                {
                    var t = (1, "a");
                    return t.Item1;
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("(1, \"a\")", code);
        AssertCompiles(code, "Tuple");
    }

    [Fact]
    public void SingleParen_StaysParenthesized_NotTuple()
    {
        // One expression in parens must remain a ParenExpr — the comma is what
        // makes a tuple, so this is precedence grouping, not a 1-tuple.
        const string src = """
            module M
            {
                public int Calc() { return (1 + 2) * 3; }
            }
            """;
        var code = EmitCSharp(src);
        // The grouped sub-expression survives as a paren (the emitter also
        // auto-parenthesizes binary ops, so the full form is `(((1 + 2)) * 3)`).
        // The point: no comma-tuple was produced from `(1 + 2)`.
        Assert.Contains("(1 + 2)", code);
        Assert.DoesNotContain("(1 + 2,", code);
        AssertCompiles(code, "SingleParen");
    }

    [Fact]
    public void Cast_StillParses_AfterTupleAddition()
    {
        // Regression guard: `(Type)x` must still be a cast, not mis-parsed now
        // that `( ... )` can also begin a tuple.
        const string src = """
            module M
            {
                public object Box(int x) { return (object)x; }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("(object)", code);
        AssertCompiles(code, "Cast");
    }

    [Fact]
    public void ObservabilityIdiom_AllThreeGapsCompose()
    {
        // The headline: named arg + new[] + tuple, the documented
        // self.Log.Log(..., properties: new[] { (k, v) }) shape, now parses
        // and lowers end-to-end.
        const string src = """
            message Ev(string k, string v);
            actor A
            {
                behavior Idle
                {
                    on Ev e =>
                    {
                        self.Log.Log(StructuredLogLevel.Information, "evt",
                            properties: new[] { (e.k, e.v) });
                    }
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("properties: new[] { (e.k, e.v) }", code);
        Assert.Contains("this.Log.Log(", code);   // self.Log resolves to the ActorBase accessor
    }
}
