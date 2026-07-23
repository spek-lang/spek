using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Spek.Tests.Emit;   // RoslynCompileHelper
using Xunit;

namespace Spek.Tests.Parser;

/// <summary>
/// Interpolated strings are parsed structurally: each <c>{ … }</c> hole is a
/// Spek expression emitted through the normal expression emitter (so field /
/// `self` rewriting and invisible-async apply inside holes), and the lexer
/// finds the right string boundary even when a hole contains an embedded
/// string, nested braces, or a nested interpolation. The literal text and
/// any <c>:format</c> / <c>,alignment</c> suffix pass through verbatim.
/// </summary>
public sealed class InterpolationTests
{
    private static (bool Ok, string Code, string Diags) Emit(string src)
    {
        var parsed = SpekCompiler.Parse(src);
        if (!parsed.Success)
            return (false, "", string.Join("; ", parsed.Diagnostics.Select(d => $"{d.Code} {d.Message}")));
        return (true, new FileEmitter().Emit(parsed.Tree!), "");
    }

    private static void AssertCompiles(string code, string label)
    {
        var (ok, summary, _) = RoslynCompileHelper.TryCompile(code, label);
        Assert.True(ok, $"did not compile:\n{summary}");
    }

    // ── Bug 2: a field reference in a hole must be rewritten to `_field` ──
    [Fact]
    public void Hole_FieldReference_RewritesAndCompiles()
    {
        const string src = """
            message Show();
            actor A
            {
                decimal balance = 0.00m;
                on Show => { System.Console.WriteLine($"bal {balance}"); }
            }
            """;
        var (ok, code, diags) = Emit(src);
        Assert.True(ok, diags);
        Assert.Contains("{_balance}", code);     // rewritten, not the raw {balance}
        Assert.DoesNotContain("{balance}", code);
        AssertCompiles(code, "InterpFieldRef");
    }

    // ── A local reference is left as-is ──
    [Fact]
    public void Hole_LocalReference_Works()
    {
        var (ok, code, diags) = Emit("""program Main { int x = 3; System.Console.WriteLine($"x={x}"); }""");
        Assert.True(ok, diags);
        Assert.Contains("$\"x={x}\"", code);
        AssertCompiles(code, "InterpLocal");
    }

    // ── Bug 1: an embedded string (with a `}` inside) no longer breaks lexing ──
    [Fact]
    public void Hole_EmbeddedStringWithBrace_ParsesAndCompiles()
    {
        var (ok, code, diags) = Emit("""program Main { var n = $"len={ "a}b".Length }"; System.Console.WriteLine(n); }""");
        Assert.True(ok, diags);
        AssertCompiles(code, "InterpEmbeddedBrace");
    }

    // ── Red-team emit-F1: a To<T> conversion inside a hole lowers to a
    //    `global::`-qualified call; the `::` must be parenthesized so C# doesn't
    //    read the first `:` as the format specifier (was CS0103 on green Spek). ──
    [Fact]
    public void Hole_ConversionLowering_ParenthesizedAndCompiles()
    {
        const string src = """
            message M(int n);
            actor A { init() { become Go; } behavior Go {
                on M m => { System.Console.WriteLine($"v={m.n.To<long>()}"); } } }
            """;
        var (ok, code, diags) = Emit(src);
        Assert.True(ok, diags);
        Assert.Contains("{(global::Spek.Conversions.To<long>", code);   // wrapped
        AssertCompiles(code, "InterpConversion");
    }

    // ── Red-team emit-F2: a ternary in a hole — its `:` is NOT the format
    //    delimiter (SplitHole must not split there), and the emit must
    //    parenthesize it (was CS8361 on green Spek). ──
    [Fact]
    public void Hole_Ternary_ParsesParenthesizedAndCompiles()
    {
        const string src = """
            message M(int n);
            actor A { init() { become Go; } behavior Go {
                on M m => { System.Console.WriteLine($"s={m.n > 0 ? "pos" : "neg"}"); } } }
            """;
        var (ok, code, diags) = Emit(src);
        Assert.True(ok, diags);
        Assert.DoesNotContain("{m.n > 0 ? \"pos\"", code);   // NOT verbatim-passthrough
        AssertCompiles(code, "InterpTernary");
    }

    // ── A ternary WITH a format suffix: the ternary colon is consumed, the
    //    trailing `:F2` still splits off as the format. ──
    [Fact]
    public void Hole_TernaryWithFormatSuffix_Compiles()
    {
        const string src = """
            message M(double d);
            actor A { init() { become Go; } behavior Go {
                on M m => { System.Console.WriteLine($"x={m.d > 0 ? m.d : 0.0:F2}"); } } }
            """;
        var (ok, code, diags) = Emit(src);
        Assert.True(ok, diags);
        Assert.Contains(":F2}", code);   // format suffix preserved
        AssertCompiles(code, "InterpTernaryFormat");
    }

    // ── False-positive guard: a plain hole is NOT over-parenthesized. ──
    [Fact]
    public void Hole_SimpleLocal_StaysBare()
    {
        var (ok, code, diags) = Emit("""program Main { int x = 3; System.Console.WriteLine($"x={x}"); }""");
        Assert.True(ok, diags);
        Assert.Contains("$\"x={x}\"", code);
        Assert.DoesNotContain("{(x)}", code);
    }

    // ── Nested braces in a hole (object/collection initializer) ──
    [Fact]
    public void Hole_NestedBraces_Compiles()
    {
        var (ok, code, diags) = Emit("""program Main { var n = $"len={ new int[]{ 1, 2, 3 }.Length }"; System.Console.WriteLine(n); }""");
        Assert.True(ok, diags);
        AssertCompiles(code, "InterpNestedBraces");
    }

    // ── Format specifier suffix is preserved verbatim ──
    [Fact]
    public void Hole_FormatSpecifier_Preserved()
    {
        var (ok, code, diags) = Emit("""program Main { decimal price = 9.5m; System.Console.WriteLine($"{price:C2}"); }""");
        Assert.True(ok, diags);
        Assert.Contains(":C2}", code);
        AssertCompiles(code, "InterpFormat");
    }

    // ── Nested interpolation ──
    [Fact]
    public void Hole_NestedInterpolation_Compiles()
    {
        var (ok, code, diags) = Emit("""program Main { int x = 1; System.Console.WriteLine($"outer { $"inner {x}" }"); }""");
        Assert.True(ok, diags);
        AssertCompiles(code, "InterpNested");
    }

    // ── Member method calls inside a hole keep their invocation ──
    // Regression: the hole sub-parse wasn't EOF-anchored, so prediction could
    // lawfully stop at `p.ToString` and silently drop the `()` — the emitted
    // C# then printed a delegate type name instead of invoking. Found by the
    // AlertHub sample; fixed via the `holeExpression : expression EOF` rule.
    [Fact]
    public void Hole_MemberMethodCall_KeepsInvocation()
    {
        const string src = """
            message Show();
            actor A
            {
                int count = 0;
                on Show => { System.Console.WriteLine($"{count.ToString()}"); }
            }
            """;
        var (ok, code, diags) = Emit(src);
        Assert.True(ok, diags);
        Assert.Contains("_count.ToString()", code);   // call intact + field rewrite
        AssertCompiles(code, "InterpMemberCall");
    }

    [Fact]
    public void Hole_MemberMethodCallWithArgs_KeepsInvocation()
    {
        var (ok, code, diags) = Emit(
            """program Main { var s = "spek"; System.Console.WriteLine($"{s.PadLeft(10, '.')}"); }""");
        Assert.True(ok, diags);
        Assert.Contains("s.PadLeft(10, '.')", code);
        AssertCompiles(code, "InterpMemberCallArgs");
    }

    // ── Verbatim-interpolated still rewrites field references ──
    [Fact]
    public void VerbatimInterpolated_FieldReference_Rewrites()
    {
        const string src = """
            message Show();
            actor A
            {
                int count = 0;
                on Show => { System.Console.WriteLine($@"count={count}"); }
            }
            """;
        var (ok, code, diags) = Emit(src);
        Assert.True(ok, diags);
        Assert.Contains("{_count}", code);
        AssertCompiles(code, "VerbatimInterpFieldRef");
    }

    // ── Interpolation-heavy source: every hole shape at once ──
    // Each hole is sub-parsed by ParseSpekExpression, which (like the main
    // parse) predicts in SLL first and falls back to full LL on a bail.
    // This drives many holes through that two-stage path in one source:
    // qualified nested calls, member calls on a field, arithmetic, a format
    // suffix, and adjacent holes — asserting the emitted C# keeps every
    // invocation intact and still rewrites fields inside holes.
    [Fact]
    public void ManyHoles_IncludingNestedCalls_ParseAndEmit()
    {
        const string src = """
            module Pricing
            {
                public string Stamp(int id)
                {
                    return "#" + id.ToString();
                }
            }

            message Order(int id, decimal amount);
            message Line(string text);
            actor Register
            {
                int sold = 0;
                decimal total = 0.00m;

                on Order x =>
                {
                    sold = sold + 1;
                    total = total + x.amount;
                    return new Line(
                        $"total: {Pricing.Stamp(x.id)} sold={sold} sum={total:F2} avg={ total / sold } tag={x.id.ToString().PadLeft(4, '0')}");
                }
            }
            """;
        var (ok, code, diags) = Emit(src);
        Assert.True(ok, diags);

        // Nested qualified call survives the sub-parse whole.
        Assert.Contains("Pricing.Stamp(x.id)", code);
        // Field references inside holes are rewritten...
        Assert.Contains("{_sold}", code);
        Assert.Contains("_total / _sold", code);
        // ...the format suffix rides along verbatim...
        Assert.Contains(":F2}", code);
        // ...and a chained member call on a message field stays an invocation.
        Assert.Contains("x.id.ToString().PadLeft(4, '0')", code);

        AssertCompiles(code, "InterpManyHoles");
    }

    // ── A hole that isn't an expression still falls back to verbatim ──
    // Pins the ParseSpekExpression null path through BOTH prediction
    // stages: the SLL parse bails, the full-LL retry also rejects, and the
    // builder keeps the hole as literal text instead of crashing or
    // reporting a diagnostic — unchanged from the single-stage behaviour.
    [Fact]
    public void Hole_NonExpression_FallsBackToVerbatimText()
    {
        var (ok, code, diags) = Emit(
            """program Main { int x = 7; System.Console.WriteLine($"x={x} and {not valid spek}"); }""");
        Assert.True(ok, diags);
        Assert.Contains("{not valid spek}", code);   // verbatim, not parsed
        Assert.Contains("{x}", code);                // the valid hole still parses
    }
}
