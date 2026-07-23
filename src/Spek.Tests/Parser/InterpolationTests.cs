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
}
