using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Xunit;
using Xunit.Abstractions;

namespace Spek.Tests.Emit;

/// <summary>
/// Language-completeness — bitwise/shift operators (<c>&amp; | ^ ~ &lt;&lt; &gt;&gt;</c>)
/// and null-coalescing (<c>??</c>). These reuse the existing <c>BinaryExpr</c> /
/// <c>UnaryExpr</c> nodes (just new <c>BinaryOp</c>/<c>UnaryOp</c> members), so all
/// the existing walkers already cover them; the work is grammar precedence +
/// verbatim operator emit. Shift is matched as adjacent <c>LT LT</c>/<c>GT GT</c>
/// at the parser level so '&gt;&gt;' never becomes a single token — the
/// <see cref="NestedGenerics_StillParse"/> test guards that invariant.
/// </summary>
public sealed class OperatorTests(ITestOutputHelper output)
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
    public void Bitwise_AndOrXor_RoundTrips()
    {
        const string src = """
            module Ops
            {
                public int Mix(int a, int b) { return (a & b) | (a ^ b); }
            }
            """;
        AssertCompiles(src, "Bitwise", " & ", " | ", " ^ ");
    }

    [Fact]
    public void Shift_RoundTrips()
    {
        const string src = """
            module Ops
            {
                public int Scale(int a) { return (a << 3) >> 1; }
            }
            """;
        AssertCompiles(src, "Shift", " << ", " >> ");
    }

    [Fact]
    public void BitwiseNot_RoundTrips()
    {
        const string src = """
            module Ops
            {
                public int ClearLow(int a) { return a & ~7; }
            }
            """;
        AssertCompiles(src, "BitNot", "~");
    }

    [Fact]
    public void NullCoalescing_RoundTrips()
    {
        const string src = """
            module Ops
            {
                public string OrElse(string s) { return s ?? "default"; }
            }
            """;
        AssertCompiles(src, "Coalesce", " ?? ");
    }

    [Fact]
    public void Precedence_BitwiseBindsLooserThanEquality()
    {
        // a & b == c  must mean  a & (b == c) in C#... actually equality binds
        // tighter than &, so this is a parse+compile smoke over the inserted
        // precedence levels. We just assert it round-trips through Roslyn.
        const string src = """
            module Ops
            {
                public bool Flags(int a, int b) { return (a & b) != 0 && (a | b) > 0; }
            }
            """;
        AssertCompiles(src, "Precedence");
    }

    [Fact]
    public void CompoundAssignments_RoundTrip()
    {
        const string src = """
            module Ops
            {
                public int Bits(int a, int b)
                {
                    int x = a;
                    x %= b; x &= 255; x |= 1; x ^= b;
                    return x;
                }
                public string Lazy(string s) { string r = s; r ??= "x"; return r; }
            }
            """;
        AssertCompiles(src, "CompoundAssign", "%=", "&=", "|=", "^=", "??=");
    }

    [Fact]
    public void NestedGenerics_StillParse()
    {
        // Regression guard: shift '>>' is matched as two GT tokens, so a nested
        // generic close ('>>' in List<List<int>>) must still parse as generics.
        const string src = """
            module Ops
            {
                public System.Collections.Generic.List<System.Collections.Generic.List<int>> Grid()
                {
                    return new System.Collections.Generic.List<System.Collections.Generic.List<int>>();
                }
            }
            """;
        AssertCompiles(src, "NestedGenerics", "List<System.Collections.Generic.List<int>>");
    }
}
