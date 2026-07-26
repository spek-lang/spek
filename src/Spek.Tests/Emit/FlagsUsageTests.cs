using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Xunit;
using Xunit.Abstractions;

namespace Spek.Tests.Emit;

/// <summary>
/// Flags usage guarantees: bitwise operators are gated to flags enums
/// (CE0131, with a did-you-mean hint when the members are hand-rolled powers
/// of two), provably-empty literal intersections are rejected (CE0132), the
/// HasFlag family refuses non-flags enums and degenerate None arguments
/// (CE0133), and the runtime verbs implement the superset/intersect/subset
/// lattice with flags-aware TryTo definedness.
/// </summary>
public sealed class FlagsUsageTests(ITestOutputHelper output)
{
    private static string EmitCSharp(string source)
    {
        var parsed = SpekCompiler.Parse(source);
        Assert.True(parsed.Success,
            string.Join("\n", parsed.Diagnostics.Select(d => $"{d.Code} {d.Message}")));
        return new FileEmitter().Emit(parsed.Tree!);
    }

    private void AssertCompiles(string code, string label)
    {
        var (ok, summary, _) = RoslynCompileHelper.TryCompile(code, label);
        if (!ok) { output.WriteLine(code); output.WriteLine(summary); }
        Assert.True(ok, $"Emitted C# did not compile:\n{summary}");
    }

    // ─── CE0131: bitwise ops gated to flags enums ───────────────────────

    [Fact]
    public void BitwiseOr_OnPlainEnum_ReportsCE0131()
    {
        const string src = """
            namespace X;
            enum Sev { Low, High }
            module M { public int F() { var x = Sev.Low | Sev.High; return 0; } }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.False(parsed.Success);
        var diag = Assert.Single(parsed.Diagnostics, d => d.Code == "CE0131");
        Assert.Contains("not a flags enum", diag.Message);
    }

    [Fact]
    public void BitwiseOr_OnHandRolledPowersOfTwo_SuggestsFlagsKeyword()
    {
        const string src = """
            namespace X;
            enum Perm { Read = 1, Write = 2, Execute = 4 }
            module M { public int F() { var x = Perm.Read | Perm.Write; return 0; } }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.False(parsed.Success);
        var diag = Assert.Single(parsed.Diagnostics, d => d.Code == "CE0131");
        Assert.Contains("did you", diag.Message);
        Assert.Contains("flags enum Perm", diag.Message);
    }

    [Fact]
    public void BitwiseOr_OnFlagsEnum_IsClean_AndCompiles()
    {
        const string src = """
            namespace X;
            flags enum Perm { Read, Write }
            module M { public bool F() { var x = Perm.Read | Perm.Write; return x.HasFlag(Perm.Read); } }
            """;
        var code = EmitCSharp(src);
        AssertCompiles(code, "FlagsOr");
    }

    // ─── CE0132: provably-empty intersections ───────────────────────────

    [Fact]
    public void BitwiseAnd_OfDistinctFlagsLiterals_ReportsCE0132()
    {
        const string src = """
            namespace X;
            flags enum Perm { Read, Write }
            module M { public int F() { var x = Perm.Read & Perm.Write; return 0; } }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.False(parsed.Success);
        var diag = Assert.Single(parsed.Diagnostics, d => d.Code == "CE0132");
        Assert.Contains("always empty", diag.Message);
        Assert.Contains("'|'", diag.Message);
    }

    [Fact]
    public void BitwiseAnd_WithUnionMember_IsAllowed()
    {
        // Read & ReadWrite is a legitimate mask test, not provably empty.
        const string src = """
            namespace X;
            flags enum Perm { Read, Write, ReadWrite = Read | Write }
            module M { public int F() { var x = Perm.Read & Perm.ReadWrite; return 0; } }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.True(parsed.Success,
            string.Join("\n", parsed.Diagnostics.Select(d => $"{d.Code} {d.Message}")));
    }

    // ─── CE0133: HasFlag family gating ──────────────────────────────────

    [Fact]
    public void HasFlag_OnPlainEnumLiteral_ReportsCE0133()
    {
        const string src = """
            namespace X;
            enum Sev { Low, High }
            module M { public bool F(Sev s) { return s.HasFlag(Sev.Low); } }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.False(parsed.Success);
        var diag = Assert.Single(parsed.Diagnostics, d => d.Code == "CE0133");
        Assert.Contains("'=='", diag.Message);
    }

    [Fact]
    public void HasFlag_None_ReportsCE0133_AlwaysTrue()
    {
        const string src = """
            namespace X;
            flags enum Perm { Read, Write }
            module M { public bool F(Perm p) { return p.HasFlag(Perm.None); } }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.False(parsed.Success);
        var diag = Assert.Single(parsed.Diagnostics, d => d.Code == "CE0133");
        Assert.Contains("always true", diag.Message);
        Assert.Contains("== Perm.None", diag.Message);
    }

    [Fact]
    public void HasAnyFlags_None_ReportsCE0133_AlwaysFalse()
    {
        const string src = """
            namespace X;
            flags enum Perm { Read, Write }
            module M { public bool F(Perm p) { return p.HasAnyFlags(Perm.None); } }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.False(parsed.Success);
        var diag = Assert.Single(parsed.Diagnostics, d => d.Code == "CE0133");
        Assert.Contains("always false", diag.Message);
    }

    [Fact]
    public void HasOnlyFlags_None_ReportsCE0133_DisguisedEquality()
    {
        // Not a constant like the other two: x.HasOnlyFlags(None) is
        // (x & ~0) == 0, i.e. exactly x == None. The message must teach
        // the equality, not claim "always true".
        const string src = """
            namespace X;
            flags enum Perm { Read, Write }
            module M { public bool F(Perm p) { return p.HasOnlyFlags(Perm.None); } }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.False(parsed.Success);
        var diag = Assert.Single(parsed.Diagnostics, d => d.Code == "CE0133");
        Assert.DoesNotContain("always true", diag.Message);
        Assert.Contains("== Perm.None", diag.Message);
    }

    [Fact]
    public void HasFlagFamily_LegitimateUse_EmitsAndCompiles()
    {
        const string src = """
            namespace X;
            flags enum Perm { Read, Write, Execute }
            module M
            {
                public bool Any(Perm p)  { return p.HasAnyFlags(Perm.Read, Perm.Write); }
                public bool Only(Perm p) { return p.HasOnlyFlags(Perm.Read, Perm.Write); }
            }
            """;
        var code = EmitCSharp(src);
        AssertCompiles(code, "FlagsFamily");
    }
}
