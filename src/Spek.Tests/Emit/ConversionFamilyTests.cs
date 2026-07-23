using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Xunit;
using Xunit.Abstractions;

namespace Spek.Tests.Emit;

/// <summary>
/// The Spek conversion family: <c>To</c> can't fail, <c>TryTo</c> can't lie.
/// spekc lowers both to static calls (<c>Spek.Conversions</c> /
/// <c>Spek.EnumConversions</c>, or <c>as</c> for reference targets), and each
/// signature family carries its own compile-time enforcement. One test per
/// row of the enforcement table; runtime exactness semantics are tested
/// directly against the runtime class at the bottom.
/// </summary>
public sealed class ConversionFamilyTests(ITestOutputHelper output)
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

    private static void AssertDoesNotCompile(string code, string label, string expectedError)
    {
        var (ok, summary, _) = RoslynCompileHelper.TryCompile(code, label);
        Assert.False(ok, "expected the emitted C# to be rejected by Roslyn");
        Assert.Contains(expectedError, summary);
    }

    // ─── To: lossless only ──────────────────────────────────────────────

    [Fact]
    public void To_Widening_LowersToStaticCall_AndCompiles()
    {
        const string src = """
            namespace X;
            module Conv
            {
                public long Widen(int x) { return x.To<long>(); }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("global::Spek.Conversions.To<long>(x)", code);
        AssertCompiles(code, "ToWidening");
    }

    [Fact]
    public void To_Narrowing_IsRejectedByRoslyn()
    {
        const string src = """
            namespace X;
            module Conv
            {
                public byte Narrow(long x) { return x.To<byte>(); }
            }
            """;
        var code = EmitCSharp(src);
        AssertDoesNotCompile(code, "ToNarrowing", "CS1503");
    }

    [Fact]
    public void To_Upcast_Compiles()
    {
        const string src = """
            namespace X;
            abstract message Shape();
            message Circ(double r) : Shape;
            module Conv
            {
                public Shape Up(Circ c) { return c.To<Shape>(); }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("global::Spek.Conversions.To<Shape>(c)", code);
        AssertCompiles(code, "ToUpcast");
    }

    [Fact]
    public void To_Downcast_IsRejectedByRoslyn()
    {
        const string src = """
            namespace X;
            abstract message Shape();
            message Circ(double r) : Shape;
            module Conv
            {
                public Circ Down(Shape s) { return s.To<Circ>(); }
            }
            """;
        var code = EmitCSharp(src);
        AssertDoesNotCompile(code, "ToDowncast", "CS1503");
    }

    // ─── TryTo: numeric family ──────────────────────────────────────────

    [Fact]
    public void TryTo_Numeric_LowersToStaticCall_AndCompiles()
    {
        const string src = """
            namespace X;
            module Conv
            {
                public byte? Narrow(long x) { return x.TryTo<byte>(); }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("global::Spek.Conversions.TryTo<byte>(x)", code);
        AssertCompiles(code, "TryToNumeric");
    }

    [Fact]
    public void TryTo_WithRoundingStrategy_PassesArgumentThrough()
    {
        const string src = """
            namespace X;
            module Conv
            {
                public int? Round(double x) { return x.TryTo<int>(MidpointRounding.ToZero); }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("global::Spek.Conversions.TryTo<int>(x, MidpointRounding.ToZero)", code);
        AssertCompiles(code, "TryToRounding");
    }

    // ─── TryTo: enum family ─────────────────────────────────────────────

    [Fact]
    public void TryTo_DeclaredEnum_RoutesToEnumConversions_AndCompiles()
    {
        const string src = """
            namespace X;
            enum Sev { Low, High }
            module Conv
            {
                public Sev? Parse(int x) { return x.TryTo<Sev>(); }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("global::Spek.EnumConversions.TryTo<Sev>(x)", code);
        AssertCompiles(code, "TryToEnum");
    }

    // ─── TryTo: reference family (lowered to `as`) ──────────────────────

    [Fact]
    public void TryTo_MessageDowncast_LowersToAs_AndCompiles()
    {
        const string src = """
            namespace X;
            abstract message Shape();
            message Circ(double r) : Shape;
            module Conv
            {
                public Circ? Down(Shape s) { return s.TryTo<Circ>(); }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("(s as Circ)", code);
        AssertCompiles(code, "TryToDowncast");
    }

    [Fact]
    public void TryTo_UnrelatedReferenceTarget_KeepsRoslynRelatednessError()
    {
        const string src = """
            namespace X;
            message Circ(double r);
            module Conv
            {
                public Circ? Nonsense(double d) { return d.TryTo<Circ>(); }
            }
            """;
        var code = EmitCSharp(src);
        // Lowered to `d as Circ`, which Roslyn rejects outright with the
        // relatedness error the lowering exists to preserve.
        AssertDoesNotCompile(code, "TryToUnrelated", "CS0039");
    }

    // ─── object sources are excluded by construction ────────────────────

    [Fact]
    public void TryTo_FromObject_HasNoOverload_AndFailsRoslyn()
    {
        const string src = """
            namespace X;
            module Conv
            {
                public int? Unbox(object o) { return o.TryTo<int>(); }
            }
            """;
        var code = EmitCSharp(src);
        AssertDoesNotCompile(code, "TryToObject", "CS1503");
    }

    // ─── CE0127: unknown conversion target ──────────────────────────────

    [Fact]
    public void TryTo_UnknownExternalTarget_ReportsCE0127()
    {
        const string src = """
            namespace X;
            module Conv
            {
                public int Use(int x) { var u = x.TryTo<Uri>(); return x; }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.False(parsed.Success);
        var diag = Assert.Single(parsed.Diagnostics, d => d.Code == "CE0127");
        Assert.Contains("Uri", diag.Message);
    }

    // ─── CE0128: To/TryTo are reserved member names ─────────────────────

    [Theory]
    [InlineData("class C { public int To(int x) { return x; } }")]
    [InlineData("module M { public int TryTo(int x) { return x; } }")]
    public void DeclaringReservedConversionName_ReportsCE0128(string decl)
    {
        var src = $"namespace X;\n{decl}";
        var parsed = SpekCompiler.Parse(src);
        Assert.False(parsed.Success);
        Assert.Contains(parsed.Diagnostics, d => d.Code == "CE0128");
    }

    // ─── CE0129: the cast operator is retired ───────────────────────────

    [Fact]
    public void Cast_NumericTarget_ReportsCE0129_TeachingToAndTryTo()
    {
        const string src = """
            namespace X;
            module Conv
            {
                public int Use(double d) { var x = (int)d; return 0; }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.False(parsed.Success);
        var diag = Assert.Single(parsed.Diagnostics, d => d.Code == "CE0129");
        Assert.Contains("To<int>", diag.Message);
        Assert.Contains("TryTo<int>", diag.Message);
    }

    [Fact]
    public void Cast_ReferenceTarget_ReportsCE0129_TeachingIsAs()
    {
        const string src = """
            namespace X;
            abstract message Shape();
            message Circ(double r) : Shape;
            module Conv
            {
                public int Use(Shape s) { var c = (Circ)s; return 0; }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.False(parsed.Success);
        var diag = Assert.Single(parsed.Diagnostics, d => d.Code == "CE0129");
        Assert.Contains("is Circ", diag.Message);
        Assert.Contains("as Circ", diag.Message);
    }

    [Fact]
    public void Cast_EnumTarget_ReportsCE0129_TeachingTryTo()
    {
        const string src = """
            namespace X;
            enum Sev { Low, High }
            module Conv
            {
                public int Use(int n) { var s = (Sev)n; return 0; }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.False(parsed.Success);
        var diag = Assert.Single(parsed.Diagnostics, d => d.Code == "CE0129");
        Assert.Contains("TryTo<Sev>", diag.Message);
    }

    [Fact]
    public void ParenthesizedExpression_IsNotACast()
    {
        const string src = """
            namespace X;
            module Conv
            {
                public int Fine(int a, int b) { return (a + b) * 2; }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.True(parsed.Success,
            string.Join("\n", parsed.Diagnostics.Select(d => $"{d.Code} {d.Message}")));
    }

    // ─── runtime semantics, tested directly ─────────────────────────────

    [Fact]
    public void Runtime_TryTo_IsExact_NotInRange()
    {
        Assert.Null(Spek.Conversions.TryTo<int>(2.9));
        Assert.Equal(2, Spek.Conversions.TryTo<int>(2.0));
        Assert.Null(Spek.Conversions.TryTo<byte>(300));
        Assert.Equal((byte)42, Spek.Conversions.TryTo<byte>(42L));
        // double → float precision loss is a lie, so it is null.
        Assert.Null(Spek.Conversions.TryTo<float>(0.1d));
        Assert.Equal(0.5f, Spek.Conversions.TryTo<float>(0.5d));
    }

    [Fact]
    public void Runtime_TryTo_RoundingStrategies()
    {
        Assert.Equal(2, Spek.Conversions.TryTo<int>(2.9, MidpointRounding.ToZero));
        Assert.Equal(3, Spek.Conversions.TryTo<int>(2.1, MidpointRounding.ToPositiveInfinity));
        Assert.Equal(2, Spek.Conversions.TryTo<int>(2.9, MidpointRounding.ToNegativeInfinity));
        Assert.Null(Spek.Conversions.TryTo<int>(1e300, MidpointRounding.ToZero));
        Assert.Null(Spek.Conversions.TryTo<int>(double.NaN, MidpointRounding.ToZero));
    }

    private enum Probe { Zero = 0, One = 1, Two = 2 }

    [Fact]
    public void Runtime_TryTo_Enum_DefinedOrNull()
    {
        Assert.Equal(Probe.One, Spek.EnumConversions.TryTo<Probe>(1));
        Assert.Null(Spek.EnumConversions.TryTo<Probe>(3));
        Assert.Null(Spek.EnumConversions.TryTo<Probe>(long.MaxValue));
    }
}
