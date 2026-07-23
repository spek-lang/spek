using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Xunit;
using Xunit.Abstractions;

namespace Spek.Tests.Emit;

/// <summary>
/// <c>flags enum</c>: values correct by construction. The compiler assigns
/// powers of two, provides <c>None = 0</c>, validates explicit values and
/// unions, and emits <c>[System.Flags]</c> so C# consumers see an ordinary
/// flags enum. CE0130 guards every way the declaration could lie.
/// </summary>
public sealed class FlagsEnumTests(ITestOutputHelper output)
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

    [Fact]
    public void FlagsEnum_AutoAssignsPowersOfTwo_ProvidesNone_EmitsAttribute()
    {
        const string src = """
            namespace X;
            flags enum Permissions { Read, Write, Execute }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("[System.Flags]", code);
        Assert.Contains("None = 0,", code);
        Assert.Contains("Read = 1,", code);
        Assert.Contains("Write = 2,", code);
        Assert.Contains("Execute = 4,", code);
        AssertCompiles(code, "FlagsAuto");
    }

    [Fact]
    public void FlagsEnum_ExplicitPowersAndUnions_EmitSymbolically()
    {
        const string src = """
            namespace X;
            flags enum P { Read = 1, Write = 2, ReadWrite = Read | Write, Execute }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("ReadWrite = Read | Write,", code);
        // Execute auto-assigns the next unused power: 4.
        Assert.Contains("Execute = 4,", code);
        AssertCompiles(code, "FlagsExplicit");
    }

    [Fact]
    public void PlainEnum_ExplicitValues_StillWork()
    {
        const string src = """
            namespace X;
            enum Plain { A, B = 5, C }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("B = 5,", code);
        Assert.DoesNotContain("[System.Flags]", code);
        AssertCompiles(code, "PlainValues");
    }

    [Theory]
    [InlineData("flags enum P { None, Read }", "None")]
    [InlineData("flags enum P { Read = 3 }", "power of two")]
    [InlineData("flags enum P { Read = 0 }", "power of two")]
    [InlineData("flags enum P { RW = Read | Write, Read, Write }", "earlier member")]
    [InlineData("flags enum P { Read = 1, Also = 1 }", "same bit value")]
    [InlineData("enum P { A, B, AB = A | B }", "flags enum")]
    public void InvalidDeclarations_ReportCE0130(string decl, string messagePart)
    {
        var src = $"namespace X;\n{decl}";
        var parsed = SpekCompiler.Parse(src);
        Assert.False(parsed.Success);
        var diag = Assert.Single(parsed.Diagnostics, d => d.Code == "CE0130");
        Assert.Contains(messagePart, diag.Message);
    }

    [Fact]
    public void Flags_IsStillAnOrdinaryIdentifier()
    {
        const string src = """
            namespace X;
            module M
            {
                public int Use(int flags) { var x = flags + 1; return x; }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.True(parsed.Success,
            string.Join("\n", parsed.Diagnostics.Select(d => $"{d.Code} {d.Message}")));
    }
}
