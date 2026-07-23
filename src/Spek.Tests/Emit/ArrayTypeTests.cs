using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Xunit;
using Xunit.Abstractions;

namespace Spek.Tests.Emit;

/// <summary>
/// Array types `T[]` (and jagged `T[][]`) in type position: return
/// types, parameters, locals. Parsed via the `(LBRACKET RBRACKET)*` suffix on
/// `type_`; emitted verbatim through TypeRef.ToString().
/// </summary>
public sealed class ArrayTypeTests(ITestOutputHelper output)
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
    public void ArrayReturnType_EmitAndCompile()
    {
        const string src = """
            module M
            {
                public int[] Make() { return new[] { 1, 2, 3 }; }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("int[] Make()", code);
        AssertCompiles(code, "ArrayReturn");
    }

    [Fact]
    public void ArrayParam_EmitAndCompile()
    {
        const string src = """
            module M
            {
                public int Len(string[] xs) { return xs.Length; }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("string[] xs", code);
        AssertCompiles(code, "ArrayParam");
    }

    [Fact]
    public void JaggedArrayType_EmitAndCompile()
    {
        const string src = """
            module M
            {
                public int Rows(int[][] g) { return g.Length; }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("int[][] g", code);
        AssertCompiles(code, "JaggedArray");
    }

    [Fact]
    public void ArrayLocal_EmitAndCompile()
    {
        const string src = """
            module M
            {
                public int First()
                {
                    int[] xs = new[] { 10, 20 };
                    return xs[0];
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("int[] xs", code);
        AssertCompiles(code, "ArrayLocal");
    }

    [Fact]
    public void ArrayMessageField_StillRejectedByCe0010()
    {
        // Arrays are mutable, so even though `int[]` now parses, it must not be
        // allowed as a message field — the immutability guarantee holds.
        const string src = "message Batch(int[] ids);";
        var diags = SpekCompiler.Parse(src).Diagnostics;
        Assert.Contains(diags, d => d.Code == "CE0010");
    }
}
