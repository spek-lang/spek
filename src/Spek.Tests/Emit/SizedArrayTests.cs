using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Xunit;
using Xunit.Abstractions;

namespace Spek.Tests.Emit;

/// <summary>
/// Typed / sized array creation: `new T[n]` (uninitialized buffer),
/// `new T[] { ... }` (typed initialized), `new T[n] { ... }`. Parsed via the
/// `new qualifiedName [ expr? ] arrayInitializer?` alternative; reuses ArrayExpr
/// with ElementType + Size. The implicit `new[] { ... }` form is unchanged.
/// </summary>
public sealed class SizedArrayTests(ITestOutputHelper output)
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
    public void SizedArray_UninitializedBuffer_EmitAndCompile()
    {
        // The capability gap: an uninitialized buffer of length n.
        const string src = """
            module M
            {
                public int Make(int n)
                {
                    var buf = new byte[n];
                    return buf.Length;
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("new byte[n]", code);
        AssertCompiles(code, "SizedArray");
    }

    [Fact]
    public void TypedArray_WithInitializer_EmitAndCompile()
    {
        const string src = """
            module M
            {
                public int Sum()
                {
                    var xs = new int[] { 1, 2, 3 };
                    return xs.Length;
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("new int[] { 1, 2, 3 }", code);
        AssertCompiles(code, "TypedArrayInit");
    }

    [Fact]
    public void ImplicitArray_StillWorks()
    {
        // Regression: the existing `new[] { ... }` form is unaffected.
        const string src = """
            module M
            {
                public int Count() { var xs = new[] { 1, 2 }; return xs.Length; }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("new[] { 1, 2 }", code);
        Assert.DoesNotContain("new int[]", code);
    }
}
