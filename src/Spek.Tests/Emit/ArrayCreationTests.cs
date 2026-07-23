using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Xunit;
using Xunit.Abstractions;

namespace Spek.Tests.Emit;

/// <summary>
/// Implicit-typed array creation `new[] { ... }`. Parsed via the
/// `NEW LBRACKET RBRACKET arrayInitializer` alternative, modeled as an
/// ArrayExpr, emitted verbatim (Roslyn infers the element type).
/// </summary>
public sealed class ArrayCreationTests(ITestOutputHelper output)
{
    private static string EmitCSharp(string source)
    {
        var parsed = SpekCompiler.Parse(source);
        Assert.True(parsed.Success,
            string.Join("\n", parsed.Diagnostics.Select(d => $"{d.Code} {d.Message}")));
        return new FileEmitter().Emit(parsed.Tree!);
    }

    [Fact]
    public void ImplicitArray_EmitAndCompile()
    {
        const string src = """
            module M
            {
                public int Count()
                {
                    var xs = new[] { 1, 2, 3 };
                    return xs.Length;
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("new[] { 1, 2, 3 }", code);

        var (ok, summary, _) = RoslynCompileHelper.TryCompile(code, "ImplicitArray");
        if (!ok) { output.WriteLine(code); output.WriteLine(summary); }
        Assert.True(ok, $"Emitted C# did not compile:\n{summary}");
    }

    [Fact]
    public void ImplicitArray_AsNamedArgValue_Composes()
    {
        // The shape the observability idiom needs: a named arg whose value is
        // an implicit array. (Tuple elements arrive with the next parser gap.)
        // Emit-only — the exact call target is irrelevant; we're checking that
        // `name: new[] { ... }` parses and lowers, not BCL overload resolution.
        const string src = """
            module M
            {
                public string J() { return string.Join(separator: ", ", value: new[] { "a", "b" }); }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("separator: \", \"", code);
        Assert.Contains("value: new[] { \"a\", \"b\" }", code);
    }

    [Fact]
    public void ImplicitArray_TrailingComma_Parses()
    {
        // `var` avoids an `int[]` return type (array types are a separate gap).
        const string src = """
            module M
            {
                public int First() { var xs = new[] { 1, 2, }; return xs[0]; }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("new[] { 1, 2 }", code);
    }
}
