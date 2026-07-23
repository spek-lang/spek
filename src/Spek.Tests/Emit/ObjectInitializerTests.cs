using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Xunit;
using Xunit.Abstractions;

namespace Spek.Tests.Emit;

/// <summary>
/// Object / collection initializers: `new T { ... }` and
/// `new T(args) { ... }`. Parsed via the `objectInitializer` braces on the
/// `newExpr` rule; emitted verbatim (Roslyn decides object-vs-collection init).
/// </summary>
public sealed class ObjectInitializerTests(ITestOutputHelper output)
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
    public void CollectionInitializer_EmitAndCompile()
    {
        const string src = """
            module M
            {
                public int Count()
                {
                    var xs = new System.Collections.Generic.List<int> { 1, 2, 3 };
                    return xs.Count;
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("new System.Collections.Generic.List<int>() { 1, 2, 3 }", code);
        AssertCompiles(code, "CollectionInit");
    }

    [Fact]
    public void ObjectInitializer_PropertyAssignments_EmitAndCompile()
    {
        const string src = """
            module M
            {
                public int Cap()
                {
                    var sb = new System.Text.StringBuilder { Capacity = 64 };
                    return sb.Capacity;
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("new System.Text.StringBuilder() { Capacity = 64 }", code);
        AssertCompiles(code, "ObjectInit");
    }

    [Fact]
    public void CtorArgsPlusInitializer_EmitAndCompile()
    {
        const string src = """
            module M
            {
                public int Count()
                {
                    var xs = new System.Collections.Generic.List<int>(8) { 1, 2 };
                    return xs.Count;
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("new System.Collections.Generic.List<int>(8) { 1, 2 }", code);
        AssertCompiles(code, "CtorPlusInit");
    }

    [Fact]
    public void PlainNew_StillEmitsWithoutBraces()
    {
        // Regression: a constructor call with no initializer must not gain `{ }`.
        const string src = """
            message Ping();
            module M
            {
                public Ping P() { return new Ping(); }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("new Ping()", code);
        Assert.DoesNotContain("new Ping() {", code);
    }
}
