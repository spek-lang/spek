using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Xunit;
using Xunit.Abstractions;

namespace Spek.Tests.Emit;

/// <summary>
/// `using var x = ...` local declarations (deterministic disposal at
/// scope end). Parsed via the optional `USING?` prefix on `varDecl`, carried
/// as VarDeclStmt.IsUsing, emitted with a `using ` prefix. This is the natural
/// C# idiom for the IDisposable runtime types (ActorSystem, TestActorSystem).
/// </summary>
public sealed class UsingVarTests(ITestOutputHelper output)
{
    private static string EmitCSharp(string source)
    {
        var parsed = SpekCompiler.Parse(source);
        Assert.True(parsed.Success,
            string.Join("\n", parsed.Diagnostics.Select(d => $"{d.Code} {d.Message}")));
        return new FileEmitter().Emit(parsed.Tree!);
    }

    [Fact]
    public void UsingVar_EmitsUsingPrefix_AndCompiles()
    {
        const string src = """
            module M
            {
                public long Use()
                {
                    using var ms = new System.IO.MemoryStream();
                    return ms.Length;
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("using var ms = new System.IO.MemoryStream();", code);

        var (ok, summary, _) = RoslynCompileHelper.TryCompile(code, "UsingVar");
        if (!ok) { output.WriteLine(code); output.WriteLine(summary); }
        Assert.True(ok, $"Emitted C# did not compile:\n{summary}");
    }

    [Fact]
    public void PlainVar_GetsNoUsingPrefix()
    {
        // Regression: a normal `var` must not gain a `using` prefix.
        const string src = """
            module M
            {
                public int Plain() { var x = 5; return x; }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("var x = 5;", code);
        Assert.DoesNotContain("using var x", code);
    }
}
