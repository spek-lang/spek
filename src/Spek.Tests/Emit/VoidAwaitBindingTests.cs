using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Xunit;
using Xunit.Abstractions;

namespace Spek.Tests.Emit;

/// <summary>
/// Invisible async — a `var` local that binds a call returning a NON-generic
/// `Task`/`ValueTask` (a void-returning async call, e.g.
/// <c>File.WriteAllTextAsync(...)</c>) used to emit
/// <c>var x = await File.WriteAllTextAsync(...)</c>. Awaiting a non-generic
/// Task yields <c>void</c>, so that's invalid C# (CS0815, "Cannot assign void
/// to an implicitly-typed variable"). The rewriter now drops the meaningless
/// binding and keeps the awaited call as a bare statement. Generic
/// <c>Task&lt;T&gt;</c> bindings (which await to a real value) and explicit
/// <c>Task</c>-typed locals (the escape hatch) are untouched.
/// </summary>
public sealed class VoidAwaitBindingTests(ITestOutputHelper output)
{
    private static string EmitCSharp(string source)
    {
        var parsed = SpekCompiler.Parse(source);
        Assert.True(parsed.Success,
            "Source must parse; diagnostics: " +
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
    public void VarBoundVoidTask_DropsBinding_AndCompiles()
    {
        // Two `var` locals bound to a void-returning async call (non-generic
        // Task). Both were eager-awaited into `var x = await …` → CS0815.
        const string src = """
            using System.IO;
            message Tick();
            actor A
            {
                on Tick =>
                {
                    var top = File.WriteAllTextAsync("p", "t");
                    if (1 == 1)
                    {
                        var nested = File.WriteAllTextAsync("q", "u");
                    }
                }
            }
            """;
        var code = EmitCSharp(src);

        // The meaningless `var x =` binding is dropped; the call stays as a
        // bare awaited statement (token-threaded, so match tolerantly).
        Assert.Contains("await File.WriteAllTextAsync(\"p\", \"t\"", code);
        Assert.Contains("await File.WriteAllTextAsync(\"q\", \"u\"", code);
        Assert.DoesNotContain("var top = await", code);
        Assert.DoesNotContain("var nested = await", code);

        // And the result is valid C# — no CS0815.
        AssertCompiles(code, "VarBoundVoidTask");
    }

    [Fact]
    public void VarBoundGenericTask_StillBindsAndAwaitsToValue()
    {
        // A generic Task<int> binding awaits to a real value, so the binding
        // MUST be preserved — `var n = await GetCountAsync(...)` — unchanged.
        const string src = """
            message Tick();
            actor Counter
            {
                on Tick =>
                {
                    var n = System.Threading.Tasks.Task.FromResult(7);
                    int doubled = n + n;
                }
            }
            """;
        var code = EmitCSharp(src);

        // Binding kept and awaited to the int value (not dropped).
        Assert.Contains("var n = await System.Threading.Tasks.Task.FromResult(7)", code);
        AssertCompiles(code, "VarBoundGenericTask");
    }

    [Fact]
    public void ExplicitTaskLocal_BindingIsKept()
    {
        // The explicit-Task escape hatch must not be dropped — it defers and is
        // joined before the method returns.
        const string src = """
            module Io
            {
                public void Peek(string path)
                {
                    Task t = System.IO.File.WriteAllTextAsync(path, "x");
                }
            }
            """;
        var code = EmitCSharp(src);

        Assert.Contains("Task t = System.IO.File.WriteAllTextAsync(path, \"x\")", code);
        Assert.DoesNotContain("Task t = await", code);
        // Joined at exit, so the Task never outlives the scope.
        Assert.Contains("await t", code);
        AssertCompiles(code, "ExplicitTaskLocal");
    }
}
