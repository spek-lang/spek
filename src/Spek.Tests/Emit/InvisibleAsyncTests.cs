using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Xunit;
using Xunit.Abstractions;

namespace Spek.Tests.Emit;

/// <summary>
/// Invisible async/await. The developer never writes `async` or
/// `await`; the compiler's semantic pass auto-awaits Task-returning
/// calls, marks the enclosing method async, and propagates async-ness
/// through Spek functions to a fixpoint. Detection is a real Roslyn
/// semantic model seeded with the framework BCL — so framework async
/// (Task.Delay, File.*Async, ...) is detected precisely.
/// </summary>
public sealed class InvisibleAsyncTests(ITestOutputHelper output)
{
    private static string EmitCSharp(string source)
    {
        var parsed = SpekCompiler.Parse(source);
        Assert.True(parsed.Success,
            "Source must parse; diagnostics: " +
            string.Join("\n", parsed.Diagnostics.Select(d => $"{d.Code} {d.Line}:{d.Column} {d.Message}")));
        return new FileEmitter().Emit(parsed.Tree!);
    }

    [Fact]
    public void Handler_AutoAwaits_TaskReturningCall()
    {
        const string src = """
            message Go();

            actor Worker
            {
                on Go => { System.Threading.Tasks.Task.Delay(1); }
            }
            """;
        var code = EmitCSharp(src);
        // Auto-awaited. (A handler also threads in the shutdown token —
        // `Delay(1, cancellationToken: this.ShutdownToken)` — so match the call
        // open-paren tolerantly; the token itself is covered by
        // InvisibleCancellationTests.)
        Assert.Contains("await System.Threading.Tasks.Task.Delay(1", code);
    }

    [Fact]
    public void ModuleMethod_BecomesAsync_WhenItAwaits()
    {
        const string src = """
            module Io
            {
                public int ReadLen(string path)
                {
                    string content = System.IO.File.ReadAllTextAsync(path);
                    return content.Length;
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("await System.IO.File.ReadAllTextAsync(path)", code);
        // void/int return type was rewritten and `async` added.
        Assert.Contains("async", code);
        Assert.Contains("System.Threading.Tasks.Task<int>", code);
    }

    [Fact]
    public void Async_Propagates_ThroughSpekCallers()
    {
        const string src = """
            module Io
            {
                public int ReadLen(string path)
                {
                    string content = System.IO.File.ReadAllTextAsync(path);
                    return content.Length;
                }

                public int Twice(string path)
                {
                    int a = ReadLen(path);
                    return a + a;
                }
            }
            """;
        var code = EmitCSharp(src);
        // ReadLen became async Task<int>; the in-source call to it is now
        // Task-typed and auto-awaited (propagation via fixpoint).
        Assert.Contains("await ReadLen(path)", code);
    }

    [Fact]
    public void ExplicitTaskLocal_SuppressedAtBinding_JoinedAtExit()
    {
        const string src = """
            module Io
            {
                public void Peek(string path)
                {
                    Task<string> t = System.IO.File.ReadAllTextAsync(path);
                }
            }
            """;
        var code = EmitCSharp(src);
        // The explicit Task<string> local opts out of await AT THE BINDING...
        Assert.Contains("Task<string> t = System.IO.File.ReadAllTextAsync(path)", code);
        Assert.DoesNotContain("Task<string> t = await", code);
        // ...but the structured join awaits it before the method returns,
        // so the Task never outlives the scope (Stage 1 closes that leak).
        Assert.Contains("await t", code);
    }

    [Fact]
    public void VarLocal_DefersAndAwaitsAtUse()
    {
        const string src = """
            module Io
            {
                public int ReadLen(string path)
                {
                    var content = System.IO.File.ReadAllTextAsync(path);
                    return content.Length;
                }
            }
            """;
        var code = EmitCSharp(src);
        // `var` binds the Task (deferred) — not awaited at the binding...
        Assert.Contains("var content = System.IO.File.ReadAllTextAsync(path)", code);
        Assert.DoesNotContain("var content = await", code);
        // ...and is awaited at the value-use (parenthesised for member access).
        Assert.Contains("(await content).Length", code);
    }

    [Fact]
    public void EndToEnd_AsyncModule_Compiles()
    {
        const string src = """
            module Io
            {
                public int ReadLen(string path)
                {
                    string content = System.IO.File.ReadAllTextAsync(path);
                    return content.Length;
                }

                public int Twice(string path)
                {
                    int a = ReadLen(path);
                    return a + a;
                }
            }
            """;
        var code = EmitCSharp(src);
        var (success, summary, _) = RoslynCompileHelper.TryCompile(code, "InvisibleAsyncTest");
        if (!success)
        {
            output.WriteLine("─── emitted C# ───");
            output.WriteLine(code);
            output.WriteLine("─── diagnostics ───");
            output.WriteLine(summary);
        }
        Assert.True(success, $"Emitted async C# did not compile:\n{summary}");
    }
}
