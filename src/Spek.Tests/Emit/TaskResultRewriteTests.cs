using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Xunit;
using Xunit.Abstractions;

namespace Spek.Tests.Emit;

/// <summary>
/// Invisible async rewrites `task.Result` into `(await task)`. Reading
/// `Task&lt;T&gt;.Result` blocks the dispatcher thread; `await task` yields the
/// same T without blocking. The rewriter is type-aware (only real
/// Task/ValueTask receivers) and only fires where an await is legal, so a
/// non-Task `.Result` property is left alone and the enclosing method/handler
/// is promoted to async.
/// </summary>
public sealed class TaskResultRewriteTests(ITestOutputHelper output)
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
    public void CallResult_RewrittenToAwait()
    {
        const string src = """
            module M
            {
                public int Grab()
                {
                    return System.Threading.Tasks.Task.FromResult(41).Result;
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("(await System.Threading.Tasks.Task.FromResult(41))", code);
        Assert.DoesNotContain(".Result", code);
        Assert.Contains("async", code);   // Grab() promoted to async
        AssertCompiles(code, "CallResult");
    }

    [Fact]
    public void LocalTaskResult_RewrittenToAwait()
    {
        const string src = """
            module M
            {
                public int Grab()
                {
                    var t = System.Threading.Tasks.Task.FromResult(7);
                    return t.Result;
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("(await t)", code);
        Assert.DoesNotContain(".Result", code);
        AssertCompiles(code, "LocalResult");
    }

    [Fact]
    public void GetAwaiterGetResult_RewrittenToAwait()
    {
        const string src = """
            module M
            {
                public int Grab()
                {
                    return System.Threading.Tasks.Task.FromResult(9).GetAwaiter().GetResult();
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("(await System.Threading.Tasks.Task.FromResult(9))", code);
        Assert.DoesNotContain("GetResult", code);
        Assert.DoesNotContain("GetAwaiter", code);
        AssertCompiles(code, "GetResult");
    }

    [Fact]
    public void Wait_RewrittenToAwaitStatement()
    {
        const string src = """
            module M
            {
                public void Drain()
                {
                    System.Threading.Tasks.Task.CompletedTask.Wait();
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("await System.Threading.Tasks.Task.CompletedTask", code);
        Assert.DoesNotContain(".Wait()", code);
        AssertCompiles(code, "Wait");
    }

    [Fact]
    public void NonTaskResult_LeftAlone()
    {
        // A user type with a plain `Result` property must NOT be rewritten —
        // the receiver isn't a Task.
        const string src = """
            public class Box { public int Result { get; set; } }
            module M
            {
                public int Read(Box b) { return b.Result; }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("b.Result", code);
        Assert.DoesNotContain("await", code);
        AssertCompiles(code, "NonTaskResult");
    }

    [Fact]
    public void SyncFileRead_RewrittenToAsyncSibling()
    {
        // File.ReadAllText(p) → await File.ReadAllTextAsync(p): same string,
        // no blocked dispatcher. (CE0115 warns; this makes it non-blocking.)
        const string src = """
            using System.IO;
            module M
            {
                public string Load(string path)
                {
                    return File.ReadAllText(path);
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("await File.ReadAllTextAsync(path)", code);
        Assert.DoesNotContain("File.ReadAllText(path)", code);   // the sync call is gone
        Assert.Contains("async", code);                          // Load() promoted to async
        AssertCompiles(code, "SyncFileRead");
    }

    [Fact]
    public void SyncFileWrite_RewrittenToAsyncSibling()
    {
        // Void-returning statement form: File.WriteAllText(p, b) →
        // await File.WriteAllTextAsync(p, b).
        const string src = """
            using System.IO;
            module M
            {
                public void Save(string path, string body)
                {
                    File.WriteAllText(path, body);
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("await File.WriteAllTextAsync(path, body)", code);
        Assert.DoesNotContain("File.WriteAllText(path, body)", code);
        AssertCompiles(code, "SyncFileWrite");
    }

    [Fact]
    public void UserDefinedFile_NotRewritten()
    {
        // A non-System.IO `File.ReadAllText` must be left alone — the rewriter
        // binds the symbol and checks the containing type is System.IO.File.
        // Tested at the rewriter level (raw C#) since Spek's class grammar
        // doesn't allow the static method needed to express it in .spek source.
        const string csharp = """
            public static class File { public static string ReadAllText(string p) => p; }
            public class M
            {
                public string Read(string path) { return File.ReadAllText(path); }
            }
            """;
        var rewritten = AsyncRewriter.Rewrite(csharp);
        Assert.Contains("File.ReadAllText(path)", rewritten);
        Assert.DoesNotContain("ReadAllTextAsync", rewritten);
        Assert.DoesNotContain("await", rewritten);
    }
}
