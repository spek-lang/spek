using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Spek.Runtime;
using Xunit;
using Xunit.Abstractions;

namespace Spek.Tests.Emit;

/// <summary>
/// Invisible cooperative cancellation. The emitter threads the actor's
/// <c>ShutdownToken</c> into auto-awaited, cancellation-accepting calls inside
/// actor handlers — never visible in Spek source. Module/static and non-actor
/// contexts are left alone (no <c>this.ShutdownToken</c> to reach).
/// </summary>
public sealed class InvisibleCancellationTests(ITestOutputHelper output)
{
    private string Emit(string src)
    {
        var parsed = SpekCompiler.Parse(src);
        Assert.True(parsed.Success,
            string.Join("\n", parsed.Diagnostics.Select(d => $"{d.Code} {d.Message}")));
        var code = new FileEmitter().Emit(parsed.Tree!);
        var (ok, summary, _) = RoslynCompileHelper.TryCompile(code, "InvisibleCancel");
        if (!ok) { output.WriteLine(code); output.WriteLine(summary); }
        Assert.True(ok, $"emitted C# did not compile:\n{summary}");
        return code;
    }

    [Fact]
    public void ActorHandler_AwaitedCall_ThreadsShutdownToken()
    {
        // Task.Delay(int) has a sibling Task.Delay(int, CancellationToken) overload.
        var code = Emit("""
            message Go();
            actor Worker
            {
                on Go => { System.Threading.Tasks.Task.Delay(50); }
            }
            """);
        Assert.Contains("Task.Delay(50, cancellationToken: this.ShutdownToken)", code);
    }

    [Fact]
    public void ActorHandler_SyncFileIo_ThreadsTokenIntoAsyncSibling()
    {
        // File.ReadAllText → File.ReadAllTextAsync(path, CancellationToken = default);
        // the optional-CT-omitted shape.
        var code = Emit("""
            message Go();
            actor Reader
            {
                on Go => { System.Console.WriteLine(System.IO.File.ReadAllText("x.txt")); }
            }
            """);
        Assert.Contains("ReadAllTextAsync", code);
        Assert.Contains("cancellationToken: this.ShutdownToken", code);
    }

    [Fact]
    public void ModuleMethod_AwaitedCall_GetsNoToken()
    {
        // Static module method: no `this`, no ShutdownToken — must be left alone.
        var code = Emit("""
            module Helper
            {
                public void DoIt() { System.Threading.Tasks.Task.Delay(50); }
            }
            """);
        Assert.Contains("Task.Delay(50)", code);
        Assert.DoesNotContain("ShutdownToken", code);
    }

    [Fact]
    public void NonCancellableCall_IsUnchanged()
    {
        // A Task-returning call whose callee takes no CancellationToken keeps its
        // argument list (here Task.Yield(), which has no CT overload).
        var code = Emit("""
            message Go();
            actor Worker
            {
                on Go => { System.Threading.Tasks.Task.Yield(); }
            }
            """);
        Assert.DoesNotContain("ShutdownToken", code);
    }

    [Fact]
    public void CallAlreadyPassingAToken_IsNotDoubleThreaded()
    {
        // The handler already supplies a token — the rewriter must not append a
        // second one (Delay(int, CancellationToken) is fully satisfied).
        var code = Emit("""
            message Go();
            actor Worker
            {
                on Go => { System.Threading.Tasks.Task.Delay(50, System.Threading.CancellationToken.None); }
            }
            """);
        Assert.Contains("CancellationToken.None", code);
        Assert.DoesNotContain("ShutdownToken", code);
    }

    [Fact]
    public void NonActorClassMethod_GetsNoToken()
    {
        // A plain `class` (not an actor) has no ShutdownToken accessor — its
        // instance methods are a different code path from a module static method,
        // and must also be left alone.
        var code = Emit("""
            class Reader
            {
                public void Go() { System.Threading.Tasks.Task.Delay(50); }
            }
            """);
        Assert.Contains("Task.Delay(50)", code);
        Assert.DoesNotContain("ShutdownToken", code);
    }

    [Fact]
    public void SyncIoVarBinding_ThreadsTokenIntoAsyncSibling()
    {
        // A `var` binding of a sync-I/O call (rewritten to its *Async sibling and
        // awaited) threads the token too. Uses File.ReadAllText → ReadAllTextAsync,
        // which returns a value, so the awaited `var` binding is well-typed.
        var code = Emit("""
            message Go();
            actor Worker
            {
                on Go =>
                {
                    var body = System.IO.File.ReadAllText("x.txt");
                    System.Console.WriteLine(body);
                }
            }
            """);
        Assert.Contains("ReadAllTextAsync", code);
        Assert.Contains("cancellationToken: this.ShutdownToken", code);
    }

    [Fact]
    public void ShutdownToken_IsCancelledAfterShutdown()
    {
        // Slice-1 policy: the token cancels by the time the system has torn down.
        var system = new ActorSystem("t");
        var token  = system.ShutdownToken;
        Assert.False(token.IsCancellationRequested);

        system.GracefulShutdown(TimeSpan.FromSeconds(1));

        Assert.True(token.IsCancellationRequested,
            "ShutdownToken must be cancelled once the system has shut down");
    }
}
