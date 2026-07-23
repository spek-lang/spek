using Spek.Compiler.Parser;
using Spek.Compiler.Semantic;
using Xunit;

namespace Spek.Tests.Semantic;

/// <summary>
/// CE0119. Concurrency in Spek comes from actors. Raw concurrency-
/// spawning BCL primitives (Task.Run, Parallel.*, ThreadPool.*, new Thread,
/// new Timer) run a delegate outside any actor's turn, bypassing the
/// serialization that makes Spek race-free — so they are a hard error in Spek
/// source. (Supersedes the narrower CE0106 Task.Run-capture warning.)
/// </summary>
public sealed class RawConcurrencyTests
{
    private static bool HasError(CompilationResult r, string code) =>
        r.Diagnostics.Any(d => d.Code == code && d.Severity == DiagnosticSeverity.Error);

    [Fact]
    public void CE0119_TaskRun_InHandler_Errors()
    {
        const string src = """
            using System.Threading.Tasks;

            message Tick();
            actor A
            {
                int counter = 0;

                on Tick =>
                {
                    Task.Run(() => { counter = counter + 1; });
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.False(parsed.Success, "CE0119 is an error — it must fail the build.");
        Assert.True(HasError(parsed, "CE0119"));
    }

    [Fact]
    public void CE0119_NewThread_Errors()
    {
        const string src = """
            using System.Threading;

            message Tick();
            actor A
            {
                on Tick =>
                {
                    var t = new Thread(() => { System.Console.WriteLine("x"); });
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.True(HasError(parsed, "CE0119"));
    }

    [Fact]
    public void CE0119_ParallelFor_Errors()
    {
        const string src = """
            using System.Threading.Tasks;

            message Tick();
            actor A
            {
                on Tick =>
                {
                    Parallel.For(0, 10, i => { System.Console.WriteLine(i); });
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.True(HasError(parsed, "CE0119"));
    }

    [Fact]
    public void CE0119_ThreadPoolQueueUserWorkItem_Errors()
    {
        const string src = """
            using System.Threading;

            message Tick();
            actor A
            {
                on Tick =>
                {
                    ThreadPool.QueueUserWorkItem(s => { System.Console.WriteLine("x"); });
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.True(HasError(parsed, "CE0119"));
    }

    [Fact]
    public void CE0119_NewTimer_WithCallback_Errors()
    {
        const string src = """
            using System.Threading;

            message Tick();
            actor A
            {
                on Tick =>
                {
                    var t = new Timer(s => { System.Console.WriteLine("x"); }, null, 0, 1000);
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.True(HasError(parsed, "CE0119"));
    }

    [Fact]
    public void CE0119_FiresInInit_NotJustHandlers()
    {
        // The retired CE0106 was handler-only; CE0119 forbids raw concurrency
        // everywhere in Spek source, init blocks included.
        const string src = """
            using System.Threading.Tasks;

            message Tick();
            actor A
            {
                int counter = 0;

                init()
                {
                    Task.Run(() => { counter = counter + 1; });
                }

                on Tick => { }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.True(HasError(parsed, "CE0119"));
    }

    [Fact]
    public void CE0119_AwaitedAsync_NotFlagged()
    {
        // The allowed line: a Task-returning BCL call (awaited by invisible
        // async inside the turn) is fine — only thread-SPAWNING is forbidden.
        const string src = """
            using System.Threading.Tasks;

            message Tick();
            actor A
            {
                on Tick =>
                {
                    Task.Delay(100);
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.DoesNotContain(parsed.Diagnostics, d => d.Code == "CE0119");
    }

    [Fact]
    public void CE0119_DomainTypeNamedTimer_NotFlagged()
    {
        // `new Timer(60)` on a domain class named Timer (no delegate argument)
        // is not a concurrency primitive — HasDelegateFirstArg keeps it clean.
        const string src = """
            class Timer { init(int seconds) { } }

            message Tick();
            actor A
            {
                on Tick =>
                {
                    var t = new Timer(60);
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.DoesNotContain(parsed.Diagnostics, d => d.Code == "CE0119");
    }
}
