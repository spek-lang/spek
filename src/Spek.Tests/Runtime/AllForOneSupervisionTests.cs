using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Spek.Runtime;
using Spek.Testing;
using Spek.Tests.Emit;
using Xunit;

namespace Spek.Tests.Runtime;

/// <summary>
/// End-to-end tests for the <c>AllForOne</c> supervise strategy — when one
/// child fails, every sibling under the same parent is restarted in
/// lockstep (loses in-memory state, reloads from the latest snapshot for
/// persistent actors). Contrasts with <c>OneForOne</c>, which leaves
/// siblings alone.
/// </summary>
public class AllForOneSupervisionTests
{
    private const string Shim = """
        namespace AllForOneDemo
        {
            internal static class Foo
            {
                public static void ThrowInvalid() =>
                    throw new System.InvalidOperationException("boom");
            }
        }
        """;

    [Fact]
    public async Task AllForOne_RestartsSiblings_WhenOneChildFailsAsync()
    {
        // Parent has two children, A (incrementing counter) and B (badChild).
        // B throws → parent's AllForOne strategy restarts BOTH (A loses its
        // in-memory count because it wasn't persisted; B also restarts).
        const string src = """
            namespace AllForOneDemo;

            message Increment();
            message Get();
            message Reply(int count);
            message Boom();

            message SpawnWorkers();
            message IncrementA();
            message AskA();
            message CrashB();
            message QueryResult();
            message QueryResultReply(int countA, bool bStopped);

            actor Counter
            {
                int count = 0;

                behavior Counting
                {
                    on Increment => { count = count + 1; }
                    on Get       => return new Reply(count);
                }
            }

            actor BadChild
            {
                behavior Idle
                {
                    on Boom => { Foo.ThrowInvalid(); }
                }
            }

            public actor Parent
            {
                ActorRef a;
                ActorRef b;

                behavior Running
                {
                    on SpawnWorkers =>
                    {
                        a = spawn<Counter>();
                        b = spawn<BadChild>();
                    }

                    on IncrementA => { a.Tell(new Increment()); }
                    on AskA       =>
                    {
                        var r = a.Ask(new Get());
                        sender.Tell(new Reply(r.count));
                    }
                    on CrashB  => { b.Tell(new Boom()); }
                }

                supervise AllForOne(on Failure: Restart);
            }
            """;

        var parse = SpekCompiler.Parse(src);
        Assert.True(parse.Success,
            string.Join("\n", parse.Diagnostics.Select(d => $"  {d.Code} {d.Line}:{d.Column} {d.Message}")));

        var csharp = new FileEmitter().Emit(parse.Tree!);

        // The emitter should have generated the broadcast call.
        Assert.Contains("RestartSiblingsOf(child)", csharp);

        var assembly = RoslynCompileHelper.CompileAndLoad(new[] { csharp, Shim }, "AllForOneE2E");

        var parentType       = assembly.GetType("AllForOneDemo.Parent")!;
        var spawnWorkersType = assembly.GetType("AllForOneDemo.SpawnWorkers")!;
        var incrementAType   = assembly.GetType("AllForOneDemo.IncrementA")!;
        var askAType         = assembly.GetType("AllForOneDemo.AskA")!;
        var crashBType       = assembly.GetType("AllForOneDemo.CrashB")!;
        var replyType        = assembly.GetType("AllForOneDemo.Reply")!;

        using var system = new TestActorSystem("allforone");
        var probe  = system.CreateProbe();
        var parent = system.Spawn(parentType);

        // Spawn children, build up A's counter to 3.
        parent.Tell(Activator.CreateInstance(spawnWorkersType)!);
        parent.Tell(Activator.CreateInstance(incrementAType)!);
        parent.Tell(Activator.CreateInstance(incrementAType)!);
        parent.Tell(Activator.CreateInstance(incrementAType)!);

        // Verify A is at 3 before the crash.
        probe.Send(parent, Activator.CreateInstance(askAType)!);
        var before = (int)replyType.GetProperty("count")!.GetValue(probe.ExpectMsg(replyType))!;
        Assert.Equal(3, before);

        // Crash B. AllForOne should also restart A — losing A's in-memory
        // count (it's non-persistent, so restart resets to 0).
        parent.Tell(Activator.CreateInstance(crashBType)!);

        // Wait for the restart work to drain (observable, not a guessed sleep).
        await system.WhenIdleAsync(TimeSpan.FromSeconds(10));

        // Ask A again — should be 0 now after the broadcast restart.
        probe.Send(parent, Activator.CreateInstance(askAType)!);
        var after = (int)replyType.GetProperty("count")!.GetValue(probe.ExpectMsg(replyType))!;
        Assert.Equal(0, after);
    }

    [Fact]
    public async Task OneForOne_DoesNotRestartSiblingsAsync()
    {
        // Same shape as AllForOne, but strategy is OneForOne — A's count
        // should survive B's crash.
        const string src = """
            namespace OneForOneDemo;

            message Increment();
            message Get();
            message Reply(int count);
            message Boom();

            message SpawnWorkers();
            message IncrementA();
            message AskA();
            message CrashB();

            actor Counter
            {
                int count = 0;

                behavior Counting
                {
                    on Increment => { count = count + 1; }
                    on Get       => return new Reply(count);
                }
            }

            actor BadChild
            {
                behavior Idle
                {
                    on Boom => { Foo.ThrowInvalid(); }
                }
            }

            public actor Parent
            {
                ActorRef a;
                ActorRef b;

                behavior Running
                {
                    on SpawnWorkers =>
                    {
                        a = spawn<Counter>();
                        b = spawn<BadChild>();
                    }

                    on IncrementA => { a.Tell(new Increment()); }
                    on AskA       =>
                    {
                        var r = a.Ask(new Get());
                        sender.Tell(new Reply(r.count));
                    }
                    on CrashB  => { b.Tell(new Boom()); }
                }

                supervise OneForOne(on Failure: Restart);
            }
            """;

        var parse = SpekCompiler.Parse(src);
        Assert.True(parse.Success);

        var csharp = new FileEmitter().Emit(parse.Tree!);
        Assert.DoesNotContain("RestartSiblingsOf(child)", csharp);

        const string oneForOneShim = """
            namespace OneForOneDemo
            {
                internal static class Foo
                {
                    public static void ThrowInvalid() =>
                        throw new System.InvalidOperationException("boom");
                }
            }
            """;

        var assembly = RoslynCompileHelper.CompileAndLoad(new[] { csharp, oneForOneShim }, "OneForOneE2E");
        var parentType       = assembly.GetType("OneForOneDemo.Parent")!;
        var spawnWorkersType = assembly.GetType("OneForOneDemo.SpawnWorkers")!;
        var incrementAType   = assembly.GetType("OneForOneDemo.IncrementA")!;
        var askAType         = assembly.GetType("OneForOneDemo.AskA")!;
        var crashBType       = assembly.GetType("OneForOneDemo.CrashB")!;
        var replyType        = assembly.GetType("OneForOneDemo.Reply")!;

        using var system = new TestActorSystem("oneforone");
        var probe  = system.CreateProbe();
        var parent = system.Spawn(parentType);

        parent.Tell(Activator.CreateInstance(spawnWorkersType)!);
        parent.Tell(Activator.CreateInstance(incrementAType)!);
        parent.Tell(Activator.CreateInstance(incrementAType)!);
        parent.Tell(Activator.CreateInstance(incrementAType)!);

        parent.Tell(Activator.CreateInstance(crashBType)!);
        await system.WhenIdleAsync(TimeSpan.FromSeconds(10));

        probe.Send(parent, Activator.CreateInstance(askAType)!);
        var after = (int)replyType.GetProperty("count")!.GetValue(probe.ExpectMsg(replyType))!;

        // A was untouched by B's crash under OneForOne — still 3.
        Assert.Equal(3, after);
    }
}
