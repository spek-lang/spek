using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Spek.Runtime;
using Spek.Testing;
using Spek.Tests.Emit;
using Xunit;

namespace Spek.Tests.Runtime;

/// <summary>
/// Full-pipeline tests for the <c>supervise</c> decl — parse a Spek source
/// that declares a supervision policy, emit C#, compile via Roslyn, load,
/// spawn, and verify that the configured policy is actually applied at
/// runtime (not just parsed and dropped).
///
/// The Spek source can't read <see cref="ActorRef.IsStopped"/> directly
/// (CE0012 — member access on an ActorRef is blocked), so each test
/// asks the parent to send back the child's <c>ActorRef</c> in a reply
/// message, and the test checks <see cref="ActorRef.IsStopped"/> on the
/// .NET side.
/// </summary>
public class SuperviseDeclEndToEndTests
{
    private const string Shim = """
        namespace SuperviseDemo
        {
            internal static class Foo
            {
                public static void ThrowInvalid() =>
                    throw new System.InvalidOperationException("boom");
            }
        }
        """;

    [Fact]
    public async Task DefaultSupervise_AppliesRestartPolicy_StopsAfterBudgetExceededAsync()
    {
        const string src = """
            namespace SuperviseDemo;

            message Boom();
            message SpawnIt();
            message ForwardBoom();
            message GetChildRef();
            message ChildRef(ActorRef child);

            actor BadChild
            {
                behavior Idle
                {
                    on Boom => { Foo.ThrowInvalid(); }
                }
            }

            public actor Parent
            {
                ActorRef child;

                behavior Running
                {
                    on SpawnIt => { child = spawn<BadChild>(); }
                    on ForwardBoom => { child.Tell(new Boom()); }
                    on GetChildRef => { sender.Tell(new ChildRef(child)); }
                }

                supervise OneForOne(on Failure: Restart, maxRetries: 3, withinTime: System.TimeSpan.FromSeconds(10));
            }
            """;

        var childStopped = await RunSupervisionScenarioAsync(src, "SuperviseRestartE2E", boomsToSend: 4);
        Assert.True(childStopped,
            "Child should be stopped after 4th Boom exceeds the 3-restart budget.");
    }

    [Fact]
    public async Task DefaultSupervise_WithStopAction_StopsChildOnFirstFailureAsync()
    {
        const string src = """
            namespace SuperviseDemo;

            message Boom();
            message SpawnIt();
            message ForwardBoom();
            message GetChildRef();
            message ChildRef(ActorRef child);

            actor BadChild
            {
                behavior Idle
                {
                    on Boom => { Foo.ThrowInvalid(); }
                }
            }

            public actor Parent
            {
                ActorRef child;

                behavior Running
                {
                    on SpawnIt => { child = spawn<BadChild>(); }
                    on ForwardBoom => { child.Tell(new Boom()); }
                    on GetChildRef => { sender.Tell(new ChildRef(child)); }
                }

                supervise OneForOne(on Failure: Stop);
            }
            """;

        var childStopped = await RunSupervisionScenarioAsync(src, "SuperviseStopE2E", boomsToSend: 1);
        Assert.True(childStopped, "Child should be stopped immediately on first Boom with on Failure: Stop.");
    }

    [Fact]
    public async Task DefaultSupervise_WithRestartNoBudget_KeepsRestartingForeverAsync()
    {
        const string src = """
            namespace SuperviseDemo;

            message Boom();
            message SpawnIt();
            message ForwardBoom();
            message GetChildRef();
            message ChildRef(ActorRef child);

            actor BadChild
            {
                behavior Idle
                {
                    on Boom => { Foo.ThrowInvalid(); }
                }
            }

            public actor Parent
            {
                ActorRef child;

                behavior Running
                {
                    on SpawnIt => { child = spawn<BadChild>(); }
                    on ForwardBoom => { child.Tell(new Boom()); }
                    on GetChildRef => { sender.Tell(new ChildRef(child)); }
                }

                supervise OneForOne(on Failure: Restart);
            }
            """;

        var childStopped = await RunSupervisionScenarioAsync(src, "SuperviseUnlimitedE2E", boomsToSend: 10);
        Assert.False(childStopped,
            "Child should still be alive — unlimited budget means 10 Booms all restart cleanly.");
    }

    // ─── Shared scenario runner ─────────────────────────────────────────────

    /// Runs the scenario and returns the child's <c>IsStopped</c> state captured
    /// *while the system is still alive* — before the `using var system` disposes.
    /// (System shutdown stops every actor gracefully, so reading
    /// IsStopped after disposal would always be true and couldn't distinguish a
    /// supervision stop from the shutdown stop.)
    private static async Task<bool> RunSupervisionScenarioAsync(
        string spekSource, string assemblyName, int boomsToSend)
    {
        var parse = SpekCompiler.Parse(spekSource);
        Assert.True(parse.Success,
            string.Join("\n", parse.Diagnostics.Select(d => $"  {d.Code} {d.Line}:{d.Column} {d.Message}")));

        var csharp = new FileEmitter().Emit(parse.Tree!);
        var assembly = RoslynCompileHelper.CompileAndLoad(new[] { csharp, Shim }, assemblyName);

        var parentType      = assembly.GetType("SuperviseDemo.Parent")!;
        var spawnItType     = assembly.GetType("SuperviseDemo.SpawnIt")!;
        var forwardBoomType = assembly.GetType("SuperviseDemo.ForwardBoom")!;
        var getChildType    = assembly.GetType("SuperviseDemo.GetChildRef")!;
        var childRefType    = assembly.GetType("SuperviseDemo.ChildRef")!;

        using var system = new TestActorSystem("s");
        var probe  = system.CreateProbe();
        var parent = system.Spawn(parentType);

        parent.Tell(Activator.CreateInstance(spawnItType)!);
        for (int i = 0; i < boomsToSend; i++)
            parent.Tell(Activator.CreateInstance(forwardBoomType)!);

        // Await quiescence without parking a thread — under parallel test
        // load a blocking wait competes with the very actors it waits on.
        await system.WhenIdleAsync(TimeSpan.FromSeconds(10));

        probe.Send(parent, Activator.CreateInstance(getChildType)!);
        var reply = probe.ExpectMsg(childRefType);
        var child = (ActorRef)childRefType.GetProperty("child")!.GetValue(reply)!;
        return child.IsStopped;   // captured before `using var system` disposes
    }
}
