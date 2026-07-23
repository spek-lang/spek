using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Spek.Runtime;
using Spek.Testing;
using Spek.Tests.Emit;
using Xunit;

namespace Spek.Tests.Runtime;

/// <summary>
/// End-to-end test for the per-child <c>supervise(fieldName, strategy: ...)</c>
/// form. The emitter has always generated the per-child branch; this test
/// verifies the runtime wiring works — specifically, that a per-child
/// override changes the policy for a specific child while other children
/// still fall through to the default <c>supervise strategy: ...;</c> form.
/// </summary>
public class PerChildSuperviseTests
{
    private const string Shim = """
        namespace PerChildDemo
        {
            internal static class Foo
            {
                public static void ThrowInvalid() =>
                    throw new System.InvalidOperationException("boom");
            }
        }
        """;

    [Fact]
    public async Task PerChildForm_OverridesDefault_ForTargetedChildAsync()
    {
        // `hot` is supervised with Stop (per-child override).
        // `cold` falls through to the default (Restart).
        // Verify that after crashing each, hot is stopped and cold is alive.
        const string src = """
            namespace PerChildDemo;

            message Boom();
            message SpawnWorkers();
            message CrashHot();
            message CrashCold();
            message GetRefs();
            message Refs(ActorRef hot, ActorRef cold);

            actor BadChild
            {
                behavior Idle { on Boom => { Foo.ThrowInvalid(); } }
            }

            public actor Parent
            {
                ActorRef hot;
                ActorRef cold;

                behavior Running
                {
                    on SpawnWorkers =>
                    {
                        hot  = spawn<BadChild>();
                        cold = spawn<BadChild>();
                    }

                    on CrashHot  => { hot.Tell(new Boom()); }
                    on CrashCold => { cold.Tell(new Boom()); }

                    on GetRefs =>
                    {
                        sender.Tell(new Refs(hot, cold));
                    }
                }

                supervise OneForOne(on Failure: Restart);
                supervise(hot, strategy: OneForOne(on Failure: Stop));
            }
            """;

        var parse = SpekCompiler.Parse(src);
        Assert.True(parse.Success,
            string.Join("\n", parse.Diagnostics.Select(d => $"  {d.Code} {d.Line}:{d.Column} {d.Message}")));

        var csharp   = new FileEmitter().Emit(parse.Tree!);
        var assembly = RoslynCompileHelper.CompileAndLoad(new[] { csharp, Shim }, "PerChildE2E");

        var parentType      = assembly.GetType("PerChildDemo.Parent")!;
        var spawnWorkersTy  = assembly.GetType("PerChildDemo.SpawnWorkers")!;
        var crashHotTy      = assembly.GetType("PerChildDemo.CrashHot")!;
        var crashColdTy     = assembly.GetType("PerChildDemo.CrashCold")!;
        var getRefsTy       = assembly.GetType("PerChildDemo.GetRefs")!;
        var refsTy          = assembly.GetType("PerChildDemo.Refs")!;

        using var system = new TestActorSystem("per-child");
        var probe  = system.CreateProbe();
        var parent = system.Spawn(parentType);

        parent.Tell(Activator.CreateInstance(spawnWorkersTy)!);

        // Pull refs back out so we can inspect them from C# (peeking at
        // `.IsStopped` from inside Spek source would trip CE0012).
        probe.Send(parent, Activator.CreateInstance(getRefsTy)!);
        var refs = probe.ExpectMsg(refsTy);
        var hot  = (ActorRef)refsTy.GetProperty("hot")!.GetValue(refs)!;
        var cold = (ActorRef)refsTy.GetProperty("cold")!.GetValue(refs)!;

        // Crash hot: override says Stop → hot is stopped.
        parent.Tell(Activator.CreateInstance(crashHotTy)!);

        // Crash cold: default says Restart → cold restarts, stays alive.
        parent.Tell(Activator.CreateInstance(crashColdTy)!);

        await system.WhenIdleAsync(TimeSpan.FromSeconds(10));

        Assert.True(hot.IsStopped,
            "hot should be stopped by the per-child Stop override");
        Assert.False(cold.IsStopped,
            "cold should be alive — default Restart policy applied");
    }
}
