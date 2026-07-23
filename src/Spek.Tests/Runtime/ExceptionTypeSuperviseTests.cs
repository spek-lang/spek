using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Spek.Runtime;
using Spek.Testing;
using Spek.Tests.Emit;
using Xunit;

namespace Spek.Tests.Runtime;

/// <summary>
/// End-to-end test for exception-type matching on <c>supervise</c> decl
/// arms. A single strategy can declare multiple <c>on Failure(ExType):
/// Action</c> arms; the runtime dispatches based on the concrete
/// exception type of the child's crash. Matches Akka's
/// <c>SupervisorStrategy</c> pattern-match convention (file an
/// <c>IOException</c> to one action, an <c>InvalidOperationException</c>
/// to another).
///
/// This test stands up a parent whose child can crash with either of
/// two exception types, and verifies:
/// <list type="bullet">
///   <item><c>IOException</c> → Restart arm fires → child stays alive.</item>
///   <item><c>InvalidOperationException</c> → Stop arm fires → child stops.</item>
///   <item>Arms match top-to-bottom; the untyped catch-all at the end
///         is the fallback.</item>
/// </list>
/// </summary>
public class ExceptionTypeSuperviseTests
{
    private const string Shim = """
        namespace ExceptionTypeDemo
        {
            internal static class Crashers
            {
                public static void ThrowIo()      =>
                    throw new System.IO.IOException("io boom");
                public static void ThrowInvalid() =>
                    throw new System.InvalidOperationException("invalid boom");
            }
        }
        """;

    [Fact]
    public async Task TypedArms_RouteByExceptionType()
    {
        // Strategy:
        //   on Failure(System.IO.IOException): Restart
        //   on Failure(System.InvalidOperationException): Stop
        //   on Failure: Escalate                     // catch-all
        //
        // Child that throws IOException should keep running (restarted);
        // child that throws InvalidOperationException should be stopped.
        const string src = """
            namespace ExceptionTypeDemo;

            message ThrowIo();
            message ThrowInvalid();
            message SpawnChild();
            message GetChild();
            message ChildRef(ActorRef child);

            actor BadChild
            {
                behavior Idle
                {
                    on ThrowIo      => { Crashers.ThrowIo();      }
                    on ThrowInvalid => { Crashers.ThrowInvalid(); }
                }
            }

            public actor Parent
            {
                ActorRef child;

                behavior Running
                {
                    on SpawnChild =>
                    {
                        child = spawn<BadChild>();
                    }

                    on ThrowIo      => { child.Tell(new ThrowIo());      }
                    on ThrowInvalid => { child.Tell(new ThrowInvalid()); }

                    on GetChild => { sender.Tell(new ChildRef(child)); }
                }

                supervise OneForOne(
                    on Failure(System.IO.IOException): Restart,
                    on Failure(System.InvalidOperationException): Stop,
                    on Failure: Escalate);
            }
            """;

        var parse = SpekCompiler.Parse(src);
        Assert.True(parse.Success,
            string.Join("\n", parse.Diagnostics.Select(d => $"  {d.Code} {d.Line}:{d.Column} {d.Message}")));

        var csharp   = new FileEmitter().Emit(parse.Tree!);
        var assembly = RoslynCompileHelper.CompileAndLoad(new[] { csharp, Shim }, "ExceptionTypeE2E");

        var parentType    = assembly.GetType("ExceptionTypeDemo.Parent")!;
        var spawnChildTy  = assembly.GetType("ExceptionTypeDemo.SpawnChild")!;
        var throwIoTy     = assembly.GetType("ExceptionTypeDemo.ThrowIo")!;
        var throwInvTy    = assembly.GetType("ExceptionTypeDemo.ThrowInvalid")!;
        var getChildTy    = assembly.GetType("ExceptionTypeDemo.GetChild")!;
        var childRefTy    = assembly.GetType("ExceptionTypeDemo.ChildRef")!;

        // Scenario A — IOException arm says Restart: child stays alive.
        {
            using var system = new TestActorSystem("exn-type-ioexception");
            var probe  = system.CreateProbe();
            var parent = system.Spawn(parentType);

            parent.Tell(Activator.CreateInstance(spawnChildTy)!);

            probe.Send(parent, Activator.CreateInstance(getChildTy)!);
            var refMsg = probe.ExpectMsg(childRefTy);
            var child  = (ActorRef)childRefTy.GetProperty("child")!.GetValue(refMsg)!;

            parent.Tell(Activator.CreateInstance(throwIoTy)!);
            await system.WhenIdleAsync(TimeSpan.FromSeconds(10));

            Assert.False(child.IsStopped,
                "IOException arm should Restart → child stays alive");
        }

        // Scenario B — InvalidOperationException arm says Stop: child stops.
        {
            using var system = new TestActorSystem("exn-type-invalidop");
            var probe  = system.CreateProbe();
            var parent = system.Spawn(parentType);

            parent.Tell(Activator.CreateInstance(spawnChildTy)!);

            probe.Send(parent, Activator.CreateInstance(getChildTy)!);
            var refMsg = probe.ExpectMsg(childRefTy);
            var child  = (ActorRef)childRefTy.GetProperty("child")!.GetValue(refMsg)!;

            parent.Tell(Activator.CreateInstance(throwInvTy)!);
            Thread.Sleep(200);

            Assert.True(child.IsStopped,
                "InvalidOperationException arm should Stop → child is dead");
        }
    }

    [Fact]
    public void UntypedCatchAll_AppliesWhenNoTypedArmMatches()
    {
        // Strategy:
        //   on Failure(System.IO.IOException): Stop
        //   on Failure: Restart                      // catch-all
        //
        // Child throws InvalidOperationException — neither matches
        // IOException, so the untyped catch-all fires: Restart → alive.
        const string src = """
            namespace ExceptionTypeDemo;

            message ThrowInvalid();
            message SpawnChild();
            message GetChild();
            message ChildRef(ActorRef child);

            actor BadChild
            {
                behavior Idle
                {
                    on ThrowInvalid => { Crashers.ThrowInvalid(); }
                }
            }

            public actor Parent
            {
                ActorRef child;

                behavior Running
                {
                    on SpawnChild  => { child = spawn<BadChild>(); }
                    on ThrowInvalid => { child.Tell(new ThrowInvalid()); }
                    on GetChild    => { sender.Tell(new ChildRef(child)); }
                }

                supervise OneForOne(
                    on Failure(System.IO.IOException): Stop,
                    on Failure: Restart);
            }
            """;

        var parse = SpekCompiler.Parse(src);
        Assert.True(parse.Success,
            string.Join("\n", parse.Diagnostics.Select(d => $"  {d.Code} {d.Line}:{d.Column} {d.Message}")));

        var csharp   = new FileEmitter().Emit(parse.Tree!);
        var assembly = RoslynCompileHelper.CompileAndLoad(new[] { csharp, Shim }, "ExceptionTypeFallbackE2E");

        var parentType    = assembly.GetType("ExceptionTypeDemo.Parent")!;
        var spawnChildTy  = assembly.GetType("ExceptionTypeDemo.SpawnChild")!;
        var throwInvTy    = assembly.GetType("ExceptionTypeDemo.ThrowInvalid")!;
        var getChildTy    = assembly.GetType("ExceptionTypeDemo.GetChild")!;
        var childRefTy    = assembly.GetType("ExceptionTypeDemo.ChildRef")!;

        using var system = new TestActorSystem("exn-type-fallback");
        var probe  = system.CreateProbe();
        var parent = system.Spawn(parentType);

        parent.Tell(Activator.CreateInstance(spawnChildTy)!);

        probe.Send(parent, Activator.CreateInstance(getChildTy)!);
        var refMsg = probe.ExpectMsg(childRefTy);
        var child  = (ActorRef)childRefTy.GetProperty("child")!.GetValue(refMsg)!;

        parent.Tell(Activator.CreateInstance(throwInvTy)!);
        Thread.Sleep(200);

        Assert.False(child.IsStopped,
            "InvalidOperationException should miss the IOException arm " +
            "and fall through to the Restart catch-all → child alive");
    }
}
