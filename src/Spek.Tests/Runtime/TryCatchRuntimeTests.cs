using Spek;
using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Spek.Runtime;
using Spek.Testing;
using Spek.Tests.Emit;
using Xunit;

namespace Spek.Tests.Runtime;

/// <summary>
/// End-to-end coverage that try/catch/throw flows correctly:
/// caught exceptions stay local; uncaught throws escalate to the
/// slot's supervision flow exactly the way unhandled
/// exceptions already did.
/// </summary>
public sealed class TryCatchRuntimeTests
{
    private const string CatchLocallySource = """
        using System;

        message DoCaught();
        message DoUncaught();
        message Recovered();
        message GetCount();
        message Count(int v);

        actor Catcher
        {
            int caughtCount = 0;

            behavior Idle
            {
                public on DoCaught => {
                    try {
                        throw new InvalidOperationException("inner");
                    } catch (InvalidOperationException ex) {
                        caughtCount = caughtCount + 1;
                        sender.Tell(new Recovered());
                    }
                }

                public on GetCount => sender.Tell(new Count(caughtCount));
            }
        }
        """;

    [Fact]
    public void CaughtException_StaysLocal_ActorContinuesProcessing()
    {
        var parse  = SpekCompiler.Parse(CatchLocallySource);
        Assert.True(parse.Success,
            "Parse failed: " +
            string.Join("\n", parse.Diagnostics.Select(d => $"  {d.Code} {d.Line}:{d.Column} {d.Message}")));

        var csharp   = new FileEmitter().Emit(parse.Tree!);
        var assembly = RoslynCompileHelper.CompileAndLoad(csharp, "TryCatchFixture");
        var catcher    = assembly.GetType("Catcher")!;
        var doCaught   = assembly.GetType("DoCaught")!;
        var recovered  = assembly.GetType("Recovered")!;
        var getCount   = assembly.GetType("GetCount")!;
        var countMsg   = assembly.GetType("Count")!;

        var sink = new RecordingDeadLetterSink();
        using var system = new TestActorSystem("try-catch-local", deadLetterSink: sink);
        var probe = system.CreateProbe();
        var actor = system.Spawn(catcher);

        // Send a message whose handler throws and catches the exception.
        probe.Send(actor, Activator.CreateInstance(doCaught)!);
        var ack = probe.ExpectMsg(recovered, TimeSpan.FromSeconds(2));
        Assert.IsType(recovered, ack);

        // Actor stayed alive — query the counter.
        probe.Send(actor, Activator.CreateInstance(getCount)!);
        var reply = probe.ExpectMsg(countMsg, TimeSpan.FromSeconds(2));
        var count = (int)countMsg.GetProperty("v")!.GetValue(reply)!;
        Assert.Equal(1, count);

        // No dead-letters — the catch swallowed the exception locally.
        Assert.Empty(sink.Records);
    }

    [Fact]
    public void UncaughtThrow_EscalatesToSupervisionFlow_DeadLetters()
    {
        // A throw with no matching catch propagates past the handler
        // body. Existing supervision flow takes over: it dead-letters
        // the message and decides Resume/Restart/Stop per the actor's
        // OnFailure policy. Default for a root actor is Stop, so the
        // actor should be stopped after this — confirming the throw
        // wasn't silently swallowed.
        const string uncaughtSource = """
            using System;

            message Boom();

            actor Bomb
            {
                behavior Idle
                {
                    public on Boom => {
                        throw new InvalidOperationException("kaboom");
                    }
                }
            }
            """;
        var parse  = SpekCompiler.Parse(uncaughtSource);
        Assert.True(parse.Success);
        var csharp   = new FileEmitter().Emit(parse.Tree!);
        var assembly = RoslynCompileHelper.CompileAndLoad(csharp, "UncaughtThrowFixture");
        var bomb = assembly.GetType("Bomb")!;
        var boom = assembly.GetType("Boom")!;

        var sink = new RecordingDeadLetterSink();
        using var system = new TestActorSystem("try-catch-escalate", deadLetterSink: sink);
        var probe = system.CreateProbe();
        var actor = system.Spawn(bomb);

        probe.Send(actor, Activator.CreateInstance(boom)!);

        // Wait until the supervision flow has dead-lettered the message.
        var deadline = DateTime.UtcNow + TimeSpan.FromSeconds(2);
        while (sink.Records.Count == 0 && DateTime.UtcNow < deadline)
            Thread.Sleep(20);

        Assert.NotEmpty(sink.Records);
        Assert.Contains(sink.Records, r => r.Cause is InvalidOperationException);
    }

    [Fact]
    public void TypedCatch_RoutesByExceptionType()
    {
        // Two catch arms — only the matching type catches. The other
        // type would escalate to supervision.
        const string typedSource = """
            using System;

            message ThrowKind(int kind);
            message Caught(string which);

            actor Typed
            {
                behavior Idle
                {
                    public on ThrowKind k => {
                        try {
                            if (k.kind == 1) {
                                throw new InvalidOperationException("invalid-op");
                            } else {
                                throw new ArgumentException("argument");
                            }
                        } catch (InvalidOperationException ex) {
                            sender.Tell(new Caught("invalid-op"));
                        } catch (ArgumentException ex) {
                            sender.Tell(new Caught("argument"));
                        }
                    }
                }
            }
            """;
        var parse  = SpekCompiler.Parse(typedSource);
        Assert.True(parse.Success);
        var csharp   = new FileEmitter().Emit(parse.Tree!);
        var assembly = RoslynCompileHelper.CompileAndLoad(csharp, "TypedCatchFixture");
        var typed     = assembly.GetType("Typed")!;
        var throwKind = assembly.GetType("ThrowKind")!;
        var caught    = assembly.GetType("Caught")!;

        using var system = new TestActorSystem("typed-catch");
        var probe = system.CreateProbe();
        var actor = system.Spawn(typed);

        probe.Send(actor, Activator.CreateInstance(throwKind, 1)!);
        var ack1 = probe.ExpectMsg(caught, TimeSpan.FromSeconds(2));
        Assert.Equal("invalid-op", caught.GetProperty("which")!.GetValue(ack1));

        probe.Send(actor, Activator.CreateInstance(throwKind, 2)!);
        var ack2 = probe.ExpectMsg(caught, TimeSpan.FromSeconds(2));
        Assert.Equal("argument", caught.GetProperty("which")!.GetValue(ack2));
    }
}
