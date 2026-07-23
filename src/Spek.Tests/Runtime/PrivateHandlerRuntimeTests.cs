using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Spek.Runtime;
using Spek.Testing;
using Spek.Tests.Emit;
using Xunit;

namespace Spek.Tests.Runtime;

/// <summary>
/// End-to-end check that `private on Foo => ...` handlers
/// dead-letter at runtime when targeted by anyone other than `self`.
/// Compiles real Spek source through the emitter + Roslyn, spawns the
/// actor in a `TestActorSystem`, and exercises both reachable and
/// unreachable code paths.
/// </summary>
public sealed class PrivateHandlerRuntimeTests
{
    private const string SpekSource = """
        namespace VisibilityFixture;

        message PublicTick();
        message PrivateTick();
        message Ping();
        message Acknowledged();
        message GetCount();
        message Count(int value);

        actor Visible
        {
            int privateCalls = 0;

            behavior Idle
            {
                public on PublicTick => sender.Tell(new Acknowledged());

                // Reaching the private handler bumps the counter so
                // tests can observe it via GetCount without losing
                // the original probe sender across the self-Tell.
                private on PrivateTick => privateCalls = privateCalls + 1;

                // Public handler that re-tells PrivateTick from inside
                // the actor — the only legal path to the private arm.
                public on Ping => self.Tell(new PrivateTick());

                public on GetCount => sender.Tell(new Count(privateCalls));
            }
        }
        """;

    private static (
        Type Visible,
        Type PublicTick,
        Type PrivateTick,
        Type Ping,
        Type Acknowledged,
        Type GetCount,
        Type Count)
        Compile()
    {
        var parse  = SpekCompiler.Parse(SpekSource);
        Assert.True(parse.Success,
            "Parse failed: " +
            string.Join("\n", parse.Diagnostics.Select(d => $"  {d.Code} {d.Line}:{d.Column} {d.Message}")));

        var csharp   = new FileEmitter().Emit(parse.Tree!);
        var assembly = RoslynCompileHelper.CompileAndLoad(csharp, "VisibilityFixture");

        return (
            assembly.GetType("VisibilityFixture.Visible")!,
            assembly.GetType("VisibilityFixture.PublicTick")!,
            assembly.GetType("VisibilityFixture.PrivateTick")!,
            assembly.GetType("VisibilityFixture.Ping")!,
            assembly.GetType("VisibilityFixture.Acknowledged")!,
            assembly.GetType("VisibilityFixture.GetCount")!,
            assembly.GetType("VisibilityFixture.Count")!);
    }

    [Fact]
    public void PublicHandler_IsReachableFromOutside()
    {
        var (visible, publicTick, _, _, ack, _, _) = Compile();
        var sink = new RecordingDeadLetterSink();
        using var system = new TestActorSystem("vis-pub", deadLetterSink: sink);
        var probe = system.CreateProbe();
        var actor = system.Spawn(visible);

        probe.Send(actor, Activator.CreateInstance(publicTick)!);
        var reply = probe.ExpectMsg(ack);

        Assert.IsType(ack, reply);
        Assert.Empty(sink.Records);
    }

    [Fact]
    public void PrivateHandler_DeadLettersWhenSenderIsNotSelf()
    {
        var (visible, _, privateTick, _, _, _, _) = Compile();
        var sink = new RecordingDeadLetterSink();
        using var system = new TestActorSystem("vis-priv-deny", deadLetterSink: sink);
        var probe = system.CreateProbe();
        var actor = system.Spawn(visible);

        probe.Send(actor, Activator.CreateInstance(privateTick)!);

        var deadline = DateTime.UtcNow + TimeSpan.FromSeconds(2);
        while (sink.Records.Count == 0 && DateTime.UtcNow < deadline)
            Thread.Sleep(20);

        Assert.Single(sink.Records);
        Assert.Contains("private handler 'PrivateTick'", sink.Records[0].Reason);
        Assert.Contains("self.Tell", sink.Records[0].Reason);
    }

    [Fact]
    public void PrivateHandler_IsReachableViaSelfTell()
    {
        // The Ping handler does `self.Tell(new PrivateTick())`. The
        // private handler increments a counter on the actor instead
        // of replying — this side-steps the fact that the private
        // arm's `sender` is `self` (the original probe ref is no
        // longer in scope). We then poll the count via a separate
        // public GetCount handler.
        var (visible, _, _, ping, _, getCount, count) = Compile();
        var sink = new RecordingDeadLetterSink();
        using var system = new TestActorSystem("vis-priv-allow", deadLetterSink: sink);
        var probe = system.CreateProbe();
        var actor = system.Spawn(visible);

        probe.Send(actor, Activator.CreateInstance(ping)!);

        // Poll via GetCount until the private handler increments the
        // counter (or the timeout expires). The mailbox-orderly
        // delivery means GetCount can't race ahead of PrivateTick:
        // both arrive after Ping's self-Tell, in that order.
        var deadline = DateTime.UtcNow + TimeSpan.FromSeconds(2);
        int value = 0;
        while (DateTime.UtcNow < deadline)
        {
            probe.Send(actor, Activator.CreateInstance(getCount)!);
            var reply = probe.ExpectMsg(count);
            value = (int)count.GetProperty("value")!.GetValue(reply)!;
            if (value == 1) break;
            Thread.Sleep(20);
        }

        Assert.Equal(1, value);
        Assert.Empty(sink.Records);
    }
}
