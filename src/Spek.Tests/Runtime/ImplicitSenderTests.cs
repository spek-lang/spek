using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Spek.Testing;
using Spek.Tests.Emit;
using Xunit;

namespace Spek.Tests.Runtime;

/// <summary>
/// Implicit sender on Tell (Akka convention). A fire-and-forget
/// <c>peer.Tell(msg)</c> from inside an actor carries that actor as the
/// sender, so the receiver's reply (<c>return reply;</c> or
/// <c>sender.Tell(reply)</c>) routes back to the telling actor — the
/// pattern every doc example (the getting-started bank) is written
/// against. Before this fix the reply addressed NoSender and vanished
/// silently; the HelloBank sample printed nothing.
/// </summary>
public class ImplicitSenderTests
{
    [Fact]
    public void TellFromHandler_ReplyRoutesBackToTellingActor()
    {
        const string src = """
            namespace ImplicitSenderDemo;

            message Query();
            message Answer();
            message Go();
            message Proof();

            actor Responder
            {
                behavior Idle
                {
                    on Query => return new Answer();
                }
            }

            public actor Requester
            {
                ActorRef peer;
                ActorRef probe;

                init(ActorRef p, ActorRef pr) { peer = p; probe = pr; }

                behavior Ready
                {
                    on Go     => { peer.Tell(new Query()); }
                    on Answer => { probe.Tell(new Proof()); }
                }
            }
            """;

        var parse = SpekCompiler.Parse(src);
        Assert.True(parse.Success,
            string.Join("\n", parse.Diagnostics.Select(d => $"{d.Code} {d.Message}")));

        var csharp = new FileEmitter().Emit(parse.Tree!);
        // The Tell inside the handler must carry the implicit sender.
        Assert.Contains("Tell(new Query(), _selfRef)", csharp);

        var assembly = RoslynCompileHelper.CompileAndLoad(csharp, "ImplicitSender");
        var requesterType = assembly.GetType("ImplicitSenderDemo.Requester")!;
        var responderType = assembly.GetType("ImplicitSenderDemo.Responder")!;
        var goType        = assembly.GetType("ImplicitSenderDemo.Go")!;
        var proofType     = assembly.GetType("ImplicitSenderDemo.Proof")!;

        using var system = new TestActorSystem("implicit-sender");
        var probe     = system.CreateProbe();
        var responder = system.Spawn(responderType);
        var requester = system.Spawn(requesterType, responder, probe.Ref);

        probe.Send(requester, Activator.CreateInstance(goType)!);

        // Reply round-trip: Go → Requester tells Query → Responder returns
        // Answer to the *implicit* sender (Requester) → Requester proves it.
        probe.ExpectMsg(proofType);
    }
}
