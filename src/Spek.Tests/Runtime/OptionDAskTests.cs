using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Spek.Runtime;
using Spek.Testing;
using Spek.Tests.Emit;
using Xunit;

namespace Spek.Tests.Runtime;

/// <summary>
/// Full-pipeline tests for Option D — handler-return-inferred reply types
/// on <c>ask</c> calls. Parses Spek source that uses
/// <c>on Ping =&gt; return new Pong();</c>, emits C#, compiles via Roslyn,
/// loads, and exercises the compiled actors to verify the reply is
/// routed correctly and the reply type is inferred for the ask caller.
/// </summary>
public class OptionDAskTests
{
    [Fact]
    public void InlineReturn_RoutesToSender_AskReceivesTypedReply()
    {
        const string src = """
            namespace OptionDDemo;

            message Ping();
            message Pong();
            message StartRound();
            message RoundDone(bool success);

            actor Ponger
            {
                behavior Listening
                {
                    on Ping => return new Pong();
                }
            }

            public actor Pinger
            {
                ActorRef peer;

                init(ActorRef p) { peer = p; }

                behavior Ready
                {
                    on StartRound =>
                    {
                        var pong = peer.Ask(new Ping());
                        sender.Tell(new RoundDone(true));
                    }
                }
            }
            """;

        var parse = SpekCompiler.Parse(src);
        Assert.True(parse.Success,
            string.Join("\n", parse.Diagnostics.Select(d => $"  {d.Code} {d.Line}:{d.Column} {d.Message}")));

        var csharp = new FileEmitter().Emit(parse.Tree!);

        // Proof-of-inference: the emitted C# should contain AskAsync<Pong>
        // (not AskAsync<Ping>). Pre-Option D would have emitted <Ping>.
        Assert.Contains("AskAsync<Pong>", csharp);

        var assembly = RoslynCompileHelper.CompileAndLoad(csharp, "OptionDInline");

        var pingerType     = assembly.GetType("OptionDDemo.Pinger")!;
        var pongerType     = assembly.GetType("OptionDDemo.Ponger")!;
        var startRoundType = assembly.GetType("OptionDDemo.StartRound")!;
        var roundDoneType  = assembly.GetType("OptionDDemo.RoundDone")!;

        using var system = new TestActorSystem("optd-inline");
        var probe  = system.CreateProbe();
        var ponger = system.Spawn(pongerType);
        var pinger = system.Spawn(pingerType, ponger);

        probe.Send(pinger, Activator.CreateInstance(startRoundType)!);

        var done = probe.ExpectMsg(roundDoneType);
        Assert.Equal(true, roundDoneType.GetProperty("success")!.GetValue(done));
    }

    [Fact]
    public void BlockReturn_WithIntermediateStatements_StillInferred()
    {
        const string src = """
            namespace OptionDDemo2;

            message GetBalance();
            message Balance(decimal amount);
            message Deposit(decimal amount);
            message Start();
            message Result(decimal finalBalance);

            actor Account
            {
                decimal balance = 0.00m;

                behavior Active
                {
                    on Deposit d => { balance = balance + d.amount; }

                    on GetBalance =>
                    {
                        return new Balance(balance);
                    }
                }
            }

            public actor Client
            {
                ActorRef account;

                init(ActorRef a) { account = a; }

                behavior Ready
                {
                    on Start =>
                    {
                        account.Tell(new Deposit(100.00m));
                        account.Tell(new Deposit(50.00m));
                        var b = account.Ask(new GetBalance());
                        sender.Tell(new Result(b.amount));
                    }
                }
            }
            """;

        var parse = SpekCompiler.Parse(src);
        Assert.True(parse.Success,
            string.Join("\n", parse.Diagnostics.Select(d => $"  {d.Code} {d.Line}:{d.Column} {d.Message}")));

        var csharp = new FileEmitter().Emit(parse.Tree!);
        Assert.Contains("AskAsync<Balance>", csharp);

        var assembly  = RoslynCompileHelper.CompileAndLoad(csharp, "OptionDBlock");
        var clientTy  = assembly.GetType("OptionDDemo2.Client")!;
        var accountTy = assembly.GetType("OptionDDemo2.Account")!;
        var startTy   = assembly.GetType("OptionDDemo2.Start")!;
        var resultTy  = assembly.GetType("OptionDDemo2.Result")!;

        using var system = new TestActorSystem("optd-block");
        var probe   = system.CreateProbe();
        var account = system.Spawn(accountTy);
        var client  = system.Spawn(clientTy, account);

        probe.Send(client, Activator.CreateInstance(startTy)!);
        var r = probe.ExpectMsg(resultTy);
        Assert.Equal(150.00m, resultTy.GetProperty("finalBalance")!.GetValue(r));
    }

    [Fact]
    public void ExplicitAskType_OverridesInference_AndUsedVerbatim()
    {
        // `ask<FastPong>` at the call site forces the reply type even when
        // other actors' handlers for the same message return different
        // types. This is the Option B escape hatch on top of Option D's
        // default inference.
        const string src = """
            namespace ExplicitAsk;

            message Ping();
            message FastPong();
            message SlowPong();

            actor Fast { behavior Idle { on Ping => return new FastPong(); } }
            actor Slow { behavior Idle { on Ping => return new SlowPong(); } }

            actor Caller
            {
                ActorRef peer;
                init(ActorRef p) { peer = p; }
                behavior Ready
                {
                    on FastPong => { }
                    on SlowPong => { }
                    on Ping =>
                    {
                        var fast = peer.Ask<FastPong>(new Ping());
                    }
                }
            }
            """;

        var parse = SpekCompiler.Parse(src);
        Assert.True(parse.Success,
            string.Join("\n", parse.Diagnostics.Select(d => $"  {d.Code} {d.Line}:{d.Column} {d.Message}")));

        var csharp = new FileEmitter().Emit(parse.Tree!);

        // Explicit form picked the caller's annotation even though inference
        // would have been ambiguous.
        Assert.Contains("AskAsync<FastPong>", csharp);
        Assert.DoesNotContain("AskAsync<object>", csharp);
    }

    [Fact]
    public void AmbiguousReturnTypes_AcrossActors_FallsBackToObject()
    {
        // Two actors both handle `Ping` but return different reply types.
        // Inference shouldn't silently pick one — it should fall back
        // to `object` and let the caller disambiguate.
        const string src = """
            namespace Ambig;

            message Ping();
            message FastPong();
            message SlowPong();

            actor Fast
            {
                behavior Idle { on Ping => return new FastPong(); }
            }

            actor Slow
            {
                behavior Idle { on Ping => return new SlowPong(); }
            }

            actor Caller
            {
                ActorRef peer;
                init(ActorRef p) { peer = p; }
                behavior Ready
                {
                    on FastPong => { }
                    on SlowPong => { }
                    on Ping =>
                    {
                        var reply = peer.Ask(new Ping());
                    }
                }
            }
            """;

        var parse = SpekCompiler.Parse(src);
        Assert.True(parse.Success,
            string.Join("\n", parse.Diagnostics.Select(d => $"  {d.Code} {d.Line}:{d.Column} {d.Message}")));

        var csharp = new FileEmitter().Emit(parse.Tree!);

        // Ambiguous → falls back to object.
        Assert.Contains("AskAsync<object>", csharp);
        Assert.DoesNotContain("AskAsync<FastPong>", csharp);
        Assert.DoesNotContain("AskAsync<SlowPong>", csharp);
    }

    [Fact]
    public void HandlerWithNoReturn_AskFallsBackToObject()
    {
        // If no handler for Ping has a `return` statement, the emitter
        // falls back to `AskAsync<object>`. Not ideal (caller has to
        // pattern-match), but better than guessing.
        const string src = """
            namespace OptionDDemo3;

            message Ping();
            message Start();

            actor Ponger
            {
                behavior Idle
                {
                    on Ping => { sender.Tell(new Pong()); }   // sender.Tell, no return
                }
            }

            message Pong();

            public actor Caller
            {
                ActorRef peer;

                init(ActorRef p) { peer = p; }

                behavior Ready
                {
                    on Start =>
                    {
                        var reply = peer.Ask(new Ping());
                    }
                }
            }
            """;

        var parse = SpekCompiler.Parse(src);
        Assert.True(parse.Success);

        var csharp = new FileEmitter().Emit(parse.Tree!);

        // No return → fall back to object. The caller's `var reply` binds
        // to an object; their choice whether to cast/pattern-match.
        Assert.Contains("AskAsync<object>", csharp);
    }
}
