using Spek.Compiler.Parser;
using Xunit;

namespace Spek.Tests.Semantic;

/// <summary>
/// CE0126 — never-handled sends. When a <c>Tell</c>/<c>Ask</c> target's
/// concrete actor type is statically known (a local or field whose only origin
/// is <c>spawn&lt;T&gt;</c>/<c>Spawn&lt;T&gt;</c>, or <c>self</c>), a message
/// handled in NO behavior of that actor (nor its base-actor chain) is provably
/// dead mail and errors at compile time. Handled-in-another-behavior stays
/// legal (that's the <c>become</c> state machine); unknown-origin refs stay
/// silent (the cross-boundary story is typed <c>ActorRef&lt;Channel&gt;</c>,
/// reserved CE0030). <c>on any</c> counts as handling everything; private
/// handlers count only for <c>self.Tell</c>.
/// </summary>
public sealed class TypedSendTests
{
    private static void AssertClean(string src)
    {
        var parsed = SpekCompiler.Parse(src);
        Assert.True(parsed.Success,
            string.Join("\n", parsed.Diagnostics.Select(d => $"{d.Code} {d.Message}")));
        Assert.DoesNotContain(parsed.Diagnostics, d => d.Code == "CE0126");
    }

    private static void AssertCE0126(string src, string mustMention)
    {
        var parsed = SpekCompiler.Parse(src);
        Assert.False(parsed.Success);
        var diag = Assert.Single(parsed.Diagnostics, d => d.Code == "CE0126");
        Assert.Contains(mustMention, diag.Message);
    }

    // ─── flagged: provably dead mail ────────────────────────────────────

    [Fact]
    public void SpawnedLocal_NeverHandledMessage_ReportsCE0126()
    {
        AssertCE0126("""
            message Ping();
            message Nope();
            actor Child { init() { become I; } behavior I { on Ping => { } } }
            actor Parent
            {
                init() { become I; }
                behavior I
                {
                    on Ping => { var c = spawn<Child>(); c.Tell(new Nope()); }
                }
            }
            """, "Nope");
    }

    [Fact]
    public void ProgramBlockSpawn_NeverHandledMessage_ReportsCE0126()
    {
        AssertCE0126("""
            message Ping();
            message Wrong();
            actor Child { init() { become I; } behavior I { on Ping => { } } }
            program Main
            {
                var sys = new ActorSystem("demo");
                var c = sys.Spawn<Child>();
                c.Tell(new Wrong());
            }
            """, "Wrong");
    }

    [Fact]
    public void TrackedField_NeverHandledMessage_ReportsCE0126()
    {
        AssertCE0126("""
            message Ping();
            message Bad();
            actor Child { init() { become I; } behavior I { on Ping => { } } }
            actor Parent
            {
                ActorRef worker;
                init() { worker = spawn<Child>(); become I; }
                behavior I { on Ping => worker.Tell(new Bad()); }
            }
            """, "Bad");
    }

    [Fact]
    public void ExternalSend_ToPrivateOnlyHandler_ReportsCE0126()
    {
        // Private handlers are reachable only via self.Tell — an external send
        // of the same type is dead mail.
        AssertCE0126("""
            message Priv();
            message Go();
            actor A
            {
                init() { become I; }
                behavior I { private on Priv => { } on Go => { } }
            }
            actor P
            {
                init() { become I; }
                behavior I
                {
                    on Go => { var a = spawn<A>(); a.Tell(new Priv()); }
                }
            }
            """, "Priv");
    }

    [Fact]
    public void AskExpression_IsCheckedToo()
    {
        AssertCE0126("""
            message Q();
            message Never();
            actor Child { init() { become I; } behavior I { on Q => return new Q(); } }
            actor P
            {
                init() { become I; }
                behavior I
                {
                    on Q => { var c = spawn<Child>(); var r = c.Ask(new Never()); }
                }
            }
            """, "Never");
    }

    // ─── silent: legitimate patterns ────────────────────────────────────

    [Fact]
    public void HandledInAnotherBehavior_IsClean()
    {
        // The union across behaviors is the surface — wrong-STATE sends are a
        // runtime concern (become state machine), not dead mail.
        AssertClean("""
            message Ping();
            message Later();
            actor Child
            {
                init() { become A; }
                behavior A { on Ping => { become B; } }
                behavior B { on Later => { } }
            }
            actor Parent
            {
                init() { become I; }
                behavior I
                {
                    on Ping => { var c = spawn<Child>(); c.Tell(new Later()); }
                }
            }
            """);
    }

    [Fact]
    public void CatchAllTarget_IsClean()
    {
        // `on any` declares "I accept unchecked mail" — a proxy never flags.
        AssertClean("""
            message Nope();
            actor Sink { init() { become I; } behavior I { on any m => { } } }
            actor P
            {
                init() { become I; }
                behavior I
                {
                    on Nope => { var s = spawn<Sink>(); s.Tell(new Nope()); }
                }
            }
            """);
    }

    [Fact]
    public void AbstractBaseHandler_CoversVariant_IsClean()
    {
        // `on Ev` receives every variant, so sending a variant is handled.
        AssertClean("""
            abstract message Ev();
            message Up() : Ev;
            actor W { init() { become I; } behavior I { on Ev e => { } } }
            actor P
            {
                init() { become I; }
                behavior I
                {
                    on Up => { var w = spawn<W>(); w.Tell(new Up()); }
                }
            }
            """);
    }

    [Fact]
    public void SelfTell_ToPrivateHandler_IsClean()
    {
        // self.Tell is the one legal route to a private handler.
        AssertClean("""
            message Pub();
            message Priv();
            actor A
            {
                init() { become I; }
                behavior I
                {
                    on Pub => { self.Tell(new Priv()); }
                    private on Priv => { }
                }
            }
            """);
    }

    [Fact]
    public void UnknownOriginRef_IsSilent()
    {
        // A ref that arrived in a message field has no statically-known type —
        // conservative silence (typed ActorRef<Channel> is the eventual answer).
        AssertClean("""
            message Nope();
            message Carry(ActorRef Target);
            actor P
            {
                init() { become I; }
                behavior I { on Carry c => { c.Target.Tell(new Nope()); } }
            }
            """);
    }

    [Fact]
    public void FieldPoisonedByNonSpawnAssignment_IsSilent()
    {
        // A field ever assigned from a non-spawn source loses its tracked type.
        AssertClean("""
            message Ping();
            message Bad();
            message Carry(ActorRef R);
            actor Child { init() { become I; } behavior I { on Ping => { } } }
            actor P
            {
                ActorRef worker;
                init() { worker = spawn<Child>(); become I; }
                behavior I
                {
                    on Carry c => { worker = c.R; }
                    on Ping => { worker.Tell(new Bad()); }
                }
            }
            """);
    }

    [Fact]
    public void OverriddenAbstractBehavior_CountsTowardSurface()
    {
        // Actor inheritance shares fields/methods, not the dispatch table — a
        // behavior always lives on the actor that declares it. The inherited
        // shape is `abstract behavior` on the base + `override behavior` on
        // the derived; handlers inside the override are the derived actor's
        // surface.
        AssertClean("""
            message Ping();
            message Shared();
            abstract actor Base
            {
                abstract behavior Common { }
            }
            actor Impl : Base
            {
                init() { become Common; }
                override behavior Common { on Shared => { } }
            }
            actor P
            {
                init() { become I; }
                behavior I
                {
                    on Ping => { var i = spawn<Impl>(); i.Tell(new Shared()); }
                }
            }
            """);
    }

    [Fact]
    public void ReassignedLocal_BecomesUnknown_IsSilent()
    {
        // Reassignment from an unknown source drops the tracked type.
        AssertClean("""
            message Ping();
            message Bad();
            message Carry(ActorRef R);
            actor Child { init() { become I; } behavior I { on Ping => { } } }
            actor P
            {
                init() { become I; }
                behavior I
                {
                    on Carry c =>
                    {
                        var w = spawn<Child>();
                        w = c.R;
                        w.Tell(new Bad());
                    }
                }
            }
            """);
    }
}
