using Spek.Compiler.Parser;
using Spek.Compiler.Semantic;
using Xunit;

namespace Spek.Tests.Semantic;

/// <summary>
/// Covers CE0011, CE0042, CE0043, CE0050, CE0051, CE0060, CE0061,
/// CE0081, CE0082 —
/// the location-based and intra-actor consistency rules in Spek.Compiler.Semantic.SemanticAnalyzer.
/// </summary>
public class SemanticAnalyzerTests
{
    private static IReadOnlyList<Diagnostic> Analyze(string source)
    {
        var result = SpekCompiler.Parse(source);
        Assert.True(result.Tree is not null,
            "Expected source to parse; diagnostics:\n" +
            string.Join("\n", result.Diagnostics.Select(d => $"  {d.Code} {d.Line}:{d.Column} {d.Message}")));
        // Parse errors would have CE0001 — filter to only semantic codes for assertions.
        return result.Diagnostics.Where(d => d.Code != "CE0001").ToList();
    }

    private static void AssertNoDiagnostics(string source)
    {
        var diagnostics = Analyze(source);
        Assert.True(diagnostics.Count == 0,
            "Expected no diagnostics but got:\n" +
            string.Join("\n", diagnostics.Select(d => $"  {d.Code} {d.Line}:{d.Column} {d.Message}")));
    }

    /// <summary>Returns ALL diagnostics including parse errors —
    /// for tests where the assertion needs to distinguish a parse
    /// failure from a missing semantic diagnostic.</summary>
    private static IReadOnlyList<Diagnostic> AnalyzeIncludingParse(string source)
        => SpekCompiler.Parse(source).Diagnostics;

    private static Diagnostic AssertOneDiagnostic(string source, string expectedCode)
    {
        var diagnostics = Analyze(source);
        Assert.Single(diagnostics);
        Assert.Equal(expectedCode, diagnostics[0].Code);
        return diagnostics[0];
    }

    // ─── CE0011 — become target must be a declared behavior ─────────────────

    [Fact]
    public void CE0011_BecomeUnknownBehavior_Reported()
    {
        const string src = """
            actor A
            {
                init() { become Missing; }
                behavior Known { on Ping => { } }
            }
            message Ping();
            """;
        var d = AssertOneDiagnostic(src, "CE0011");
        Assert.Contains("Missing", d.Message);
    }

    [Fact]
    public void CE0011_BecomeKnownBehavior_NoDiagnostic()
    {
        const string src = """
            actor A
            {
                init() { become Known; }
                behavior Known { on Ping => { } }
            }
            message Ping();
            """;
        AssertNoDiagnostics(src);
    }

    // ─── CE0042 — ask only inside an on-handler ─────────────────────────────

    [Fact]
    public void CE0042_AskInsideOnHandler_NoDiagnostic()
    {
        // The actor handles R too — otherwise the self-ask is provably dead
        // mail and (correctly) trips CE0126.
        const string src = """
            message Q();
            message R();
            actor A
            {
                behavior Idle
                {
                    on Q => { var r = self.Ask(new R()); }
                    on R => { }
                }
            }
            """;
        AssertNoDiagnostics(src);
    }

    [Fact]
    public void CE0042_AskInInit_Reported()
    {
        // Target is a plain field reference so we don't also trip CE0043 via self.
        const string src = """
            message Q();
            actor A
            {
                int peer = 0;
                init() { var r = peer.Ask(new Q()); }
                behavior Idle { on Q => { } }
            }
            """;
        AssertOneDiagnostic(src, "CE0042");
    }

    [Fact]
    public void CE0042_AskInMethod_Reported()
    {
        const string src = """
            message Q();
            actor A
            {
                int peer = 0;
                void Helper() { var r = peer.Ask(new Q()); }
                behavior Idle { on Q => { } }
            }
            """;
        AssertOneDiagnostic(src, "CE0042");
    }

    // ─── CE0043 — self/sender only inside an on-handler ─────────────────────

    [Fact]
    public void CE0043_SelfInOnHandler_NoDiagnostic()
    {
        // Read self — doesn't need to go anywhere, just prove it's allowed in scope.
        const string src = """
            message Ping();
            actor A
            {
                behavior Idle { on Ping => { var me = self; var s = sender; } }
            }
            """;
        AssertNoDiagnostics(src);
    }

    [Fact]
    public void CE0043_SelfInMethod_Reported()
    {
        const string src = """
            message Ping();
            actor A
            {
                void Helper() { var x = self; }
                behavior Idle { on Ping => { } }
            }
            """;
        AssertOneDiagnostic(src, "CE0043");
    }

    [Fact]
    public void CE0043_SenderInInit_Reported()
    {
        const string src = """
            message Ping();
            actor A
            {
                init() { var x = sender; }
                behavior Idle { on Ping => { } }
            }
            """;
        AssertOneDiagnostic(src, "CE0043");
    }

    // ─── CE0050 — persist only inside an on-handler ─────────────────────────

    [Fact]
    public void CE0050_PersistInOnHandler_NoDiagnostic()
    {
        const string src = """
            message Ping();
            actor A
            {
                int counter = 0;
                behavior Idle { on Ping => { persist; } }
                on Restore(Snapshot s) => { counter = s.Get<int>("counter"); }
            }
            """;
        AssertNoDiagnostics(src);
    }

    [Fact]
    public void CE0050_PersistInInit_Reported()
    {
        const string src = """
            message Ping();
            actor A
            {
                init() { persist; }
                behavior Idle { on Ping => { } }
                on Restore(Snapshot s) => { }
            }
            """;
        // Init-block persist triggers CE0050 (not an on-handler). Nothing else.
        AssertOneDiagnostic(src, "CE0050");
    }

    [Fact]
    public void CE0050_PersistInMethod_Reported()
    {
        const string src = """
            message Ping();
            actor A
            {
                void Snapshot() { persist; }
                behavior Idle { on Ping => { } }
                on Restore(Snapshot s) => { }
            }
            """;
        AssertOneDiagnostic(src, "CE0050");
    }

    // ─── CE0051 — become allowed in on-handler, init, and on Restore ────────

    [Fact]
    public void CE0051_BecomeInInit_NoDiagnostic()
    {
        const string src = """
            actor A
            {
                init() { become Idle; }
                behavior Idle { on Ping => { } }
            }
            message Ping();
            """;
        AssertNoDiagnostics(src);
    }

    [Fact]
    public void CE0051_BecomeInRestore_NoDiagnostic()
    {
        const string src = """
            message Ping();
            actor A
            {
                int x = 0;
                behavior Idle { on Ping => { persist; } }
                on Restore(Snapshot s) => { x = s.Get<int>("x"); become Idle; }
            }
            """;
        AssertNoDiagnostics(src);
    }

    [Fact]
    public void CE0051_BecomeInMethod_Reported()
    {
        const string src = """
            message Ping();
            actor A
            {
                void SwitchIt() { become Idle; }
                behavior Idle { on Ping => { } }
            }
            """;
        var diagnostics = Analyze(src);
        Assert.Single(diagnostics);
        Assert.Equal("CE0051", diagnostics[0].Code);
    }

    [Fact]
    public void CE0051_BecomeInPreStart_NoDiagnostic()
    {
        // Per Akka convention, `become` in PreStart is how you set initial behavior
        // before the first message — accepted.
        const string src = """
            message Ping();
            actor A
            {
                behavior Idle { on Ping => { } }
                on PreStart => { become Idle; }
            }
            """;
        AssertNoDiagnostics(src);
    }

    [Fact]
    public void CE0051_BecomeInPostStop_NoDiagnostic()
    {
        // Pointless but harmless — matches lenient lifecycle convention.
        const string src = """
            message Ping();
            actor A
            {
                behavior Idle { on Ping => { } }
                on PostStop => { become Idle; }
            }
            """;
        AssertNoDiagnostics(src);
    }

    // ─── CE0060 — Restore declared without persist/passivate ────────────────

    [Fact]
    public void CE0060_RestoreWithoutPersistOrPassivate_Reported()
    {
        const string src = """
            message Ping();
            actor A
            {
                int x = 0;
                behavior Idle { on Ping => { } }
                on Restore(Snapshot s) => { x = s.Get<int>("x"); }
            }
            """;
        AssertOneDiagnostic(src, "CE0060");
    }

    [Fact]
    public void CE0060_RestoreWithPassivate_NoDiagnostic()
    {
        const string src = """
            message Ping();
            actor A
            {
                int x = 0;
                passivate after System.TimeSpan.FromMinutes(5);
                behavior Idle { on Ping => { } }
                on Restore(Snapshot s) => { x = s.Get<int>("x"); }
            }
            """;
        AssertNoDiagnostics(src);
    }

    [Fact]
    public void CE0060_RestoreWithPersist_NoDiagnostic()
    {
        const string src = """
            message Ping();
            actor A
            {
                int x = 0;
                behavior Idle { on Ping => { x = x + 1; persist; } }
                on Restore(Snapshot s) => { x = s.Get<int>("x"); }
            }
            """;
        AssertNoDiagnostics(src);
    }

    // ─── CE0061 retired — passivate without an explicit Restore ───
    // is now valid; the emitter auto-generates a symmetric OnRestore.

    [Fact]
    public void Passivate_WithoutExplicitRestore_NoLongerReported()
    {
        // Was CE0061. Auto-restore now rehydrates the captured fields, so an explicit
        // `on Restore` is optional — this must be clean.
        const string src = """
            message Ping();
            actor A
            {
                int x = 0;
                passivate after System.TimeSpan.FromMinutes(5);
                behavior Idle { on Ping => { } }
            }
            """;
        AssertNoDiagnostics(src);
    }

    [Fact]
    public void Passivate_WithExplicitRestore_NoDiagnostic()
    {
        const string src = """
            message Ping();
            actor A
            {
                int x = 0;
                passivate after System.TimeSpan.FromMinutes(5);
                behavior Idle { on Ping => { } }
                on Restore(Snapshot s) => { x = s.Get<int>("x"); }
            }
            """;
        AssertNoDiagnostics(src);
    }

    // ─── CE0010 — message field types must be immutable ─────────────────────

    [Fact]
    public void CE0010_PrimitiveFields_NoDiagnostic()
    {
        AssertNoDiagnostics("message M(int x, string y, decimal z, bool b);");
    }

    [Fact]
    public void CE0010_MessageFieldReferencingAnotherMessage_NoDiagnostic()
    {
        const string src = """
            message Inner(int x);
            message Outer(Inner child);
            """;
        AssertNoDiagnostics(src);
    }

    [Fact]
    public void CE0010_TypeParameter_NoDiagnostic()
    {
        // T is a type parameter of the message — allowed (constraint at instantiation).
        AssertNoDiagnostics("message Wrap<T>(T value);");
    }

    [Fact]
    public void CE0010_ActorRefField_NoDiagnostic()
    {
        // ActorRef is an opaque capability handle, immutable by design.
        AssertNoDiagnostics("message Wire(ActorRef target);");
    }

    [Fact]
    public void CE0010_ImmutableList_NoDiagnostic()
    {
        AssertNoDiagnostics("message Batch(ImmutableList<int> ids);");
    }

    [Fact]
    public void CE0010_MutableList_Reported()
    {
        AssertOneDiagnostic("message Bad(List<int> ids);", "CE0010");
    }

    [Fact]
    public void CE0010_IReadOnlyList_Reported()
    {
        // Readonly interfaces lie — underlying implementation can still be mutable.
        AssertOneDiagnostic("message Bad(IReadOnlyList<int> ids);", "CE0010");
    }

    [Fact]
    public void CE0010_UnknownClass_Reported()
    {
        AssertOneDiagnostic("message Bad(Customer c);", "CE0010");
    }

    [Fact]
    public void CE0010_ImmutableListOfMutableType_Reported()
    {
        // ImmutableList wrapper doesn't save a mutable element type.
        AssertOneDiagnostic("message Bad(ImmutableList<Customer> xs);", "CE0010");
    }

    // ─── CE0012 — no member access on actor refs except Tell ─────────────────

    [Fact]
    public void CE0012_TellOnSender_NoDiagnostic()
    {
        const string src = """
            message Ping();
            message Pong();
            actor A
            {
                behavior Idle { on Ping => { sender.Tell(new Pong()); } }
            }
            """;
        AssertNoDiagnostics(src);
    }

    [Fact]
    public void CE0012_NonTellMethodOnSender_Reported()
    {
        const string src = """
            message Ping();
            actor A
            {
                behavior Idle { on Ping => { sender.Shutdown(); } }
            }
            """;
        AssertOneDiagnostic(src, "CE0012");
    }

    [Fact]
    public void CE0012_MemberAccessOnSelf_Reported()
    {
        const string src = """
            message Ping();
            actor A
            {
                int balance = 0;
                behavior Idle { on Ping => { var x = self.balance; } }
            }
            """;
        // Reading `self.balance` goes through CE0012 — force internal field access
        // via plain name instead. Diagnostic may report multiple times if additional
        // rules flag the same target; here we just assert CE0012 is present.
        var diagnostics = Analyze(src);
        Assert.Contains(diagnostics, d => d.Code == "CE0012");
    }

    [Fact]
    public void CE0012_MemberAccessOnActorRefField_Reported()
    {
        const string src = """
            message Ping();
            actor A
            {
                ActorRef peer;
                behavior Idle { on Ping => { var x = peer.name; } }
            }
            """;
        var diagnostics = Analyze(src);
        Assert.Contains(diagnostics, d => d.Code == "CE0012");
    }

    [Fact]
    public void SelfSystemShutdown_NotCe0012()
    {
        // `self.System` is an ambient ActorBase accessor, not an actor-ref
        // member, so `self.System.Shutdown()` must NOT trip CE0012.
        const string src = """
            message Boom();
            actor Watchdog
            {
                behavior Idle { on Boom => { self.System.Shutdown(); } }
            }
            """;
        AssertNoDiagnostics(src);
    }

    [Theory]
    [InlineData("self.Metrics.Counter(\"x\");")]
    [InlineData("self.Log.Log(StructuredLogLevel.Information, \"e\");")]
    [InlineData("var t = self.TraceContext;")]
    public void SelfAccessors_NotCe0012(string body)
    {
        // Regression: the shipped self.X observability accessors resolve to
        // ActorBase properties and must be exempt from CE0012 (they were
        // erroneously rejected before the carve-out).
        var src = $$"""
            message Ping();
            actor A
            {
                behavior Idle { on Ping => { {{body}} } }
            }
            """;
        Assert.DoesNotContain(Analyze(src), d => d.Code == "CE0012");
    }

    [Fact]
    public void CE0012_MethodCallOnSpawnResult_Reported()
    {
        const string src = """
            message Ping();
            actor Child { behavior Idle { on Ping => { } } }
            actor A
            {
                behavior Idle { on Ping => { spawn<Child>().DoSomething(); } }
            }
            """;
        var diagnostics = Analyze(src);
        Assert.Contains(diagnostics, d => d.Code == "CE0012");
    }

    // ─── CE0020 — ask/Tell payloads must be declared messages ────────────────

    [Fact]
    public void CE0020_AskWithKnownMessage_NoDiagnostic()
    {
        // The actor handles R too — otherwise the self-ask is provably dead
        // mail and (correctly) trips CE0126.
        const string src = """
            message Q();
            message R();
            actor A
            {
                behavior Idle
                {
                    on Q => { var r = self.Ask(new R()); }
                    on R => { }
                }
            }
            """;
        AssertNoDiagnostics(src);
    }

    [Fact]
    public void CE0020_AskWithUnknownType_Reported()
    {
        const string src = """
            message Q();
            actor A
            {
                behavior Idle { on Q => { var r = self.Ask(new Undeclared()); } }
            }
            """;
        AssertOneDiagnostic(src, "CE0020");
    }

    [Fact]
    public void CE0020_TellNewMessage_NoDiagnostic()
    {
        const string src = """
            message Ping();
            message Pong();
            actor A
            {
                behavior Idle { on Ping => { sender.Tell(new Pong()); } }
            }
            """;
        AssertNoDiagnostics(src);
    }

    [Fact]
    public void CE0020_TellPrimitive_Reported()
    {
        const string src = """
            message Ping();
            actor A
            {
                behavior Idle { on Ping => { sender.Tell(42); } }
            }
            """;
        AssertOneDiagnostic(src, "CE0020");
    }

    [Fact]
    public void CE0020_TellActorRef_Reported()
    {
        const string src = """
            message Ping();
            actor A
            {
                behavior Idle { on Ping => { sender.Tell(self); } }
            }
            """;
        AssertOneDiagnostic(src, "CE0020");
    }

    [Fact]
    public void CE0020_TellLocalMessageVariable_NoDiagnostic()
    {
        const string src = """
            message Ping();
            message Pong();
            actor A
            {
                behavior Idle
                {
                    on Ping =>
                    {
                        var reply = new Pong();
                        sender.Tell(reply);
                    }
                }
            }
            """;
        AssertNoDiagnostics(src);
    }

    [Fact]
    public void CE0020_TellFieldPrimitive_Reported()
    {
        const string src = """
            message Ping();
            actor A
            {
                int counter = 0;
                behavior Idle { on Ping => { sender.Tell(counter); } }
            }
            """;
        AssertOneDiagnostic(src, "CE0020");
    }

    [Fact]
    public void CE0020_TellUnknownField_NoDiagnosticByDesign()
    {
        // Fail-open: the typer can't classify a field of an unknown class type,
        // so CE0020 doesn't fire. The deliberate "don't false-positive" escape
        // valve — note CE0010 still fires on the field *declaration* separately.
        const string src = """
            message Ping();
            actor A
            {
                Customer peer;
                behavior Idle { on Ping => { sender.Tell(peer); } }
            }
            """;
        var diagnostics = Analyze(src);
        Assert.DoesNotContain(diagnostics, d => d.Code == "CE0020");
    }

    // ─── CE0014 — unused behavior detection ─────────────────────────────────

    [Fact]
    public void CE0014_UnusedBehavior_Reported()
    {
        // 'Orphan' is never reached via become — should be flagged.
        const string src = """
            message Ping();
            actor A
            {
                init() { become Idle; }
                behavior Idle { on Ping => { } }
                behavior Orphan { on Ping => { } }
            }
            """;
        AssertOneDiagnostic(src, "CE0014");
    }

    [Fact]
    public void CE0014_BehaviorReachedFromOtherBehavior_NoDiagnostic()
    {
        // Every behavior is reachable: init → Idle → Busy.
        const string src = """
            message Start();
            message Done();
            actor A
            {
                init() { become Idle; }
                behavior Idle { on Start => { become Busy; } }
                behavior Busy { on Done  => { become Idle; } }
            }
            """;
        AssertNoDiagnostics(src);
    }

    [Fact]
    public void CE0014_BehaviorReachedFromRestore_NoDiagnostic()
    {
        // Behavior reached only from on Restore still counts as used.
        const string src = """
            message Ping();
            actor A
            {
                int x = 0;
                passivate after System.TimeSpan.FromMinutes(1);
                init() { become Idle; }
                behavior Idle  { on Ping => { persist; } }
                behavior Frozen { on Ping => { } }
                on Restore(Snapshot s) => { x = s.Get<int>("x"); become Frozen; }
            }
            """;
        AssertNoDiagnostics(src);
    }

    [Fact]
    public void CE0014_AbstractActor_NoDiagnostic()
    {
        // Abstract actors skip the check — derived actors may reference
        // inherited behaviors that look unused in the parent.
        const string src = """
            message Ping();
            abstract actor Base
            {
                behavior Idle { on Ping => { } }
                behavior UnusedHere { on Ping => { } }
            }
            """;
        AssertNoDiagnostics(src);
    }

    // ─── Param typing — CE0020 sees through method/init params ──────────────

    [Fact]
    public void CE0020_PrimitiveMethodParam_PassedToTell_Reported()
    {
        // Method param 'x' is int — Tell expects a message. Requires the
        // ExpressionTyper to know the param's declared type.
        // (An `init(ActorRef p)` block sets `peer`, suppressing
        // CE0109's "non-nullable field without initializer" warning so
        // we can assert solely on the CE0020 case.)
        const string src = """
            message Ping();
            actor A
            {
                ActorRef peer;
                init(ActorRef p) { peer = p; }
                void Send(int x) { peer.Tell(x); }
                behavior Idle { on Ping => { } }
            }
            """;
        AssertOneDiagnostic(src, "CE0020");
    }

    [Fact]
    public void CE0020_ActorRefInitParam_StoredAsField_NoFalsePositive()
    {
        // Init param is an ActorRef — using it later should not trip any
        // rule, and assigning to a field should classify correctly.
        const string src = """
            message Ping();
            actor A
            {
                ActorRef peer;
                init(ActorRef p) { peer = p; }
                behavior Idle { on Ping => { peer.Tell(new Ping()); } }
            }
            """;
        AssertNoDiagnostics(src);
    }

    // ─── CE0080 — hostile namespace imports ─────────────────────────────────

    [Fact]
    public void CE0080_SystemReflection_Reported()
    {
        const string src = """
            using System.Reflection;
            message Ping();
            actor A { behavior Idle { on Ping => { } } }
            """;
        AssertOneDiagnostic(src, "CE0080");
    }

    [Fact]
    public void CE0080_SystemRuntimeCompilerServices_Reported()
    {
        const string src = """
            using System.Runtime.CompilerServices;
            message Ping();
            actor A { behavior Idle { on Ping => { } } }
            """;
        AssertOneDiagnostic(src, "CE0080");
    }

    [Fact]
    public void CE0080_SystemReflectionEmit_Reported()
    {
        const string src = """
            using System.Reflection.Emit;
            message Ping();
            actor A { behavior Idle { on Ping => { } } }
            """;
        AssertOneDiagnostic(src, "CE0080");
    }

    [Fact]
    public void CE0080_BenignImport_NoDiagnostic()
    {
        const string src = """
            using System.Collections.Immutable;
            message Ping();
            actor A { behavior Idle { on Ping => { } } }
            """;
        AssertNoDiagnostics(src);
    }

    // ─── CE0086 — interop using bypasses safety ─────────────────────────────

    [Fact]
    public void InteropUsing_BypassesCE0080_EmitsCE0086()
    {
        const string src = """
            interop using System.Reflection;
            message Ping();
            actor A { behavior Idle { on Ping => { } } }
            """;
        var d = AssertOneDiagnostic(src, "CE0086");
        Assert.Contains("interop using", d.Message);
        Assert.Contains("System.Reflection", d.Message);
    }

    [Fact]
    public void InteropUsing_BenignNamespace_NoDiagnostic()
    {
        // `interop using` on a non-hostile namespace is just a `using`
        // — no advisory needed.
        const string src = """
            interop using System.Collections.Immutable;
            message Ping();
            actor A { behavior Idle { on Ping => { } } }
            """;
        AssertNoDiagnostics(src);
    }

    [Fact]
    public void PlainUsing_HostileNamespace_HintsAtInteropOption()
    {
        const string src = """
            using System.Reflection;
            message Ping();
            actor A { behavior Idle { on Ping => { } } }
            """;
        var d = AssertOneDiagnostic(src, "CE0080");
        Assert.Contains("interop using", d.Message);
    }

    // ─── CE0096 — handler pattern type resolution ───────────────────────────

    [Fact]
    public void CE0096_PublicHandler_OnUndeclaredType_Reported()
    {
        const string src = """
            actor A { behavior Idle { public on FileSystemEventArgs ev => { } } }
            """;
        var d = AssertOneDiagnostic(src, "CE0096");
        Assert.Contains("FileSystemEventArgs", d.Message);
        Assert.Contains("private on", d.Message);
    }

    [Fact]
    public void CE0096_DefaultVisibilityHandler_OnUndeclaredType_Reported()
    {
        // Default visibility is public; same rule applies.
        const string src = """
            actor A { behavior Idle { on FileSystemEventArgs ev => { } } }
            """;
        var d = AssertOneDiagnostic(src, "CE0096");
        Assert.Contains("public on FileSystemEventArgs", d.Message);
    }

    [Fact]
    public void CE0096_InternalHandler_OnUndeclaredType_Reported()
    {
        const string src = """
            actor A { behavior Idle { internal on FileSystemEventArgs ev => { } } }
            """;
        var d = AssertOneDiagnostic(src, "CE0096");
        Assert.Contains("internal on FileSystemEventArgs", d.Message);
    }

    [Fact]
    public void CE0096_PrivateHandler_OnUndeclaredType_NoDiagnostic()
    {
        // Private handlers escape the rule — they're not part of the
        // public API surface so binding to a BCL type is fine.
        const string src = """
            actor A { behavior Idle { private on FileSystemEventArgs ev => { } } }
            """;
        AssertNoDiagnostics(src);
    }

    // ─── CE0139 — a generic message can't be handled yet (red-team emit-D) ────

    [Fact]
    public void CE0139_GenericMessageHandler_Reported()
    {
        const string src = """
            message Envelope<T>(T payload);
            actor A { init() { become Idle; } behavior Idle { on Envelope e => { } } }
            """;
        var d = Analyze(src);
        Assert.Contains(d, x => x.Code == "CE0139");
    }

    [Fact]
    public void CE0139_PrivateGenericMessageHandler_AlsoReported()
    {
        // Private handlers escape CE0096 but NOT CE0139 — a private handler on
        // a generic message emits the same open-generic `case` (CS0305).
        const string src = """
            message Envelope<T>(T payload);
            actor A { init() { become Idle; } behavior Idle { private on Envelope e => { } } }
            """;
        Assert.Contains(Analyze(src), x => x.Code == "CE0139");
    }

    [Fact]
    public void CE0139_NonGenericMessageHandler_NoDiagnostic()
    {
        const string src = """
            message Plain(int n);
            actor A { init() { become Idle; } behavior Idle { on Plain p => { } } }
            """;
        Assert.DoesNotContain(Analyze(src), x => x.Code == "CE0139");
    }

    [Fact]
    public void CE0096_PublicHandler_OnDeclaredMessage_NoDiagnostic()
    {
        const string src = """
            message Ping();
            actor A { behavior Idle { public on Ping => { } } }
            """;
        AssertNoDiagnostics(src);
    }

    // ─── CE0087 — reader-handler mutations ──────────────────────────────────

    [Fact]
    public void CE0087_ReaderHandler_AssigningField_Reported()
    {
        const string src = """
            message Tick();
            actor A
            {
                int count = 0;
                behavior Idle
                {
                    reader on Tick => count = count + 1;
                }
            }
            """;
        var all = AnalyzeIncludingParse(src);
        Assert.True(all.All(d => d.Code != "CE0001"),
            "Parse failed: " + string.Join("\n", all.Select(d => $"  {d.Code} {d.Line}:{d.Column} {d.Message}")));
        var d = AssertOneDiagnostic(src, "CE0087");
        Assert.Contains("count", d.Message);
    }

    [Fact]
    public void CE0087_ReaderHandler_FieldMemberAssign_Reported()
    {
        // foo.x = Y where foo is a field — mutation through the field.
        const string src = """
            message Tick();
            message Holder(int x);
            actor A
            {
                Holder h = new Holder(0);
                behavior Idle
                {
                    reader on Tick => h.x = 5;
                }
            }
            """;
        var diagnostics = Analyze(src);
        Assert.Contains(diagnostics, d => d.Code == "CE0087");
    }

    [Fact]
    public void CE0087_ReaderHandler_LocalReassignment_NoDiagnostic()
    {
        // Reassigning a local var (declared inside the handler) is
        // fine — it doesn't touch actor state.
        const string src = """
            message Tick();
            actor A
            {
                behavior Idle
                {
                    reader on Tick => {
                        var x = 1;
                        x = 2;
                    }
                }
            }
            """;
        AssertNoDiagnostics(src);
    }

    [Fact]
    public void CE0087_ReaderHandler_Persist_Reported()
    {
        // persist is a statement, so it requires a block body.
        const string src = """
            message Tick();
            actor A
            {
                int count = 0;
                behavior Idle
                {
                    reader on Tick => { persist; }
                }
            }
            """;
        var d = AssertOneDiagnostic(src, "CE0087");
        Assert.Contains("persist", d.Message);
    }

    [Fact]
    public void CE0087_ReaderHandler_Become_Reported()
    {
        const string src = """
            message Tick();
            actor A
            {
                behavior Idle
                {
                    reader on Tick => { become Busy; }
                }
                behavior Busy
                {
                    on Tick => { }
                }
            }
            """;
        var d = AssertOneDiagnostic(src, "CE0087");
        Assert.Contains("become", d.Message);
    }

    [Fact]
    public void CE0087_WriterHandler_MutatingField_NoDiagnostic()
    {
        // Writer handlers (default + explicit) can mutate freely.
        const string src = """
            message Tick();
            actor A
            {
                int count = 0;
                behavior Idle
                {
                    writer on Tick => count = count + 1;
                }
            }
            """;
        AssertNoDiagnostics(src);
    }

    [Fact]
    public void CE0087_DefaultHandler_MutatingField_NoDiagnostic()
    {
        // Default mode is writer; actors without an explicit reader/writer mode continue to work.
        const string src = """
            message Tick();
            actor A
            {
                int count = 0;
                behavior Idle
                {
                    on Tick => count = count + 1;
                }
            }
            """;
        AssertNoDiagnostics(src);
    }

    [Fact]
    public void CE0087_ReaderHandler_ReadingFieldOnly_NoDiagnostic()
    {
        // A reader that only reads (no assignment) is exactly what
        // we want to allow.
        const string src = """
            message Read();
            message Reply(int v);
            actor A
            {
                int count = 0;
                behavior Idle
                {
                    reader on Read => sender.Tell(new Reply(count));
                }
            }
            """;
        AssertNoDiagnostics(src);
    }

    // ─── CE0083 — dispatcher-blocking calls ─────────────────────────────────

    [Fact]
    public void CE0083_ThreadSleep_Reported()
    {
        const string src = """
            message Ping();
            actor A { behavior Idle { on Ping => Thread.Sleep(1000); } }
            """;
        AssertOneDiagnostic(src, "CE0083");
    }

    [Fact]
    public void CE0083_TaskWaitAll_Reported()
    {
        const string src = """
            message Ping();
            actor A { behavior Idle { on Ping => Task.WaitAll(); } }
            """;
        AssertOneDiagnostic(src, "CE0083");
    }

    [Fact]
    public void CE0083_ConsoleReadLine_Reported()
    {
        const string src = """
            message Ping();
            actor A { behavior Idle { on Ping => Console.ReadLine(); } }
            """;
        AssertOneDiagnostic(src, "CE0083");
    }

    [Fact]
    public void DotWait_NotDiagnosed_RewrittenInstead()
    {
        // `task.Wait()` is no longer a CE0083 error — invisible async rewrites
        // it to `await task` (see TaskResultRewriteTests). `.WaitAll()` /
        // `.WaitAny()` (no value-preserving rewrite) stay CE0083 errors.
        const string src = """
            message Ping();
            actor A { behavior Idle { on Ping => System.Threading.Tasks.Task.CompletedTask.Wait(); } }
            """;
        var diagnostics = Analyze(src);
        Assert.DoesNotContain(diagnostics, d => d.Code == "CE0083");
    }

    [Fact]
    public void CE0083_BenignCall_NoDiagnostic()
    {
        const string src = """
            message Ping();
            actor A { behavior Idle { on Ping => self.Tell(new Ping()); } }
            """;
        AssertNoDiagnostics(src);
    }

    [Fact]
    public void DotResult_NotDiagnosed_RewrittenInstead()
    {
        // `.Result` on a Task is rewritten to `await` by the invisible-async
        // pass (see TaskResultRewriteTests), so it is NOT a diagnostic — no
        // CE0114, and certainly not the CE0083 reserved for .Wait()/.GetResult().
        const string src = """
            message Ping();
            actor A { behavior Idle { on Ping => System.Console.WriteLine(System.Threading.Tasks.Task.FromResult(5).Result); } }
            """;
        var diagnostics = Analyze(src);
        Assert.DoesNotContain(diagnostics, d => d.Code is "CE0114" or "CE0083");
    }

    // ─── CE0115 — synchronous BCL I/O (warning, has *Async sibling) ──────────

    [Fact]
    public void CE0115_SyncFileRead_WarnsWithAsyncSibling()
    {
        const string src = """
            message Load(string path);
            actor A { behavior Idle { on Load => System.Console.WriteLine(File.ReadAllText(path)); } }
            """;
        var diagnostics = Analyze(src);
        var ce0115 = Assert.Single(diagnostics, d => d.Code == "CE0115");
        // It's a *warning*, not a hard error — sync I/O is sometimes legitimate.
        Assert.Equal(DiagnosticSeverity.Warning, ce0115.Severity);
        Assert.Contains("File.ReadAllTextAsync", ce0115.Message);
    }

    [Fact]
    public void CE0115_SyncFileWrite_Warns()
    {
        const string src = """
            message Save(string path, string body);
            actor A { behavior Idle { on Save => File.WriteAllText(path, body); } }
            """;
        var diagnostics = Analyze(src);
        Assert.Contains(diagnostics, d => d.Code == "CE0115");
    }

    [Fact]
    public void CE0115_AlreadyAsync_NoDiagnostic()
    {
        // The *Async sibling is exactly what we steer toward — it must not warn.
        const string src = """
            message Load(string path);
            actor A { behavior Idle { on Load => System.Console.WriteLine(File.ReadAllTextAsync(path)); } }
            """;
        var diagnostics = Analyze(src);
        Assert.DoesNotContain(diagnostics, d => d.Code == "CE0115");
    }

    // ─── CE0116 — sequential await in a foreach (hint) ──────────────────────

    [Fact]
    public void CE0116_AsyncCallInForeach_Hints()
    {
        const string src = """
            message Run(System.Collections.Generic.List<string> ids);
            actor A
            {
                behavior Idle { on Run r => { foreach (var id in r.ids) { directory.FetchAsync(id); } } }
            }
            """;
        var hint = Assert.Single(Analyze(src), d => d.Code == "CE0116");
        Assert.Equal(DiagnosticSeverity.Hint, hint.Severity);   // editor-only, never fails the build
    }

    [Fact]
    public void CE0116_SyncForeach_NoHint()
    {
        const string src = """
            message Run(System.Collections.Generic.List<string> ids);
            actor A
            {
                behavior Idle { on Run r => { foreach (var id in r.ids) { Process(id); } } }
            }
            """;
        Assert.DoesNotContain(Analyze(src), d => d.Code == "CE0116");
    }

    [Fact]
    public void CE0116_AsyncCallOutsideLoop_NoHint()
    {
        const string src = """
            message Run(string id);
            actor A
            {
                behavior Idle { on Run r => { directory.FetchAsync(r.id); } }
            }
            """;
        Assert.DoesNotContain(Analyze(src), d => d.Code == "CE0116");
    }

    // ─── CE0084 — process-escape calls ──────────────────────────────────────

    [Fact]
    public void CE0084_EnvironmentExit_Reported()
    {
        const string src = """
            message Ping();
            actor A { behavior Idle { on Ping => Environment.Exit(0); } }
            """;
        AssertOneDiagnostic(src, "CE0084");
    }

    [Fact]
    public void CE0084_EnvironmentFailFast_Reported()
    {
        const string src = """
            message Ping();
            actor A { behavior Idle { on Ping => Environment.FailFast(); } }
            """;
        AssertOneDiagnostic(src, "CE0084");
    }

    [Fact]
    public void CE0084_ProcessKill_Reported()
    {
        const string src = """
            message Ping();
            actor A { behavior Idle { on Ping => Process.Kill(); } }
            """;
        AssertOneDiagnostic(src, "CE0084");
    }

    // ─── CE0085 — moved-value mutation tracking ─────────────────────────────

    [Fact]
    public void CE0085_MutateAfterTell_Reported()
    {
        const string src = """
            message Update(int v);
            actor A {
              behavior Idle {
                on Update u => {
                  peer.Tell(u);
                  u.v = 99;
                }
              }
            }
            """;
        var diagnostics = Analyze(src);
        Assert.Contains(diagnostics, d => d.Code == "CE0085");
    }

    [Fact]
    public void CE0085_MutateBeforeTell_NoDiagnostic()
    {
        const string src = """
            message Update(int v);
            actor A {
              behavior Idle {
                on Update u => {
                  u.v = 99;
                  peer.Tell(u);
                }
              }
            }
            """;
        var diagnostics = Analyze(src);
        Assert.DoesNotContain(diagnostics, d => d.Code == "CE0085");
    }

    [Fact]
    public void CE0085_ReassignmentAfterTell_NoDiagnostic()
    {
        // Bare reassignment isn't mutation of the moved value.
        const string src = """
            message Update(int v);
            actor A {
              behavior Idle {
                on Update u => {
                  peer.Tell(u);
                  u = new Update(99);
                }
              }
            }
            """;
        var diagnostics = Analyze(src);
        Assert.DoesNotContain(diagnostics, d => d.Code == "CE0085");
    }

    [Fact]
    public void CE0085_MutateInForeachAfterTell_Reported()
    {
        // Pin: the move analysis descends into foreach bodies — sending the
        // loop variable then mutating it within an iteration is a race.
        const string src = """
            message Update(int v);
            message Batch();
            actor A {
              behavior Idle {
                on Batch b => {
                  var msgs = new System.Collections.Generic.List<Update>();
                  foreach (var m in msgs) {
                    peer.Tell(m);
                    m.v = 99;
                  }
                }
              }
            }
            """;
        var diagnostics = Analyze(src);
        Assert.Contains(diagnostics, d => d.Code == "CE0085");
    }

    [Fact]
    public void CE0085_CastAlias_MutateAfterTell_Reported()
    {
        // A cast is alias-transparent: `var b = (Update)u` still names the
        // same object, so the cast must not launder the moved-ness off u.
        const string src = """
            message Update(int v);
            actor A {
              behavior Idle {
                on Update u => {
                  var b = (Update)u;
                  peer.Tell(u);
                  b.v = 99;
                }
              }
            }
            """;
        var diagnostics = Analyze(src);
        Assert.Contains(diagnostics, d => d.Code == "CE0085");
    }

    [Fact]
    public void CE0085_CastAlias_NoSend_Clean()
    {
        const string src = """
            message Update(int v);
            actor A {
              behavior Idle {
                on Update u => {
                  var b = (Update)u;
                  b.v = 99;
                }
              }
            }
            """;
        var diagnostics = Analyze(src);
        Assert.DoesNotContain(diagnostics, d => d.Code == "CE0085");
    }

    [Fact]
    public void CE0085_IsCaptureAlias_MutateAfterTell_Reported()
    {
        // `if (u is Update s)` — inside the branch, s IS u; sending s moves u
        // too, so mutating u afterwards is a race.
        const string src = """
            message Update(int v);
            actor A {
              behavior Idle {
                on Update u => {
                  if (u is Update s) {
                    peer.Tell(s);
                    u.v = 99;
                  }
                }
              }
            }
            """;
        var diagnostics = Analyze(src);
        Assert.Contains(diagnostics, d => d.Code == "CE0085");
    }

    [Fact]
    public void CE0113_NullConditionalAssignment_Reported()
    {
        // `u?.v = 1` has no C# lowering (no null-conditional assignment);
        // catch it at the Spek line, not as an opaque .g.cs error.
        const string src = """
            message Update(int v);
            actor A {
              behavior Idle {
                on Update u => {
                  u?.v = 1;
                }
              }
            }
            """;
        var diagnostics = Analyze(src);
        Assert.Contains(diagnostics, d => d.Code == "CE0113");
    }

    [Fact]
    public void CE0113_NullConditionalRead_Clean()
    {
        const string src = """
            message Update(int v);
            message Reply(int v);
            actor A {
              behavior Idle {
                on Update u => {
                  var x = u?.v;
                  return new Reply(99);
                }
              }
            }
            """;
        var diagnostics = Analyze(src);
        Assert.DoesNotContain(diagnostics, d => d.Code == "CE0113");
    }

    [Fact]
    public void CE0085_MutateAliasAfterTell_Reported()
    {
        // `var w = u` aliases u; after u is moved, mutating the alias w is a
        // mutation of the value the receiver now owns.
        const string src = """
            message Update(int v);
            actor A {
              behavior Idle {
                on Update u => {
                  var w = u;
                  peer.Tell(u);
                  w.v = 99;
                }
              }
            }
            """;
        Assert.Contains(Analyze(src), d => d.Code == "CE0085");
    }

    [Fact]
    public void CE0085_AliasAfterTell_ThenMutate_Reported()
    {
        // Aliasing a value that's *already* moved still makes the alias moved.
        const string src = """
            message Update(int v);
            actor A {
              behavior Idle {
                on Update u => {
                  peer.Tell(u);
                  var w = u;
                  w.v = 99;
                }
              }
            }
            """;
        Assert.Contains(Analyze(src), d => d.Code == "CE0085");
    }

    [Fact]
    public void CE0085_MoveViaAlias_MutateOriginal_Reported()
    {
        // Moving an alias (w) moves the value, so mutating the original (u) is
        // also flagged — alias groups are symmetric.
        const string src = """
            message Update(int v);
            actor A {
              behavior Idle {
                on Update u => {
                  var w = u;
                  peer.Tell(w);
                  u.v = 99;
                }
              }
            }
            """;
        Assert.Contains(Analyze(src), d => d.Code == "CE0085");
    }

    [Fact]
    public void CE0085_AliasChain_Reported()
    {
        // Transitive aliasing: u -> w -> x. Moving u moves the whole group.
        const string src = """
            message Update(int v);
            actor A {
              behavior Idle {
                on Update u => {
                  var w = u;
                  var x = w;
                  peer.Tell(u);
                  x.v = 99;
                }
              }
            }
            """;
        Assert.Contains(Analyze(src), d => d.Code == "CE0085");
    }

    [Fact]
    public void CE0085_MutateAliasBeforeTell_NoDiagnostic()
    {
        // Mutating through an alias *before* the move is fine.
        const string src = """
            message Update(int v);
            actor A {
              behavior Idle {
                on Update u => {
                  var w = u;
                  w.v = 99;
                  peer.Tell(u);
                }
              }
            }
            """;
        Assert.DoesNotContain(Analyze(src), d => d.Code == "CE0085");
    }

    // ─── CE0013 — duplicate declarations ────────────────────────────────────

    [Fact]
    public void CE0013_DuplicateMessage_Reported()
    {
        const string src = """
            message Ping();
            message Ping();
            actor A { behavior Idle { on Ping => { } } }
            """;
        AssertOneDiagnostic(src, "CE0013");
    }

    [Fact]
    public void CE0013_DuplicateActor_Reported()
    {
        const string src = """
            message Ping();
            actor A { behavior Idle { on Ping => { } } }
            actor A { behavior Idle { on Ping => { } } }
            """;
        AssertOneDiagnostic(src, "CE0013");
    }

    [Fact]
    public void CE0013_DuplicateBehavior_Reported()
    {
        const string src = """
            message Ping();
            actor A
            {
                behavior Idle { on Ping => { } }
                behavior Idle { on Ping => { } }
            }
            """;
        AssertOneDiagnostic(src, "CE0013");
    }

    [Fact]
    public void CE0013_DuplicateField_Reported()
    {
        const string src = """
            message Ping();
            actor A
            {
                int x = 0;
                int x = 1;
                behavior Idle { on Ping => { } }
            }
            """;
        AssertOneDiagnostic(src, "CE0013");
    }

    [Fact]
    public void CE0013_DuplicateMethod_Reported()
    {
        const string src = """
            message Ping();
            actor A
            {
                void Helper() { }
                void Helper() { }
                behavior Idle { on Ping => { } }
            }
            """;
        AssertOneDiagnostic(src, "CE0013");
    }

    [Fact]
    public void CE0013_NoDuplicates_NoDiagnostic()
    {
        // Idle → Busy via become so CE0014 (unused behavior) doesn't also fire.
        const string src = """
            message Ping();
            message Pong();
            actor A
            {
                int x = 0;
                string y = "hi";
                void Helper() { }
                void Other() { }
                behavior Idle { on Ping => { become Busy; } }
                behavior Busy { on Pong => { } }
            }
            """;
        AssertNoDiagnostics(src);
    }

    // ─── Span reporting — diagnostic carries the offending token's location ─

    [Fact]
    public void Diagnostic_CarriesSourceLocation()
    {
        const string src = """
            actor A
            {
                init() { become Missing; }
                behavior Known { on Ping => { } }
            }
            message Ping();
            """;
        var d = AssertOneDiagnostic(src, "CE0011");
        Assert.Equal(3, d.Line);
        Assert.True(d.Column > 0);
    }

    // ─── CE0081 — unreachable supervise arm ─────────────────────────────────

    [Fact]
    public void CE0081_TypedArmAfterCatchAll_Reported()
    {
        // The untyped `on Failure: Stop` catches everything first, so
        // the typed IOException arm below it can never fire.
        const string src = """
            message Ping();
            actor Parent
            {
                behavior Idle { on Ping => { } }
                supervise OneForOne(
                    on Failure: Stop,
                    on Failure(System.IO.IOException): Restart);
            }
            """;
        AssertOneDiagnostic(src, "CE0081");
    }

    [Fact]
    public void CE0081_DuplicateTypedArm_Reported()
    {
        // Two arms narrowing to the same exception type — the second
        // is dead code.
        const string src = """
            message Ping();
            actor Parent
            {
                behavior Idle { on Ping => { } }
                supervise OneForOne(
                    on Failure(System.IO.IOException): Restart,
                    on Failure(System.IO.IOException): Stop);
            }
            """;
        AssertOneDiagnostic(src, "CE0081");
    }

    [Fact]
    public void CE0081_OrderedArms_NoDiagnostic()
    {
        // Typed arms first, untyped catch-all at the end → no warnings.
        const string src = """
            message Ping();
            actor Parent
            {
                behavior Idle { on Ping => { } }
                supervise OneForOne(
                    on Failure(System.IO.IOException): Restart,
                    on Failure(System.InvalidOperationException): Stop,
                    on Failure: Escalate);
            }
            """;
        AssertNoDiagnostics(src);
    }

    // ─── CE0082 — duplicate untyped catch-all ───────────────────────────────

    [Fact]
    public void CE0082_TwoUntypedArms_Reported()
    {
        // Two `on Failure: Action` with no type narrowing. The second
        // is unreachable — the first catch-all wins.
        const string src = """
            message Ping();
            actor Parent
            {
                behavior Idle { on Ping => { } }
                supervise OneForOne(
                    on Failure: Restart,
                    on Failure: Stop);
            }
            """;
        AssertOneDiagnostic(src, "CE0082");
    }

    [Fact]
    public void CE0082_SingleUntypedArm_NoDiagnostic()
    {
        const string src = """
            message Ping();
            actor Parent
            {
                behavior Idle { on Ping => { } }
                supervise OneForOne(on Failure: Restart);
            }
            """;
        AssertNoDiagnostics(src);
    }

    // ─── CE0117 — unknown supervise option ──────────────────────────────────

    [Fact]
    public void CE0117_UnknownOption_Reported()
    {
        // `maxRetries`/`withinTime` are ordinary named arguments now, so a typo
        // parses cleanly and is caught here rather than as a raw parse error.
        const string src = """
            message Ping();
            actor Parent
            {
                behavior Idle { on Ping => { } }
                supervise OneForOne(on Failure: Restart, maxRetres: 2);
            }
            """;
        AssertOneDiagnostic(src, "CE0117");
    }

    [Fact]
    public void CE0117_KnownOptions_NoDiagnostic()
    {
        const string src = """
            message Ping();
            actor Parent
            {
                behavior Idle { on Ping => { } }
                supervise OneForOne(on Failure: Restart, maxRetries: 2, withinTime: System.TimeSpan.FromSeconds(30));
            }
            """;
        AssertNoDiagnostics(src);
    }

    [Fact]
    public void SuperviseOptionNames_AreNotReservedWords()
    {
        // The whole point of routing options through the named-argument grammar:
        // `maxRetries`/`withinTime` are usable as ordinary identifiers.
        const string src = """
            message Ping();
            actor Parent
            {
                behavior Idle
                {
                    on Ping =>
                    {
                        var maxRetries = 5;
                        var withinTime = maxRetries + 1;
                    }
                }
            }
            """;
        AssertNoDiagnostics(src);
    }

    // ─── CE0118 — supervise decl + explicit OnChildFailure override ──────────

    [Fact]
    public void CE0118_SuperviseAndOnChildFailureOverride_Reported()
    {
        // Both forms present: the supervise decl generates OnChildFailure, so the
        // hand-written override would be silently dropped — flag it instead.
        const string src = """
            message Ping();
            actor Parent
            {
                behavior Idle { on Ping => { } }
                supervise OneForOne(on Failure: Restart);
                FailureDirective OnChildFailure(ActorRef child, System.Exception cause, object msg)
                {
                    return FailureDirective.Resume;
                }
            }
            """;
        AssertOneDiagnostic(src, "CE0118");
    }

    [Fact]
    public void CE0118_SuperviseAlone_NoDiagnostic()
    {
        const string src = """
            message Ping();
            actor Parent
            {
                behavior Idle { on Ping => { } }
                supervise OneForOne(on Failure: Restart);
            }
            """;
        AssertNoDiagnostics(src);
    }

    [Fact]
    public void CE0118_OnChildFailureAlone_NoDiagnostic()
    {
        const string src = """
            message Ping();
            actor Parent
            {
                behavior Idle { on Ping => { } }
                FailureDirective OnChildFailure(ActorRef child, System.Exception cause, object msg)
                {
                    return FailureDirective.Resume;
                }
            }
            """;
        AssertNoDiagnostics(src);
    }

    // ─── CE0090 — channel input not covered by actor ────────────────────────

    [Fact]
    public void CE0090_MissingInputHandler_Reported()
    {
        // Channel declares `on Shutdown` but the actor that implements
        // it only handles Ping — Shutdown coverage gap.
        const string src = """
            message Ping();
            message Shutdown();

            channel Hostable
            {
                on Ping;
                on Shutdown;
            }

            actor PartialHost : Hostable
            {
                behavior Running { on Ping => { } }
            }
            """;
        AssertOneDiagnostic(src, "CE0090");
    }

    [Fact]
    public void CE0090_AllInputsHandled_NoDiagnostic()
    {
        const string src = """
            message Ping();
            message Shutdown();

            channel Hostable
            {
                on Ping;
                on Shutdown;
            }

            actor FullHost : Hostable
            {
                behavior Running
                {
                    on Ping     => { }
                    on Shutdown => { }
                }
            }
            """;
        AssertNoDiagnostics(src);
    }

    [Fact]
    public void CE0090_SingleHandlerSatisfiesMultipleChannels()
    {
        // Two channels both declare `on Ping` — a single handler on the
        // actor satisfies both inputs. Matches C# explicit-interface
        // semantics (one method implements multiple interfaces that
        // declare the same signature).
        const string src = """
            message Ping();
            channel Pingable1 { on Ping; }
            channel Pingable2 { on Ping; }

            actor Pinger : Pingable1, Pingable2
            {
                behavior Idle { on Ping => { } }
            }
            """;
        AssertNoDiagnostics(src);
    }

    // ─── CE0091 — unknown channel / base actor in colon list ────────────────

    [Fact]
    public void CE0091_UnknownChannelName_Reported()
    {
        const string src = """
            message Ping();
            actor Foo : NoSuchChannel
            {
                behavior Idle { on Ping => { } }
            }
            """;
        AssertOneDiagnostic(src, "CE0091");
    }

    [Fact]
    public void CE0091_TwoActorsInColonList_Reported()
    {
        // Only one base actor is permitted — a second actor-typed name
        // after the first should flag as misuse. (Bases are abstract so the
        // only diagnostic is CE0091 — a concrete base would also trip CE0123.)
        const string src = """
            message Ping();
            abstract actor BaseOne { }
            abstract actor BaseTwo { }

            actor Child : BaseOne, BaseTwo
            {
                behavior Idle { on Ping => { } }
            }
            """;
        AssertOneDiagnostic(src, "CE0091");
    }

    // ─── CE0092 — strict emits enforcement on sender.Tell ───────────────────

    [Fact]
    public void CE0092_SenderTellOfNonEmitNonReply_Reported()
    {
        // Channel declares `emits StatusChanged`. The handler has no
        // return, and the actor fires `sender.Tell(new Unrelated())` —
        // Unrelated isn't the reply type (there's no return) and isn't
        // in emits. That's a CE0092.
        const string src = """
            message Ping();
            message StatusChanged(string status);
            message Unrelated();

            channel Observable
            {
                on Ping;
                emits StatusChanged;
            }

            actor Leak : Observable
            {
                behavior Idle
                {
                    on Ping => { sender.Tell(new Unrelated()); }
                }
            }
            """;
        AssertOneDiagnostic(src, "CE0092");
    }

    [Fact]
    public void CE0092_SenderTellOfEmitDeclaredType_NoDiagnostic()
    {
        const string src = """
            message Ping();
            message StatusChanged(string status);

            channel Observable
            {
                on Ping;
                emits StatusChanged;
            }

            actor Broadcaster : Observable
            {
                behavior Idle
                {
                    on Ping => { sender.Tell(new StatusChanged("alive")); }
                }
            }
            """;
        AssertNoDiagnostics(src);
    }

    [Fact]
    public void CE0092_SenderTellOfReplyType_NoDiagnostic()
    {
        // `sender.Tell(new Pong())` is equivalent to `return new Pong();` —
        // matches the handler's Option-D inferred reply type, so it's a
        // reply, not an emit. Even though Pong isn't in `emits`, this is
        // fine.
        const string src = """
            message Ping();
            message Pong();

            channel Pingable
            {
                on Ping;
            }

            actor Echo : Pingable
            {
                behavior Idle
                {
                    on Ping =>
                    {
                        sender.Tell(new Pong());
                        return new Pong();
                    }
                }
            }
            """;
        AssertNoDiagnostics(src);
    }

    [Fact]
    public void CE0092_SelfTellAlwaysFree_NoDiagnostic()
    {
        // `self.Tell(X)` is internal message pump — never gated.
        const string src = """
            message Ping();
            message InternalTick();

            channel Pingable { on Ping; }

            actor Ticker : Pingable
            {
                behavior Idle
                {
                    on Ping => { self.Tell(new InternalTick()); }
                    on InternalTick => { }
                }
            }
            """;
        AssertNoDiagnostics(src);
    }

    [Fact]
    public void CE0092_EmitsAny_OptsOutOfEnforcement()
    {
        // `emits any;` is the advisory escape hatch — CE0092 is suppressed
        // for the entire actor.
        const string src = """
            message Ping();
            message Anything();

            channel Loose
            {
                on Ping;
                emits any;
            }

            actor Loosey : Loose
            {
                behavior Idle
                {
                    on Ping => { sender.Tell(new Anything()); }
                }
            }
            """;
        AssertNoDiagnostics(src);
    }

    [Fact]
    public void CE0092_NoChannel_NoEnforcement()
    {
        // An actor that implements no channel is exempt from CE0092.
        // Without a channel there's no contract to violate.
        const string src = """
            message Ping();
            message Anything();

            actor Freeform
            {
                behavior Idle
                {
                    on Ping => { sender.Tell(new Anything()); }
                }
            }
            """;
        AssertNoDiagnostics(src);
    }

    // ─── Channel inheritance — coverage walks bases ───────────────────

    [Fact]
    public void CE0090_InheritedInputUncovered_Reported()
    {
        // ServerHost inherits `on Shutdown` from HostBase. The actor
        // covers Reboot but not Shutdown — coverage check must walk
        // inheritance and flag the gap.
        const string src = """
            message Shutdown();
            message Reboot();

            channel HostBase { on Shutdown; }
            channel ServerHost : HostBase { on Reboot; }

            actor PartialServer : ServerHost
            {
                behavior Idle { on Reboot => { } }
            }
            """;
        AssertOneDiagnostic(src, "CE0090");
    }

    [Fact]
    public void CE0090_AllInheritedInputsCovered_NoDiagnostic()
    {
        const string src = """
            message Shutdown();
            message Reboot();

            channel HostBase { on Shutdown; }
            channel ServerHost : HostBase { on Reboot; }

            actor FullServer : ServerHost
            {
                behavior Idle
                {
                    on Shutdown => { }
                    on Reboot   => { }
                }
            }
            """;
        AssertNoDiagnostics(src);
    }

    [Fact]
    public void CE0090_DiamondInheritance_DeduplicatesInputs()
    {
        // `Bottom : Left, Right` where Left and Right both inherit from
        // Top. Top declares `on Shutdown;` — the actor only needs ONE
        // handler, not two.
        const string src = """
            message Shutdown();

            channel Top                  { on Shutdown; }
            channel Left   : Top         { }
            channel Right  : Top         { }
            channel Bottom : Left, Right { }

            actor Diamond : Bottom
            {
                behavior Idle { on Shutdown => { } }
            }
            """;
        AssertNoDiagnostics(src);
    }

    [Fact]
    public void CE0092_InheritedEmits_AreAccepted()
    {
        // The base channel declares `emits Heartbeat;`. A derived channel
        // inherits that, and an actor sender.Tell-ing a Heartbeat shouldn't
        // trip CE0092 — emits enforcement walks ancestors.
        const string src = """
            message Ping();
            message Heartbeat();

            channel Pulsable { on Ping; emits Heartbeat; }
            channel WatchedPulsable : Pulsable { }

            actor Watcher : WatchedPulsable
            {
                behavior Idle
                {
                    on Ping => { sender.Tell(new Heartbeat()); }
                }
            }
            """;
        AssertNoDiagnostics(src);
    }

    [Fact]
    public void CE0092_InheritedEmitsAny_DisablesEnforcement()
    {
        // Base channel uses `emits any;` as the advisory escape hatch.
        // Derived channel inherits — strict enforcement should be off
        // for actors implementing the derived channel. (`channel Loose : { … }`
        // with an empty colon list isn't valid grammar, so the base declares
        // no bases.)
        const string src = """
            message Ping();
            message Whatever();

            channel Loose      { on Ping; emits any; }
            channel StillLoose : Loose { }

            actor Loosey : StillLoose
            {
                behavior Idle
                {
                    on Ping => { sender.Tell(new Whatever()); }
                }
            }
            """;
        AssertNoDiagnostics(src);
    }

    // ─── CE0093 — unknown base channel ──────────────────────────────────────

    [Fact]
    public void CE0093_UnknownBaseChannel_Reported()
    {
        const string src = """
            message Ping();
            channel Derived : DoesNotExist { on Ping; }
            """;
        AssertOneDiagnostic(src, "CE0093");
    }

    [Fact]
    public void CE0093_BaseIsAMessageNotChannel_Reported()
    {
        // Spelling mistake: user named a `message` where a `channel`
        // belonged. Should still flag CE0093 — a message isn't a valid base.
        const string src = """
            message Ping();
            message Shutdown();
            channel ServerHost : Shutdown { on Ping; }
            """;
        AssertOneDiagnostic(src, "CE0093");
    }

    // ─── CE0094 — circular channel inheritance ──────────────────────────────

    [Fact]
    public void CE0094_DirectCycle_ReportedOnBothChannels()
    {
        // `A : B` and `B : A` — both channels are part of the cycle.
        // We expect a diagnostic on each (one per channel decl).
        const string src = """
            message Ping();
            channel A : B { on Ping; }
            channel B : A { }
            """;
        var diagnostics = Analyze(src);
        Assert.Equal(2, diagnostics.Count(d => d.Code == "CE0094"));
    }

    [Fact]
    public void CE0094_TransitiveCycle_Reported()
    {
        // `A : B`, `B : C`, `C : A` — three-channel cycle. Each should
        // get a CE0094.
        const string src = """
            message Ping();
            channel A : B { on Ping; }
            channel B : C { }
            channel C : A { }
            """;
        var diagnostics = Analyze(src);
        Assert.Equal(3, diagnostics.Count(d => d.Code == "CE0094"));
    }

    [Fact]
    public void CE0094_NoCycle_NoDiagnostic()
    {
        // Linear chain — no cycle.
        const string src = """
            message Ping();
            channel A          { on Ping; }
            channel B : A      { }
            channel C : B      { }
            """;
        AssertNoDiagnostics(src);
    }
}
