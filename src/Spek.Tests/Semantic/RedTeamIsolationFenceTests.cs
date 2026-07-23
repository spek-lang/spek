using Spek.Compiler.Parser;
using Spek.Compiler.Semantic;
using Xunit;

namespace Spek.Tests.Semantic;

/// <summary>
/// The permanent adversarial fence for actor-isolation: every probe a red-team
/// swept against CE0135 / CE0136 / CE0137 / CE0112 / CE0010, pinned so a hole
/// that was closed cannot silently reopen and a wall that stands cannot silently
/// fall. Organised by rule. "Hole" = a probe that used to slip through and now
/// must be caught; "wall" = a probe that was already caught (or already correct)
/// and must stay that way. Where a probe is documented residue (a boundary the
/// analyzer deliberately does not cross), the test pins the residue with a note,
/// so a future change that alters it shows up here for a conscious decision.
/// </summary>
public sealed class RedTeamIsolationFenceTests
{
    private static bool HasError(CompilationResult r, string code) =>
        r.Diagnostics.Any(d => d.Code == code && d.Severity == DiagnosticSeverity.Error);

    private static bool HasWarning(CompilationResult r, string code) =>
        r.Diagnostics.Any(d => d.Code == code && d.Severity == DiagnosticSeverity.Warning);

    private static int Count(CompilationResult r, string code) =>
        r.Diagnostics.Count(d => d.Code == code);

    /// <summary>A confined class that stores a callback — the "outlives the
    /// turn" registration shape the escape rules exist for.</summary>
    private const string Registry = """
        class Registry { public void Register(System.Action cb) { } }
        """;

    private const string RegistryArr = """
        class Registry { public void RegisterArr(System.Action[] cbs) { } }
        """;

    private const string RegistryObj = """
        class Registry { public void RegisterObj(object o) { } }
        """;

    // ───────────────────────── CE0135 — capture-escape (writes) ─────────────
    // The unifying reachability fix: a state-writing lambda hidden inside ANY
    // value-carrying container still escapes when the container is handed out.

    [Fact] // v1a — inline lambdas inside a ternary
    public void CE0135_TernaryInlineLambdas_Escapes()
    {
        var src = $$"""
            {{Registry}}
            message Up(int Delta);
            actor A {
                int seen = 0;
                Registry registry = new Registry();
                on Up u => { registry.Register(u.Delta > 0 ? (() => seen = seen + 1) : (() => { })); }
            }
            """;
        Assert.True(HasError(SpekCompiler.Parse(src), "CE0135"));
    }

    [Fact] // v1b — tainted locals inside a ternary
    public void CE0135_TernaryTaintedLocals_Escapes()
    {
        var src = $$"""
            {{Registry}}
            message Up(int Delta);
            actor A {
                int seen = 0;
                Registry registry = new Registry();
                on Up u => {
                    var bump = () => seen = seen + 1;
                    var noop = () => { };
                    registry.Register(u.Delta > 0 ? bump : noop);
                }
            }
            """;
        Assert.True(HasError(SpekCompiler.Parse(src), "CE0135"));
    }

    [Fact] // v1c — null-coalesce
    public void CE0135_NullCoalesce_Escapes()
    {
        var src = $$"""
            {{Registry}}
            message Up();
            actor A {
                int seen = 0;
                Registry registry = new Registry();
                on Up u => {
                    var bump = () => seen = seen + 1;
                    var noop = () => { };
                    registry.Register(bump ?? noop);
                }
            }
            """;
        Assert.True(HasError(SpekCompiler.Parse(src), "CE0135"));
    }

    [Fact] // v2a — array literal
    public void CE0135_ArrayLiteral_Escapes()
    {
        var src = $$"""
            {{RegistryArr}}
            message Up();
            actor A {
                int seen = 0;
                Registry registry = new Registry();
                on Up u => {
                    var bump = () => seen = seen + 1;
                    registry.RegisterArr(new System.Action[] { bump, bump });
                }
            }
            """;
        Assert.True(HasError(SpekCompiler.Parse(src), "CE0135"));
    }

    [Fact] // v2b — array carried through a local
    public void CE0135_ArrayThroughLocal_Escapes()
    {
        var src = $$"""
            {{RegistryArr}}
            message Up();
            actor A {
                int seen = 0;
                Registry registry = new Registry();
                on Up u => {
                    var bump = () => seen = seen + 1;
                    var arr = new System.Action[] { bump };
                    registry.RegisterArr(arr);
                }
            }
            """;
        Assert.True(HasError(SpekCompiler.Parse(src), "CE0135"));
    }

    [Fact] // v3a — tuple argument
    public void CE0135_TupleArgument_Escapes()
    {
        var src = $$"""
            {{RegistryObj}}
            message Up();
            actor A {
                int seen = 0;
                Registry registry = new Registry();
                on Up u => {
                    var bump = () => seen = seen + 1;
                    registry.RegisterObj((bump, 3));
                }
            }
            """;
        Assert.True(HasError(SpekCompiler.Parse(src), "CE0135"));
    }

    [Fact] // v3b — tuple returned from a method
    public void CE0135_TupleReturnedFromMethod_Escapes()
    {
        var src = $$"""
            {{RegistryObj}}
            message Up();
            actor A {
                int seen = 0;
                Registry registry = new Registry();
                private object Make() {
                    var bump = () => seen = seen + 1;
                    return (bump, 3);
                }
                on Up u => { registry.RegisterObj(Make()); }
            }
            """;
        Assert.True(HasError(SpekCompiler.Parse(src), "CE0135"));
    }

    [Fact] // v4 — switch expression arms
    public void CE0135_SwitchExpressionArms_Escapes()
    {
        var src = $$"""
            {{Registry}}
            message Up(int Delta);
            actor A {
                int seen = 0;
                Registry registry = new Registry();
                on Up u => {
                    var bump = () => seen = seen + 1;
                    var noop = () => { };
                    registry.Register(u.Delta switch { 0 => noop, _ => bump });
                }
            }
            """;
        Assert.True(HasError(SpekCompiler.Parse(src), "CE0135"));
    }

    [Fact] // v5a — indirect invocation of a tainted local
    public void CE0135_IndirectInvocationOfTaintedLocal_Escapes()
    {
        var src = $$"""
            {{Registry}}
            message Up();
            actor A {
                int seen = 0;
                Registry registry = new Registry();
                on Up u => {
                    var bump = () => seen = seen + 1;
                    registry.Register(() => bump());
                }
            }
            """;
        Assert.True(HasError(SpekCompiler.Parse(src), "CE0135"));
    }

    [Fact] // v6a — object initializer value
    public void CE0135_ObjectInitializerValue_Escapes()
    {
        var src = $$"""
            {{RegistryObj}}
            class Holder { public System.Action Callback = null; }
            message Up();
            actor A {
                int seen = 0;
                Registry registry = new Registry();
                on Up u => {
                    var bump = () => seen = seen + 1;
                    registry.RegisterObj(new Holder { Callback = bump });
                }
            }
            """;
        Assert.True(HasError(SpekCompiler.Parse(src), "CE0135"));
    }

    [Fact] // v8a — single copy taints the copy
    public void CE0135_SingleCopy_Escapes()
    {
        var src = $$"""
            {{Registry}}
            message Up();
            actor A {
                int seen = 0;
                Registry registry = new Registry();
                on Up u => { var bump = () => seen = seen + 1; var b = bump; registry.Register(b); }
            }
            """;
        Assert.True(HasError(SpekCompiler.Parse(src), "CE0135"));
    }

    [Fact] // v8b — copy chain
    public void CE0135_DoubleCopy_Escapes()
    {
        var src = $$"""
            {{Registry}}
            message Up();
            actor A {
                int seen = 0;
                Registry registry = new Registry();
                on Up u => { var bump = () => seen = seen + 1; var b = bump; var b2 = b; registry.Register(b2); }
            }
            """;
        Assert.True(HasError(SpekCompiler.Parse(src), "CE0135"));
    }

    [Fact] // v8c — conditional reassignment taints
    public void CE0135_ConditionalRebind_Escapes()
    {
        var src = $$"""
            {{Registry}}
            message Up(int Delta);
            actor A {
                int seen = 0;
                Registry registry = new Registry();
                on Up u => {
                    var bump = () => seen = seen + 1;
                    System.Action b = () => { };
                    if (u.Delta > 0) { b = bump; }
                    registry.Register(b);
                }
            }
            """;
        Assert.True(HasError(SpekCompiler.Parse(src), "CE0135"));
    }

    [Fact] // v9b — LINQ projection returns a tainted local
    public void CE0135_SelectProjectionReturnsTainted_Escapes()
    {
        var src = $$"""
            using System.Linq;
            using System.Collections.Generic;
            {{RegistryObj}}
            message Up();
            actor A {
                int seen = 0;
                Registry registry = new Registry();
                List<int> items = new List<int>();
                on Up u => {
                    var bump = () => seen = seen + 1;
                    registry.RegisterObj(items.Select(x => bump));
                }
            }
            """;
        Assert.True(HasError(SpekCompiler.Parse(src), "CE0135"));
    }

    [Fact] // v11a — mutating method through a class-field alias
    public void CE0135_MutatingMethodThroughClassAlias_Escapes()
    {
        var src = """
            class Registry { public void Register(System.Action cb) { } }
            class Counter { int n = 0; public void Bump() { n = n + 1; } }
            message Up();
            actor A {
                Counter helper = new Counter();
                Registry registry = new Registry();
                on Up u => { var h = helper; registry.Register(() => h.Bump()); }
            }
            """;
        Assert.True(HasError(SpekCompiler.Parse(src), "CE0135"));
    }

    // Walls — these were already caught and must stay caught.

    [Theory]
    [InlineData("v5b register-inside-lambda", """
        class Registry { public void Register(System.Action cb) { } }
        class Outer { public void Do(System.Action cb) { } }
        message Up();
        actor A {
            int seen = 0;
            Registry registry = new Registry();
            Outer outer = new Outer();
            on Up u => { var bump = () => seen = seen + 1; outer.Do(() => { registry.Register(bump); }); }
        }
        """)]
    [InlineData("v6b new ctor arg", """
        class Registry { public void RegisterObj(object o) { } }
        class Holder { public System.Action Callback = null; init(System.Action cb) { Callback = cb; } }
        message Up();
        actor A {
            int seen = 0;
            Registry registry = new Registry();
            on Up u => { var bump = () => seen = seen + 1; registry.RegisterObj(new Holder(bump)); }
        }
        """)]
    [InlineData("v9c select inline nested", """
        using System.Linq;
        using System.Collections.Generic;
        class Registry { public void RegisterObj(object o) { } }
        message Up();
        actor A {
            int seen = 0;
            Registry registry = new Registry();
            List<int> items = new List<int>();
            on Up u => { registry.RegisterObj(items.Select(x => (() => seen = seen + 1))); }
        }
        """)]
    [InlineData("v11b direct field mutating", """
        class Registry { public void Register(System.Action cb) { } }
        class Counter { int n = 0; public void Bump() { n = n + 1; } }
        message Up();
        actor A {
            Counter helper = new Counter();
            Registry registry = new Registry();
            on Up u => { registry.Register(() => helper.Bump()); }
        }
        """)]
    public void CE0135_Walls_StayCaught(string _, string src) =>
        Assert.True(HasError(SpekCompiler.Parse(src), "CE0135"));

    [Fact] // b11 — escapes from init and term lifecycle bodies
    public void CE0135_InitAndTerm_Escapes()
    {
        var src = """
            class Registry { public void Register(System.Action a) { } }
            message Go();
            actor Probe {
                int seen = 0;
                Registry reg = new Registry();
                init() { reg.Register(() => seen = seen + 1); }
                term { reg.Register(() => seen = seen + 1); }
            }
            """;
        var r = SpekCompiler.Parse(src);
        Assert.True(Count(r, "CE0135") >= 2);
    }

    [Fact] // a3b — a WRITE escape through a confined method fires despite trust
    public void CE0135_WriteEscapeThroughConfinedMethod_FiresDespiteTrust()
    {
        var src = """
            message Go();
            class Sneaky { public void Take(System.Action a) { Acme.Global.Store(a); } }
            actor Probe {
                int seen = 0;
                Sneaky s = new Sneaky();
                on Go => { s.Take(() => seen = seen + 1); }
            }
            """;
        Assert.True(HasError(SpekCompiler.Parse(src), "CE0135"));
    }

    // ───────────── CE0135 — false-positive fixes (must NOT fire) ────────────

    [Fact] // a6 — Sort/Where are synchronous, so a write callback is safe
    public void CE0135_MutatingWhere_IsSynchronousAndSafe()
    {
        var src = """
            message Go();
            actor Probe {
                int count = 0;
                System.Collections.Generic.List<int> items = new System.Collections.Generic.List<int>();
                on Go => { var r = items.Where(x => { count = count + 1; return x > 0; }); }
            }
            """;
        Assert.False(HasError(SpekCompiler.Parse(src), "CE0135"));
    }

    [Fact] // b12 — ForEach still errors; Sort (write and read) does not
    public void CE0135_ForEachErrors_ButSortIsSynchronousSafe()
    {
        var src = """
            message Go();
            actor Probe {
                int total = 0;
                int count = 0;
                int offset = 0;
                System.Collections.Generic.List<int> items = new System.Collections.Generic.List<int>();
                on Go => {
                    items.ForEach(x => total = total + x);
                    items.Sort((a, b) => { count = count + 1; return a - b; });
                    items.Sort((a, b) => (a + offset) - (b + offset));
                }
            }
            """;
        var r = SpekCompiler.Parse(src);
        // Exactly one CE0135 — the ForEach — and no Sort noise (write or read).
        Assert.Equal(1, Count(r, "CE0135"));
        Assert.Contains(r.Diagnostics, d => d.Code == "CE0135" && d.Message.Contains("ForEach"));
        Assert.DoesNotContain(r.Diagnostics, d => d.Code == "CE0135" && d.Message.Contains("Sort"));
        Assert.False(HasWarning(r, "CE0136"));   // the read-Sort stays silent too
    }

    // ───────────────────────── CE0136 — read-capture trust ──────────────────

    [Fact] // a3 — a confined class that forwards its delegate to a foreign sink
    public void CE0136_ConfinedClassForwardsToForeignSink_Warns()
    {
        var src = """
            message Go();
            class Sneaky { public void Take(System.Action a) { Acme.Global.Store(a); } }
            actor Probe {
                string name = "probe";
                Sneaky s = new Sneaky();
                on Go => { s.Take(() => System.Console.WriteLine(name)); }
            }
            """;
        Assert.True(HasWarning(SpekCompiler.Parse(src), "CE0136"));
    }

    [Fact] // a4 — a module function that forwards to a foreign sink
    public void CE0136_ModuleForwardsToForeignSink_Warns()
    {
        var src = """
            message Go();
            module Dispatcher { public void Fan(System.Action a) { Acme.Global.Store(a); } }
            actor Probe {
                string name = "probe";
                on Go => { Dispatcher.Fan(() => System.Console.WriteLine(name)); }
            }
            """;
        Assert.True(HasWarning(SpekCompiler.Parse(src), "CE0136"));
    }

    [Fact] // a1b — DOCUMENTED RESIDUE. A confined class STORES a read-capturing
           // delegate in its own field, then a reader handler fires it
           // concurrently with the writer — a real race. It is NOT caught: the
           // per-method trust check (MethodKeepsDelegatesOnThread) treats storing
           // in an own field as on-thread (which it must, to keep the legitimate
           // "confined store, never fired" case in SpawnClassArgTests green), and
           // the concurrent-reader-fire is a whole-actor reader/writer property no
           // per-method check models. This is the narrower sibling of the a3/a4
           // foreign-sink hole, which IS closed. Pinned as residue so a future
           // change that alters it is a conscious decision, not a silent drift.
    public void CE0136_ConfinedClassStoresThenReaderFires_IsResidue_NotCaught()
    {
        var src = """
            message Go();
            class FakeQuery {
                System.Action? saved;
                public void Where(System.Action a) { saved = a; }
                public void FireStored() { if (saved != null) { saved(); } }
            }
            actor Probe {
                string name = "probe";
                FakeQuery q = new FakeQuery();
                on Go => { q.Where(() => System.Console.WriteLine(name)); }
                reader on Go => { q.FireStored(); }
            }
            """;
        Assert.False(HasWarning(SpekCompiler.Parse(src), "CE0136"));
    }

    [Fact] // a1 — a foreign type: the LINQ name table still silences 'Where'
           // (accepted residue), but 'Register' warns. This is the boundary.
    public void CE0136_ForeignReceiver_NameTableResidue_WarnsOnlyOnRegister()
    {
        var src = """
            message Go();
            actor Probe {
                string name = "probe";
                Acme.Storage.Sink sink = new Acme.Storage.Sink();
                on Go => {
                    sink.Where(() => System.Console.WriteLine(name));
                    sink.Register(() => System.Console.WriteLine(name));
                    sink.Select(() => System.Console.WriteLine(name));
                    sink.Aggregate(() => System.Console.WriteLine(name));
                }
            }
            """;
        var r = SpekCompiler.Parse(src);
        Assert.Equal(1, Count(r, "CE0136"));
        Assert.Contains(r.Diagnostics, d => d.Code == "CE0136" && d.Message.Contains("'Register'"));
    }

    [Fact] // b7b — an inherited field read is recognised as capturable state
    public void CE0136_InheritedFieldRead_Warns()
    {
        var src = """
            message Go();
            abstract actor Base {
                protected string label = "base";
                public abstract void Handle();
            }
            actor Derived : Base {
                string ownLabel = "own";
                Acme.Storage.Sink sink = new Acme.Storage.Sink();
                init() { become Default; }
                behavior Default {
                    on Go => {
                        sink.Register(() => System.Console.WriteLine(label));
                        sink.Register(() => System.Console.WriteLine(ownLabel));
                    }
                }
                public void Handle() { label = "x"; }
            }
            """;
        var r = SpekCompiler.Parse(src);
        Assert.Equal(2, Count(r, "CE0136"));  // inherited 'label' AND own 'ownLabel'
        Assert.Contains(r.Diagnostics, d => d.Code == "CE0136" && d.Message.Contains("'label'"));
    }

    [Fact] // b10 — a read capture escaping from a reader handler
    public void CE0136_ReaderHandlerEscape_Warns()
    {
        var src = """
            message Query();
            actor Probe {
                string name = "probe";
                Acme.Storage.Sink sink = new Acme.Storage.Sink();
                init() { become Default; }
                behavior Default {
                    reader on Query => { sink.Register(() => System.Console.WriteLine(name)); }
                }
            }
            """;
        Assert.True(HasWarning(SpekCompiler.Parse(src), "CE0136"));
    }

    [Fact] // read capture into a real LINQ operator on a real collection: silent
    public void CE0136_ReadCaptureIntoRealLinq_StaysSilent()
    {
        var src = """
            using System.Linq;
            using System.Collections.Generic;
            message Go();
            actor Probe {
                int filterId = 7;
                List<int> items = new List<int>();
                on Go => { var r = items.Where(x => x == filterId); }
            }
            """;
        Assert.False(HasWarning(SpekCompiler.Parse(src), "CE0136"));
    }

    // ───────────────────────── CE0137 — spawn class sharing ─────────────────

    [Fact] // v3c — a class field reached through ImmutableArray.Create
    public void CE0137_ClassFieldThroughImmutableArrayCreate_Errors()
    {
        var src = """
            using System.Collections.Immutable;
            class Registry { public int n = 0; public void Bump() { n = n + 1; } }
            message Go();
            actor Child { Registry r; init(ImmutableArray<Registry> regs) { r = regs[0]; } }
            actor Sender { Registry reg = new Registry(); on Go => { var kid = spawn<Child>(ImmutableArray.Create(reg)); } }
            """;
        Assert.True(HasError(SpekCompiler.Parse(src), "CE0137"));
    }

    [Fact] // v4a — a class field reached through an object initializer
    public void CE0137_ClassFieldThroughObjectInitializer_Errors()
    {
        var src = """
            class Registry { public int n = 0; public void Bump() { n = n + 1; } }
            class Holder { public Registry Reg; }
            message Go();
            actor Child { Holder h; init(Holder holder) { h = holder; } }
            actor Sender { Registry reg = new Registry(); on Go => { var kid = spawn<Child>(new Holder { Reg = reg }); } }
            """;
        Assert.True(HasError(SpekCompiler.Parse(src), "CE0137"));
    }

    [Fact] // v9a — a local `new`ed capturing a class field, then spawned
    public void CE0137_LocalNewedCapturingClassField_Errors()
    {
        var src = """
            class Registry { public int n = 0; public void Bump() { n = n + 1; } }
            class Wrapper { public Registry inner; init(Registry r) { inner = r; } }
            message Go();
            actor Child { Wrapper w; init(Wrapper wrap) { w = wrap; } }
            actor Sender {
                Registry reg = new Registry();
                on Go => { var w = new Wrapper(reg); var kid = spawn<Child>(w); reg.Bump(); }
            }
            """;
        Assert.True(HasError(SpekCompiler.Parse(src), "CE0137"));
    }

    [Fact] // baseline + v1c walls
    public void CE0137_DirectFieldAndWrapperField_Errors()
    {
        var direct = """
            class Registry { int n = 0; public void Bump() { n = n + 1; } }
            actor Child { Registry r; init(Registry reg) { r = reg; } }
            actor Sender { Registry reg = new Registry(); on Go => { var kid = spawn<Child>(reg); } }
            message Go();
            """;
        Assert.True(HasError(SpekCompiler.Parse(direct), "CE0137"));
    }

    [Fact] // v10a — spawning `self` (an ActorRef) is legal, NOT a class share
    public void CE0137_SpawnSelf_IsLegal()
    {
        var src = """
            message Go();
            actor Child { ActorRef parent; init(ActorRef p) { parent = p; } on Go => { } }
            actor Sender { on Go => { var kid = spawn<Child>(self); } }
            """;
        Assert.False(HasError(SpekCompiler.Parse(src), "CE0137"));
    }

    [Fact] // the legal gift: a fresh instance passed inline stays legal
    public void CE0137_FreshInstanceGift_IsLegal()
    {
        var src = """
            class Registry { int n = 0; public void Bump() { n = n + 1; } }
            actor Child { Registry r; init(Registry reg) { r = reg; } on Go => { } }
            actor Sender { on Go => { var kid = spawn<Child>(new Registry()); } }
            message Go();
            """;
        Assert.False(HasError(SpekCompiler.Parse(src), "CE0137"));
    }

    // ───────────────────────── CE0112 — region field recursion ──────────────

    [Fact] // v2c — a mutable class nested inside an immutable container
    public void CE0112_MutableClassInsideImmutableContainer_Errors()
    {
        var src = """
            using System.Collections.Immutable;
            class Registry { public int n = 0; public void Bump() { n = n + 1; } }
            shared Pool { ImmutableArray<Registry> items = ImmutableArray.Create(new Registry()); }
            message Poke();
            actor Reader { use Pool p; reader on Poke => { p.items[0].Bump(); } }
            """;
        Assert.True(HasError(SpekCompiler.Parse(src), "CE0112"));
    }

    [Fact] // region-native mutable collections of primitives stay legal
    public void CE0112_ImmutableContainerOfPrimitives_IsLegal()
    {
        var src = """
            using System.Collections.Immutable;
            shared Pool { ImmutableArray<int> items = ImmutableArray.Create(1); }
            message Poke();
            actor Reader { use Pool p; reader on Poke => { var x = p.items; } }
            """;
        Assert.False(HasError(SpekCompiler.Parse(src), "CE0112"));
    }

    // ───────────────────────── CE0010 — ask-reply return channel ────────────

    [Fact] // v5a — returning a confined class field as a reply
    public void CE0010_ReturnConfinedClassField_Errors()
    {
        var src = """
            class Registry { public int n = 0; public void Bump() { n = n + 1; } }
            message GetReg();
            actor Keeper { Registry reg = new Registry(); on GetReg => { reg.Bump(); return reg; } }
            """;
        Assert.True(HasError(SpekCompiler.Parse(src), "CE0010"));
    }

    [Fact] // v5b — returning a message that carries a confined class
    public void CE0010_ReturnMessageCarryingClass_Errors()
    {
        var src = """
            class Registry { public int n = 0; public void Bump() { n = n + 1; } }
            message Box<T>(T value);
            message GetReg();
            actor Holder { Registry reg = new Registry(); on GetReg => return new Box<Registry>(reg); }
            """;
        Assert.True(HasError(SpekCompiler.Parse(src), "CE0010"));
    }

    [Fact] // a private method returning its own class field internally is fine —
           // only a handler return is an ask-reply.
    public void CE0010_PrivateMethodReturningClassField_IsLegal()
    {
        var src = """
            class Registry { public int n = 0; public void Bump() { n = n + 1; } }
            message Up();
            actor A {
                Registry registry = new Registry();
                private Registry GetReg() { return registry; }
                on Up u => { GetReg().Bump(); }
            }
            """;
        Assert.False(HasError(SpekCompiler.Parse(src), "CE0010"));
    }

    [Fact] // returning a fresh instance as a reply is the legal gift
    public void CE0010_ReturnFreshInstance_IsLegal()
    {
        var src = """
            class Registry { public int n = 0; }
            message GetReg();
            actor Keeper { on GetReg => { return new Registry(); } }
            """;
        Assert.False(HasError(SpekCompiler.Parse(src), "CE0010"));
    }

    // ───────────────────────── Inheritance — inherited state visibility ─────

    [Fact] // b7 — a lambda writing an INHERITED field escapes (was invisible)
    public void CE0135_InheritedFieldWrite_Escapes()
    {
        var src = """
            class Registry { public void Register(System.Action a) { } }
            message Go();
            abstract actor Base { protected int count = 0; public abstract void Handle(); }
            actor Derived : Base {
                Registry reg = new Registry();
                int ownField = 0;
                init() { become Default; }
                behavior Default {
                    on Go => {
                        reg.Register(() => count = count + 1);
                        reg.Register(() => ownField = ownField + 1);
                    }
                }
                public void Handle() { count = count + 1; }
            }
            """;
        var r = SpekCompiler.Parse(src);
        Assert.Equal(2, Count(r, "CE0135"));  // inherited 'count' AND own 'ownField'
        Assert.Contains(r.Diagnostics, d => d.Code == "CE0135" && d.Message.Contains("'count'"));
    }
}
