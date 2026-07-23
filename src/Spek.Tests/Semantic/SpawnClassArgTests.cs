using Spek.Compiler.Parser;
using Spek.Compiler.Semantic;
using Xunit;

namespace Spek.Tests.Semantic;

/// <summary>
/// CE0137. A spawn argument whose static type is a mutable Spek `class` may
/// not alias the spawning actor's state: `spawn&lt;Child&gt;(reg)` hands the
/// child a reference the sender also keeps, so two actors hold one mutable
/// object — share-XOR-mutate broken with no lambda in sight. The sibling
/// routes are already gated (CE0010: message fields, CE0112: region fields);
/// this closes the third. The legal hand-off is the constructor gift: a fresh
/// `new` passed inline, or a `new`-initialized local never also stored in a
/// field — one owner at every instant.
/// </summary>
public sealed class SpawnClassArgTests
{
    private static bool HasError(CompilationResult r, string code) =>
        r.Diagnostics.Any(d => d.Code == code && d.Severity == DiagnosticSeverity.Error);

    /// <summary>A mutable Spek class plus a child actor that accepts one at
    /// construction — the two halves every sharing shape needs.</summary>
    private const string RegistryAndChild = """
        message Go();

        class Registry
        {
            int n = 0;
            public void Bump() { n = n + 1; }
        }

        actor Child
        {
            Registry r;
            init(Registry reg) { r = reg; }
            on Go => { r.Bump(); }
        }
        """;

    [Fact]
    public void CE0137_ClassTypedActorField_Errors()
    {
        // The repro's core: the sender's own field handed to the child. Both
        // actors now reach one mutable object.
        const string src = $$"""
            {{RegistryAndChild}}

            actor Sender
            {
                Registry reg = new Registry();

                on Go =>
                {
                    var kid = spawn<Child>(reg);
                    kid.Tell(new Go());
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.False(parsed.Success, "CE0137 is an error — it must fail the build.");
        Assert.True(HasError(parsed, "CE0137"));

        // The message must name the shared field and teach the gift idiom.
        var d = parsed.Diagnostics.First(x => x.Code == "CE0137");
        Assert.Contains("'reg'", d.Message);
        Assert.Contains("new Registry()", d.Message);
    }

    [Fact]
    public void CE0137_SelfQualifiedField_Errors()
    {
        // The `self.reg` spelling of the same share.
        const string src = $$"""
            {{RegistryAndChild}}

            actor Sender
            {
                Registry reg = new Registry();

                on Go =>
                {
                    var kid = spawn<Child>(self.reg);
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.True(HasError(parsed, "CE0137"));
    }

    [Fact]
    public void CE0137_LocalTaintedByField_Errors()
    {
        // Laundering through a local changes nothing — `r` is the same
        // object as `reg`. Same intra-body taint pattern as CE0135.
        const string src = $$"""
            {{RegistryAndChild}}

            actor Sender
            {
                Registry reg = new Registry();

                on Go =>
                {
                    var r = reg;
                    var kid = spawn<Child>(r);
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.True(HasError(parsed, "CE0137"));

        var d = parsed.Diagnostics.First(x => x.Code == "CE0137");
        Assert.Contains("aliases actor field 'reg'", d.Message);
    }

    [Fact]
    public void CE0137_FreshInstanceInline_NotFlagged()
    {
        // The constructor gift: the sender constructs and never holds — the
        // child is the only owner from the first instant.
        const string src = $$"""
            {{RegistryAndChild}}

            actor Sender
            {
                on Go =>
                {
                    var kid = spawn<Child>(new Registry());
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.DoesNotContain(parsed.Diagnostics, d => d.Code == "CE0137");
    }

    [Fact]
    public void CE0137_GiftedLocal_NeverFieldAssigned_NotFlagged()
    {
        // The gift may pass through a local — building the object up before
        // the spawn is ordinary code, and the sender still keeps nothing.
        const string src = $$"""
            {{RegistryAndChild}}

            actor Sender
            {
                on Go =>
                {
                    var r = new Registry();
                    r.Bump();
                    var kid = spawn<Child>(r);
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.DoesNotContain(parsed.Diagnostics, d => d.Code == "CE0137");
    }

    [Fact]
    public void CE0137_GiftedLocal_FieldAssignedBeforeSpawn_Errors()
    {
        // `new` provenance is not enough: parking the same local in a field
        // first means the sender keeps a reference after all.
        const string src = $$"""
            {{RegistryAndChild}}

            actor Sender
            {
                Registry mine = null;

                on Go =>
                {
                    var r = new Registry();
                    mine = r;
                    var kid = spawn<Child>(r);
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.True(HasError(parsed, "CE0137"));
    }

    [Fact]
    public void CE0137_GiftedLocal_FieldAssignedAfterSpawn_Errors()
    {
        // Order doesn't launder it: storing the gifted local AFTER the spawn
        // is the same two-owner outcome, so the store is tracked body-wide.
        const string src = $$"""
            {{RegistryAndChild}}

            actor Sender
            {
                Registry mine = null;

                on Go =>
                {
                    var r = new Registry();
                    var kid = spawn<Child>(r);
                    mine = r;
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.True(HasError(parsed, "CE0137"));

        var d = parsed.Diagnostics.First(x => x.Code == "CE0137");
        Assert.Contains("also stored in actor state", d.Message);
    }

    [Fact]
    public void CE0137_LocalReassignedFromField_Errors()
    {
        // A plain reassignment re-binds the taint exactly as `var` does:
        // after `r = reg;` the local is the field's object again.
        const string src = $$"""
            {{RegistryAndChild}}

            actor Sender
            {
                Registry reg = new Registry();

                on Go =>
                {
                    var r = new Registry();
                    r = reg;
                    var kid = spawn<Child>(r);
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.True(HasError(parsed, "CE0137"));
    }

    [Fact]
    public void CE0137_ForeignTypedField_NotFlagged()
    {
        // PINNED RESIDUE — a FOREIGN-typed field argument stays silent, the
        // same stance CE0135/CE0136 take: whether a StringBuilder is mutable
        // is knowable to a human, but the analyzer has no foreign type
        // resolution and does not guess. That remainder belongs to the
        // runtime turn guard (in design).
        const string src = """
            message Go();

            actor Child
            {
                System.Text.StringBuilder b;
                init(System.Text.StringBuilder sb) { b = sb; }
                on Go => { }
            }

            actor Sender
            {
                System.Text.StringBuilder log = new System.Text.StringBuilder();

                on Go =>
                {
                    var kid = spawn<Child>(log);
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.DoesNotContain(parsed.Diagnostics, d => d.Code == "CE0137");
    }

    [Fact]
    public void CE0137_NonClassArgs_NotFlagged()
    {
        // Primitives, strings, messages, and ActorRefs are share-safe by
        // construction — none of them is a mutable Spek class.
        const string src = """
            message Go();
            message Config(string Name, int Limit);

            actor Child
            {
                init(int n, string label, Config cfg, ActorRef parent) { }
                on Go => { }
            }

            actor Sender
            {
                int count = 3;
                string name = "sender";
                Config cfg = null;
                ActorRef upstream = null;

                on Go =>
                {
                    var kid = spawn<Child>(count, name, cfg, upstream);
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.DoesNotContain(parsed.Diagnostics, d => d.Code == "CE0137");
    }

    [Fact]
    public void CE0137_RegionFieldThroughUseHandle_Errors()
    {
        // A class-typed region field is already CE0112 at its declaration;
        // the spawn that would hand it to a child is flagged too, so the
        // sharing site carries its own caret.
        const string src = """
            message Go();

            class Counter
            {
                int n = 0;
                public void Inc() { n = n + 1; }
            }

            shared Stats
            {
                Counter hits = new Counter();
            }

            actor Child
            {
                Counter c;
                init(Counter counter) { c = counter; }
                on Go => { }
            }

            actor Sender
            {
                use Stats stats;

                on Go =>
                {
                    var kid = spawn<Child>(stats.hits);
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.True(HasError(parsed, "CE0112"));
        Assert.True(HasError(parsed, "CE0137"));
    }

    [Fact]
    public void CE0137_LaunderingChain_DelegateInClassViaSpawn_Errors()
    {
        // The launder2 repro, verbatim: a name-capturing delegate is parked
        // inside the class (legal — CE0136 trusts the confined receiver
        // precisely because the class cannot reach another thread), then the
        // class itself is spawned away. Without CE0137 the child fires the
        // delegate on its own thread and the trust was a lie; with it, the
        // chain dies at the spawn.
        const string src = """
            message Go();

            class Registry
            {
                System.Action? saved;
                public void Register(System.Action a) { saved = a; }
                public void FireStored() { if (saved != null) { saved(); } }
            }

            actor Child
            {
                Registry r;
                init(Registry reg) { r = reg; }
                on Go => { r.FireStored(); }
            }

            actor Sender
            {
                string name = "sender";
                Registry reg = new Registry();

                on Go =>
                {
                    reg.Register(() => System.Console.WriteLine(name));
                    var kid = spawn<Child>(reg);
                    kid.Tell(new Go());
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.False(parsed.Success);
        Assert.True(HasError(parsed, "CE0137"));
        // The delegate registration itself stays clean — the receiver is a
        // confined class, and with CE0137 that confinement now actually holds.
        Assert.DoesNotContain(parsed.Diagnostics, d => d.Code == "CE0136");
        Assert.DoesNotContain(parsed.Diagnostics, d => d.Code == "CE0135");
    }

    [Fact]
    public void CE0137_ClassParam_StoredAndForwarded_Errors()
    {
        // A class-typed parameter is a gift received — but storing it makes
        // this actor the owner, so forwarding it to a child as well puts two
        // owners on one mutable object. Same rule as the gifted local, with
        // the binding one step earlier.
        const string src = $$"""
            {{RegistryAndChild}}

            actor Relay
            {
                Registry mine = null;

                init(Registry g)
                {
                    mine = g;
                    var kid = spawn<Child>(g);
                }

                on Go => { }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.True(HasError(parsed, "CE0137"));

        var d = parsed.Diagnostics.First(x => x.Code == "CE0137");
        Assert.Contains("a parameter that is also stored in actor state", d.Message);
    }

    [Fact]
    public void CE0137_ClassParam_ForwardedThenStored_Errors()
    {
        // Order doesn't matter here either: the store is tracked body-wide,
        // exactly as for a gifted local.
        const string src = $$"""
            {{RegistryAndChild}}

            actor Relay
            {
                Registry mine = null;

                init(Registry g)
                {
                    var kid = spawn<Child>(g);
                    mine = g;
                }

                on Go => { }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.True(HasError(parsed, "CE0137"));
    }

    [Fact]
    public void CE0137_ClassParam_PureRelay_NotFlagged()
    {
        // The legitimate gift chain: the parameter passes straight through
        // and the relay keeps nothing. What the PARENT handed in was already
        // checked at the parent's own spawn, so the chain has one owner at
        // every hop.
        const string src = $$"""
            {{RegistryAndChild}}

            actor Relay
            {
                init(Registry g)
                {
                    var kid = spawn<Child>(g);
                }

                on Go => { }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.DoesNotContain(parsed.Diagnostics, d => d.Code == "CE0137");
    }

    [Fact]
    public void CE0137_ClassParam_StoredOnly_NotFlagged()
    {
        // Storing the received class is the ordinary ownership transfer every
        // Child in these tests performs — with no spawn there is no second
        // owner and nothing to flag.
        const string src = $$"""
            {{RegistryAndChild}}

            actor Keeper
            {
                Registry mine = null;

                init(Registry g)
                {
                    mine = g;
                }

                on Go => { }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.DoesNotContain(parsed.Diagnostics, d => d.Code == "CE0137");
    }

    [Fact]
    public void CE0137_MethodClassParam_StoredAndForwarded_Errors()
    {
        // The method-parameter spelling of the same share — parameters seed
        // the gift map for every parameterized actor body, not just init.
        const string src = $$"""
            {{RegistryAndChild}}

            actor Relay
            {
                Registry mine = null;

                private void Route(Registry g)
                {
                    mine = g;
                    var kid = spawn<Child>(g);
                }

                on Go => { }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.True(HasError(parsed, "CE0137"));
    }

    [Fact]
    public void CE0137_FiresInInit_NotJustHandlers()
    {
        // Same body coverage as CE0135: init is a natural place to spawn
        // children, and the rule follows the escape pass everywhere.
        const string src = $$"""
            {{RegistryAndChild}}

            actor Sender
            {
                Registry reg = new Registry();

                init()
                {
                    var kid = spawn<Child>(reg);
                }

                on Go => { }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.True(HasError(parsed, "CE0137"));
    }

}
