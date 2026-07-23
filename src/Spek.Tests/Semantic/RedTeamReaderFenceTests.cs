using Spek.Compiler.Parser;
using Spek.Compiler.Semantic;
using Xunit;

namespace Spek.Tests.Semantic;

/// <summary>
/// The permanent adversarial fence for the reader/writer discipline (CE0087):
/// every probe a red-team swept against <c>reader on</c> handlers, pinned so a
/// hole that was closed cannot silently reopen and a wall that stands cannot
/// silently fall. The red-team found CE0087 shared the shape-not-reachability
/// blindness the escape rules had (a mutation reached through an alias / paren /
/// ternary / lambda / sibling method slipped the check), plus a broader gap —
/// the rule never computed the actor's own transitive mutating-method set.
/// <para>
/// The five holes closed here:
/// <list type="number">
///   <item>HOLE #1 — a reader calls a sibling method that writes state (bare
///     <c>M()</c>, <c>self.M()</c>, a transitive chain, or a helper that mutates
///     a confined-class field).</item>
///   <item>HOLE #2 — a reader writes a region field through a local alias of the
///     <c>use</c> handle.</item>
///   <item>HOLE #3 — a reader mutates a BCL collection field in place
///     (<c>items.Add(...)</c>) by a known-mutating method name.</item>
///   <item>HOLE #4 — a reader calls a confined-class mutating method through a
///     non-direct receiver (paren / alias / ternary / typed alias).</item>
///   <item>HOLE #5 — a reader mutates inside a synchronously-invoked lambda.</item>
/// </list>
/// "Hole" = a probe that used to slip through and now must be caught (CE0087);
/// "wall" = a probe that was already blocked (or already correct) and must stay
/// that way. Where a probe is documented residue or out of scope, the note says
/// so and the pin records the deliberate boundary.
/// </para>
/// </summary>
public sealed class RedTeamReaderFenceTests
{
    private static CompilationResult Parse(string src) => SpekCompiler.Parse(src);

    private static bool HasError(string src, string code) =>
        Parse(src).Diagnostics.Any(d => d.Code == code && d.Severity == DiagnosticSeverity.Error);

    private static bool HasCE0087(string src) => HasError(src, "CE0087");

    private static void AssertHole(string src) =>
        Assert.True(HasCE0087(src),
            "Expected CE0087; got: " + string.Join(", ",
                Parse(src).Diagnostics.Select(d => d.Code)));

    private static void AssertCleanOfCE0087(string src) =>
        Assert.DoesNotContain(Parse(src).Diagnostics, d => d.Code == "CE0087");

    // ───────────────────────── HOLE #1 — sibling method writes state ─────────

    [Fact] // p05 — bare `DoMutate()` writing a field
    public void H1_BareSiblingMethodWritesField_Errors()
    {
        const string src = """
            namespace Probe;
            message Bump();
            public actor Worker
            {
                int count = 0;
                void DoMutate() { count = count + 1; }
                behavior Default
                {
                    reader on Bump b => { DoMutate(); }
                }
            }
            """;
        AssertHole(src);
    }

    [Fact] // p05b — `self.DoMutate()` (also CE0012, but CE0087 now fires too)
    public void H1_SelfSiblingMethodWritesField_Errors()
    {
        const string src = """
            namespace Probe;
            message Bump();
            public actor Worker
            {
                int count = 0;
                void DoMutate() { count = count + 1; }
                behavior Default
                {
                    reader on Bump b => { self.DoMutate(); }
                }
            }
            """;
        AssertHole(src);
    }

    [Fact] // p05c — transitive chain: Outer() -> Inner() -> write
    public void H1_TransitiveSiblingChain_Errors()
    {
        const string src = """
            namespace Probe;
            message Bump();
            public actor Worker
            {
                int count = 0;
                void Inner() { count = count + 1; }
                void Outer() { Inner(); Inner(); }
                behavior Default
                {
                    reader on Bump b => { Outer(); }
                }
            }
            """;
        AssertHole(src);
    }

    [Fact] // p05d — a helper that mutates a confined-class field (StateMutation can't see this — the enriched fixpoint must)
    public void H1_SiblingMethodMutatesConfinedClassField_Errors()
    {
        const string src = """
            namespace Probe;
            class Counter
            {
                int n = 0;
                public void Inc() { n = n + 1; }
                public int Value() { return n; }
            }
            message Bump();
            public actor Worker
            {
                Counter c = new Counter();
                void Helper() { c.Inc(); }
                behavior Default
                {
                    reader on Bump b => { Helper(); }
                }
            }
            """;
        AssertHole(src);
    }

    // ───────────────────────── HOLE #2 — region write via `use` alias ────────

    [Fact] // p04 — `var c = cache; c.lastPrice = 99;`
    public void H2_RegionWriteThroughAlias_Errors()
    {
        const string src = """
            namespace Probe;
            message Tick();
            shared MarketCache
            {
                long lastPrice = 0;
            }
            public actor Worker
            {
                use MarketCache cache;
                behavior Default
                {
                    reader on Tick t => { var c = cache; c.lastPrice = 99; }
                }
            }
            """;
        AssertHole(src);
    }

    [Fact] // p04b — the direct region write (wall — was already caught)
    public void H2_RegionWriteDirect_StillErrors()
    {
        const string src = """
            namespace Probe;
            message Tick();
            shared MarketCache
            {
                long lastPrice = 0;
            }
            public actor Worker
            {
                use MarketCache cache;
                behavior Default
                {
                    reader on Tick t => { cache.lastPrice = 99; }
                }
            }
            """;
        AssertHole(src);
    }

    // ───────────────────────── HOLE #3 — BCL collection mutation ─────────────

    [Fact] // p08d — `items.Add(b.v)` on a List field
    public void H3_ListAddInReader_Errors()
    {
        const string src = """
            namespace Probe;
            message Bump(int v);
            public actor Worker
            {
                System.Collections.Generic.List<int> items = new System.Collections.Generic.List<int>();
                behavior Default
                {
                    reader on Bump b => { items.Add(b.v); }
                }
            }
            """;
        AssertHole(src);
    }

    [Fact] // p08c — `scores[b.k] = 1` indexer set (wall — the AssignExpr arm already caught it)
    public void H3_DictionaryIndexerSet_StillErrors()
    {
        const string src = """
            namespace Probe;
            message Bump(int k);
            public actor Worker
            {
                System.Collections.Generic.Dictionary<int,int> scores = new System.Collections.Generic.Dictionary<int,int>();
                behavior Default
                {
                    reader on Bump b => { scores[b.k] = 1; }
                }
            }
            """;
        AssertHole(src);
    }

    [Fact] // A read-only BCL call (ContainsKey / indexer-get) must stay clean —
            // this is the exact shape of the flagship Readers.spek Board.
    public void H3_ReadOnlyCollectionCallsInReader_Clean()
    {
        const string src = """
            namespace Probe;
            message Lookup(int key);
            message LookupReply(int value);
            public actor Board
            {
                System.Collections.Generic.Dictionary<int, int> scores =
                    new System.Collections.Generic.Dictionary<int, int>();
                behavior Default
                {
                    reader on Lookup l =>
                    {
                        var v = 0;
                        if (scores.ContainsKey(l.key)) { v = scores[l.key]; }
                        return new LookupReply(v);
                    }
                }
            }
            """;
        AssertCleanOfCE0087(src);
    }

    // ───────────────────────── HOLE #4 — confined-class non-direct receiver ──

    [Fact] // p03 — `var h = c; h.Inc();`
    public void H4_ClassMutationThroughLocalAlias_Errors()
    {
        AssertHole(CounterProbe("{ var h = c; h.Inc(); }"));
    }

    [Fact] // p03b — `(m.flag ? a : b2).Inc();`
    public void H4_ClassMutationThroughTernary_Errors()
    {
        const string src = """
            namespace Probe;
            class Counter
            {
                int n = 0;
                public void Inc() { n = n + 1; }
                public int Value() { return n; }
            }
            message Bump(bool flag);
            public actor Worker
            {
                Counter a = new Counter();
                Counter b2 = new Counter();
                behavior Default
                {
                    reader on Bump m => { (m.flag ? a : b2).Inc(); }
                }
            }
            """;
        AssertHole(src);
    }

    [Fact] // p03c — `(c).Inc();`
    public void H4_ClassMutationThroughParens_Errors()
    {
        AssertHole(CounterProbe("{ (c).Inc(); }"));
    }

    [Fact] // p03d — `self.c.Inc();` (wall — direct self.field receiver already caught)
    public void H4_ClassMutationThroughSelfField_StillErrors()
    {
        AssertHole(CounterProbe("{ self.c.Inc(); }"));
    }

    [Fact] // p03e — `Counter h = c; h.Inc();` (typed alias)
    public void H4_ClassMutationThroughTypedAlias_Errors()
    {
        AssertHole(CounterProbe("{ Counter h = c; h.Inc(); }"));
    }

    [Fact] // A pure method on a confined class (Value()) through an alias stays clean —
            // the class's own MutatingMethods set is authoritative.
    public void H4_PureClassMethodThroughAlias_Clean()
    {
        const string src = """
            namespace Probe;
            class Counter
            {
                int n = 0;
                public void Inc() { n = n + 1; }
                public int Value() { return n; }
            }
            message Peek();
            message Reply(int v);
            public actor Worker
            {
                Counter c = new Counter();
                behavior Default
                {
                    reader on Peek p => { var h = c; return new Reply(h.Value()); }
                }
            }
            """;
        AssertCleanOfCE0087(src);
    }

    /// <summary>The p03 family shares a Counter class + single reader body.</summary>
    private static string CounterProbe(string readerBody) => $$"""
        namespace Probe;
        class Counter
        {
            int n = 0;
            public void Inc() { n = n + 1; }
            public int Value() { return n; }
        }
        message Bump();
        public actor Worker
        {
            Counter c = new Counter();
            behavior Default
            {
                reader on Bump b => {{readerBody}}
            }
        }
        """;

    // ───────────────────────── HOLE #5 — mutation inside a lambda ────────────

    [Fact] // p06 — `b.items.ForEach(x => count = count + 1)`
    public void H5_MutationInForEachLambda_Errors()
    {
        const string src = """
            namespace Probe;
            message Bump(System.Collections.Generic.List<int> items);
            public actor Worker
            {
                int count = 0;
                behavior Default
                {
                    reader on Bump b => { b.items.ForEach(x => count = count + 1); }
                }
            }
            """;
        AssertHole(src);
    }

    [Fact] // p06b — `Enumerable.Select(b.items, x => count = count + x)`
    public void H5_MutationInLinqLambda_Errors()
    {
        const string src = """
            namespace Probe;
            message Bump(System.Collections.Generic.List<int> items);
            public actor Worker
            {
                int count = 0;
                behavior Default
                {
                    reader on Bump b =>
                    {
                        var r = System.Linq.Enumerable.Select(b.items, x => count = count + x);
                        return;
                    }
                }
            }
            """;
        AssertHole(src);
    }

    [Fact] // p06c — a lambda writing a region field
    public void H5_RegionMutationInLambda_Errors()
    {
        const string src = """
            namespace Probe;
            message Bump(System.Collections.Generic.List<int> items);
            shared MarketCache { long lastPrice = 0; }
            public actor Worker
            {
                use MarketCache cache;
                behavior Default
                {
                    reader on Bump b => { b.items.ForEach(x => cache.lastPrice = x); }
                }
            }
            """;
        AssertHole(src);
    }

    [Fact] // p06d — lambda over a LOCAL collection still mutates a field
    public void H5_MutationInLambdaOverLocalCollection_Errors()
    {
        const string src = """
            namespace Probe;
            message Bump();
            public actor Worker
            {
                int count = 0;
                behavior Default
                {
                    reader on Bump b =>
                    {
                        var items = new System.Collections.Generic.List<int>();
                        items.ForEach(x => count = count + 1);
                    }
                }
            }
            """;
        AssertHole(src);
    }

    [Fact] // p06e — block-bodied lambda; the fully-leaking probe (was clean, now caught)
    public void H5_MutationInBlockLambda_Errors()
    {
        const string src = """
            namespace Probe;
            message Bump();
            public actor Worker
            {
                int count = 0;
                behavior Default
                {
                    reader on Bump b =>
                    {
                        var items = new System.Collections.Generic.List<int>();
                        var n = System.Linq.Enumerable.Count(items, x => { count = count + 1; return true; });
                    }
                }
            }
            """;
        AssertHole(src);
    }

    // ───────────────────────── Walls — direct forms & control flow ───────────

    [Fact] // p01 — the base direct field write
    public void Wall_DirectFieldWrite_Errors()
    {
        AssertHole(ScalarProbe("{ count = count + 1; }"));
    }

    [Fact] // p08a — compound assignment
    public void Wall_CompoundAssign_Errors()
    {
        AssertHole(ScalarProbe("{ count += 1; }"));
    }

    [Fact] // p07a — write inside an `if`
    public void Wall_WriteInIf_Errors()
    {
        AssertHole(ScalarProbe("{ if (true) { count = count + 1; } }"));
    }

    [Fact] // p07d — write inside a try/catch
    public void Wall_WriteInTryCatch_Errors()
    {
        AssertHole(ScalarProbe(
            "{ try { count = count + 1; } catch (System.Exception e) { count = count + 2; } }"));
    }

    [Fact] // p07c — write inside a foreach body
    public void Wall_WriteInForeach_Errors()
    {
        const string src = """
            namespace Probe;
            message Bump(System.Collections.Generic.List<int> items);
            public actor Worker
            {
                int count = 0;
                behavior Default
                {
                    reader on Bump b => { foreach (var x in b.items) { count = count + x; } }
                }
            }
            """;
        AssertHole(src);
    }

    [Fact] // p07b — write inside a switch section
    public void Wall_WriteInSwitch_Errors()
    {
        const string src = """
            namespace Probe;
            message Bump(int k);
            public actor Worker
            {
                int count = 0;
                behavior Default
                {
                    reader on Bump b =>
                    {
                        switch (b.k)
                        {
                            case 1: count = count + 1; break;
                            default: break;
                        }
                    }
                }
            }
            """;
        AssertHole(src);
    }

    [Fact] // p10 — copy to a local, mutate the local, store back to the field
    public void Wall_LocalRoundTripStoreBack_Errors()
    {
        const string src = """
            namespace Probe;
            message Bump();
            public actor Worker
            {
                int count = 0;
                behavior Default
                {
                    reader on Bump b =>
                    {
                        var local = count;
                        local = local + 1;
                        count = local;
                    }
                }
            }
            """;
        AssertHole(src);
    }

    [Fact] // p09b — `persist` is a writer-class op
    public void Wall_PersistInReader_Errors()
    {
        AssertHole(ScalarProbe("{ persist; }"));
    }

    [Fact] // p09c — `become` swaps behavior (writer-class)
    public void Wall_BecomeInReader_Errors()
    {
        const string src = """
            namespace Probe;
            message Bump();
            public actor Worker
            {
                behavior Default
                {
                    reader on Bump b => { become Other; }
                }
                behavior Other
                {
                    on Bump b => { }
                }
            }
            """;
        AssertHole(src);
    }

    // ───────────────────────── Walls — clean & blocked-elsewhere ─────────────

    [Fact] // p11 — pure local read; the critical false-positive guard
    public void Wall_PureLocalRead_Clean()
    {
        const string src = """
            namespace Probe;
            message Lookup(int k);
            message Reply(int v);
            public actor Worker
            {
                int count = 5;
                behavior Default
                {
                    reader on Lookup l =>
                    {
                        var v = 0;
                        v = v + count;
                        return new Reply(v);
                    }
                }
            }
            """;
        AssertCleanOfCE0087(src);
    }

    [Fact] // p02 — `var s = self; s.count = …` stays blocked by CE0012 (member
            // access on an actor ref). CE0087 need not fire; the mutation is
            // already unreachable. Pin: still an error, still not clean.
    public void Wall_SelfAliasBlockedByCE0012()
    {
        const string src = """
            namespace Probe;
            message Bump();
            public actor Worker
            {
                int count = 0;
                behavior Default
                {
                    reader on Bump b => { var s = self; s.count = s.count + 1; }
                }
            }
            """;
        Assert.True(HasError(src, "CE0012"));
        Assert.False(Parse(src).Success);
    }

    [Fact] // p08b — `count++` is not expressible (CE0001 parse-blocked); the
            // increment operators never reach the reader/writer check. Pin the
            // block so a future grammar that admits `++` is forced to revisit
            // CE0087.
    public void Wall_PostIncrementParseBlocked()
    {
        const string src = """
            namespace Probe;
            message Bump();
            public actor Worker
            {
                int count = 0;
                behavior Default
                {
                    reader on Bump b => { count++; }
                }
            }
            """;
        Assert.False(Parse(src).Success);
    }

    [Fact] // p08b2 — `++count` shares p08b's fate: parse-blocked (CE0001), so the
            // pre-increment operator likewise never reaches the reader/writer check.
    public void Wall_PreIncrementParseBlocked()
    {
        const string src = """
            namespace Probe;
            message Bump();
            public actor Worker
            {
                int count = 0;
                behavior Default
                {
                    reader on Bump b => { ++count; }
                }
            }
            """;
        Assert.False(Parse(src).Success);
    }

    [Fact] // p09a — spawning a child from a reader. OUT OF SCOPE per the task: a
            // low-priority design question (does a reader lock permit a spawn?),
            // deliberately left as-is. Pinned clean so closing it later is a
            // conscious choice, not an accident.
    public void Wall_SpawnFromReader_OutOfScope_Clean()
    {
        const string src = """
            namespace Probe;
            message Bump();
            public actor Child { behavior Default { on Bump b => { } } }
            public actor Worker
            {
                behavior Default
                {
                    reader on Bump b => { var kid = spawn<Child>(); }
                }
            }
            """;
        AssertCleanOfCE0087(src);
    }

    /// <summary>A scalar-field actor whose single reader body varies per probe.</summary>
    private static string ScalarProbe(string readerBody) => $$"""
        namespace Probe;
        message Bump();
        public actor Worker
        {
            int count = 0;
            behavior Default
            {
                reader on Bump b => {{readerBody}}
            }
        }
        """;
}
