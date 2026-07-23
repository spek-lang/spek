using Spek.Compiler.Parser;
using Spek.Compiler.Semantic;
using Xunit;

namespace Spek.Tests.Semantic;

/// <summary>
/// Confinement safety for the mutable, single-owner <c>class</c>. A
/// class instance is race-free because it never escapes its owning actor
/// (messages/regions are blocked) and is only mutated where mutation is safe
/// (writer handlers are serial; a concurrent reader handler may not mutate it).
///
/// Rules exercised here:
/// <list type="bullet">
///   <item>CE0013 — duplicate class / field / method names.</item>
///   <item>CE0087 (extended) — a reader handler calling a *mutating* class
///     method on a confined field. Pure methods + writer handlers are fine.</item>
///   <item>CE0112 — a mutable class as a shared-region field (escape).</item>
///   <item>CE0010 — a mutable class in a message field (escape — already free
///     because a class isn't immutable).</item>
/// </list>
/// </summary>
public sealed class ClassConfinementTests
{
    private static IReadOnlyList<Diagnostic> Diagnose(string src) =>
        SpekCompiler.Parse(src).Diagnostics;

    private static void AssertHas(string src, string code) =>
        Assert.Contains(Diagnose(src), d => d.Code == code);

    private static void AssertClean(string src)
    {
        var parsed = SpekCompiler.Parse(src);
        Assert.True(parsed.Success,
            "Expected no errors; got: " +
            string.Join("\n", parsed.Diagnostics.Select(d => $"{d.Code} {d.Line}:{d.Column} {d.Message}")));
    }

    // ── CE0087 × properties: a method that writes a PROPERTY mutates state
    //    exactly like a field write (guarantee-completeness pass) ──

    [Fact]
    public void CE0087_ReaderCallsMethodWritingProperty_Reported()
    {
        const string src = """
            class Doc {
              public int Version { get; set; }
              public void Bump() { Version = Version + 1; }
            }
            message Q();
            actor A {
              Doc doc = new Doc();
              reader on Q q => { doc.Bump(); }
            }
            """;
        AssertHas(src, "CE0087");
    }

    [Fact]
    public void CE0087_ReaderCallsMethodReadingProperty_Clean()
    {
        const string src = """
            class Doc {
              public int Version { get; set; }
              public int Peek() { return Version; }
            }
            message Q();
            actor A {
              Doc doc = new Doc();
              reader on Q q => { var v = doc.Peek(); }
            }
            """;
        AssertClean(src);
    }

    // ── CE0013 duplicates ──
    [Fact]
    public void CE0013_DuplicateClass()    => AssertHas("class A { int n = 0; } class A { int m = 0; }", "CE0013");

    [Fact]
    public void CE0013_DuplicateField()    => AssertHas("class A { int n = 0; int n = 1; }", "CE0013");

    [Fact]
    public void CE0013_DuplicateMethod()   =>
        AssertHas("class A { public void M() { } public void M() { } }", "CE0013");

    // ── CE0087 extended to confined class methods ──

    private const string CounterClass = """
        class Counter
        {
            int n = 0;
            public void Inc() { n = n + 1; }     // mutating
            public int Value() { return n; }      // pure
        }
        """;

    [Fact]
    public void CE0087_ReaderCallsMutatingMethod_Errors()
    {
        var src = CounterClass + """

            message Bump();
            actor Worker
            {
                Counter c = new Counter();
                reader on Bump => { c.Inc(); }
            }
            """;
        AssertHas(src, "CE0087");
    }

    [Fact]
    public void CE0087_WriterCallsMutatingMethod_Ok()
    {
        var src = CounterClass + """

            message Bump();
            actor Worker
            {
                Counter c = new Counter();
                writer on Bump => { c.Inc(); }
            }
            """;
        Assert.DoesNotContain(Diagnose(src), d => d.Code == "CE0087");
    }

    [Fact]
    public void CE0087_ReaderCallsPureMethod_Ok()
    {
        var src = CounterClass + """

            message Peek();
            actor Worker
            {
                Counter c = new Counter();
                reader on Peek => { return c.Value(); }
            }
            """;
        Assert.DoesNotContain(Diagnose(src), d => d.Code == "CE0087");
    }

    [Fact]
    public void CE0087_TransitiveMutatingMethod_Errors()
    {
        // AddTwice doesn't write a field directly — it calls Add (which does),
        // so the fixpoint classifies AddTwice as mutating too.
        const string src = """
            class Acc
            {
                int total = 0;
                public void Add(int x)      { total = total + x; }
                public void AddTwice(int x) { self.Add(x); self.Add(x); }
                public int Get()            { return total; }
            }

            message Go();
            actor Worker
            {
                Acc a = new Acc();
                reader on Go => { a.AddTwice(2); }
            }
            """;
        AssertHas(src, "CE0087");
    }

    // ── CE0112 region escape ──
    [Fact]
    public void CE0112_MutableClassAsRegionField_Errors()
    {
        var src = CounterClass + """

            shared Cache
            {
                Counter c = new Counter();
            }
            """;
        AssertHas(src, "CE0112");
    }

    // ── CE0010 message escape (already free — a class isn't immutable) ──
    [Fact]
    public void CE0010_MutableClassInMessageField_Errors()
    {
        var src = CounterClass + """

            message Submit(Counter c);
            """;
        AssertHas(src, "CE0010");
    }

    // ── A confined class used correctly analyzes clean ──
    [Fact]
    public void ConfinedClass_UsedFromWriter_IsClean()
    {
        var src = CounterClass + """

            message Bump();
            message Read();
            actor Worker
            {
                Counter c = new Counter();
                writer on Bump => { c.Inc(); }
                reader on Read => { return c.Value(); }
            }
            """;
        AssertClean(src);
    }
}
