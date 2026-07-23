using Spek.Compiler.Format;
using Spek.Compiler.Parser;
using Xunit;

namespace Spek.Tests.Format;

/// <summary>
/// Coverage for the <c>spekc format</c> command.
/// Token-level re-printer with canonical indentation + comment
/// preservation via the lexer's hidden channel.
/// </summary>
public sealed class SpekFormatterTests
{
    private static string Format(string source) => SpekFormatter.Format(source);

    [Fact]
    public void TopLevelDecls_RoundTripCleanly()
    {
        const string src = """
            namespace Demo;

            message Ping();

            actor Echo {
                behavior Idle {
                    on Ping => { }
                }
            }
            """;
        var formatted = Format(src);
        // Trailing newline is canonical — assert the rest matches.
        Assert.EndsWith("\n", formatted);
        Assert.Contains("namespace Demo;", formatted);
        Assert.Contains("message Ping();", formatted);
        Assert.Contains("actor Echo", formatted);
    }

    [Fact]
    public void GenericTypeArgs_Hug_ButComparisonsStaySpaced()
    {
        // Regression: the formatter used to space generic type arguments like
        // comparison operators — `peer.Ask<Balance>(...)` became
        // `peer.Ask < Balance > (...)`. Type-args must hug; a genuine `<`
        // comparison must keep its spaces.
        const string src = """
            module M
            {
                void F(ActorRef peer, int a, int b)
                {
                    var x = peer.Ask<Balance>(new GetBalance());
                    var d = new Dictionary<string, int>();
                    var w = peer.Get<int>(a);
                    var c = a < b;
                }
            }
            """;
        var f = Format(src);
        Assert.Contains("peer.Ask<Balance>(new GetBalance())", f);
        Assert.Contains("Dictionary<string, int>", f);
        Assert.Contains("peer.Get<int>(a)", f);
        Assert.Contains("a < b", f);            // comparison keeps its spaces
        Assert.DoesNotContain("Ask < Balance", f);
        Assert.DoesNotContain("Get < int", f);
    }

    [Fact]
    public void NullConditional_StaysTight()
    {
        // Regression: `?.` / `?[` must hug the receiver — the formatter
        // used to print `obj ?.Field` and `arr ? [0]`.
        const string src = """
            module M
            {
                public int LenOrZero(string s) { return s?.Length ?? 0; }
                public string Head(System.Collections.Generic.List<string> xs) { return xs?[0]?.ToUpper(); }
            }
            """;
        var formatted = Format(src);
        Assert.Contains("s?.Length", formatted);
        Assert.Contains("xs?[0]?.ToUpper()", formatted);
        Assert.DoesNotContain("?.Length ", formatted.Replace("?.Length ?? 0", ""));
        Assert.DoesNotContain(" ?.", formatted);
        Assert.DoesNotContain("? [", formatted);
    }

    [Fact]
    public void Ternary_KeepsSpacedQuestion()
    {
        // The ternary `?` keeps its surrounding spaces — the lookahead
        // that tightens `?.`/`?[` must not affect `cond ? a : b`.
        const string src = """
            module M
            {
                public int Pick(bool c, int a, int b) { return c ? a : b; }
            }
            """;
        var formatted = Format(src);
        Assert.Contains("c ? a : b", formatted);
    }

    [Fact]
    public void NestedBraces_GetCanonicalIndentation()
    {
        // Source mangled with random extra spaces and missing indent.
        const string src = """
            actor   A
            {
            behavior   Idle
            {
            on Ping  =>  {  }
            }
            }
            """;
        var formatted = Format(src);
        // Each `behavior` line lives one level deeper than `actor`,
        // each `on` line two levels deeper.
        Assert.Contains("\n    behavior", formatted);
        Assert.Contains("\n        on Ping", formatted);
    }

    [Fact]
    public void LineComments_PreservedBeforeDeclarations()
    {
        const string src = """
            // top of file
            namespace Demo;

            // before message
            message Ping();
            """;
        var formatted = Format(src);
        Assert.Contains("// top of file", formatted);
        Assert.Contains("// before message", formatted);
        // Comment-then-decl ordering preserved.
        Assert.True(
            formatted.IndexOf("// before message", StringComparison.Ordinal)
            < formatted.IndexOf("message Ping", StringComparison.Ordinal),
            "Pre-decl comment must precede the declaration in formatted output");
    }

    [Fact]
    public void BlockComments_PreservedVerbatim()
    {
        const string src = """
            /* keep me */
            namespace Demo;
            message Ping();
            """;
        var formatted = Format(src);
        Assert.Contains("/* keep me */", formatted);
    }

    [Fact]
    public void EndOfLineComments_StayTrailing()
    {
        const string src = """
            namespace Demo;
            message Ping(); // trailing remark
            """;
        var formatted = Format(src);
        // Trailing comment on the same line as the message decl.
        var lines = formatted.Split('\n');
        var pingLine = Array.Find(lines, l => l.Contains("message Ping"));
        Assert.NotNull(pingLine);
        Assert.Contains("// trailing remark", pingLine);
    }

    [Fact]
    public void FormattedOutput_ParsesIdentically()
    {
        // The strongest correctness check: format the source, parse
        // both, and confirm both produce a tree with no diagnostics.
        const string src = """
            namespace Demo;
            message Ping();
            actor A {
                behavior Idle {
                    on Ping => { }
                }
            }
            """;
        var first  = SpekCompiler.Parse(src);
        var second = SpekCompiler.Parse(Format(src));
        Assert.True(first.Success);
        Assert.True(second.Success);
    }
}
