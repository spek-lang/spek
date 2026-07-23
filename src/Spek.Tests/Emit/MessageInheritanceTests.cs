using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Xunit;
using Xunit.Abstractions;

namespace Spek.Tests.Emit;

/// <summary>
/// Message/record inheritance — the polymorphic dispatch contract. An
/// <c>abstract message</c> is a family base a handler keys on (<c>on Base</c>
/// receives every variant); variants name it after the field list. Lowers to
/// abstract/derived C# records, so <c>case Base</c> in the generated dispatch
/// switch matches every variant by C# pattern matching. Base-carries-fields is
/// deferred (CE0125); the base must be an empty abstract message (CE0124 guards
/// the base itself).
/// </summary>
public sealed class MessageInheritanceTests(ITestOutputHelper output)
{
    private static string EmitCSharp(string source)
    {
        var parsed = SpekCompiler.Parse(source);
        Assert.True(parsed.Success,
            string.Join("\n", parsed.Diagnostics.Select(d => $"{d.Code} {d.Message}")));
        return new FileEmitter().Emit(parsed.Tree!);
    }

    private void AssertCompiles(string code, string label)
    {
        var (ok, summary, _) = RoslynCompileHelper.TryCompile(code, label);
        if (!ok) { output.WriteLine(code); output.WriteLine(summary); }
        Assert.True(ok, $"Emitted C# did not compile:\n{summary}");
    }

    private const string Family = """
        namespace X;

        abstract message ClusterEvent();
        message NodeUp(string Node)   : ClusterEvent;
        message NodeDown(string Node) : ClusterEvent;
        message Count(int N);

        actor Watcher
        {
            int seen = 0;
            init() { become Active; }
            behavior Active
            {
                on ClusterEvent e => { seen = seen + 1; }
                on Count c        => return new Count(seen);
            }
        }
        """;

    // ─── emit ───────────────────────────────────────────────────────────

    [Fact]
    public void AbstractMessage_EmitsAbstractRecord()
    {
        var code = EmitCSharp(Family);
        Assert.Contains("public abstract record ClusterEvent();", code);
    }

    [Fact]
    public void Variant_EmitsBaseClause()
    {
        var code = EmitCSharp(Family);
        Assert.Contains("public record NodeUp(string Node) : ClusterEvent();", code);
        Assert.Contains("public record NodeDown(string Node) : ClusterEvent();", code);
    }

    [Fact]
    public void OnBaseHandler_EmitsBaseCase_AndCompiles()
    {
        var code = EmitCSharp(Family);
        // The dispatch arm keys on the base; C# pattern matching routes every
        // variant here.
        Assert.Contains("case ClusterEvent e:", code);
        AssertCompiles(code, "MessageFamily");
    }

    // ─── CE0124: base list ──────────────────────────────────────────────

    [Fact]
    public void ExtendConcreteMessage_ReportsCE0124()
    {
        const string src = """
            namespace X;
            message A();
            message B() : A;
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.False(parsed.Success);
        var diag = Assert.Single(parsed.Diagnostics, d => d.Code == "CE0124");
        Assert.Contains("not", diag.Message);
        Assert.Contains("abstract", diag.Message);
    }

    [Fact]
    public void UnknownMessageBase_ReportsCE0124()
    {
        const string src = """
            namespace X;
            message B() : Nope;
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.False(parsed.Success);
        var diag = Assert.Single(parsed.Diagnostics, d => d.Code == "CE0124");
        Assert.Contains("Nope", diag.Message);
    }

    // ─── CE0125: abstract base must be empty ────────────────────────────

    [Fact]
    public void AbstractMessageWithFields_ReportsCE0125()
    {
        const string src = """
            namespace X;
            abstract message A(int x);
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.False(parsed.Success);
        var diag = Assert.Single(parsed.Diagnostics, d => d.Code == "CE0125");
        Assert.Contains("empty", diag.Message);
    }
}
