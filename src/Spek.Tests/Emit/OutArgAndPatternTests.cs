using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Xunit;
using Xunit.Abstractions;

namespace Spek.Tests.Emit;

/// <summary>
/// Guards the call-site out/ref argument forms and switch-expression type
/// patterns. Everything here lowers verbatim to C#; the tests exist because
/// these forms shipped without dedicated coverage and the gap was only
/// noticed by auditing rather than by a red test. The reader-handler rule is
/// the one semantic addition: an <c>out</c>/<c>ref</c> argument is a write
/// to its target, so aiming it at actor state from a <c>reader on</c>
/// handler is the same race CE0087 already flags for assignments.
/// </summary>
public sealed class OutArgAndPatternTests(ITestOutputHelper output)
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

    // ─── out-argument forms ─────────────────────────────────────────────

    private const string OutForms = """
        namespace X;

        message Parse(string text);
        message R(int n);

        actor P
        {
            behavior Default
            {
                on Parse p =>
                {
                    if (int.TryParse(p.text, out var a)) { return new R(a); }
                    int b = 0;
                    if (int.TryParse(p.text, out b)) { return new R(b); }
                    if (int.TryParse(p.text, out int c)) { return new R(c); }
                    if (int.TryParse(p.text, out _)) { return new R(1); }
                    return new R(0);
                }
            }
        }
        """;

    [Fact]
    public void OutArguments_AllFourForms_EmitVerbatim_AndCompile()
    {
        var code = EmitCSharp(OutForms);
        Assert.Contains("out var a", code);
        Assert.Contains("out b", code);
        Assert.Contains("out int c", code);
        Assert.Contains("out _", code);
        AssertCompiles(code, "OutForms");
    }

    [Fact]
    public void TypedOutDeclaration_QualifiedType_Emits()
    {
        const string src = """
            namespace X;
            message M(string s);
            message R(int n);
            actor A
            {
                behavior Default
                {
                    on M m =>
                    {
                        if (System.Int32.TryParse(m.s, out System.Int32 v)) { return new R(v); }
                        return new R(0);
                    }
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("out System.Int32 v", code);
        AssertCompiles(code, "TypedOutQualified");
    }

    // ─── switch-expression type patterns ────────────────────────────────

    private const string TypePatterns = """
        namespace X;

        abstract message Shape();
        message Circ(double r)    : Shape;
        message Sq(double side)   : Shape;
        message Classify(Shape shape);
        message R(int n);

        actor Q
        {
            behavior Default
            {
                on Classify m =>
                {
                    var label = m.shape switch
                    {
                        Circ c => c.r > 1.0 ? 1 : 2,
                        Sq s   => 3,
                        _      => 0,
                    };
                    return new R(label);
                }
            }
        }
        """;

    [Fact]
    public void SwitchTypePatterns_BindAndEmit_AndCompile()
    {
        var code = EmitCSharp(TypePatterns);
        Assert.Contains("Circ c =>", code);
        Assert.Contains("Sq s =>", code);
        AssertCompiles(code, "TypePatterns");
    }

    [Fact]
    public void SwitchTypePattern_WithoutBinding_Emits()
    {
        const string src = """
            namespace X;
            abstract message Shape();
            message Circ(double r) : Shape;
            message Classify(Shape shape);
            message R(int n);
            actor A
            {
                behavior Default
                {
                    on Classify m =>
                    {
                        var n = m.shape switch { Circ => 1, _ => 0 };
                        return new R(n);
                    }
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("Circ => 1", code);
        AssertCompiles(code, "TypePatternNoBinding");
    }

    // ─── CE0087: out/ref into actor state from a reader handler ─────────

    [Fact]
    public void ReaderHandler_OutIntoActorField_ReportsCE0087()
    {
        const string src = """
            namespace X;
            message M(string s);
            message R(int n);
            actor A
            {
                int cached = 0;
                behavior Default
                {
                    reader on M m =>
                    {
                        int.TryParse(m.s, out cached);
                        return new R(cached);
                    }
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.False(parsed.Success);
        var diag = Assert.Single(parsed.Diagnostics, d => d.Code == "CE0087");
        Assert.Contains("cached", diag.Message);
        Assert.Contains("out", diag.Message);
    }

    [Fact]
    public void ReaderHandler_OutIntoLocal_IsClean()
    {
        const string src = """
            namespace X;
            message M(string s);
            message R(int n);
            actor A
            {
                behavior Default
                {
                    reader on M m =>
                    {
                        if (int.TryParse(m.s, out var v)) { return new R(v); }
                        return new R(0);
                    }
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.True(parsed.Success,
            string.Join("\n", parsed.Diagnostics.Select(d => $"{d.Code} {d.Message}")));
    }

    [Fact]
    public void WriterHandler_OutIntoActorField_IsAllowed()
    {
        const string src = """
            namespace X;
            message M(string s);
            message R(int n);
            actor A
            {
                int cached = 0;
                behavior Default
                {
                    writer on M m =>
                    {
                        int.TryParse(m.s, out cached);
                        return new R(cached);
                    }
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.True(parsed.Success,
            string.Join("\n", parsed.Diagnostics.Select(d => $"{d.Code} {d.Message}")));
    }
}
