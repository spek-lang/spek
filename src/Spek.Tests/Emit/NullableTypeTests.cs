using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Xunit;

namespace Spek.Tests.Emit;

/// <summary>
/// Coverage for the nullable-reference-type postfix syntax.
/// `T?` parses, the AST records nullability, the emitter passes the
/// `?` through to the generated C# so Roslyn's nullable analysis
/// (CS86xx warnings) does the actual null-tracking work.
/// </summary>
public sealed class NullableTypeTests
{
    private static string EmitCSharp(string source)
    {
        var parsed = SpekCompiler.Parse(source);
        Assert.True(parsed.Success,
            "Source must parse; diagnostics: " +
            string.Join("\n", parsed.Diagnostics.Select(d => $"{d.Code} {d.Line}:{d.Column} {d.Message}")));
        return new FileEmitter().Emit(parsed.Tree!);
    }

    [Fact]
    public void NullableField_EmitsQuestionMark()
    {
        // Actor fields emit with a leading underscore in the C#
        // backing field name, so the assertion checks for `string? _name`.
        const string src = """
            actor A
            {
                string? name = null;
                behavior Idle { on Tick => { } }
            }
            message Tick();
            """;
        var code = EmitCSharp(src);
        Assert.Contains("string? _name", code);
    }

    [Fact]
    public void NonNullableField_NoQuestionMark()
    {
        const string src = """
            actor A
            {
                string name = "x";
                behavior Idle { on Tick => { } }
            }
            message Tick();
            """;
        var code = EmitCSharp(src);
        Assert.Contains("string _name", code);
        Assert.DoesNotContain("string? _name", code);
    }

    [Fact]
    public void NullableMessageField_EmitsQuestionMarkOnRecord()
    {
        const string src = """
            message Reply(string? body);
            """;
        var code = EmitCSharp(src);
        Assert.Contains("public record Reply(string? body);", code);
    }

    [Fact]
    public void NullableInitParameter_EmitsQuestionMark()
    {
        const string src = """
            actor A
            {
                init(string? maybe) { }
                behavior Idle { on Tick => { } }
            }
            message Tick();
            """;
        var code = EmitCSharp(src);
        Assert.Contains("string? maybe", code);
    }

    [Fact]
    public void NullableGenericTypeArgument_EmitsCorrectly()
    {
        const string src = """
            actor A
            {
                List<string?> names = new List<string?>();
                behavior Idle { on Tick => { } }
            }
            message Tick();
            """;
        var code = EmitCSharp(src);
        Assert.Contains("List<string?> _names", code);
    }
}
