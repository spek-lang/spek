using Spek.Compiler.AST;
using Spek.Compiler.Parser;
using Xunit;

namespace Spek.Tests.Parser;

/// <summary>
/// Parse-level coverage for Spek enum declarations. Enums are
/// permitted as <c>message</c> field types (CE0010 whitelists them) and
/// emit as plain C# enums in the generated code.
/// </summary>
public class EnumDeclParseTests
{
    private static SpekFile ParseOrFail(string source)
    {
        var r = SpekCompiler.Parse(source);
        Assert.True(r.Success,
            string.Join("\n", r.Diagnostics.Select(d => $"  {d.Code} {d.Line}:{d.Column} {d.Message}")));
        return r.Tree!;
    }

    [Fact]
    public void MinimalEnum_ParsesAllMembers()
    {
        const string src = """
            enum HostState
            {
                Running,
                Paused,
                Stopped,
            }
            """;
        var tree = ParseOrFail(src);
        var en   = Assert.Single(tree.Declarations.OfType<EnumDecl>());

        Assert.Equal("HostState", en.Name);
        Assert.Equal(3, en.Members.Count);
        Assert.Equal("Running", en.Members[0].Name);
        Assert.Equal("Paused",  en.Members[1].Name);
        Assert.Equal("Stopped", en.Members[2].Name);
    }

    [Fact]
    public void EnumWithoutTrailingComma_ParsesFine()
    {
        const string src = """
            enum Color { Red, Green, Blue }
            """;
        var tree = ParseOrFail(src);
        var en   = tree.Declarations.OfType<EnumDecl>().Single();

        Assert.Equal(3, en.Members.Count);
    }

    [Fact]
    public void PublicEnum_ParsesWithVisibilityModifier()
    {
        const string src = """
            public enum Severity { Low, Medium, High }
            """;
        var tree = ParseOrFail(src);
        var en   = tree.Declarations.OfType<EnumDecl>().Single();

        Assert.Equal(Visibility.Public, en.Visibility);
        Assert.Equal("Severity", en.Name);
    }

    [Fact]
    public void EnumAsMessageFieldType_NoCE0010Diagnostic()
    {
        // The whole point of Option C: a Spek enum can be a message field
        // type without tripping CE0010 (which rejects mutable types).
        const string src = """
            enum HostState { Running, Paused, Stopped }

            message StateChanged(HostState state);
            """;
        var r = SpekCompiler.Parse(src);
        Assert.True(r.Success,
            string.Join("\n", r.Diagnostics.Select(d => $"  {d.Code} {d.Line}:{d.Column} {d.Message}")));
        Assert.DoesNotContain(r.Diagnostics, d => d.Code == "CE0010");
    }
}
