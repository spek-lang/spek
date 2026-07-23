using System.Linq;
using Spek.Compiler.Parser;
using Spek.LanguageServer;
using Xunit;

namespace Spek.Tests.LanguageServer;

/// <summary>
/// Coverage for <see cref="FileConventions"/> — the C#-style one-type-per-file
/// and file-name-matches-type editor hints (suggestions, not compiler
/// diagnostics), and the exemptions that keep them from flagging files that
/// can't be split.
/// </summary>
public class FileConventionsTests
{
    private static string[] Codes(string source, string fileName)
    {
        var tree = SpekCompiler.Parse(source).Tree!;
        return FileConventions.Check(tree, fileName).Select(d => d.Code).ToArray();
    }

    [Fact]
    public void TwoActorsInOneFile_FlagsTheSecond()
    {
        var codes = Codes("actor A { } actor B { }", "A");
        Assert.Single(codes);
        Assert.Equal("one-type-per-file", codes[0]);
    }

    [Fact]
    public void SingleType_WrongFileName_IsFlagged()
    {
        Assert.Contains("file-name-mismatch", Codes("enum Color { Red, Green }", "Palette"));
    }

    [Fact]
    public void AreHints_NotCompilerWarnings()
    {
        var diags = FileConventions.Check(
            SpekCompiler.Parse("actor A { } actor B { }").Tree!, "Wrong").ToList();
        Assert.NotEmpty(diags);
        Assert.All(diags, d => Assert.Equal(
            Spek.Compiler.Semantic.DiagnosticSeverity.Hint, d.Severity));
        // No CE#### codes — these are tooling suggestions, not compiler diagnostics.
        Assert.All(diags, d => Assert.DoesNotContain("CE", d.Code));
    }

    [Fact]
    public void SingleType_MatchingFileName_IsClean()
    {
        Assert.Empty(Codes("enum Color { Red, Green }", "Color"));
    }

    [Fact]
    public void EnumWithMessages_IsClean()
    {
        // The lifecycle-messages shape: one enum + messages that reference it.
        // Single-file compilation (CE0010) requires the co-location, so neither
        // check should fire (message-only vocabulary is exempt).
        var src = "enum HostState { Up, Down } message StateChanged(HostState State); message Shutdown();";
        Assert.Empty(Codes(src, "LifecycleMessages"));
    }

    [Fact]
    public void NestedType_DoesNotCount()
    {
        // One top-level module with a nested module — a single top-level type.
        Assert.DoesNotContain("CE0120", Codes("module Outer { module Inner { } }", "Outer"));
    }

    [Fact]
    public void MessagesOnly_IsClean()
    {
        Assert.Empty(Codes("message Ping(); message Pong();", "Events"));
    }
}
