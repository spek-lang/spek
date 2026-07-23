using System.Linq;
using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Xunit;

namespace Spek.Tests.Emit;

/// <summary>
/// <c>///</c> doc comments in <c>.spek</c> source are carried through to the
/// emitted C#, so converting a documented <c>.cs</c> file to <c>.spek</c> keeps
/// its public API documentation (the enabler for dogfooding library code in Spek).
/// </summary>
public sealed class DocCommentEmitTests
{
    private static string Emit(string source)
    {
        var parsed = SpekCompiler.Parse(source);
        Assert.True(parsed.Success,
            string.Join("\n", parsed.Diagnostics.Select(d => $"{d.Code} {d.Message}")));
        return new FileEmitter().Emit(parsed.Tree!);
    }

    [Fact]
    public void EnumAndMembers_CarryDocComments()
    {
        var code = Emit("""
            /// <summary>A colour.</summary>
            enum Color
            {
                /// <summary>The red one.</summary>
                Red,
                Green,
            }
            """);

        Assert.Contains("/// <summary>A colour.</summary>", code);
        Assert.Contains("/// <summary>The red one.</summary>", code);
    }

    [Fact]
    public void Message_CarriesDocComment()
    {
        var code = Emit("""
            /// <summary>Stops the host.</summary>
            message Shutdown();
            """);

        Assert.Contains("/// <summary>Stops the host.</summary>", code);
        Assert.Contains("public record Shutdown();", code);
    }

    [Fact]
    public void MultiLineDocBlock_IsCarriedInOrder()
    {
        var code = Emit("""
            /// <summary>
            /// A multi-line summary.
            /// </summary>
            message Ping();
            """);

        Assert.Contains("/// <summary>\n/// A multi-line summary.\n/// </summary>",
            code.Replace("\r\n", "\n"));
    }

    [Fact]
    public void NonAdjacentDocBlock_DoesNotAttach()
    {
        // The `///` block isn't adjacent to the decl (a plain `//` sits between),
        // so it must not become the declaration's doc comment.
        var code = Emit("""
            /// stale note about something else
            // regular comment
            message Ping();
            """);

        Assert.DoesNotContain("stale note", code);
    }

    [Fact]
    public void NoDocComment_EmitsNoDocLines()
    {
        Assert.DoesNotContain("///", Emit("message Ping();"));
    }
}
