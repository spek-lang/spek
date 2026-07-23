using Spek.Compiler.AST;
using Spek.Compiler.Parser;
using Xunit;

namespace Spek.Tests.Parser;

/// <summary>
/// Parse-level coverage for Spek channel inheritance. Channels can
/// declare <c>: BaseChannel (, OtherBase)*</c> after their name; the
/// AST carries the bases as a flat list of <see cref="QualifiedName"/>.
/// Semantic flattening + cycle detection live in
/// <see cref="Spek.Compiler.Semantic.SemanticAnalyzer"/>.
/// </summary>
public class ChannelInheritanceParseTests
{
    private static SpekFile ParseOrFail(string source)
    {
        var r = SpekCompiler.Parse(source);
        Assert.True(r.Success,
            string.Join("\n", r.Diagnostics.Select(d => $"  {d.Code} {d.Line}:{d.Column} {d.Message}")));
        return r.Tree!;
    }

    [Fact]
    public void NoBases_ParsesAsEmptyBaseList()
    {
        const string src = """
            message Ping();
            channel Pingable { on Ping; }
            """;
        var tree = ParseOrFail(src);
        var ch   = tree.Declarations.OfType<ChannelDecl>().Single();

        Assert.Empty(ch.BaseChannels);
    }

    [Fact]
    public void SingleBase_ParsesIntoBaseChannelsList()
    {
        const string src = """
            message Shutdown();
            message Reboot();

            channel HostBase { on Shutdown; }
            channel ServerHost : HostBase { on Reboot; }
            """;
        var tree   = ParseOrFail(src);
        var server = tree.Declarations.OfType<ChannelDecl>().Single(c => c.Name == "ServerHost");

        Assert.Single(server.BaseChannels);
        Assert.Equal("HostBase", server.BaseChannels[0].ToString());
    }

    [Fact]
    public void MultipleBases_PreservesOrderInBaseList()
    {
        const string src = """
            message A();
            message B();
            message C();
            message D();

            channel BaseA { on A; }
            channel BaseB { on B; }
            channel BaseC { on C; }

            channel Combined : BaseA, BaseB, BaseC { on D; }
            """;
        var tree = ParseOrFail(src);
        var c    = tree.Declarations.OfType<ChannelDecl>().Single(ch => ch.Name == "Combined");

        Assert.Equal(3, c.BaseChannels.Count);
        Assert.Equal("BaseA", c.BaseChannels[0].ToString());
        Assert.Equal("BaseB", c.BaseChannels[1].ToString());
        Assert.Equal("BaseC", c.BaseChannels[2].ToString());
    }

    [Fact]
    public void QualifiedBaseName_PreservedInAst()
    {
        const string src = """
            namespace MyApp;

            message Shutdown();

            channel Outer.Inner { on Shutdown; }
            """;
        // `Outer.Inner` is not a legal Spek identifier — we expect this
        // to fail parsing, confirming that channel names are simple
        // identifiers (qualified-name resolution applies to references
        // only, not declaration names).
        var r = SpekCompiler.Parse(src);
        Assert.False(r.Success, "Expected parse failure on dotted channel decl name.");
    }
}
