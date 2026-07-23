using Spek.Compiler.AST;
using Spek.Compiler.Parser;
using Xunit;

namespace Spek.Tests.Parser;

/// <summary>
/// Parse-level coverage for Spek <c>channel</c> declarations and
/// <c>actor Foo : ChannelA, ChannelB</c> implementation syntax.
///
/// Channels are the Axum-influenced bidirectional protocol contract:
/// each channel lists <c>on MessageType;</c> inputs the implementing
/// actor accepts, and <c>emits MessageType;</c> (or <c>emits any;</c>)
/// unprompted events the actor may produce. No payload is declared
/// inside a channel — every type referenced is a pre-existing
/// <c>message</c>. Reply types stay on the handlers (Option D).
/// </summary>
public class ChannelDeclParseTests
{
    private static SpekFile ParseOrFail(string source)
    {
        var r = SpekCompiler.Parse(source);
        Assert.True(r.Success,
            string.Join("\n", r.Diagnostics.Select(d => $"  {d.Code} {d.Line}:{d.Column} {d.Message}")));
        return r.Tree!;
    }

    [Fact]
    public void MinimalChannel_ParsesWithInputsOnly()
    {
        const string src = """
            message Shutdown();
            message Reboot();

            channel ConsoleHost
            {
                on Shutdown;
                on Reboot;
            }
            """;
        var tree    = ParseOrFail(src);
        var channel = Assert.Single(tree.Declarations.OfType<ChannelDecl>());

        Assert.Equal("ConsoleHost", channel.Name);
        var inputs = channel.Members.OfType<ChannelInput>().ToList();
        Assert.Equal(2, inputs.Count);
        Assert.Equal("Shutdown", inputs[0].MessageType.ToString());
        Assert.Equal("Reboot",  inputs[1].MessageType.ToString());
        Assert.Empty(channel.Members.OfType<ChannelEmits>());
    }

    [Fact]
    public void Channel_WithEmits_ParsesTypedEvents()
    {
        const string src = """
            message Shutdown();
            message StatusChanged(string status);

            channel ConsoleHost
            {
                on Shutdown;
                emits StatusChanged;
            }
            """;
        var tree    = ParseOrFail(src);
        var channel = tree.Declarations.OfType<ChannelDecl>().Single();

        var emits = Assert.Single(channel.Members.OfType<ChannelEmits>());
        Assert.False(emits.IsAny);
        Assert.NotNull(emits.MessageType);
        Assert.Equal("StatusChanged", emits.MessageType!.ToString());
    }

    [Fact]
    public void Channel_WithEmitsAny_ParsesAsAdvisoryEscapeHatch()
    {
        // `emits any;` opts this channel's implementing actors out of
        // strict emits enforcement — they can Tell anything unprompted.
        const string src = """
            message Shutdown();

            channel ConsoleHost
            {
                on Shutdown;
                emits any;
            }
            """;
        var tree    = ParseOrFail(src);
        var channel = tree.Declarations.OfType<ChannelDecl>().Single();

        var emits = Assert.Single(channel.Members.OfType<ChannelEmits>());
        Assert.True(emits.IsAny);
        Assert.Null(emits.MessageType);
    }

    [Fact]
    public void Channel_PublicVisibility_ParsesWithModifier()
    {
        const string src = """
            message Ping();
            public channel Greeter { on Ping; }
            """;
        var tree    = ParseOrFail(src);
        var channel = tree.Declarations.OfType<ChannelDecl>().Single();

        Assert.Equal(Visibility.Public, channel.Visibility);
        Assert.Equal("Greeter", channel.Name);
    }

    [Fact]
    public void Channel_QualifiedMessageType_ParsesDottedName()
    {
        // Channels reference message types by name; the parser should
        // accept dotted/qualified names for messages declared in nested
        // namespaces.
        const string src = """
            namespace MyApp.Hosting;

            message Shutdown();

            channel Host
            {
                on MyApp.Hosting.Shutdown;
            }
            """;
        var tree    = ParseOrFail(src);
        var channel = tree.Declarations.OfType<ChannelDecl>().Single();
        var input   = Assert.Single(channel.Members.OfType<ChannelInput>());

        Assert.Equal("MyApp.Hosting.Shutdown", input.MessageType.ToString());
    }

    [Fact]
    public void ActorImplementsChannel_ParsesSingleChannel()
    {
        // `actor Foo : MyChannel { }` — single-name colon list, the
        // parser stashes this as BaseActor. Semantic analysis later
        // decides whether it's a base actor or a channel impl.
        const string src = """
            message Ping();
            channel Pingable { on Ping; }

            actor Echo : Pingable
            {
                behavior Idle { on Ping => { } }
            }
            """;
        var tree  = ParseOrFail(src);
        var actor = tree.Declarations.OfType<ActorDecl>().Single();

        Assert.NotNull(actor.BaseActor);
        Assert.Equal("Pingable", actor.BaseActor!.ToString());
        Assert.Empty(actor.ImplementedChannels);
    }

    [Fact]
    public void ActorImplementsChannels_ParsesMultipleChannels()
    {
        // `actor Foo : A, B, C` — first name stays in BaseActor (the
        // AstBuilder is naive; semantic analyzer reclassifies if needed).
        // Additional names land in ImplementedChannels.
        const string src = """
            message Ping();
            message Pong();
            message Shutdown();

            channel Pingable  { on Ping; }
            channel Pongable  { on Pong; }
            channel Shutdownable { on Shutdown; }

            actor Worker : Pingable, Pongable, Shutdownable
            {
                behavior Idle
                {
                    on Ping     => { }
                    on Pong     => { }
                    on Shutdown => { }
                }
            }
            """;
        var tree  = ParseOrFail(src);
        var actor = tree.Declarations.OfType<ActorDecl>().Single();

        Assert.NotNull(actor.BaseActor);
        Assert.Equal("Pingable", actor.BaseActor!.ToString());
        Assert.Equal(2, actor.ImplementedChannels.Count);
        Assert.Equal("Pongable",     actor.ImplementedChannels[0].ToString());
        Assert.Equal("Shutdownable", actor.ImplementedChannels[1].ToString());
    }

    [Fact]
    public void ChannelWithEmits_FullMix_ParsesCompleteShape()
    {
        // Exercise the full surface in one go — inputs + typed emits
        // + the `any` escape hatch can coexist within a single channel
        // if the user wants "mostly-strict, but trust me for these one-off
        // events."
        const string src = """
            message Shutdown();
            message Reboot();
            message StatusChanged(string status);
            message MetricEmitted(string name, int value);

            public channel ManagedHost
            {
                on Shutdown;
                on Reboot;
                emits StatusChanged;
                emits MetricEmitted;
            }
            """;
        var tree    = ParseOrFail(src);
        var channel = tree.Declarations.OfType<ChannelDecl>().Single();

        Assert.Equal(Visibility.Public, channel.Visibility);
        Assert.Equal(2, channel.Members.OfType<ChannelInput>().Count());
        Assert.Equal(2, channel.Members.OfType<ChannelEmits>().Count());
        Assert.All(channel.Members.OfType<ChannelEmits>(), e => Assert.False(e.IsAny));
    }
}
