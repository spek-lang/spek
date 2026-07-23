using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using Spek.LanguageServer;
using Xunit;
using Range = OmniSharp.Extensions.LanguageServer.Protocol.Models.Range;

namespace Spek.Tests.LanguageServer;

/// <summary>
/// Coverage for <see cref="SpekCallHierarchyHandler"/>'s message-flow graph:
/// prepare anchors on a message, incoming lists the handlers that send it, and
/// outgoing lists the handlers that receive what a handler sends.
/// </summary>
public class CallHierarchyTests
{
    // Server handles Ping (and replies Pong); Client handles Pong (and sends Ping).
    private const string PingPong = """
        message Ping();
        message Pong();

        actor Server
        {
            on Ping => return new Pong();
        }

        actor Client
        {
            Spek.ActorRef server;
            on Pong =>
            {
                server.Tell(new Ping());
            }
        }
        """;

    private static (DocumentCache Cache, DocumentUri Uri) Load(string src)
    {
        var cache = new DocumentCache();
        var uri   = DocumentUri.From("file:///ch.spek");
        cache.Update(uri, src.Replace("\r\n", "\n"));
        return (cache, uri);
    }

    private static CallHierarchyItem Item(DocumentUri uri, string message, string actor) => new()
    {
        Name           = message,
        Detail         = actor,
        Kind           = SymbolKind.Event,
        Uri            = uri,
        Range          = new Range(new Position(0, 0), new Position(0, 0)),
        SelectionRange = new Range(new Position(0, 0), new Position(0, 0)),
    };

    private static Position PosAt(string src, int offset)
    {
        int line = 0, ch = 0;
        for (int i = 0; i < offset; i++) { if (src[i] == '\n') { line++; ch = 0; } else ch++; }
        return new Position(line, ch);
    }

    [Fact]
    public async Task Prepare_OnMessage_ReturnsHandlerRoots()
    {
        var src = PingPong.Replace("\r\n", "\n");
        var (cache, uri) = Load(src);
        var handler = new SpekCallHierarchyHandler(cache);

        // Cursor on `Ping` inside `new Ping()` in the Client body.
        int off = src.IndexOf("new Ping()", StringComparison.Ordinal) + "new ".Length;
        var items = await handler.Handle(new CallHierarchyPrepareParams
        {
            TextDocument = new TextDocumentIdentifier { Uri = uri },
            Position     = PosAt(src, off),
        }, CancellationToken.None);

        var item = Assert.Single(items!);
        Assert.Equal("Ping", item.Name);       // the message
        Assert.Equal("Server", item.Detail);   // handled by Server
        Assert.Equal(SymbolKind.Event, item.Kind);
    }

    [Fact]
    public async Task Incoming_ListsSenders()
    {
        var (cache, uri) = Load(PingPong);
        var handler = new SpekCallHierarchyHandler(cache);

        // Who sends Ping (which Server handles)? Client's `on Pong` handler.
        var result = await handler.Handle(new CallHierarchyIncomingCallsParams
        {
            Item = Item(uri, "Ping", "Server"),
        }, CancellationToken.None);

        var call = Assert.Single(result!);
        Assert.Equal("Pong", call.From.Name);     // the sender is Client's on-Pong handler
        Assert.Equal("Client", call.From.Detail);
        Assert.NotEmpty(call.FromRanges);
    }

    [Fact]
    public async Task Outgoing_ListsReceivers()
    {
        var (cache, uri) = Load(PingPong);
        var handler = new SpekCallHierarchyHandler(cache);

        // Server's `on Ping` sends Pong → received by Client's `on Pong`.
        var result = await handler.Handle(new CallHierarchyOutgoingCallsParams
        {
            Item = Item(uri, "Ping", "Server"),
        }, CancellationToken.None);

        var call = Assert.Single(result!);
        Assert.Equal("Pong", call.To.Name);
        Assert.Equal("Client", call.To.Detail);
        Assert.NotEmpty(call.FromRanges);
    }

    [Fact]
    public async Task Prepare_OffAMessage_ReturnsNothing()
    {
        var src = PingPong.Replace("\r\n", "\n");
        var (cache, uri) = Load(src);
        var handler = new SpekCallHierarchyHandler(cache);

        // Cursor on the `server` field name — not a message.
        int off = src.IndexOf("server;", StringComparison.Ordinal);
        var items = await handler.Handle(new CallHierarchyPrepareParams
        {
            TextDocument = new TextDocumentIdentifier { Uri = uri },
            Position     = PosAt(src, off),
        }, CancellationToken.None);

        Assert.True(items is null || items.Count() == 0);
    }
}
