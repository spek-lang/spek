using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using Spek.LanguageServer;
using Xunit;
using Range = OmniSharp.Extensions.LanguageServer.Protocol.Models.Range;

namespace Spek.Tests.LanguageServer;

/// <summary>
/// Coverage for <see cref="SpekInlayHintsHandler"/> — the <c>: ReplyType</c>
/// annotation it renders after an <c>.Ask(…)</c> whose reply type is inferred.
/// </summary>
public class InlayHintTests
{
    private static async Task<List<InlayHint>> HintsAsync(string src)
    {
        src = src.Replace("\r\n", "\n");
        var cache = new DocumentCache();
        var uri   = DocumentUri.From("file:///inlay.spek");
        cache.Update(uri, src);

        var handler = new SpekInlayHintsHandler(cache);
        var result  = await handler.Handle(new InlayHintParams
        {
            TextDocument = new TextDocumentIdentifier { Uri = uri },
            Range        = new Range(new Position(0, 0), new Position(10_000, 0)),
        }, CancellationToken.None);

        return (result ?? Enumerable.Empty<InlayHint>()).ToList();
    }

    // A Counter that replies to GetCount with Count, and a Reader that asks it.
    private const string CounterAndReader = """
        message GetCount();
        message Count(int n);

        actor Counter
        {
            int total = 5;
            on GetCount => return new Count(total);
        }

        actor Reader
        {
            Spek.ActorRef peer;
            on GetCount =>
            {
                var c = peer.Ask(new GetCount());
            }
        }
        """;

    [Fact]
    public async Task Ask_ShowsInferredReplyTypeAsync()
    {
        var src   = CounterAndReader.Replace("\r\n", "\n");
        var hints = await HintsAsync(src);

        var hint = Assert.Single(hints);
        Assert.Equal(": Count", hint.Label.String);
        Assert.Equal(InlayHintKind.Type, hint.Kind);

        // Placed just past the closing ')' of `peer.Ask(new GetCount())`.
        const string call = "peer.Ask(new GetCount())";
        int end = src.IndexOf(call, StringComparison.Ordinal) + call.Length;
        int line = 0, ch = 0;
        for (int i = 0; i < end; i++) { if (src[i] == '\n') { line++; ch = 0; } else ch++; }
        Assert.Equal(line, hint.Position.Line);
        Assert.Equal(ch,   hint.Position.Character);
    }

    [Fact]
    public async Task ExplicitReplyType_GetsNoHintAsync()
    {
        // `.Ask<Count>(…)` already names the type in source, so nothing to add.
        var hints = await HintsAsync(CounterAndReader.Replace(
            "peer.Ask(new GetCount())", "peer.Ask<Count>(new GetCount())"));
        Assert.Empty(hints);
    }

    [Fact]
    public async Task UninferableAsk_GetsNoHintAsync()
    {
        // No handler replies to Mystery, so there is no reply type to surface.
        var hints = await HintsAsync(CounterAndReader.Replace(
            "peer.Ask(new GetCount())", "peer.Ask(new Mystery())"));
        Assert.Empty(hints);
    }
}
