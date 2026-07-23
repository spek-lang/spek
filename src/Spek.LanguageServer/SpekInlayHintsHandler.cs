using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities;
using OmniSharp.Extensions.LanguageServer.Protocol.Document;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using Spek.Compiler.AST;
using Spek.Compiler.Semantic;
using Range = OmniSharp.Extensions.LanguageServer.Protocol.Models.Range;

namespace Spek.LanguageServer;

/// <summary>
/// Supplies <c>textDocument/inlayHint</c>. Spek's headline convenience — a
/// handler replies by <c>return</c>ing a message, and a caller reads that reply
/// straight out of <c>target.Ask(new Query())</c> — means the reply type is never
/// written down. This surfaces it: after each <c>.Ask(…)</c> whose reply type the
/// compiler infers, it renders a subtle <c>: ReplyType</c> annotation, the way a
/// type-hint inlay reveals an inferred <c>var</c>. Asks written with an explicit
/// <c>.Ask&lt;T&gt;(…)</c> already show their type, so they get no hint.
/// </summary>
internal sealed class SpekInlayHintsHandler : InlayHintsHandlerBase
{
    private readonly DocumentCache _cache;

    public SpekInlayHintsHandler(DocumentCache cache) => _cache = cache;

    protected override InlayHintRegistrationOptions CreateRegistrationOptions(
        InlayHintClientCapabilities capability, ClientCapabilities clientCapabilities) =>
        new()
        {
            DocumentSelector = TextDocumentSelector.ForLanguage("spek"),
            ResolveProvider  = false,
        };

    public override Task<InlayHintContainer?> Handle(
        InlayHintParams request, CancellationToken cancellationToken)
    {
        var entry = _cache.Get(request.TextDocument.Uri);
        var tree  = entry?.Tree ?? entry?.LastGoodTree;
        if (tree is null) return Task.FromResult<InlayHintContainer?>(null);

        var symbols = SymbolTable.Build(tree);
        var hints   = new List<InlayHint>();

        foreach (var node in AstWalk.EnumerateAll(tree))
        {
            if (node is not AskExpr ask) continue;
            if (ask.ExplicitReplyType is not null) continue;   // reply type already written
            if (ask.Message is not NewExpr msg) continue;      // need the message name to infer
            var reply = symbols.InferReplyType(msg.Type);
            if (reply is null) continue;

            // Span() sets EndColumn = stop-token(`)`).Column + 1; because ANTLR
            // columns are 0-based, that value used as a 0-based LSP character lands
            // just past the closing `)`, exactly where the annotation should read.
            var pos = new Position(ask.Span.EndLine - 1, ask.Span.EndColumn);
            if (!InRange(pos, request.Range)) continue;

            hints.Add(new InlayHint
            {
                Position    = pos,
                Label       = $": {reply}",
                Kind        = InlayHintKind.Type,
                PaddingLeft = false,
                Tooltip     = new StringOrMarkupContent("Inferred reply type of this .Ask(…)"),
            });
        }

        return Task.FromResult<InlayHintContainer?>(new InlayHintContainer(hints));
    }

    public override Task<InlayHint> Handle(InlayHint request, CancellationToken cancellationToken) =>
        Task.FromResult(request);

    private static bool InRange(Position p, Range r) =>
        (p.Line > r.Start.Line || (p.Line == r.Start.Line && p.Character >= r.Start.Character)) &&
        (p.Line < r.End.Line   || (p.Line == r.End.Line   && p.Character <= r.End.Character));
}
