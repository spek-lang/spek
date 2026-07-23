using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities;
using OmniSharp.Extensions.LanguageServer.Protocol.Document;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using Spek.Compiler.AST;
using Range = OmniSharp.Extensions.LanguageServer.Protocol.Models.Range;

namespace Spek.LanguageServer;

/// <summary>
/// Reframes <c>textDocument/prepareCallHierarchy</c> as a Spek <em>message-flow</em>
/// graph. Actors don't call each other; they send messages, and a message is
/// handled by an <c>on Msg =&gt;</c> arm. So the hierarchy's nodes are handlers
/// (each shown as the message it receives, detailed by its actor), and the edges
/// are message sends:
/// <list type="bullet">
///   <item><b>incoming</b> — the handlers that send the message this handler
///         receives (every <c>new Msg(…)</c> that flows in via Tell/Ask/return);</item>
///   <item><b>outgoing</b> — the handlers that receive the messages this handler
///         sends.</item>
/// </list>
/// Prepare anchors on the message under the cursor and offers its handlers as the
/// roots to explore from.
/// </summary>
internal sealed class SpekCallHierarchyHandler : CallHierarchyHandlerBase
{
    private readonly DocumentCache _cache;

    public SpekCallHierarchyHandler(DocumentCache cache) => _cache = cache;

    protected override CallHierarchyRegistrationOptions CreateRegistrationOptions(
        CallHierarchyCapability capability, ClientCapabilities clientCapabilities) =>
        new() { DocumentSelector = TextDocumentSelector.ForLanguage("spek") };

    // ── prepare: the message under the cursor → its handler(s) as roots ──
    public override Task<Container<CallHierarchyItem>?> Handle(
        CallHierarchyPrepareParams request, CancellationToken cancellationToken)
    {
        var tree = Tree(request.TextDocument.Uri);
        if (tree is null) return NullAsync<CallHierarchyItem>();

        var (kind, name, _) = SymbolUnderCursor.Resolve(
            tree, (int)request.Position.Line + 1, (int)request.Position.Character + 1);
        if (kind != ReferenceFinder.Kind.Message || name is null)
            return NullAsync<CallHierarchyItem>();

        var items = HandlersFor(tree, name)
            .Select(h => ItemFor(h, request.TextDocument.Uri))
            .ToList();
        return items.Count == 0
            ? NullAsync<CallHierarchyItem>()
            : Task.FromResult<Container<CallHierarchyItem>?>(new Container<CallHierarchyItem>(items));
    }

    // ── incoming: who SENDS the message this handler receives ──
    public override Task<Container<CallHierarchyIncomingCall>?> Handle(
        CallHierarchyIncomingCallsParams request, CancellationToken cancellationToken)
    {
        var uri  = request.Item.Uri;
        var tree = Tree(uri);
        if (tree is null) return NullAsync<CallHierarchyIncomingCall>();

        var received = request.Item.Name;   // the message this handler receives
        var calls = new List<CallHierarchyIncomingCall>();

        foreach (var sender in AllHandlers(tree))
        {
            var sites = SendSites(sender.Handler, received).ToList();
            if (sites.Count == 0) continue;
            calls.Add(new CallHierarchyIncomingCall
            {
                From       = ItemFor(sender, uri),
                FromRanges = new Container<Range>(sites.Select(ToRange)),
            });
        }
        return Task.FromResult<Container<CallHierarchyIncomingCall>?>(
            new Container<CallHierarchyIncomingCall>(calls));
    }

    // ── outgoing: which handlers RECEIVE the messages this handler sends ──
    public override Task<Container<CallHierarchyOutgoingCall>?> Handle(
        CallHierarchyOutgoingCallsParams request, CancellationToken cancellationToken)
    {
        var uri  = request.Item.Uri;
        var tree = Tree(uri);
        if (tree is null) return NullAsync<CallHierarchyOutgoingCall>();

        var self = AllHandlers(tree).FirstOrDefault(
            h => h.Actor.Name == request.Item.Detail && h.Message == request.Item.Name);
        if (self is null)
            return Task.FromResult<Container<CallHierarchyOutgoingCall>?>(
                new Container<CallHierarchyOutgoingCall>());

        var calls = new List<CallHierarchyOutgoingCall>();
        foreach (var sent in AstWalk.EnumerateAll(self.Handler)
                     .OfType<NewExpr>()
                     .GroupBy(n => n.Type.Simple))
        {
            var ranges = new Container<Range>(sent.Select(n => ToRange(n.Span)));
            foreach (var receiver in HandlersFor(tree, sent.Key))
                calls.Add(new CallHierarchyOutgoingCall
                {
                    To         = ItemFor(receiver, uri),
                    FromRanges = ranges,
                });
        }
        return Task.FromResult<Container<CallHierarchyOutgoingCall>?>(
            new Container<CallHierarchyOutgoingCall>(calls));
    }

    // ── helpers ──

    private SpekFile? Tree(DocumentUri uri)
    {
        var entry = _cache.Get(uri);
        return entry?.Tree ?? entry?.LastGoodTree;
    }

    private static Task<Container<T>?> NullAsync<T>() => Task.FromResult<Container<T>?>(null);

    private sealed record HandlerSite(ActorDecl Actor, OnHandler Handler, string Message);

    private static IEnumerable<HandlerSite> AllHandlers(SpekFile tree) =>
        from a in tree.Declarations.OfType<ActorDecl>()
        from b in a.Members.OfType<BehaviorDecl>()
        from h in b.Handlers
        let msg = MessageOf(h.Pattern)
        where msg is not null
        select new HandlerSite(a, h, msg);

    private static IEnumerable<HandlerSite> HandlersFor(SpekFile tree, string message) =>
        AllHandlers(tree).Where(h => h.Message == message);

    // Every `new <message>(…)` this handler constructs — a Tell/Ask/return send.
    private static IEnumerable<SourceSpan> SendSites(OnHandler handler, string message) =>
        AstWalk.EnumerateAll(handler)
            .OfType<NewExpr>()
            .Where(n => n.Type.Simple == message)
            .Select(n => n.Span);

    private static string? MessageOf(MessagePattern p) => p switch
    {
        NamedBindPattern n => n.MessageType.Simple,
        NoBindPattern n    => n.MessageType.Simple,
        _                  => null,   // catch-all handler receives no single message
    };

    private static SourceSpan PatternSpan(MessagePattern p) => p switch
    {
        NamedBindPattern n => n.MessageType.Span,
        NoBindPattern n    => n.MessageType.Span,
        _                  => p.Span,
    };

    private static CallHierarchyItem ItemFor(HandlerSite s, DocumentUri uri) => new()
    {
        Name           = s.Message,
        Detail         = s.Actor.Name,
        Kind           = SymbolKind.Event,
        Uri            = uri,
        Range          = ToRange(s.Handler.Span),
        SelectionRange = ToRange(PatternSpan(s.Handler.Pattern)),
    };

    // SourceSpan columns are 1-based (ANTLR column + 1); LSP is 0-based.
    private static Range ToRange(SourceSpan s) => new(
        new Position(s.StartLine - 1, s.StartColumn - 1),
        new Position(s.EndLine   - 1, s.EndColumn   - 1));
}
