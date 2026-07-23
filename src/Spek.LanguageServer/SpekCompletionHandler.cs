using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities;
using OmniSharp.Extensions.LanguageServer.Protocol.Document;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace Spek.LanguageServer;

/// <summary>
/// Completion — returns the static list of Spek keywords plus whatever
/// identifiers are in scope at the cursor position (actor fields, behavior
/// names, pattern bindings, and every declared <c>message</c> / <c>actor</c>
/// type in the current file).
///
/// Local <c>var</c> binding tracking inside statement sequences is deferred;
/// the current walker collects declarations but doesn't track positional
/// order so bindings introduced after the cursor would leak into scope.
/// Planned for a later iteration.
/// </summary>
internal sealed class SpekCompletionHandler : CompletionHandlerBase
{
    private readonly DocumentCache _cache;

    public SpekCompletionHandler(DocumentCache cache) => _cache = cache;

    public static readonly string[] Keywords =
    [
        // Declaration-level
        "actor", "abstract", "behavior", "channel", "class", "emits", "enum", "flags",
        "interface", "message", "module", "namespace", "shared", "using", "init",
        "program", "where",

        // Control flow
        "if", "else", "for", "foreach", "in", "while", "do", "break", "continue",
        "switch", "return", "become", "spawn", "persist", "new", "base",
        "try", "catch", "finally", "throw", "when",

        // Expressions / type operations
        "is", "as", "not", "and", "or",

        // Actor-context
        "self", "sender", "on", "reader", "writer", "event", "term", "use",

        // Lifecycle event names (used after `on`)
        "PreStart", "PostStop", "Restore",

        // Supervision
        "supervise", "passivate", "strategy", "after",
        "OneForOne", "AllForOne", "Failure",
        "Restart", "Stop", "Escalate", "Resume",
        "maxRetries", "withinTime",

        // Modifiers
        "public", "private", "protected", "internal",
        "override", "var", "any", "ref", "out",
        "transient", "deprecated", "retired", "interop",

        // Property accessors
        "get", "set",

        // Primitive literals / types
        "true", "false", "null", "void",
        "bool", "int", "long", "decimal", "string", "double",
        "ActorRef", "Snapshot",
    ];

    protected override CompletionRegistrationOptions CreateRegistrationOptions(
        CompletionCapability capability, ClientCapabilities clientCapabilities) =>
        new()
        {
            DocumentSelector = TextDocumentSelector.ForLanguage("spek"),
            ResolveProvider = false,
        };

    public override Task<CompletionItem> Handle(
        CompletionItem request, CancellationToken cancellationToken) =>
        Task.FromResult(request);

    public override Task<CompletionList> Handle(
        CompletionParams request, CancellationToken cancellationToken)
    {
        var items = new List<CompletionItem>(Keywords.Length + 16);

        foreach (var keyword in Keywords)
        {
            items.Add(new CompletionItem
            {
                Label = keyword,
                Kind = CompletionItemKind.Keyword,
                InsertText = keyword,
            });
        }

        // Layer on in-scope identifiers from the current file at the cursor
        // position: fields, behaviors, pattern bindings, declared message
        // and actor types.
        var entry = _cache.Get(request.TextDocument.Uri);
        if (entry?.Tree is not null)
        {
            var line   = (int)request.Position.Line + 1;
            var column = (int)request.Position.Character + 1;
            var scoped = CompletionContext.Collect(entry.Tree, line, column);

            foreach (var symbol in scoped)
            {
                items.Add(new CompletionItem
                {
                    Label = symbol.Name,
                    Kind = symbol.Kind,
                    Detail = symbol.Detail,
                    InsertText = symbol.Name,
                });
            }
        }

        return Task.FromResult(new CompletionList(items));
    }
}
