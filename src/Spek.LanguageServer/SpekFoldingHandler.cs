using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities;
using OmniSharp.Extensions.LanguageServer.Protocol.Document;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using Spek.Compiler.AST;

namespace Spek.LanguageServer;

/// <summary>
/// Supplies <c>textDocument/foldingRange</c> so editors can collapse the
/// brace-bodied constructs of a Spek file: each top-level declaration (actor,
/// module, class, channel, enum, shared region, program, multi-line message)
/// and, inside an actor, its behaviors, handlers, init, methods, and lifecycle
/// hooks. Single-line declarations produce no fold.
/// </summary>
internal sealed class SpekFoldingHandler : FoldingRangeHandlerBase
{
    private readonly DocumentCache _cache;

    public SpekFoldingHandler(DocumentCache cache) => _cache = cache;

    protected override FoldingRangeRegistrationOptions CreateRegistrationOptions(
        FoldingRangeCapability capability, ClientCapabilities clientCapabilities) =>
        new() { DocumentSelector = TextDocumentSelector.ForLanguage("spek") };

    public override Task<Container<FoldingRange>?> Handle(
        FoldingRangeRequestParam request, CancellationToken cancellationToken)
    {
        var entry = _cache.Get(request.TextDocument.Uri);
        if (entry?.Tree is null)
            return Task.FromResult<Container<FoldingRange>?>(null);

        var ranges = new List<FoldingRange>();
        foreach (var decl in entry.Tree.Declarations)
        {
            Add(decl.Span, ranges);
            if (decl is ActorDecl a)
            {
                foreach (var member in a.Members)
                {
                    Add(member.Span, ranges);
                    if (member is BehaviorDecl b)
                        foreach (var handler in b.Handlers)
                            Add(handler.Span, ranges);
                }
            }
        }

        return Task.FromResult<Container<FoldingRange>?>(new Container<FoldingRange>(ranges));
    }

    // Spek spans are 1-based; LSP folding ranges are 0-based. Only multi-line
    // spans are foldable.
    private static void Add(SourceSpan span, List<FoldingRange> ranges)
    {
        if (span.EndLine <= span.StartLine) return;
        ranges.Add(new FoldingRange
        {
            StartLine = span.StartLine - 1,
            EndLine   = span.EndLine - 1,
            Kind      = FoldingRangeKind.Region,
        });
    }
}
