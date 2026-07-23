using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities;
using OmniSharp.Extensions.LanguageServer.Protocol.Document;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using Spek.Compiler.AST;

namespace Spek.LanguageServer;

/// <summary>
/// Resolves <c>textDocument/references</c> requests — "find all references"
/// in editor parlance. Walks the parsed AST and returns every span where
/// the symbol the cursor is on appears.
///
/// Scope is currently per-file. Cross-file references (when a message is
/// declared in one .spek file and used in another) require a
/// workspace-aware document cache; that lifts to a future iteration.
/// </summary>
internal sealed class SpekReferencesHandler : ReferencesHandlerBase
{
    private readonly DocumentCache _cache;

    public SpekReferencesHandler(DocumentCache cache) => _cache = cache;

    protected override ReferenceRegistrationOptions CreateRegistrationOptions(
        ReferenceCapability capability, ClientCapabilities clientCapabilities) =>
        new() { DocumentSelector = TextDocumentSelector.ForLanguage("spek") };

    public override Task<LocationContainer?> Handle(
        ReferenceParams request, CancellationToken cancellationToken)
    {
        var entry = _cache.Get(request.TextDocument.Uri);
        if (entry?.Tree is null) return Task.FromResult<LocationContainer?>(null);

        var line   = (int)request.Position.Line + 1;
        var column = (int)request.Position.Character + 1;

        var (kind, name, owner) = SymbolUnderCursor.Resolve(entry.Tree, line, column);
        if (kind is null) return Task.FromResult<LocationContainer?>(null);

        var spans = request.Context?.IncludeDeclaration == true
            ? ReferenceFinder.FindAllOccurrences(entry.Tree, kind.Value, name!, owner)
            : ReferenceFinder.FindReferences(entry.Tree, kind.Value, name!, owner);

        var locations = spans.Select(s => new Location
        {
            Uri   = request.TextDocument.Uri,
            Range = SpekDefinitionHandler.SpanToRange(s),
        }).ToArray();

        return Task.FromResult<LocationContainer?>(LocationContainer.From(locations));
    }
}
