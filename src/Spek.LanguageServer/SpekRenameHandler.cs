using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities;
using OmniSharp.Extensions.LanguageServer.Protocol.Document;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace Spek.LanguageServer;

/// <summary>
/// Resolves <c>textDocument/rename</c> requests. Replaces every textual
/// occurrence of a named declaration (its declaration site plus all
/// references within the file) with the new name and returns a
/// <see cref="WorkspaceEdit"/>.
///
/// The current scope is per-file rename. Cross-file rename — when a
/// message declared in <c>messages.spek</c> is referenced from
/// <c>actors.spek</c> — needs a workspace-aware document cache and
/// lifts to a future iteration.
/// </summary>
internal sealed class SpekRenameHandler : RenameHandlerBase
{
    private readonly DocumentCache _cache;

    public SpekRenameHandler(DocumentCache cache) => _cache = cache;

    protected override RenameRegistrationOptions CreateRegistrationOptions(
        RenameCapability capability, ClientCapabilities clientCapabilities) =>
        new()
        {
            DocumentSelector = TextDocumentSelector.ForLanguage("spek"),
            PrepareProvider  = false,    // PrepareRename support deferred
        };

    public override Task<WorkspaceEdit?> Handle(
        RenameParams request, CancellationToken cancellationToken)
    {
        var entry = _cache.Get(request.TextDocument.Uri);
        if (entry?.Tree is null) return Task.FromResult<WorkspaceEdit?>(null);

        var newName = request.NewName;
        if (string.IsNullOrWhiteSpace(newName) || !IsValidIdentifier(newName))
            return Task.FromResult<WorkspaceEdit?>(null);

        var line   = request.Position.Line + 1;
        var column = request.Position.Character + 1;

        var (kind, name, owner) = SymbolUnderCursor.Resolve(entry.Tree, line, column);
        if (kind is null) return Task.FromResult<WorkspaceEdit?>(null);

        var spans = ReferenceFinder.FindAllOccurrences(entry.Tree, kind.Value, name!, owner);
        if (spans.Count == 0) return Task.FromResult<WorkspaceEdit?>(null);

        var edits = spans.Select(s => new TextEdit
        {
            Range   = SpekDefinitionHandler.SpanToRange(s),
            NewText = newName,
        }).ToArray();

        var workspaceEdit = new WorkspaceEdit
        {
            Changes = new Dictionary<DocumentUri, IEnumerable<TextEdit>>
            {
                [request.TextDocument.Uri] = edits,
            },
        };

        return Task.FromResult<WorkspaceEdit?>(workspaceEdit);
    }

    /// <summary>
    /// Conservative identifier check: starts with letter or underscore,
    /// rest is letters / digits / underscores. Matches Spek lexer's
    /// <c>IDENTIFIER</c> rule. Does NOT check for keyword collisions —
    /// the user may pick a name that's a keyword and the next compile
    /// will surface that as a CE0001 syntax error, which is honest
    /// feedback rather than silent rename refusal.
    /// </summary>
    private static bool IsValidIdentifier(string s)
    {
        if (s.Length == 0) return false;
        if (!(char.IsLetter(s[0]) || s[0] == '_')) return false;
        for (var i = 1; i < s.Length; i++)
            if (!(char.IsLetterOrDigit(s[i]) || s[i] == '_')) return false;
        return true;
    }
}
