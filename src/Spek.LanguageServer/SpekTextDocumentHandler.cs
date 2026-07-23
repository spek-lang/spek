using MediatR;
using Microsoft.Extensions.Logging;
using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities;
using OmniSharp.Extensions.LanguageServer.Protocol.Document;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using OmniSharp.Extensions.LanguageServer.Protocol.Server;
using OmniSharp.Extensions.LanguageServer.Protocol.Server.Capabilities;

namespace Spek.LanguageServer;

/// <summary>
/// Tracks open Spek documents. Parses on open/change via
/// <see cref="DocumentCache"/> and publishes diagnostics to the client
/// so users see CE squiggles live as they type.
/// </summary>
internal sealed class SpekTextDocumentHandler : TextDocumentSyncHandlerBase
{
    private readonly ILanguageServerFacade _router;
    private readonly DocumentCache _cache;
    private readonly ILogger<SpekTextDocumentHandler> _logger;

    public SpekTextDocumentHandler(
        ILanguageServerFacade router,
        DocumentCache cache,
        ILogger<SpekTextDocumentHandler> logger)
    {
        _router = router;
        _cache  = cache;
        _logger = logger;
    }

    public override TextDocumentAttributes GetTextDocumentAttributes(DocumentUri uri) =>
        new(uri, "spek");

    protected override TextDocumentSyncRegistrationOptions CreateRegistrationOptions(
        TextSynchronizationCapability capability, ClientCapabilities clientCapabilities) =>
        new()
        {
            DocumentSelector = TextDocumentSelector.ForLanguage("spek"),
            Change = TextDocumentSyncKind.Full,
            Save = new SaveOptions { IncludeText = false },
        };

    public override Task<Unit> Handle(DidOpenTextDocumentParams request, CancellationToken cancellationToken)
    {
        RefreshDocument(request.TextDocument.Uri, request.TextDocument.Text);
        return Unit.Task;
    }

    public override Task<Unit> Handle(DidChangeTextDocumentParams request, CancellationToken cancellationToken)
    {
        var fullText = request.ContentChanges.LastOrDefault()?.Text;
        if (fullText is not null) RefreshDocument(request.TextDocument.Uri, fullText);
        return Unit.Task;
    }

    public override Task<Unit> Handle(DidSaveTextDocumentParams request, CancellationToken cancellationToken)
        => Unit.Task;

    public override Task<Unit> Handle(DidCloseTextDocumentParams request, CancellationToken cancellationToken)
    {
        _cache.Remove(request.TextDocument.Uri);
        _router.TextDocument.PublishDiagnostics(new PublishDiagnosticsParams
        {
            Uri = request.TextDocument.Uri,
            Diagnostics = new Container<Diagnostic>(),
        });
        return Unit.Task;
    }

    private void RefreshDocument(DocumentUri uri, string source)
    {
        try
        {
            _cache.Update(uri, source);
            var entry = _cache.Get(uri);
            if (entry is null) return;

            // Compiler diagnostics, plus the C#-style file-organization
            // conventions (one top-level type per file, file named after it).
            IEnumerable<Spek.Compiler.Semantic.Diagnostic> diags = entry.Diagnostics;
            var path = uri.GetFileSystemPath();
            if (entry.Tree is not null && !string.IsNullOrEmpty(path))
                diags = diags.Concat(FileConventions.Check(
                    entry.Tree, System.IO.Path.GetFileNameWithoutExtension(path)));

            var lspDiags = diags.Select(DiagnosticMapper.ToLsp).ToArray();
            _router.TextDocument.PublishDiagnostics(new PublishDiagnosticsParams
            {
                Uri = uri,
                Diagnostics = new Container<Diagnostic>(lspDiags),
            });
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Spek compilation threw for {Uri}", uri);
        }
    }
}
