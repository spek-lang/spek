using MediatR;
using Microsoft.Extensions.Logging.Abstractions;
using Newtonsoft.Json.Linq;
using OmniSharp.Extensions.JsonRpc;
using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using OmniSharp.Extensions.LanguageServer.Protocol.Progress;
using OmniSharp.Extensions.LanguageServer.Protocol.Server;
using Spek.LanguageServer;
using Xunit;

namespace Spek.Tests.LanguageServer;

/// <summary>
/// Coverage for <see cref="SpekTextDocumentHandler"/> — the didOpen /
/// didChange / didClose lifecycle that keeps <see cref="DocumentCache"/>
/// current and publishes live diagnostics. The handler's only server
/// dependency is <c>ILanguageServerFacade.TextDocument.PublishDiagnostics</c>
/// (an extension over <c>SendNotification</c>), so a recording facade is
/// enough to observe every publish without booting a server.
/// </summary>
public class TextDocumentSyncTests
{
    private const string BrokenSource = """
        message Tick();
        actor A { behavior Idle { on Tick => { Thread.Sleep(100); } } }
        """;

    private const string FixedSource = """
        message Tick();
        actor A { behavior Idle { on Tick => { } } }
        """;

    private static (SpekTextDocumentHandler Handler, RecordingFacade Facade, DocumentCache Cache) Build()
    {
        var facade = new RecordingFacade();
        var cache  = new DocumentCache();
        var handler = new SpekTextDocumentHandler(
            facade, cache, NullLogger<SpekTextDocumentHandler>.Instance);
        return (handler, facade, cache);
    }

    private static Task OpenAsync(SpekTextDocumentHandler handler, DocumentUri uri, string text) =>
        handler.Handle(new DidOpenTextDocumentParams
        {
            TextDocument = new TextDocumentItem
            {
                Uri        = uri,
                LanguageId = "spek",
                Version    = 1,
                Text       = text,
            },
        }, CancellationToken.None);

    private static Task ChangeAsync(SpekTextDocumentHandler handler, DocumentUri uri, params string[] fullTexts) =>
        handler.Handle(new DidChangeTextDocumentParams
        {
            TextDocument   = new OptionalVersionedTextDocumentIdentifier { Uri = uri, Version = 2 },
            ContentChanges = new Container<TextDocumentContentChangeEvent>(
                fullTexts.Select(t => new TextDocumentContentChangeEvent { Text = t })),
        }, CancellationToken.None);

    [Fact]
    public async Task DidOpenWithError_PublishesDiagnostic_DidChangeFix_ClearsItAsync()
    {
        var (handler, facade, cache) = Build();
        var uri = DocumentUri.From("file:///A.spek");

        await OpenAsync(handler, uri, BrokenSource);

        var opened = Assert.Single(facade.Published);
        Assert.Equal(uri, opened.Uri);
        Assert.Contains(opened.Diagnostics, d => d.Code?.String == "CE0083");
        Assert.NotNull(cache.Get(uri));

        await ChangeAsync(handler, uri, FixedSource);

        Assert.Equal(2, facade.Published.Count);
        Assert.Equal(uri, facade.Published[1].Uri);
        Assert.Empty(facade.Published[1].Diagnostics);
        Assert.Equal(LspTestSupport.Normalize(FixedSource), cache.Get(uri)!.Source);
    }

    [Fact]
    public async Task DidChange_UsesTheLastContentChangeAsync()
    {
        // Full-sync mode: only the final full text of the batch counts.
        var (handler, facade, cache) = Build();
        var uri = DocumentUri.From("file:///A.spek");

        await OpenAsync(handler, uri, FixedSource);
        await ChangeAsync(handler, uri, BrokenSource, FixedSource);

        Assert.Equal(2, facade.Published.Count);
        Assert.Empty(facade.Published[1].Diagnostics);
        Assert.Equal(LspTestSupport.Normalize(FixedSource), cache.Get(uri)!.Source);
    }

    [Fact]
    public async Task DidChange_WithNoContentChanges_PublishesNothingAsync()
    {
        var (handler, facade, _) = Build();
        var uri = DocumentUri.From("file:///A.spek");
        await OpenAsync(handler, uri, FixedSource);

        await ChangeAsync(handler, uri /* no content changes */);

        Assert.Single(facade.Published);   // only the didOpen publish
    }

    [Fact]
    public async Task DidClose_EvictsTheDocument_AndClearsDiagnosticsAsync()
    {
        var (handler, facade, cache) = Build();
        var uri = DocumentUri.From("file:///A.spek");
        await OpenAsync(handler, uri, BrokenSource);
        Assert.NotEmpty(Assert.Single(facade.Published).Diagnostics);

        await handler.Handle(new DidCloseTextDocumentParams
        {
            TextDocument = new TextDocumentIdentifier { Uri = uri },
        }, CancellationToken.None);

        Assert.Null(cache.Get(uri));
        Assert.Equal(2, facade.Published.Count);
        Assert.Equal(uri, facade.Published[1].Uri);
        Assert.Empty(facade.Published[1].Diagnostics);
    }

    [Fact]
    public async Task DidSave_PublishesNothingAsync()
    {
        var (handler, facade, _) = Build();
        var uri = DocumentUri.From("file:///A.spek");
        await OpenAsync(handler, uri, FixedSource);

        await handler.Handle(new DidSaveTextDocumentParams
        {
            TextDocument = new TextDocumentIdentifier { Uri = uri },
        }, CancellationToken.None);

        Assert.Single(facade.Published);   // only the didOpen publish
    }

    [Fact]
    public async Task FileConventionHints_RideAlongWithCompilerDiagnosticsAsync()
    {
        // A single-type file named after the wrong type gets the
        // file-name-mismatch hint through the same publish pipeline.
        var (handler, facade, _) = Build();
        var uri = DocumentUri.From("file:///Wrong.spek");

        await OpenAsync(handler, uri, "class Something { }");

        var published = Assert.Single(facade.Published);
        var hint = Assert.Single(published.Diagnostics, d => d.Code?.String == "file-name-mismatch");
        Assert.Equal(DiagnosticSeverity.Hint, hint.Severity);
        Assert.Contains("Something.spek", hint.Message, StringComparison.Ordinal);
    }

    [Fact]
    public void TextDocumentAttributes_ReportTheSpekLanguage()
    {
        var (handler, _, _) = Build();
        var uri = DocumentUri.From("file:///A.spek");

        var attributes = handler.GetTextDocumentAttributes(uri);

        Assert.Equal(uri, attributes.Uri);
        Assert.Equal("spek", attributes.LanguageId);
    }

    /// <summary>
    /// Minimal <see cref="ILanguageServerFacade"/> that records every
    /// <see cref="PublishDiagnosticsParams"/> notification the handler sends
    /// (PublishDiagnostics is an extension method over
    /// <see cref="IResponseRouter.SendNotification(IRequest)"/>). Everything
    /// the handler never touches throws.
    /// </summary>
    private sealed class RecordingFacade : ILanguageServerFacade, ITextDocumentLanguageServer
    {
        public List<PublishDiagnosticsParams> Published { get; } = new();

        public ITextDocumentLanguageServer TextDocument => this;

        public void SendNotification(string method) { }

        public void SendNotification<T>(string method, T @params) { }

        public void SendNotification(IRequest request)
        {
            if (request is PublishDiagnosticsParams p) Published.Add(p);
        }

        // --- surface the handler never touches -----------------------------

        public INotebookDocumentLanguageServer NotebookDocument => throw new NotSupportedException();
        public IClientLanguageServer Client => throw new NotSupportedException();
        public IGeneralLanguageServer General => throw new NotSupportedException();
        public IWindowLanguageServer Window => throw new NotSupportedException();
        public IWorkspaceLanguageServer Workspace => throw new NotSupportedException();
        public IProgressManager ProgressManager => throw new NotSupportedException();
        public InitializeParams ClientSettings => throw new NotSupportedException();
        public InitializeResult ServerSettings => throw new NotSupportedException();

        public IResponseRouterReturns SendRequest<T>(string method, T @params) =>
            throw new NotSupportedException();

        public IResponseRouterReturns SendRequest(string method) =>
            throw new NotSupportedException();

        public Task<TResponse> SendRequest<TResponse>(IRequest<TResponse> request, CancellationToken cancellationToken) =>
            throw new NotSupportedException();

        public bool TryGetRequest(long id, out string method, out TaskCompletionSource<JToken> pendingTask)
        {
            method = null!;
            pendingTask = null!;
            return false;
        }

        public object? GetService(Type serviceType) => null;

        public IDisposable Register(Action<ILanguageServerRegistry> registryAction) =>
            throw new NotSupportedException();
    }
}
