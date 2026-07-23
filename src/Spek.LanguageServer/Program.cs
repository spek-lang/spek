using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Logging;
using OmniSharp.Extensions.LanguageServer.Server;
using Spek.LanguageServer;

// Read from stdin, write to stdout — the LSP transport for most editors.
var server = await OmniSharp.Extensions.LanguageServer.Server.LanguageServer.From(options => options
    .WithInput(Console.OpenStandardInput())
    .WithOutput(Console.OpenStandardOutput())
    .ConfigureLogging(l =>
    {
        l.AddLanguageProtocolLogging();
        l.SetMinimumLevel(LogLevel.Warning);
    })
    .WithServices(services =>
    {
        // Shared document cache: the sync handler writes, feature handlers read.
        services.AddSingleton<DocumentCache>();
    })
    .WithHandler<SpekTextDocumentHandler>()
    .WithHandler<SpekDefinitionHandler>()
    .WithHandler<SpekHoverHandler>()
    .WithHandler<SpekDocumentSymbolHandler>()
    .WithHandler<SpekCompletionHandler>()
    .WithHandler<SpekReferencesHandler>()
    .WithHandler<SpekRenameHandler>()
    .WithHandler<SpekCodeActionHandler>()
    .WithHandler<SpekFormattingHandler>()
    .WithHandler<SpekFoldingHandler>()
    .WithHandler<SpekSemanticTokensHandler>()
    .WithHandler<SpekSignatureHelpHandler>()
    .WithHandler<SpekInlayHintsHandler>()
    .WithHandler<SpekCallHierarchyHandler>()
    .WithHandler<SpekCodeLensHandler>()
    .WithHandler<SpekEmitCSharpHandler>()
);

await server.WaitForExit;
