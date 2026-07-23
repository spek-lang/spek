using Antlr4.Runtime;
using OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities;
using OmniSharp.Extensions.LanguageServer.Protocol.Document;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using Spek.Compiler.Grammar;
using Spek.Compiler.Semantic;

namespace Spek.LanguageServer;

/// <summary>
/// Supplies <c>textDocument/semanticTokens</c> to refine editor highlighting
/// beyond what the TextMate grammar can do. The grammar only sees regex shapes
/// (every PascalCase name is "a type"); here we lex the document and colour each
/// identifier by its <em>resolved</em> kind — an actor is a class, a message is a
/// struct, a channel is an interface, an enum is an enum, a module is a
/// namespace — at the identifier's exact lexer position (no span guessing).
/// Identifiers that don't resolve to a declared type are left to the grammar.
/// </summary>
internal sealed class SpekSemanticTokensHandler : SemanticTokensHandlerBase
{
    private readonly DocumentCache _cache;

    public SpekSemanticTokensHandler(DocumentCache cache) => _cache = cache;

    private static readonly SemanticTokensLegend Legend = new()
    {
        TokenTypes     = SemanticTokenType.Defaults.ToArray(),
        TokenModifiers = SemanticTokenModifier.Defaults.ToArray(),
    };

    protected override SemanticTokensRegistrationOptions CreateRegistrationOptions(
        SemanticTokensCapability capability, ClientCapabilities clientCapabilities) =>
        new()
        {
            DocumentSelector = TextDocumentSelector.ForLanguage("spek"),
            Legend           = Legend,
            Full             = true,
            Range            = true,
        };

    protected override Task Tokenize(
        SemanticTokensBuilder builder, ITextDocumentIdentifierParams identifier,
        CancellationToken cancellationToken)
    {
        var entry = _cache.Get(identifier.TextDocument.Uri);
        if (entry?.Tree is null) return Task.CompletedTask;

        var symbols = SymbolTable.Build(entry.Tree);
        var actors   = symbols.Actors.Select(a => a.Name).ToHashSet(StringComparer.Ordinal);
        var messages = symbols.Messages.Select(m => m.Name).ToHashSet(StringComparer.Ordinal);
        var channels = symbols.Channels.Select(c => c.Name).ToHashSet(StringComparer.Ordinal);
        var enums    = symbols.Enums.Select(e => e.Name).ToHashSet(StringComparer.Ordinal);
        var modules  = symbols.Modules.Select(m => m.Name).ToHashSet(StringComparer.Ordinal);
        var classes  = symbols.Classes.Select(c => c.Name).ToHashSet(StringComparer.Ordinal);

        var lexer  = new SpekLexer(CharStreams.fromString(entry.Source));
        var stream = new CommonTokenStream(lexer);
        stream.Fill();

        // Lexer tokens come in source order, so they're already sorted by
        // (line, column) — the order the builder wants.
        foreach (var tok in stream.GetTokens())
        {
            if (tok.Type != SpekLexer.IDENTIFIER) continue;
            var text = tok.Text;
            if (text is null) continue;

            SemanticTokenType? kind =
                actors.Contains(text) || classes.Contains(text) ? SemanticTokenType.Class
                : messages.Contains(text)                       ? SemanticTokenType.Struct
                : channels.Contains(text)                       ? SemanticTokenType.Interface
                : enums.Contains(text)                          ? SemanticTokenType.Enum
                : modules.Contains(text)                        ? SemanticTokenType.Namespace
                : null;
            if (kind is null) continue;

            // ANTLR: Line is 1-based, Column (CharPositionInLine) is 0-based.
            // LSP wants 0-based line + 0-based character.
            builder.Push(tok.Line - 1, tok.Column, text.Length, kind, Array.Empty<SemanticTokenModifier>());
        }

        return Task.CompletedTask;
    }

    protected override Task<SemanticTokensDocument> GetSemanticTokensDocument(
        ITextDocumentIdentifierParams @params, CancellationToken cancellationToken) =>
        Task.FromResult(new SemanticTokensDocument(Legend));
}
