using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities;
using OmniSharp.Extensions.LanguageServer.Protocol.Document;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using Spek.Compiler.Format;
using Range = OmniSharp.Extensions.LanguageServer.Protocol.Models.Range;

namespace Spek.LanguageServer;

/// <summary>
/// Resolves <c>textDocument/formatting</c> requests by re-printing the whole
/// document through <see cref="SpekFormatter"/> (canonical indentation and
/// whitespace, every comment preserved). Returns a single full-document
/// replacement, or no edit when the source is already formatted. This is what
/// gives editors format-document and format-on-save.
/// </summary>
internal sealed class SpekFormattingHandler : DocumentFormattingHandlerBase
{
    private readonly DocumentCache _cache;

    public SpekFormattingHandler(DocumentCache cache) => _cache = cache;

    protected override DocumentFormattingRegistrationOptions CreateRegistrationOptions(
        DocumentFormattingCapability capability, ClientCapabilities clientCapabilities) =>
        new()
        {
            DocumentSelector = TextDocumentSelector.ForLanguage("spek"),
        };

    public override Task<TextEditContainer?> Handle(
        DocumentFormattingParams request, CancellationToken cancellationToken)
    {
        var entry = _cache.Get(request.TextDocument.Uri);
        if (entry is null) return Task.FromResult<TextEditContainer?>(null);

        var source    = entry.Source;
        var formatted = SpekFormatter.Format(source);
        if (formatted == source)
            return Task.FromResult<TextEditContainer?>(null);   // already canonical — no edits

        // One edit that replaces the whole buffer with the formatted text.
        var edit = new TextEdit
        {
            Range   = new Range(new Position(0, 0), EndOf(source)),
            NewText = formatted,
        };
        return Task.FromResult<TextEditContainer?>(new TextEditContainer(edit));
    }

    // The position just past the last character, so the replacement Range spans
    // the entire document regardless of its final line's length.
    private static Position EndOf(string source)
    {
        int line = 0;
        int lastLineStart = 0;
        for (int i = 0; i < source.Length; i++)
            if (source[i] == '\n') { line++; lastLineStart = i + 1; }
        return new Position(line, source.Length - lastLineStart);
    }
}
