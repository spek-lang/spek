using Antlr4.Runtime;
using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities;
using OmniSharp.Extensions.LanguageServer.Protocol.Document;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using Spek.Compiler.Grammar;
using Spek.Compiler.Semantic;

namespace Spek.LanguageServer;

/// <summary>
/// Supplies <c>textDocument/signatureHelp</c> for message construction. When the
/// cursor is inside a <c>new SomeMessage( … )</c> argument list, this surfaces the
/// message's fields as the signature and highlights the parameter the cursor is
/// on.
///
/// Two design points make it work while you're mid-type — which is exactly when
/// you want it. First, it lexes the <em>live</em> source (the ANTLR lexer is
/// tolerant of the half-typed call the parser rejects) to locate the enclosing
/// call and count arguments. Second, it resolves the field list from the
/// document's last good parse
/// (<see cref="DocumentCache.Entry.LastGoodTree"/>), so an unclosed
/// <c>new Deposit(</c> — which leaves the live parse null — still gets help,
/// because the message <em>declaration</em> parsed fine earlier.
/// </summary>
internal sealed class SpekSignatureHelpHandler : SignatureHelpHandlerBase
{
    private readonly DocumentCache _cache;

    public SpekSignatureHelpHandler(DocumentCache cache) => _cache = cache;

    protected override SignatureHelpRegistrationOptions CreateRegistrationOptions(
        SignatureHelpCapability capability, ClientCapabilities clientCapabilities) =>
        new()
        {
            DocumentSelector    = TextDocumentSelector.ForLanguage("spek"),
            TriggerCharacters   = new Container<string>("(", ","),
            RetriggerCharacters = new Container<string>(","),
        };

    public override Task<SignatureHelp?> Handle(
        SignatureHelpParams request, CancellationToken cancellationToken)
    {
        var none = Task.FromResult<SignatureHelp?>(null);

        var entry = _cache.Get(request.TextDocument.Uri);
        var tree  = entry?.Tree ?? entry?.LastGoodTree;
        if (entry is null || tree is null) return none;

        // Lex the live source — tolerant of the unclosed call the parser chokes on.
        var lexer  = new SpekLexer(CharStreams.fromString(entry.Source));
        var stream = new CommonTokenStream(lexer);
        stream.Fill();
        var toks = stream.GetTokens()
            .Where(t => t.Channel == Lexer.DefaultTokenChannel && t.Type != TokenConstants.EOF)
            .ToList();

        int line = (int)request.Position.Line + 1;   // ANTLR line is 1-based
        int col  = (int)request.Position.Character;   // column is 0-based on both sides

        // Index of the last token that starts at or before the cursor.
        int cur = -1;
        for (int i = 0; i < toks.Count; i++)
        {
            var t = toks[i];
            if (t.Line < line || (t.Line == line && t.Column < col)) cur = i;
            else break;
        }
        if (cur < 0) return none;

        // Walk back to the '(' that opens the argument list the cursor sits in,
        // counting top-level commas to know which parameter is active.
        int depth = 0, activeParam = 0, openIdx = -1;
        for (int i = cur; i >= 0; i--)
        {
            switch (toks[i].Type)
            {
                case SpekLexer.RPAREN: depth++; break;
                case SpekLexer.LPAREN:
                    if (depth == 0) openIdx = i;
                    else depth--;
                    break;
                case SpekLexer.COMMA:
                    if (depth == 0) activeParam++;
                    break;
            }
            if (openIdx >= 0) break;
        }
        if (openIdx <= 0) return none;

        // The token before '(' must be an identifier preceded by `new` — i.e. a
        // `new Msg(` message construction. Anything else, we stay quiet.
        var nameTok = toks[openIdx - 1];
        if (nameTok.Type != SpekLexer.IDENTIFIER) return none;
        if (openIdx - 2 < 0 || toks[openIdx - 2].Type != SpekLexer.NEW) return none;

        var symbols = SymbolTable.Build(tree);
        var msg = symbols.Messages.FirstOrDefault(m => m.Name == nameTok.Text);
        if (msg is null) return none;

        var paramLabels = msg.Fields.Select(f => $"{f.Type} {f.Name}").ToArray();
        var signature = new SignatureInformation
        {
            Label      = $"{msg.Name}({string.Join(", ", paramLabels)})",
            Parameters = new Container<ParameterInformation>(
                paramLabels.Select(p => new ParameterInformation { Label = p })),
        };

        return Task.FromResult<SignatureHelp?>(new SignatureHelp
        {
            Signatures      = new Container<SignatureInformation>(signature),
            ActiveSignature = 0,
            ActiveParameter = paramLabels.Length == 0
                ? 0
                : Math.Min(activeParam, paramLabels.Length - 1),
        });
    }
}
