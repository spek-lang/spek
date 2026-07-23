using BenchmarkDotNet.Attributes;
using MediatR;
using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using Spek.LanguageServer;

namespace Spek.Benchmarks.Toolchain;

/// <summary>
/// Editor-interaction latency: the per-keystroke and per-request costs the
/// language server pays. Every feature handler re-derives what it needs from
/// the DocumentCache entry on each request (SymbolTable.Build, AST position
/// resolution, a full re-lex for semantic tokens) — these benchmarks price
/// that design on large documents.
///
/// The handlers are internal to Spek.LanguageServer, and only Spek.Tests is
/// on the InternalsVisibleTo list, so GlobalSetup constructs them
/// reflectively (one Activator call, setup-only) and invokes them through
/// the public MediatR IRequestHandler interfaces they implement — the same
/// entry point the server's dispatcher uses, with zero per-op reflection.
/// </summary>
[MemoryDiagnoser]
public class LspBenchmarks
{
    private const int TypingDocLines = 1000;
    private const int LargeDocLines  = 5000;

    private readonly DocumentCache _cache = new();
    private readonly DocumentUri _typingUri = DocumentUri.From("file:///bench/Typing.spek");
    private readonly DocumentUri _largeUri  = DocumentUri.From("file:///bench/Large.spek");

    private string _typingSource = null!;
    private IRequestHandler<HoverParams, Hover?> _hover = null!;
    private IRequestHandler<DefinitionParams, LocationOrLocationLinks?> _definition = null!;
    private IRequestHandler<SemanticTokensParams, SemanticTokens?> _semanticTokens = null!;
    private HoverParams _hoverParams = null!;
    private DefinitionParams _definitionParams = null!;
    private SemanticTokensParams _semanticTokensParams = null!;

    [GlobalSetup]
    public async Task Setup()
    {
        _typingSource = SyntheticSpekSource.Generate(TypingDocLines);
        var largeSource = SyntheticSpekSource.Generate(LargeDocLines);

        // didOpen for the large document — feature requests hit the cached
        // entry, exactly as they do after the editor opens a file.
        _cache.Update(_largeUri, largeSource);
        if (_cache.Get(_largeUri)?.Tree is null)
            throw new InvalidOperationException("large synthetic document failed to parse");

        _hover          = CreateHandler<IRequestHandler<HoverParams, Hover?>>("SpekHoverHandler");
        _definition     = CreateHandler<IRequestHandler<DefinitionParams, LocationOrLocationLinks?>>("SpekDefinitionHandler");
        _semanticTokens = CreateHandler<IRequestHandler<SemanticTokensParams, SemanticTokens?>>("SpekSemanticTokensHandler");

        // Cursor inside the `Audit{N}` type of the LAST cell's
        // `new Audit{N}(...)` — a message reference near the end of the file,
        // so position resolution walks the whole declaration list.
        var position = PositionInside(largeSource, "new Audit", "new ".Length + 1);
        _hoverParams = new HoverParams
        {
            TextDocument = new TextDocumentIdentifier(_largeUri),
            Position     = position,
        };
        _definitionParams = new DefinitionParams
        {
            TextDocument = new TextDocumentIdentifier(_largeUri),
            Position     = position,
        };
        _semanticTokensParams = new SemanticTokensParams
        {
            TextDocument = new TextDocumentIdentifier(_largeUri),
        };

        // Guard: each request must resolve, or the benchmark would silently
        // measure the cheap not-found path.
        if (await _hover.Handle(_hoverParams, CancellationToken.None) is null)
            throw new InvalidOperationException("hover did not resolve the target token");
        if (await _definition.Handle(_definitionParams, CancellationToken.None) is null)
            throw new InvalidOperationException("definition did not resolve the target token");
        var tokens = await _semanticTokens.Handle(_semanticTokensParams, CancellationToken.None);
        if (tokens is null || tokens.Data.Length == 0)
            throw new InvalidOperationException("semantic tokens returned an empty document");
    }

    /// <summary>The debounced keystroke path: DocumentCache.Update on a
    /// 1,000-line document — a full ANTLR reparse plus single-file semantic
    /// analysis (SpekCompiler.Parse), the exact work SpekTextDocumentHandler
    /// triggers on every didChange (sync kind is Full, so the whole document
    /// re-parses regardless of edit size). Excludes only the
    /// publishDiagnostics mapping and transport, which are proportional to
    /// the diagnostic count — zero on this clean document. Rough short-run
    /// figure (Apple M5): ~11 ms and ~9.4 MB allocated per keystroke on
    /// 1,000 lines — the allocation is the number to watch, since typing at
    /// 5 keystrokes per second makes it a ~47 MB/s garbage stream.</summary>
    [Benchmark]
    public void DidChangeReparse() => _cache.Update(_typingUri, _typingSource);

    /// <summary>textDocument/hover against the cached 5,000-line document.
    /// No reparse — the cost is SymbolTable.Build over every declaration in
    /// the file plus the PositionResolver AST walk to the cursor, repeated
    /// per request because handlers derive symbols on demand rather than
    /// caching them on the entry. Rough short-run figure: ~107 µs / 153 KB
    /// per request even at 5,000 lines — the on-demand symbol build is
    /// cheap.</summary>
    [Benchmark]
    public async Task<Hover?> HoverOnLargeDoc() =>
        await _hover.Handle(_hoverParams, CancellationToken.None);

    /// <summary>textDocument/definition against the cached 5,000-line
    /// document — same SymbolTable.Build + PositionResolver shape as hover
    /// plus the declaration-span lookup, priced separately because editors
    /// fire it on every ctrl-click and peek. Rough short-run figure: ~59 µs
    /// / 153 KB per request.</summary>
    [Benchmark]
    public async Task<LocationOrLocationLinks?> DefinitionOnLargeDoc() =>
        await _definition.Handle(_definitionParams, CancellationToken.None);

    /// <summary>textDocument/semanticTokens/full on the 5,000-line document —
    /// editors request this constantly (on open, on edit, on visible-range
    /// change). The handler re-lexes the ENTIRE document with the ANTLR lexer
    /// and rebuilds the symbol table per request; the token-stream fill
    /// dominates. Rough short-run figure: ~6.8 ms and 7.3 MB per request on
    /// 5,000 lines — two orders of magnitude above hover, and it fires far
    /// more often.</summary>
    [Benchmark]
    public async Task<SemanticTokens?> SemanticTokensFullDoc() =>
        await _semanticTokens.Handle(_semanticTokensParams, CancellationToken.None);

    /// <summary>Constructs an internal Spek.LanguageServer handler by name
    /// and returns it typed as the public MediatR interface it implements.
    /// Reflection happens once here in setup; benchmark ops call through the
    /// interface directly.</summary>
    private T CreateHandler<T>(string typeName) where T : class
    {
        var type = typeof(DocumentCache).Assembly.GetType($"Spek.LanguageServer.{typeName}")
            ?? throw new InvalidOperationException($"handler type '{typeName}' not found");
        return (T)Activator.CreateInstance(type, _cache)!;
    }

    /// <summary>LSP (0-based) position <paramref name="offsetIntoMatch"/>
    /// characters past the LAST occurrence of <paramref name="anchor"/>.</summary>
    private static Position PositionInside(string source, string anchor, int offsetIntoMatch)
    {
        var index = source.LastIndexOf(anchor, StringComparison.Ordinal);
        if (index < 0) throw new InvalidOperationException($"anchor '{anchor}' not found");
        index += offsetIntoMatch;

        int line = 0, lineStart = 0;
        for (int i = 0; i < index; i++)
        {
            if (source[i] != '\n') continue;
            line++;
            lineStart = i + 1;
        }
        return new Position(line, index - lineStart);
    }
}
