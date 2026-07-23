using System.Collections.Concurrent;
using OmniSharp.Extensions.LanguageServer.Protocol;
using Spek.Compiler.AST;
using Spek.Compiler.Parser;
using Spek.Compiler.Semantic;

namespace Spek.LanguageServer;

/// <summary>
/// Thread-safe in-memory view of every open Spek document. The
/// <see cref="SpekTextDocumentHandler"/> writes on every change; LSP
/// feature handlers (definition, hover) read. Keeping a single cache
/// means we parse each edit once, not once per LSP request.
/// </summary>
public sealed class DocumentCache
{
    // LastGoodTree retains the most recent successfully-parsed tree so feature
    // handlers (signature help, etc.) still have the document's declarations to
    // work with while an edit-in-progress leaves the live parse (Tree) null.
    public sealed record Entry(
        string Source, SpekFile? Tree, IReadOnlyList<Diagnostic> Diagnostics,
        SpekFile? LastGoodTree = null);

    private readonly ConcurrentDictionary<DocumentUri, Entry> _documents = new();

    public void Update(DocumentUri uri, string source)
    {
        var result   = SpekCompiler.Parse(source);
        var lastGood = result.Tree ?? Get(uri)?.LastGoodTree;
        _documents[uri] = new Entry(source, result.Tree, result.Diagnostics, lastGood);
    }

    public void Remove(DocumentUri uri) => _documents.TryRemove(uri, out _);

    public Entry? Get(DocumentUri uri) => _documents.TryGetValue(uri, out var e) ? e : null;
}
