using Newtonsoft.Json.Linq;
using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities;
using OmniSharp.Extensions.LanguageServer.Protocol.Document;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using Spek.Compiler.AST;
using Range = OmniSharp.Extensions.LanguageServer.Protocol.Models.Range;

namespace Spek.LanguageServer;

/// <summary>
/// Supplies <c>textDocument/codeLens</c> for two features.
///
/// <para><b>Run-test lenses</b> on Spek's convention-based tests: a top-level
/// <c>class</c> or <c>module</c> whose name ends in <c>Tests</c> is a test
/// container, and each of its parameterless public methods is a single test.
/// The container gets a "Run all tests" lens; each test method gets a
/// "Run test" lens. Both carry the <c>spek.runTest</c> command with a
/// <c>FullyQualifiedName~…</c> filter and the file path, which the VS Code
/// extension turns into a <c>dotnet test</c> run in the enclosing project.</para>
///
/// <para><b>Message-flow lenses</b> — navigate by protocol, not symbol. Every
/// <c>message X(...)</c> declaration gets an "N senders · M handlers" lens
/// (senders = <c>new X(...)</c> construction sites, which is how Tell/ask
/// carry their payload; handlers = <c>on X</c> pattern arms), and every
/// <c>on X</c> handler arm gets a "→ N senders" lens. Zero counts render on
/// purpose — "0 senders · 1 handler" is a smell worth seeing. Both carry the
/// <c>editor.action.showReferences</c> command with LSP-shaped arguments
/// (uri string, anchor <c>{line, character}</c>, <c>Location[]</c>); the
/// extension rehydrates them into VS Code types and executes the built-in
/// peek. Counts are per-file, same scope as the reference finder.</para>
/// </summary>
internal sealed class SpekCodeLensHandler : CodeLensHandlerBase
{
    private readonly DocumentCache _cache;

    public SpekCodeLensHandler(DocumentCache cache) => _cache = cache;

    protected override CodeLensRegistrationOptions CreateRegistrationOptions(
        CodeLensCapability capability, ClientCapabilities clientCapabilities) =>
        new()
        {
            DocumentSelector = TextDocumentSelector.ForLanguage("spek"),
            ResolveProvider  = false,
        };

    public override Task<CodeLensContainer?> Handle(
        CodeLensParams request, CancellationToken cancellationToken)
    {
        var entry = _cache.Get(request.TextDocument.Uri);
        var tree  = entry?.Tree ?? entry?.LastGoodTree;
        if (tree is null) return Task.FromResult<CodeLensContainer?>(null);

        var ns     = tree.Namespace?.Name.ToString();
        var path   = request.TextDocument.Uri.GetFileSystemPath();
        var lenses = new List<CodeLens>();

        foreach (var (name, span, methods) in TestContainers(tree))
        {
            var containerFilter = ns is null ? name : $"{ns}.{name}";
            lenses.Add(Lens(span, "▶ Run all tests", containerFilter, path));
            foreach (var m in methods)
                lenses.Add(Lens(m.Span, "▶ Run test", $"{containerFilter}.{m.Name}", path));
        }

        AddMessageFlowLenses(tree, request.TextDocument.Uri, lenses);

        return Task.FromResult<CodeLensContainer?>(new CodeLensContainer(lenses));
    }

    // ─── message-flow lenses ─────────────────────────────────────────────

    private static void AddMessageFlowLenses(SpekFile tree, DocumentUri uri, List<CodeLens> lenses)
    {
        // One classified occurrence walk per distinct message name, shared by
        // the decl lens (senders + handlers) and every `on X` arm lens.
        var byName = new Dictionary<string, IReadOnlyList<ReferenceFinder.ClassifiedOccurrence>>();
        IReadOnlyList<ReferenceFinder.ClassifiedOccurrence> Occurrences(string name) =>
            byName.TryGetValue(name, out var cached)
                ? cached
                : byName[name] = ReferenceFinder.ClassifyMessageReferences(tree, name);

        foreach (var msg in tree.Declarations.OfType<MessageDecl>())
        {
            var occurrences = Occurrences(msg.Name);
            var senders  = Spans(occurrences, ReferenceFinder.MessageUsage.Send);
            var handlers = Spans(occurrences, ReferenceFinder.MessageUsage.Handle);
            lenses.Add(FlowLens(
                msg.Span,
                $"{Plural(senders.Count, "sender")} · {Plural(handlers.Count, "handler")}",
                uri,
                senders.Concat(handlers)));
        }

        foreach (var handler in AstWalk.EnumerateAll(tree).OfType<OnHandler>())
        {
            var msgName = handler.Pattern switch
            {
                NamedBindPattern nbp => nbp.MessageType.Simple,
                NoBindPattern nop    => nop.MessageType.Simple,
                _                    => null,   // catch-all / event arms name no message
            };
            if (msgName is null) continue;

            var senders = Spans(Occurrences(msgName), ReferenceFinder.MessageUsage.Send);
            lenses.Add(FlowLens(
                handler.Span, $"→ {Plural(senders.Count, "sender")}", uri, senders));
        }
    }

    private static List<SourceSpan> Spans(
        IReadOnlyList<ReferenceFinder.ClassifiedOccurrence> occurrences,
        ReferenceFinder.MessageUsage usage) =>
        occurrences.Where(o => o.Usage == usage).Select(o => o.Span).ToList();

    private static string Plural(int count, string noun) =>
        $"{count} {noun}{(count == 1 ? "" : "s")}";

    /// <summary>
    /// A lens carrying VS Code's built-in <c>editor.action.showReferences</c>
    /// command. Arguments stay LSP-shaped (uri string, position object,
    /// location array) — the extension middleware converts them to
    /// <c>vscode.Uri</c>/<c>Position</c>/<c>Location[]</c> before executing.
    /// The anchor position is the lens's own line so the peek opens in place.
    /// </summary>
    private static CodeLens FlowLens(
        SourceSpan span, string title, DocumentUri uri, IEnumerable<SourceSpan> targets)
    {
        var anchor = new JObject
        {
            ["line"]      = span.StartLine - 1,
            ["character"] = Math.Max(0, span.StartColumn - 1),
        };
        var locations = new JArray();
        foreach (var t in targets)
        {
            var range = SpekDefinitionHandler.SpanToRange(t);
            locations.Add(new JObject
            {
                ["uri"]   = uri.ToString(),
                ["range"] = new JObject
                {
                    ["start"] = new JObject
                    {
                        ["line"] = range.Start.Line, ["character"] = range.Start.Character,
                    },
                    ["end"] = new JObject
                    {
                        ["line"] = range.End.Line, ["character"] = range.End.Character,
                    },
                },
            });
        }

        return new CodeLens
        {
            Range   = new Range(new Position(span.StartLine - 1, 0), new Position(span.StartLine - 1, 0)),
            Command = new Command
            {
                Title     = title,
                Name      = "editor.action.showReferences",
                Arguments = new JArray { uri.ToString(), anchor, locations },
            },
        };
    }

    // ResolveProvider is false, so lenses arrive fully built; echo any back.
    public override Task<CodeLens> Handle(CodeLens request, CancellationToken cancellationToken) =>
        Task.FromResult(request);

    private static IEnumerable<(string Name, SourceSpan Span, IReadOnlyList<MethodDecl> Methods)>
        TestContainers(SpekFile tree)
    {
        foreach (var decl in tree.Declarations)
        {
            switch (decl)
            {
                case ClassDecl c when IsTestName(c.Name):
                    yield return (c.Name, c.Span, TestMethods(c.Methods));
                    break;
                case ModuleDecl m when IsTestName(m.Name):
                    yield return (m.Name, m.Span, TestMethods(m.Methods));
                    break;
            }
        }
    }

    private static bool IsTestName(string name) => name.EndsWith("Tests", StringComparison.Ordinal);

    private static IReadOnlyList<MethodDecl> TestMethods(IEnumerable<MethodDecl> methods) =>
        methods.Where(m => m.Visibility == Visibility.Public && m.Parameters.Count == 0).ToList();

    private static CodeLens Lens(SourceSpan span, string title, string filter, string path) => new()
    {
        Range   = new Range(new Position(span.StartLine - 1, 0), new Position(span.StartLine - 1, 0)),
        Command = new Command
        {
            Title     = title,
            Name      = "spek.runTest",
            Arguments = new JArray { filter, path },
        },
    };
}
