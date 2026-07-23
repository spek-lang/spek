using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities;
using OmniSharp.Extensions.LanguageServer.Protocol.Document;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using Spek.Compiler.AST;
using Spek.Compiler.Emit;

namespace Spek.LanguageServer;

/// <summary>
/// Populates the editor's outline view and symbol picker (Ctrl+O / Ctrl+Shift+O
/// in VS Code) with the tree of top-level and per-actor declarations in the
/// current Spek document.
/// </summary>
internal sealed class SpekDocumentSymbolHandler : DocumentSymbolHandlerBase
{
    private readonly DocumentCache _cache;

    public SpekDocumentSymbolHandler(DocumentCache cache) => _cache = cache;

    protected override DocumentSymbolRegistrationOptions CreateRegistrationOptions(
        DocumentSymbolCapability capability, ClientCapabilities clientCapabilities) =>
        new() { DocumentSelector = TextDocumentSelector.ForLanguage("spek") };

    public override Task<SymbolInformationOrDocumentSymbolContainer?> Handle(
        DocumentSymbolParams request, CancellationToken cancellationToken)
    {
        var entry = _cache.Get(request.TextDocument.Uri);
        if (entry?.Tree is null)
            return Task.FromResult<SymbolInformationOrDocumentSymbolContainer?>(null);

        var symbols = BuildSymbols(entry.Tree).ToArray();
        var wrapped = symbols.Select(s => new SymbolInformationOrDocumentSymbol(s)).ToArray();
        return Task.FromResult<SymbolInformationOrDocumentSymbolContainer?>(
            new SymbolInformationOrDocumentSymbolContainer(wrapped));
    }

    private static IEnumerable<DocumentSymbol> BuildSymbols(SpekFile file)
    {
        foreach (var decl in file.Declarations)
        {
            switch (decl)
            {
                case MessageDecl m:
                    yield return MakeSymbol(
                        name: m.Name,
                        detail: FormatMessageDetail(m),
                        kind: SymbolKind.Struct,
                        span: m.Span,
                        selection: m.Span);
                    break;

                case ActorDecl a:
                    yield return MakeSymbol(
                        name: a.Name,
                        detail: FormatActorDetail(a),
                        kind: SymbolKind.Class,
                        span: a.Span,
                        selection: a.Span,
                        children: BuildActorChildren(a).ToArray());
                    break;

                case ProgramDecl p:
                    yield return MakeSymbol(
                        name: p.Name,
                        detail: "program entry point",
                        kind: SymbolKind.Function,
                        span: p.Span,
                        selection: p.Span);
                    break;
            }
        }
    }

    private static IEnumerable<DocumentSymbol> BuildActorChildren(ActorDecl a)
    {
        foreach (var member in a.Members)
        {
            switch (member)
            {
                case FieldDecl f:
                    yield return MakeSymbol(
                        name: f.Name,
                        detail: f.Type.ToString(),
                        kind: SymbolKind.Field,
                        span: f.Span,
                        selection: f.Span);
                    break;

                case BehaviorDecl b:
                    yield return MakeSymbol(
                        name: b.Name,
                        detail: $"{b.Handlers.Count} handler{(b.Handlers.Count == 1 ? "" : "s")}",
                        kind: SymbolKind.Method,
                        span: b.Span,
                        selection: b.Span,
                        children: b.Handlers.Select(h => MakeSymbol(
                            name: FormatHandlerName(h),
                            detail: "",
                            kind: SymbolKind.Event,
                            span: h.Span,
                            selection: h.Span)).ToArray());
                    break;

                case MethodDecl m:
                    yield return MakeSymbol(
                        name: m.Name,
                        detail: $"({string.Join(", ", m.Parameters.Select(p => p.Type.ToString()))})",
                        kind: SymbolKind.Method,
                        span: m.Span,
                        selection: m.Span);
                    break;

                case InitBlock init:
                    yield return MakeSymbol(
                        name: "init",
                        detail: $"({string.Join(", ", init.Parameters.Select(p => p.Type.ToString()))})",
                        kind: SymbolKind.Constructor,
                        span: init.Span,
                        selection: init.Span);
                    break;

                case LifecycleHook hook:
                    yield return MakeSymbol(
                        name: FormatLifecycleName(hook),
                        detail: "lifecycle",
                        kind: SymbolKind.Event,
                        span: hook.Span,
                        selection: hook.Span);
                    break;

                case PassivateDecl p:
                    yield return MakeSymbol(
                        name: $"passivate after {new ExpressionEmitter(new HashSet<string>()).Emit(p.Timeout)}",
                        detail: "idle unload",
                        kind: SymbolKind.Property,
                        span: p.Span,
                        selection: p.Span);
                    break;
            }
        }
    }

    private static DocumentSymbol MakeSymbol(
        string name,
        string detail,
        SymbolKind kind,
        SourceSpan span,
        SourceSpan selection,
        DocumentSymbol[]? children = null) =>
        new()
        {
            Name = name,
            Detail = detail,
            Kind = kind,
            Range = SpekDefinitionHandler.SpanToRange(span),
            SelectionRange = SpekDefinitionHandler.SpanToRange(selection),
            Children = children is null
                ? new Container<DocumentSymbol>()
                : new Container<DocumentSymbol>(children),
        };

    private static string FormatMessageDetail(MessageDecl m)
    {
        var fields = string.Join(", ", m.Fields.Select(f => $"{f.Type} {f.Name}"));
        return $"({fields})";
    }

    private static string FormatActorDetail(ActorDecl a)
    {
        var modifiers = new List<string>();
        if (a.IsAbstract) modifiers.Add("abstract");
        modifiers.Add(a.Visibility.ToString().ToLowerInvariant());
        return string.Join(" ", modifiers);
    }

    private static string FormatHandlerName(OnHandler h) => h.Pattern switch
    {
        NamedBindPattern n => $"on {n.MessageType} {n.Binding}",
        NoBindPattern n    => $"on {n.MessageType}",
        CatchAllPattern c  => $"on {c.Binding}",
        _                  => "on ?",
    };

    private static string FormatLifecycleName(LifecycleHook hook) => hook.Event switch
    {
        PreStartEvent   => "on PreStart",
        PostStopEvent   => "on PostStop",
        RestoreEvent re => $"on Restore({re.SnapshotType} {re.Binding})",
        _               => "on ?",
    };
}
