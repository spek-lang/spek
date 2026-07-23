using MediatR;
using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities;
using OmniSharp.Extensions.LanguageServer.Protocol.Document;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using Spek.Compiler.AST;
using Spek.Compiler.Semantic;

namespace Spek.LanguageServer;

/// <summary>
/// Resolves <c>textDocument/definition</c> requests — the "jump to
/// declaration" action. Currently handles message/actor type references
/// and <c>become</c> behavior targets. Cross-file resolution (using
/// <c>SpekCompilation</c>) is deferred to a future iteration.
/// </summary>
internal sealed class SpekDefinitionHandler : DefinitionHandlerBase
{
    private readonly DocumentCache _cache;

    public SpekDefinitionHandler(DocumentCache cache) => _cache = cache;

    protected override DefinitionRegistrationOptions CreateRegistrationOptions(
        DefinitionCapability capability, ClientCapabilities clientCapabilities) =>
        new() { DocumentSelector = TextDocumentSelector.ForLanguage("spek") };

    public override Task<LocationOrLocationLinks?> Handle(
        DefinitionParams request, CancellationToken cancellationToken)
    {
        var entry = _cache.Get(request.TextDocument.Uri);
        if (entry?.Tree is null) return Task.FromResult<LocationOrLocationLinks?>(null);

        // LSP positions are 0-based; Spek spans are 1-based.
        var line   = (int)request.Position.Line + 1;
        var column = (int)request.Position.Character + 1;
        var chain  = PositionResolver.FindChain(entry.Tree, line, column);
        if (chain.Count == 0) return Task.FromResult<LocationOrLocationLinks?>(null);

        var target = Resolve(chain, entry.Tree);
        if (target is null) return Task.FromResult<LocationOrLocationLinks?>(null);

        return Task.FromResult<LocationOrLocationLinks?>(
            LocationOrLocationLinks.From(
                new Location
                {
                    Uri = request.TextDocument.Uri,
                    Range = SpanToRange(target.Span),
                }));
    }

    /// <summary>
    /// Given the chain of AST nodes at the cursor (innermost last), picks
    /// the one that refers to a named declaration and returns that
    /// declaration. Order of checks walks outward from innermost; the first
    /// match wins.
    /// </summary>
    private static AstNode? Resolve(IReadOnlyList<AstNode> chain, SpekFile file)
    {
        var symbols = SymbolTable.Build(file);

        // BecomeStmt: resolve the behavior name within the enclosing actor.
        if (chain.OfType<BecomeStmt>().LastOrDefault() is { } bs &&
            chain.OfType<ActorDecl>().FirstOrDefault() is { } enclosingActor)
        {
            return enclosingActor.Members
                .OfType<BehaviorDecl>()
                .FirstOrDefault(b => b.Name == bs.BehaviorName);
        }

        // AskExpr's message type → message declaration.
        if (chain.OfType<AskExpr>().LastOrDefault() is { Message: NewExpr askMsg })
            return symbols.ResolveMessage(askMsg.Type);

        // NewExpr's Type → message declaration (most common case).
        if (chain.OfType<NewExpr>().LastOrDefault() is { } newExpr)
            return symbols.ResolveMessage(newExpr.Type);

        // SpawnExpr's type argument → actor declaration.
        if (chain.OfType<SpawnExpr>().LastOrDefault() is { } spawn && spawn.TypeArgs.Count > 0)
            return symbols.ResolveActor(spawn.TypeArgs[0].Name);

        // TypeRef in a field or parameter — could be an actor, a message,
        // or a primitive. Try actor first (most useful for navigation).
        if (chain.OfType<TypeRef>().LastOrDefault() is { } type)
        {
            var actor = symbols.ResolveActor(type.Name);
            if (actor is not null) return actor;
            return symbols.ResolveMessage(type.Name);
        }

        // Bare QualifiedName — fall back to "try message then actor."
        if (chain.OfType<QualifiedName>().LastOrDefault() is { } qn)
        {
            var msg = symbols.ResolveMessage(qn);
            if (msg is not null) return msg;
            return symbols.ResolveActor(qn);
        }

        return null;
    }

    internal static OmniSharp.Extensions.LanguageServer.Protocol.Models.Range SpanToRange(SourceSpan span)
    {
        // Spek: 1-based, end-exclusive. LSP: 0-based, end-exclusive.
        var startLine = Math.Max(0, span.StartLine - 1);
        var startCol  = Math.Max(0, span.StartColumn - 1);
        var endLine   = Math.Max(0, span.EndLine - 1);
        var endCol    = Math.Max(0, span.EndColumn - 1);
        return new OmniSharp.Extensions.LanguageServer.Protocol.Models.Range(
            new Position(startLine, startCol),
            new Position(endLine, endCol));
    }
}
