using MediatR;
using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities;
using OmniSharp.Extensions.LanguageServer.Protocol.Document;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using Spek.Compiler.AST;
using Spek.Compiler.Semantic;

namespace Spek.LanguageServer;

/// <summary>
/// Resolves <c>textDocument/hover</c> — shows type/declaration info when
/// the user hovers over an identifier. Covers messages, actors, behaviors,
/// and fields. Field-of-current-actor resolution uses
/// <see cref="ActorSymbols.Fields"/>; cross-actor field access isn't
/// allowed (CE0012) so we don't try to resolve through <c>ActorRef</c>s.
/// </summary>
internal sealed class SpekHoverHandler : HoverHandlerBase
{
    private readonly DocumentCache _cache;

    public SpekHoverHandler(DocumentCache cache) => _cache = cache;

    protected override HoverRegistrationOptions CreateRegistrationOptions(
        HoverCapability capability, ClientCapabilities clientCapabilities) =>
        new() { DocumentSelector = TextDocumentSelector.ForLanguage("spek") };

    public override Task<Hover?> Handle(HoverParams request, CancellationToken cancellationToken)
    {
        var entry = _cache.Get(request.TextDocument.Uri);
        if (entry?.Tree is null) return Task.FromResult<Hover?>(null);

        var line   = (int)request.Position.Line + 1;
        var column = (int)request.Position.Character + 1;
        var chain  = PositionResolver.FindChain(entry.Tree, line, column);
        if (chain.Count == 0) return Task.FromResult<Hover?>(null);

        var markdown = Format(chain, entry.Tree);
        if (markdown is null) return Task.FromResult<Hover?>(null);

        return Task.FromResult<Hover?>(new Hover
        {
            Contents = new MarkedStringsOrMarkupContent(new MarkupContent
            {
                Kind  = MarkupKind.Markdown,
                Value = markdown,
            }),
        });
    }

    private static string? Format(IReadOnlyList<AstNode> chain, SpekFile file)
    {
        var symbols = SymbolTable.Build(file);

        // BecomeStmt first — behavior name.
        if (chain.OfType<BecomeStmt>().LastOrDefault() is { } bs &&
            chain.OfType<ActorDecl>().FirstOrDefault() is { } enclosingActor)
        {
            var beh = enclosingActor.Members
                .OfType<BehaviorDecl>()
                .FirstOrDefault(b => b.Name == bs.BehaviorName);
            if (beh is not null)
                return $"```spek\nbehavior {beh.Name}\n```\n\nDefined on actor `{enclosingActor.Name}`.";
        }

        // Type in new / spawn (a `.Ask(new Msg())` message is a NewExpr) —
        // format as a message or actor signature.
        var targetName =
            chain.OfType<NewExpr>().LastOrDefault()?.Type
            ?? (chain.OfType<SpawnExpr>().LastOrDefault()?.TypeArgs.FirstOrDefault()?.Name)
            ?? chain.OfType<TypeRef>().LastOrDefault()?.Name
            ?? chain.OfType<QualifiedName>().LastOrDefault();

        if (targetName is not null)
        {
            if (symbols.ResolveMessage(targetName) is { } msg)
                return FormatMessage(msg);
            if (symbols.ResolveActor(targetName) is { } actor)
                return FormatActor(actor);
        }

        // Bare NameExpr — check whether it's a field of the enclosing actor.
        if (chain.OfType<NameExpr>().LastOrDefault() is { } name &&
            name.Name.Parts.Count == 1 &&
            chain.OfType<ActorDecl>().FirstOrDefault() is { } containingActor)
        {
            var actorSymbols = symbols.GetActorSymbols(containingActor.Name);
            if (actorSymbols is not null &&
                actorSymbols.Fields.TryGetValue(name.Name.Simple, out var field))
            {
                return $"```spek\n{field.Type} {field.Name}\n```\n\nField of actor `{containingActor.Name}`.";
            }
        }

        return null;
    }

    private static string FormatMessage(MessageDecl m)
    {
        var fields = string.Join(", ", m.Fields.Select(f => $"{f.Type} {f.Name}"));
        return $"```spek\nmessage {m.Name}({fields})\n```";
    }

    private static string FormatActor(ActorDecl a)
    {
        var modifiers = new List<string>();
        if (a.IsAbstract) modifiers.Add("abstract");
        modifiers.Add(a.Visibility.ToString().ToLowerInvariant());
        var sig = $"{string.Join(" ", modifiers)} actor {a.Name}";

        var behaviorCount = a.Members.OfType<BehaviorDecl>().Count();
        var fieldCount    = a.Members.OfType<FieldDecl>().Count();

        return $"```spek\n{sig}\n```\n\n" +
               $"{behaviorCount} behavior{(behaviorCount == 1 ? "" : "s")}, " +
               $"{fieldCount} field{(fieldCount == 1 ? "" : "s")}.";
    }
}
