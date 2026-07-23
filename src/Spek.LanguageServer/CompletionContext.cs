using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using Spek.Compiler.AST;

namespace Spek.LanguageServer;

/// <summary>
/// Given a parsed <see cref="SpekFile"/> and a cursor position, returns
/// the in-scope identifiers that should appear in a completion list
/// alongside the static keyword set. The current scope covers:
///
/// <list type="bullet">
///   <item>Fields of the enclosing actor.</item>
///   <item>Behavior names of the enclosing actor (useful for <c>become</c>).</item>
///   <item>Pattern binding of the enclosing <c>on</c> handler (e.g. <c>p</c>
///         in <c>on Ping p =&gt; ...</c>).</item>
///   <item>Every <c>message</c> type declared at file scope.</item>
///   <item>Every <c>actor</c> type declared at file scope.</item>
/// </list>
///
/// Local <c>var</c> bindings are deferred — they need order-aware scope
/// tracking inside statement sequences that the current AST walker
/// doesn't do yet.
/// </summary>
public static class CompletionContext
{
    public record Symbol(string Name, CompletionItemKind Kind, string? Detail);

    public static IReadOnlyList<Symbol> Collect(SpekFile file, int line, int column)
    {
        var result = new List<Symbol>();

        var chain = PositionResolver.FindChain(file, line, column);
        var enclosingActor   = chain.OfType<ActorDecl>().FirstOrDefault();
        var enclosingHandler = chain.OfType<OnHandler>().FirstOrDefault();

        if (enclosingActor is not null)
        {
            foreach (var field in enclosingActor.Members.OfType<FieldDecl>())
            {
                result.Add(new Symbol(
                    field.Name,
                    CompletionItemKind.Field,
                    $"{field.Type} (field of {enclosingActor.Name})"));
            }

            foreach (var beh in enclosingActor.Members.OfType<BehaviorDecl>())
            {
                result.Add(new Symbol(
                    beh.Name,
                    CompletionItemKind.EnumMember,
                    $"behavior of {enclosingActor.Name}"));
            }
        }

        if (enclosingHandler is not null)
        {
            switch (enclosingHandler.Pattern)
            {
                case NamedBindPattern nb:
                    result.Add(new Symbol(
                        nb.Binding,
                        CompletionItemKind.Variable,
                        nb.MessageType.ToString()));
                    break;

                case CatchAllPattern cp:
                    result.Add(new Symbol(cp.Binding, CompletionItemKind.Variable, "object"));
                    break;
            }
        }

        foreach (var decl in file.Declarations)
        {
            switch (decl)
            {
                case MessageDecl m:
                    var msgFields = string.Join(", ", m.Fields.Select(f => $"{f.Type} {f.Name}"));
                    result.Add(new Symbol(
                        m.Name,
                        CompletionItemKind.Struct,
                        $"message ({msgFields})"));
                    break;

                case ActorDecl a:
                    result.Add(new Symbol(
                        a.Name,
                        CompletionItemKind.Class,
                        "actor"));
                    break;

                case EnumDecl e:
                    result.Add(new Symbol(
                        e.Name,
                        CompletionItemKind.Enum,
                        $"enum ({e.Members.Count} members)"));
                    foreach (var em in e.Members)
                    {
                        result.Add(new Symbol(
                            $"{e.Name}.{em.Name}",
                            CompletionItemKind.EnumMember,
                            $"member of {e.Name}"));
                    }
                    break;

                case ChannelDecl c:
                    result.Add(new Symbol(
                        c.Name,
                        CompletionItemKind.Interface,
                        "channel"));
                    break;

                case InterfaceDecl i:
                    result.Add(new Symbol(
                        i.Name,
                        CompletionItemKind.Interface,
                        "interface"));
                    break;
            }
        }

        return result;
    }
}
