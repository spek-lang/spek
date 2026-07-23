using Spek.Compiler.AST;

namespace Spek.LanguageServer;

/// <summary>
/// Maps a cursor position to the named declaration the cursor is on
/// (either at the declaration site or at any reference). Shared by
/// <c>textDocument/references</c> and <c>textDocument/rename</c>.
///
/// Returns the symbol's <see cref="ReferenceFinder.Kind"/>, simple name,
/// and (for behavior names) the owning actor needed to scope the lookup.
/// </summary>
internal static class SymbolUnderCursor
{
    public static (ReferenceFinder.Kind? Kind, string? Name, ActorDecl? Owner) Resolve(
        SpekFile file, int line, int column)
    {
        var chain = PositionResolver.FindChain(file, line, column);
        if (chain.Count == 0) return (null, null, null);

        // 1) Cursor on the declaration site itself.
        if (chain.OfType<MessageDecl>().LastOrDefault() is { } md
            && IsOnNameToken(md.Span, "message", md.Name, line, column))
            return (ReferenceFinder.Kind.Message, md.Name, null);

        if (chain.OfType<ChannelDecl>().LastOrDefault() is { } cd
            && IsOnNameToken(cd.Span, "channel", cd.Name, line, column))
            return (ReferenceFinder.Kind.Channel, cd.Name, null);

        if (chain.OfType<EnumDecl>().LastOrDefault() is { } ed
            && IsOnNameToken(ed.Span, "enum", ed.Name, line, column))
            return (ReferenceFinder.Kind.Enum, ed.Name, null);

        if (chain.OfType<ActorDecl>().LastOrDefault() is { } ad
            && IsOnNameToken(ad.Span, "actor", ad.Name, line, column))
            return (ReferenceFinder.Kind.Actor, ad.Name, null);

        if (chain.OfType<BehaviorDecl>().LastOrDefault() is { } bd)
        {
            if (IsOnNameToken(bd.Span, "behavior", bd.Name, line, column))
            {
                var owner = chain.OfType<ActorDecl>().FirstOrDefault();
                return (ReferenceFinder.Kind.Behavior, bd.Name, owner);
            }
        }

        // 2) Cursor on a reference site.
        if (chain.OfType<BecomeStmt>().LastOrDefault() is { } bs)
        {
            var owner = chain.OfType<ActorDecl>().FirstOrDefault();
            return (ReferenceFinder.Kind.Behavior, bs.BehaviorName, owner);
        }

        // Covers `.Ask(new Msg())` too — its message is a NewExpr child.
        if (chain.OfType<NewExpr>().LastOrDefault() is { } ne)
            return (ReferenceFinder.Kind.Message, ne.Type.Simple, null);

        if (chain.OfType<NamedBindPattern>().LastOrDefault() is { } nbp)
            return (ReferenceFinder.Kind.Message, nbp.MessageType.Simple, null);

        if (chain.OfType<NoBindPattern>().LastOrDefault() is { } nop)
            return (ReferenceFinder.Kind.Message, nop.MessageType.Simple, null);

        if (chain.OfType<ChannelInput>().LastOrDefault() is { } ci)
            return (ReferenceFinder.Kind.Message, ci.MessageType.Simple, null);

        if (chain.OfType<ChannelEmits>().LastOrDefault() is { } cem
            && cem.MessageType is { } mt)
            return (ReferenceFinder.Kind.Message, mt.Simple, null);

        if (chain.OfType<SpawnExpr>().LastOrDefault() is { } se && se.TypeArgs.Count > 0)
            return (ReferenceFinder.Kind.Actor, se.TypeArgs[0].Name.Simple, null);

        // TypeRef in field / param / message-field position. Could be a
        // message, an actor, an enum — the file decl table picks the kind.
        if (chain.OfType<TypeRef>().LastOrDefault() is { } tr)
        {
            var s = tr.Name.Simple;
            if (file.Declarations.OfType<MessageDecl>().Any(m => m.Name == s))
                return (ReferenceFinder.Kind.Message, s, null);
            if (file.Declarations.OfType<ActorDecl>().Any(a => a.Name == s))
                return (ReferenceFinder.Kind.Actor, s, null);
            if (file.Declarations.OfType<EnumDecl>().Any(e => e.Name == s))
                return (ReferenceFinder.Kind.Enum, s, null);
            if (file.Declarations.OfType<ChannelDecl>().Any(c => c.Name == s))
                return (ReferenceFinder.Kind.Channel, s, null);
        }

        return (null, null, null);
    }

    /// <summary>
    /// True when (line, column) lies on the name token of a top-level
    /// declaration whose start token is <paramref name="keyword"/>. We
    /// don't have raw token spans, so we approximate from the decl's
    /// start column: name is at <c>StartColumn + keyword.Length + 1</c>
    /// (one space) and runs <paramref name="name"/>'s length.
    /// </summary>
    private static bool IsOnNameToken(SourceSpan declSpan, string keyword, string name,
                                      int line, int column)
    {
        if (line != declSpan.StartLine) return false;
        var nameStart = declSpan.StartColumn + keyword.Length + 1;
        return column >= nameStart && column <= nameStart + name.Length;
    }
}
