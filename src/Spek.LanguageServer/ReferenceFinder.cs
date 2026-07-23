using System.Collections;
using Spek.Compiler.AST;

namespace Spek.LanguageServer;

/// <summary>
/// Locates every textual occurrence of a named declaration within a
/// parsed <see cref="SpekFile"/>. Powers both
/// <c>textDocument/references</c> (find-references) and
/// <c>textDocument/rename</c>.
///
/// Scoping rules:
/// <list type="bullet">
///   <item>Message / actor / channel / enum names — file-wide.</item>
///   <item>Behavior names — scoped to the enclosing actor (a behavior
///         in actor A is unrelated to a same-named behavior in
///         actor B).</item>
///   <item>Enum members, field accesses, local <c>var</c> bindings —
///         deferred (their scoping is more nuanced).</item>
/// </list>
/// </summary>
public static class ReferenceFinder
{
    public enum Kind { Message, Actor, Channel, Enum, Behavior }

    /// <summary>
    /// Returns every span in <paramref name="file"/> that references the
    /// named declaration of kind <paramref name="kind"/> with simple name
    /// <paramref name="name"/>, INCLUDING its declaration site. Useful for
    /// rename (which wants every occurrence).
    /// </summary>
    public static IReadOnlyList<SourceSpan> FindAllOccurrences(
        SpekFile file, Kind kind, string name, ActorDecl? behaviorOwner = null)
    {
        var results = new List<SourceSpan>();
        AddDeclarationSpan(file, kind, name, behaviorOwner, results);
        AddReferenceSpans(file, kind, name, behaviorOwner, results);
        return Dedupe(results);
    }

    /// <summary>
    /// Returns every reference span for the named declaration, EXCLUDING
    /// its declaration site. For find-references LSP queries that report
    /// the declaration separately.
    /// </summary>
    public static IReadOnlyList<SourceSpan> FindReferences(
        SpekFile file, Kind kind, string name, ActorDecl? behaviorOwner = null)
    {
        var results = new List<SourceSpan>();
        AddReferenceSpans(file, kind, name, behaviorOwner, results);
        return Dedupe(results);
    }

    /// <summary>
    /// Dedupes by (line, column) keys — multiple AST shapes can carry the
    /// same source token (e.g. <c>spawn&lt;Worker&gt;()</c> exposes
    /// <c>Worker</c> both via <see cref="SpawnExpr.TypeArgs"/> and via the
    /// inner <see cref="TypeRef"/>; we only want one occurrence per
    /// physical token).
    /// </summary>
    private static IReadOnlyList<SourceSpan> Dedupe(List<SourceSpan> input)
    {
        var seen = new HashSet<(int, int, int, int)>();
        var output = new List<SourceSpan>(input.Count);
        foreach (var s in input)
        {
            var key = (s.StartLine, s.StartColumn, s.EndLine, s.EndColumn);
            if (seen.Add(key)) output.Add(s);
        }
        return output;
    }

    private static void AddDeclarationSpan(
        SpekFile file, Kind kind, string name, ActorDecl? behaviorOwner,
        List<SourceSpan> results)
    {
        foreach (var decl in file.Declarations)
        {
            switch (kind)
            {
                case Kind.Message when decl is MessageDecl m && m.Name == name:
                    results.Add(NameSpanOfDecl(m.Span, "message", m.Name));
                    return;
                case Kind.Actor when decl is ActorDecl a && a.Name == name:
                    results.Add(NameSpanOfDecl(a.Span, "actor", a.Name));
                    return;
                case Kind.Channel when decl is ChannelDecl c && c.Name == name:
                    results.Add(NameSpanOfDecl(c.Span, "channel", c.Name));
                    return;
                case Kind.Enum when decl is EnumDecl e && e.Name == name:
                    results.Add(NameSpanOfDecl(e.Span, "enum", e.Name));
                    return;
                case Kind.Behavior when decl is ActorDecl ad && ReferenceEquals(ad, behaviorOwner):
                    foreach (var b in ad.Members.OfType<BehaviorDecl>())
                        if (b.Name == name)
                        {
                            results.Add(NameSpanOfDecl(b.Span, "behavior", b.Name));
                            return;
                        }
                    break;
            }
        }
    }

    private static void AddReferenceSpans(
        SpekFile file, Kind kind, string name, ActorDecl? behaviorOwner,
        List<SourceSpan> results)
    {
        foreach (var node in AstWalk.EnumerateAll(file))
        {
            switch (kind)
            {
                case Kind.Message: AddIfMessageRef(node, name, results); break;
                case Kind.Actor:   AddIfActorRef(node, name, results);   break;
                case Kind.Channel: AddIfChannelRef(node, name, results); break;
                case Kind.Enum:    AddIfEnumRef(node, name, results);    break;
                case Kind.Behavior:
                    if (node is BecomeStmt bs && bs.BehaviorName == name)
                    {
                        // Only count become-targets within the owning actor.
                        if (behaviorOwner is null || NodeBelongsToActor(file, node, behaviorOwner))
                            results.Add(bs.Span);
                    }
                    break;
            }
        }
    }

    private static void AddIfMessageRef(AstNode node, string name, List<SourceSpan> results)
    {
        switch (node)
        {
            case NewExpr ne when ne.Type.Simple == name:
                // Covers `.Ask(new Msg())` too — the message is a NewExpr child.
                results.Add(ne.Type.Span);
                break;
            case NamedBindPattern nbp when nbp.MessageType.Simple == name:
                results.Add(nbp.MessageType.Span);
                break;
            case NoBindPattern nop when nop.MessageType.Simple == name:
                results.Add(nop.MessageType.Span);
                break;
            case ChannelInput ci when ci.MessageType.Simple == name:
                results.Add(ci.MessageType.Span);
                break;
            case ChannelEmits ce when ce.MessageType is { } mt && mt.Simple == name:
                results.Add(mt.Span);
                break;
            case TypeRef tr when tr.Name.Simple == name:
                results.Add(tr.Span);
                break;
        }
    }

    private static void AddIfActorRef(AstNode node, string name, List<SourceSpan> results)
    {
        switch (node)
        {
            case SpawnExpr se:
                foreach (var ta in se.TypeArgs)
                    if (ta.Name.Simple == name) results.Add(ta.Span);
                break;
            case ActorDecl ad:
                if (ad.BaseActor is { } ba && ba.Simple == name) results.Add(ba.Span);
                break;
            case TypeRef tr when tr.Name.Simple == name:
                results.Add(tr.Span);
                break;
        }
    }

    private static void AddIfChannelRef(AstNode node, string name, List<SourceSpan> results)
    {
        switch (node)
        {
            case ActorDecl ad:
                // The parser splits the actor's colon list naively: first
                // name → BaseActor, rest → ImplementedChannels. Either could
                // turn out to be a channel after symbol resolution; check
                // both.
                if (ad.BaseActor is { } ba && ba.Simple == name)
                    results.Add(ba.Span);
                foreach (var ic in ad.ImplementedChannels)
                    if (ic.Simple == name) results.Add(ic.Span);
                break;
            case ChannelDecl cd:
                foreach (var bc in cd.BaseChannels)
                    if (bc.Simple == name) results.Add(bc.Span);
                break;
        }
    }

    private static void AddIfEnumRef(AstNode node, string name, List<SourceSpan> results)
    {
        // Enum types can appear as message-field types; member access
        // (`HostState.Paused`) is a qualified-name reference but the AST
        // currently represents that as a NameExpr with a 2-part QualifiedName.
        if (node is TypeRef tr && tr.Name.Simple == name)
            results.Add(tr.Span);
        else if (node is NameExpr ne && ne.Name.Parts.Count >= 1 && ne.Name.Parts[0] == name)
            results.Add(ne.Name.Span);
    }

    /// <summary>True if <paramref name="node"/> is contained within
    /// <paramref name="actor"/>'s span.</summary>
    private static bool NodeBelongsToActor(SpekFile file, AstNode node, ActorDecl actor)
    {
        return node.Span.StartLine >= actor.Span.StartLine
            && node.Span.EndLine   <= actor.Span.EndLine;
    }

    /// <summary>
    /// The decl span typically covers the entire declaration block
    /// (`message Foo(int x);`); for rename we want only the name token's
    /// span. Reuses the decl's start position + a length offset based on
    /// the keyword + name. This is an approximation — we don't have the
    /// lexer's token spans, so this gives a best-effort highlight that always
    /// covers the name and may bleed slightly into surrounding tokens.
    /// </summary>
    private static SourceSpan NameSpanOfDecl(SourceSpan declSpan, string keyword, string name)
    {
        // Approximate: start at decl start + (visibility-prefix-aware)
        // keyword length + 1 space; end at start + name length. Good
        // enough for LSP highlight purposes since editors typically
        // display the whole range.
        var nameStartCol = declSpan.StartColumn + keyword.Length + 1;
        return new SourceSpan(
            declSpan.StartLine, nameStartCol,
            declSpan.StartLine, nameStartCol + name.Length);
    }

}
