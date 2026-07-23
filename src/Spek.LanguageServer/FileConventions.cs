using Spek.Compiler.AST;
using Spek.Compiler.Semantic;

namespace Spek.LanguageServer;

/// <summary>
/// C#-style file-organization conventions, surfaced as editor <b>hints</b>
/// (suggestions — <see cref="Spek.Compiler.Semantic.DiagnosticSeverity.Hint"/>).
/// These are tooling nudges, not compiler diagnostics: they carry no <c>CE</c>
/// code, aren't in the error catalog, and never appear in <c>spekc</c> or a
/// build. Two of them:
///
/// <list type="bullet">
///   <item><b>one-type-per-file</b> — a <c>.spek</c> file should declare a single
///     top-level "substantial" type (module / actor / enum / class / channel /
///     shared region). Extra ones should move to their own file.</item>
///   <item><b>file-name-mismatch</b> — a single-purpose type file should be named
///     after the type it declares.</item>
/// </list>
///
/// <para><c>message</c> declarations are exempt: they're an actor's small value
/// vocabulary and group freely, and single-file compilation (CE0010) often
/// *requires* a message to sit beside the enum/type it references. Nested types
/// are exempt too — they aren't top-level. So the hints only fire where a file
/// genuinely could and should be split.</para>
/// </summary>
internal static class FileConventions
{
    public static IEnumerable<Diagnostic> Check(SpekFile file, string fileNameNoExt)
    {
        var substantial = file.Declarations.Where(IsSubstantial).ToList();
        var hasMessages = file.Declarations.OfType<MessageDecl>().Any();

        // one-type-per-file: everything after the first substantial type should
        // move to its own file.
        if (substantial.Count > 1)
        {
            foreach (var d in substantial.Skip(1))
            {
                var (kw, name) = KeywordAndName(d);
                yield return Diagnostic.At("one-type-per-file", NameSpan(d.Span, kw, name),
                    $"'{name}' could move to its own file '{name}.spek'. A Spek file " +
                    "declares one top-level type (C# convention); nested types and " +
                    "messages are exempt.",
                    DiagnosticSeverity.Hint);
            }
        }

        // file-name-mismatch: only for a single-purpose file (exactly one
        // substantial type, no messages), so a themed message file isn't pushed
        // to rename after an incidental co-located type.
        if (substantial.Count == 1 && !hasMessages)
        {
            var (kw, name) = KeywordAndName(substantial[0]);
            if (!string.Equals(name, fileNameNoExt, StringComparison.Ordinal))
                yield return Diagnostic.At("file-name-mismatch", NameSpan(substantial[0].Span, kw, name),
                    $"File '{fileNameNoExt}.spek' declares '{name}'; naming it " +
                    $"'{name}.spek' matches the C# convention.",
                    DiagnosticSeverity.Hint);
        }
    }

    private static bool IsSubstantial(TopLevelDecl d) =>
        d is ModuleDecl or ActorDecl or EnumDecl or ClassDecl or ChannelDecl or SharedRegionDecl;

    private static (string Keyword, string Name) KeywordAndName(TopLevelDecl d) => d switch
    {
        ModuleDecl m       => ("module", m.Name),
        ActorDecl a        => ("actor", a.Name),
        EnumDecl e         => ("enum", e.Name),
        ClassDecl c        => ("class", c.Name),
        ChannelDecl ch     => ("channel", ch.Name),
        SharedRegionDecl s => ("shared", s.Name),
        MessageDecl msg    => ("message", msg.Name),
        _                  => ("", ""),
    };

    // Span of just the type name (after the keyword), for a tidy squiggle.
    private static SourceSpan NameSpan(SourceSpan declSpan, string keyword, string name)
    {
        var nameStartCol = declSpan.StartColumn + keyword.Length + 1;
        return new SourceSpan(declSpan.StartLine, nameStartCol,
                              declSpan.StartLine, nameStartCol + name.Length);
    }
}
