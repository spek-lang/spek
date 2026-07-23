using System.Text.RegularExpressions;
using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Spek.Compiler.Semantic;
using Spek.Tests.Emit;   // RoslynCompileHelper (internal, same assembly)
using Xunit;
using Xunit.Abstractions;

namespace Spek.Tests.Docs;

/// <summary>
/// Compiles the Spek code snippets embedded in <c>docs/**/*.md</c> so the
/// published documentation can never silently drift from the language. Every
/// fenced <c>```spek</c> block is extracted and — depending on its mode — run
/// through <see cref="SpekCompiler.Parse"/> and, for full programs, emitted to
/// C# and Roslyn-compiled via <see cref="RoslynCompileHelper"/>.
///
/// Mode is chosen per block:
/// <list type="bullet">
///   <item><b>parse</b> (syntax-only) — the default for any block whose first
///     declaration is a top-level keyword (<c>program</c>/<c>module</c>/
///     <c>actor</c>/<c>message</c>/<c>enum</c>/<c>shared</c>/<c>channel</c>/
///     <c>using</c>/<c>namespace</c>). It must parse without a grammar error
///     (<see cref="SpekCompiler.ParseToTree"/>, no semantic pass) — this
///     catches the common doc rot (renamed keywords, changed syntax) without
///     false positives from fragments that reference declarations defined
///     elsewhere in the prose.</item>
///   <item><b>compile</b> — opt-in via <c>&lt;!-- spek-test: compile --&gt;</c>
///     for self-contained snippets: full parse + semantic + emit + Roslyn
///     compile, the end-to-end guarantee.</item>
///   <item><b>ignore</b> — fragments (not starting with a top-level keyword),
///     snippets with an ellipsis placeholder (<c>{ ... }</c>), and anything
///     tagged <c>&lt;!-- spek-test: ignore --&gt;</c>.</item>
/// </list>
/// <c>docs/design/</c> defaults to <b>ignore</b> (design conversations contain
/// aspirational / parked syntax). <c>reference/errors.md</c> carries a
/// file-level <c>ignore</c> default (its snippets are deliberately invalid
/// "trigger" examples); the genuine "fix" snippets opt back in with a
/// per-block <c>compile</c> directive.
/// </summary>
public sealed class DocSnippetTests(ITestOutputHelper output)
{
    public enum Mode { Auto, Compile, Parse, Ignore }

    public sealed record Snippet(string File, int Line, Mode Mode, string Source);

    // ── extraction ───────────────────────────────────────────────────────────

    private static readonly string[] TopLevelKeywords =
        { "program", "module", "actor", "class", "message", "enum", "shared", "channel", "interface", "using", "namespace" };
    private static readonly string[] Modifiers =
        { "public", "internal", "private", "protected", "abstract" };

    // The mode is the first word after the colon; any text up to `-->` is an
    // optional human explanation (kept inside the comment so it never renders).
    private static readonly Regex FileDefaultRx =
        new(@"^<!--\s*spek-test-default\s*:\s*(?<m>\w+)\b.*-->\s*$", RegexOptions.Compiled);
    private static readonly Regex BlockDirectiveRx =
        new(@"^<!--\s*spek-test\s*:\s*(?<m>\w+)\b.*-->\s*$", RegexOptions.Compiled);

    private static readonly Lazy<List<Snippet>> AllSnippets = new(Extract);

    private static string DocsRoot()
    {
        var dir = AppContext.BaseDirectory;
        while (dir is not null && !File.Exists(Path.Combine(dir, "src", "Spek.slnx")))
            dir = Path.GetDirectoryName(dir);
        if (dir is null)
            throw new InvalidOperationException("Could not find src/Spek.slnx above the test assembly.");
        return Path.Combine(dir, "docs");
    }

    // The docs tree can live outside this repo (it was relocated during the
    // public-release split). When it's absent, the extractor still guards the
    // repo-root README's snippets and the coverage floor drops accordingly.
    private static bool DocsPresent() => Directory.Exists(DocsRoot());

    private static Mode ParseMode(string s) => s.ToLowerInvariant() switch
    {
        "auto"    => Mode.Auto,
        "compile" => Mode.Compile,
        "parse"   => Mode.Parse,
        "ignore"  => Mode.Ignore,
        _ => throw new InvalidOperationException($"Unknown spek-test mode '{s}'."),
    };

    private static List<Snippet> Extract()
    {
        var root = DocsRoot();
        var readme = Path.Combine(Path.GetDirectoryName(root)!, "README.md");
        var snippets = new List<Snippet>();

        // Also scan the repo-root README — it's the front door, so keep its
        // ```spek example honest (compile-tested) rather than letting it rot.
        var files = DocsPresent()
            ? Directory.EnumerateFiles(root, "*.md", SearchOption.AllDirectories).ToList()
            : new List<string>();
        if (File.Exists(readme)) files.Add(readme);

        foreach (var path in files)
        {
            var rel   = path == readme ? "README.md" : Path.GetRelativePath(root, path).Replace('\\', '/');
            var lines = File.ReadAllLines(path);

            // Design docs hold aspirational / parked syntax — skip by default.
            var fileDefault = rel.StartsWith("design/", StringComparison.Ordinal) ? Mode.Ignore : Mode.Auto;
            Mode? pending = null;   // a per-block directive awaiting the next fence

            for (var i = 0; i < lines.Length; i++)
            {
                var line = lines[i];
                var t    = line.Trim();

                if (line.TrimStart().StartsWith("```", StringComparison.Ordinal))
                {
                    var isSpek    = line.TrimStart().StartsWith("```spek", StringComparison.Ordinal);
                    var fenceLine = i + 1;                  // 1-based
                    var content   = new List<string>();
                    for (i++; i < lines.Length; i++)
                    {
                        if (lines[i].TrimStart().StartsWith("```", StringComparison.Ordinal)) break;
                        content.Add(lines[i]);
                    }

                    if (isSpek)
                    {
                        var source = string.Join("\n", content);
                        var mode   = pending
                            ?? (fileDefault == Mode.Auto ? Classify(source) : fileDefault);
                        if (!string.IsNullOrWhiteSpace(source))
                            snippets.Add(new Snippet(rel, fenceLine, mode, source));
                    }
                    pending = null;
                    continue;
                }

                if (FileDefaultRx.Match(t) is { Success: true } fd) { fileDefault = ParseMode(fd.Groups["m"].Value); continue; }
                if (BlockDirectiveRx.Match(t) is { Success: true } bd) { pending = ParseMode(bd.Groups["m"].Value); continue; }

                // A directive only applies if it immediately precedes the fence
                // (blank lines between are allowed; any other content cancels it).
                if (t.Length > 0) pending = null;
            }
        }
        return snippets;
    }

    /// <summary>Auto-mode: a block whose first real declaration starts with a
    /// top-level keyword is a complete program → <b>parse</b> (syntax-only);
    /// fragments and ellipsis-placeholder snippets → <b>ignore</b>. Full
    /// compilation is opt-in (see the <c>compile</c> directive).</summary>
    private static Mode Classify(string source)
    {
        // Illustrative placeholders (`{ ... }`, `// ...`, `/* ... */`) are not
        // valid Spek — any snippet with an ellipsis is prose, not a program.
        if (source.Contains("...", StringComparison.Ordinal)) return Mode.Ignore;

        foreach (var raw in source.Split('\n'))
        {
            var l = raw.Trim();
            if (l.Length == 0 || l.StartsWith("//", StringComparison.Ordinal)) continue;

            var first = FirstToken(l);
            if (Array.IndexOf(Modifiers, first) >= 0)
                first = FirstToken(l[first.Length..].TrimStart());

            return Array.IndexOf(TopLevelKeywords, first) >= 0 ? Mode.Parse : Mode.Ignore;
        }
        return Mode.Ignore;   // empty / comment-only
    }

    private static string FirstToken(string s)
    {
        var n = 0;
        while (n < s.Length && (char.IsLetterOrDigit(s[n]) || s[n] == '_')) n++;
        return s[..n];
    }

    // ── tests ────────────────────────────────────────────────────────────────

    public static IEnumerable<object[]> Checkable() =>
        AllSnippets.Value
            .Where(s => s.Mode is Mode.Compile or Mode.Parse)
            .Select(s => new object[] { s.File, s.Line });

    [Theory]
    [MemberData(nameof(Checkable))]
    public void DocSnippet_IsValid(string file, int line)
    {
        var s = AllSnippets.Value.First(x => x.File == file && x.Line == line);

        if (s.Mode == Mode.Parse)
        {
            // Syntax-only: must parse as valid Spek grammar. Semantic errors
            // (e.g. a message defined elsewhere in the doc) are NOT failures —
            // a doc fragment legitimately references things from its prose.
            var (tree, diags) = SpekCompiler.ParseToTree(s.Source);
            Assert.True(tree is not null && diags.Count == 0,
                $"{file}:{line} — snippet has a syntax error:\n" +
                string.Join("\n", diags.Select(d => $"  {d.Code} ({d.Line}:{d.Column}) {d.Message}")) +
                $"\n--- snippet ---\n{s.Source}");
            return;
        }

        // Mode.Compile — the full end-to-end guarantee.
        var parsed = SpekCompiler.Parse(s.Source);
        var errors = parsed.Diagnostics.Where(d => d.Severity == DiagnosticSeverity.Error).ToList();
        Assert.True(parsed.Success,
            $"{file}:{line} — snippet did not parse/analyze cleanly:\n" +
            string.Join("\n", errors.Select(d => $"  {d.Code} ({d.Line}:{d.Column}) {d.Message}")) +
            $"\n--- snippet ---\n{s.Source}");

        // Emit in test mode so a `*Tests` container in the docs (e.g. testing.md) gets
        // its `using Spek.Testing;` + [SpekTest] shape; a no-op for non-test snippets.
        var csharp = new FileEmitter().Emit(parsed.Tree!, emitTests: true);
        var (ok, summary, _) = RoslynCompileHelper.TryCompile(csharp, "DocSnippet");
        Assert.True(ok,
            $"{file}:{line} — emitted C# did not compile:\n{summary}\n--- snippet ---\n{s.Source}");
    }

    /// <summary>Reports coverage and guards against the extractor silently
    /// breaking (which would make every snippet "pass" by checking none).</summary>
    [Fact]
    public void DocSnippet_CoverageReport()
    {
        var all     = AllSnippets.Value;
        var compile = all.Count(s => s.Mode == Mode.Compile);
        var parse   = all.Count(s => s.Mode == Mode.Parse);
        var ignore  = all.Count(s => s.Mode == Mode.Ignore);

        output.WriteLine($"doc spek snippets: {all.Count} total — " +
                         $"{compile} compile, {parse} parse, {ignore} ignored");

        // With the docs tree relocated out of this repo, only the README's
        // snippets remain — the floor drops to "the extractor found something."
        var floor = DocsPresent() ? 30 : 1;
        Assert.True(compile + parse >= floor,
            $"doc-snippet coverage collapsed: only {compile + parse} snippets checked. " +
            "The extractor may be broken.");
    }
}
