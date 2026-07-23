using System.Text.RegularExpressions;
using Xunit;

namespace Spek.Tests.Tooling;

/// <summary>
/// Keeps the VS Code TextMate grammar in sync with the language. The
/// <c>.tmLanguage.json</c> can't be mechanically generated from the ANTLR
/// lexer, so it drifts silently when keywords are added. This test extracts
/// every keyword token from <c>SpekLexer.g4</c> (the source of truth) and
/// asserts the TextMate grammar colors it somewhere.
/// </summary>
public sealed class TmLanguageSyncTests
{
    private static string RepoRoot()
    {
        var dir = AppContext.BaseDirectory;
        while (dir is not null && !File.Exists(Path.Combine(dir, "src", "Spek.slnx")))
            dir = Path.GetDirectoryName(dir);
        Assert.NotNull(dir);
        return dir!;
    }

    [Fact]
    public void EveryLexerKeyword_IsColoredByTheTextMateGrammar()
    {
        var root = RepoRoot();
        var lexer = File.ReadAllText(Path.Combine(root, "src", "Spek.Compiler", "Grammar", "SpekLexer.g4"));
        var tm    = File.ReadAllText(Path.Combine(root, "tools", "vscode-spek", "syntaxes", "spek.tmLanguage.json"));

        // Keyword tokens are `NAME : 'literal';` where the literal is purely
        // alphabetic (operators/punctuation tokens have symbol literals).
        var keywords = Regex.Matches(lexer, @"^\s*[A-Z_]+\s*:\s*'([a-zA-Z]+)'\s*;", RegexOptions.Multiline)
            .Select(m => m.Groups[1].Value)
            .Distinct(StringComparer.Ordinal)
            .ToList();

        Assert.True(keywords.Count >= 60,
            $"keyword extraction looks broken — only found {keywords.Count}");

        var missing = keywords
            .Where(k => !Regex.IsMatch(tm, $@"\b{Regex.Escape(k)}\b"))
            .ToList();

        Assert.True(missing.Count == 0,
            "Keywords in SpekLexer.g4 but absent from spek.tmLanguage.json — " +
            "add them to the appropriate scope so they colorize: " +
            string.Join(", ", missing));
    }

    [Fact]
    public void ShowcaseFixture_StaysCompileValid()
    {
        // The colorization showcase doubles as a language-torture file; keep it
        // honest so eyeballing it after a grammar change means something.
        var root = RepoRoot();
        var path = Path.Combine(root, "tools", "vscode-spek", "examples", "showcase.spek");
        var source = File.ReadAllText(path);

        var parsed = Spek.Compiler.Parser.SpekCompiler.Parse(source);
        Assert.True(parsed.Success,
            "tools/vscode-spek/examples/showcase.spek no longer compiles:\n" +
            string.Join("\n", parsed.Diagnostics.Select(d => $"{d.Code} {d.Message}")));
    }
}
