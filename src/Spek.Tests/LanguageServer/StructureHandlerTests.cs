using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using Spek.Compiler.Format;
using Spek.LanguageServer;
using Xunit;

namespace Spek.Tests.LanguageServer;

/// <summary>
/// Coverage for <see cref="SpekDocumentSymbolHandler"/> — the outline tree:
/// top-level messages/actors/programs, and inside an actor its fields,
/// passivate clause, init, behaviors (with their handlers as children), and
/// lifecycle hooks.
/// </summary>
public class DocumentSymbolHandlerTests
{
    private const string Doc = """
        message Ping(int count);
        message Pong(int count);

        actor Echo
        {
            int hits = 0;

            passivate after System.TimeSpan.FromMinutes(30);

            init() { become Idle; }

            behavior Idle
            {
                on Ping p =>
                {
                    become Busy;
                }
            }

            behavior Busy
            {
                on Ping => { become Idle; }
                on Pong => { }
            }

            on PostStop => { }
        } // end-echo

        program Main
        {
            var system = new ActorSystem("Demo");
            system.AwaitTermination();
        }
        """;

    private static async Task<List<DocumentSymbol>> SymbolsAsync(string source)
    {
        var (cache, uri) = LspTestSupport.Open("file:///symbols.spek", source);
        var handler = new SpekDocumentSymbolHandler(cache);
        var result = await handler.Handle(new DocumentSymbolParams
        {
            TextDocument = new TextDocumentIdentifier { Uri = uri },
        }, CancellationToken.None);

        Assert.NotNull(result);
        return result!.Select(item =>
        {
            Assert.True(item.IsDocumentSymbol);
            return item.DocumentSymbol!;
        }).ToList();
    }

    [Fact]
    public async Task TopLevel_ListsMessagesActorAndProgramAsync()
    {
        var symbols = await SymbolsAsync(Doc);

        Assert.Equal(new[] { "Ping", "Pong", "Echo", "Main" }, symbols.Select(s => s.Name));
        Assert.Equal(
            new[] { SymbolKind.Struct, SymbolKind.Struct, SymbolKind.Class, SymbolKind.Function },
            symbols.Select(s => s.Kind));

        Assert.Equal("(int count)", symbols[0].Detail);
        Assert.Equal("program entry point", symbols[3].Detail);

        // The actor's range spans its whole body.
        var echo = symbols[2];
        Assert.Equal(LspTestSupport.LineOf(Doc, "actor Echo"),  echo.Range.Start.Line);
        Assert.Equal(LspTestSupport.LineOf(Doc, "// end-echo"), echo.Range.End.Line);
    }

    [Fact]
    public async Task Actor_Children_CoverFieldPassivateInitBehaviorsAndLifecycleAsync()
    {
        var symbols = await SymbolsAsync(Doc);
        var children = symbols.Single(s => s.Name == "Echo").Children!.ToList();

        Assert.Equal(6, children.Count);

        Assert.Equal("hits", children[0].Name);
        Assert.Equal(SymbolKind.Field, children[0].Kind);
        Assert.Equal("int", children[0].Detail);

        Assert.StartsWith("passivate after", children[1].Name, StringComparison.Ordinal);
        Assert.Equal(SymbolKind.Property, children[1].Kind);
        Assert.Equal("idle unload", children[1].Detail);

        Assert.Equal("init", children[2].Name);
        Assert.Equal(SymbolKind.Constructor, children[2].Kind);
        Assert.Equal("()", children[2].Detail);

        Assert.Equal("Idle", children[3].Name);
        Assert.Equal(SymbolKind.Method, children[3].Kind);
        Assert.Equal("1 handler", children[3].Detail);

        Assert.Equal("Busy", children[4].Name);
        Assert.Equal("2 handlers", children[4].Detail);

        Assert.Equal("on PostStop", children[5].Name);
        Assert.Equal(SymbolKind.Event, children[5].Kind);
        Assert.Equal("lifecycle", children[5].Detail);
    }

    [Fact]
    public async Task Behaviors_ListTheirHandlersAsChildrenAsync()
    {
        var symbols = await SymbolsAsync(Doc);
        var behaviors = symbols.Single(s => s.Name == "Echo").Children!
            .Where(c => c.Kind == SymbolKind.Method)
            .ToDictionary(c => c.Name, c => c.Children!.ToList());

        var idleHandler = Assert.Single(behaviors["Idle"]);
        Assert.Equal("on Ping p", idleHandler.Name);
        Assert.Equal(SymbolKind.Event, idleHandler.Kind);
        Assert.Equal(LspTestSupport.LineOf(Doc, "on Ping p =>"), idleHandler.Range.Start.Line);

        Assert.Equal(new[] { "on Ping", "on Pong" }, behaviors["Busy"].Select(h => h.Name));
        Assert.All(behaviors["Busy"], h => Assert.Equal(SymbolKind.Event, h.Kind));
    }

    [Fact]
    public async Task UnparseableDocument_ReturnsNullAsync()
    {
        var (cache, uri) = LspTestSupport.Open("file:///broken-symbols.spek",
            "actor A { behavior Idle { on Tick => {  ");   // unterminated
        var handler = new SpekDocumentSymbolHandler(cache);

        var result = await handler.Handle(new DocumentSymbolParams
        {
            TextDocument = new TextDocumentIdentifier { Uri = uri },
        }, CancellationToken.None);

        Assert.Null(result);
    }
}

/// <summary>
/// Coverage for <see cref="SpekFoldingHandler"/> — folding ranges for
/// multi-line actor / behavior / handler bodies; single-line declarations
/// produce no fold.
/// </summary>
public class FoldingHandlerTests
{
    private const string Doc = """
        message Ping(int count);
        message Pong(int count);

        actor Echo
        {
            behavior Idle
            {
                on Ping p =>
                {
                    become Busy;
                } // end-handler
            } // end-idle

            behavior Busy
            {
                on Ping => { become Idle; }
                on Pong => { }
            } // end-busy
        } // end-echo
        """;

    private static async Task<List<FoldingRange>?> FoldsAsync(string source)
    {
        var (cache, uri) = LspTestSupport.Open("file:///folding.spek", source);
        var handler = new SpekFoldingHandler(cache);
        var result = await handler.Handle(new FoldingRangeRequestParam
        {
            TextDocument = new TextDocumentIdentifier { Uri = uri },
        }, CancellationToken.None);
        return result?.ToList();
    }

    [Fact]
    public async Task ActorBehaviorAndHandlerBodies_AllFoldAsync()
    {
        var folds = await FoldsAsync(Doc);
        Assert.NotNull(folds);

        (int Start, int End) Expected(string startMarker, string endMarker) =>
            (LspTestSupport.LineOf(Doc, startMarker), LspTestSupport.LineOf(Doc, endMarker));

        var expected = new[]
        {
            Expected("actor Echo",    "// end-echo"),
            Expected("behavior Idle", "// end-idle"),
            Expected("on Ping p =>",  "// end-handler"),
            Expected("behavior Busy", "// end-busy"),
        };

        var actual = folds!.Select(f => ((int)f.StartLine, (int)f.EndLine)).ToList();
        foreach (var pair in expected)
            Assert.Contains(pair, actual);

        // Nothing else folds: the single-line message declarations and the
        // single-line handlers in Busy produce no ranges.
        Assert.Equal(expected.Length, folds.Count);
        Assert.All(folds, f => Assert.Equal(FoldingRangeKind.Region, f.Kind));
    }

    [Fact]
    public async Task UnparseableDocument_ReturnsNullAsync()
    {
        Assert.Null(await FoldsAsync("actor A { behavior Idle { on Tick => {  "));
    }
}

/// <summary>
/// Coverage for <see cref="SpekFormattingHandler"/> — a mis-formatted
/// document gets one full-document edit whose text is exactly what
/// <c>spekc format</c> would print; a canonical document gets no edits.
/// </summary>
public class FormattingHandlerTests
{
    private const string Misformatted = """
        message Ping();
        actor   A
        {
        behavior   Idle
        {
        on Ping  =>  {  }
        }
        }
        """;

    private static async Task<TextEditContainer?> FormatAsync(string source)
    {
        var (cache, uri) = LspTestSupport.Open("file:///formatting.spek", source);
        var handler = new SpekFormattingHandler(cache);
        return await handler.Handle(new DocumentFormattingParams
        {
            TextDocument = new TextDocumentIdentifier { Uri = uri },
            Options      = new FormattingOptions(),
        }, CancellationToken.None);
    }

    [Fact]
    public async Task MisformattedDocument_GetsOneFullDocumentReplacementAsync()
    {
        var source = LspTestSupport.Normalize(Misformatted);
        var result = await FormatAsync(source);

        Assert.NotNull(result);
        var edit = Assert.Single(result!);

        // The replacement range covers the whole buffer.
        Assert.Equal(new Position(0, 0), edit.Range.Start);
        Assert.Equal(source.Count(c => c == '\n'), edit.Range.End.Line);
        Assert.Equal(source.Length - (source.LastIndexOf('\n') + 1), edit.Range.End.Character);

        // Canonical indentation: behavior one level in, handler two.
        Assert.Contains("\n    behavior Idle", edit.NewText, StringComparison.Ordinal);
        Assert.Contains("\n        on Ping", edit.NewText, StringComparison.Ordinal);
        Assert.StartsWith("message Ping();", edit.NewText, StringComparison.Ordinal);

        // Applying the edit yields a canonical document — formatting again
        // produces no further edits.
        Assert.Null(await FormatAsync(edit.NewText));
    }

    [Fact]
    public async Task Edit_MatchesSpekcFormatCliOutputAsync()
    {
        var source = LspTestSupport.Normalize(Misformatted);
        var result = await FormatAsync(source);
        var edit = Assert.Single(result!);

        // `spekc format <file>` prints the formatted source to stdout; the
        // LSP edit must produce byte-identical text.
        var tmp = Path.Combine(Path.GetTempPath(), $"spek-lsp-format-{Guid.NewGuid():N}.spek");
        try
        {
            await File.WriteAllTextAsync(tmp, source);
            var stdout = new StringWriter();
            var stderr = new StringWriter();
            var exitCode = Spek.Cli.CliRunner.Run(new[] { "format", tmp }, stdout, stderr);

            Assert.Equal(0, exitCode);
            Assert.Equal(stdout.ToString(), edit.NewText);
        }
        finally
        {
            File.Delete(tmp);
        }
    }

    [Fact]
    public async Task CanonicalDocument_GetsNoEditsAsync()
    {
        var canonical = SpekFormatter.Format(LspTestSupport.Normalize(Misformatted));
        Assert.Null(await FormatAsync(canonical));
    }

    [Fact]
    public async Task UnopenedDocument_ReturnsNullAsync()
    {
        var handler = new SpekFormattingHandler(new DocumentCache());
        var result = await handler.Handle(new DocumentFormattingParams
        {
            TextDocument = new TextDocumentIdentifier { Uri = DocumentUri.From("file:///never-opened.spek") },
            Options      = new FormattingOptions(),
        }, CancellationToken.None);

        Assert.Null(result);
    }
}

/// <summary>
/// Coverage for <see cref="SpekSemanticTokensHandler"/> — the token stream
/// that colours identifiers by their resolved declaration kind: actor →
/// class, message → struct, channel → interface, enum → enum, module →
/// namespace. Undeclared identifiers get no token.
/// </summary>
public class SemanticTokensHandlerTests
{
    private const string Doc = """
        message Ping();

        enum Color { Red, Blue }

        channel Host { on Ping; }

        module MathUtil
        {
            public int Twice(int x) { return x * 2; }
        }

        actor Echo
        {
            behavior Idle
            {
                on Ping => { var e = spawn<Echo>(); }
            }
        }
        """;

    private sealed record DecodedToken(int Line, int Char, int Length, SemanticTokenType Type);

    private static async Task<List<DecodedToken>> TokensAsync(string source)
    {
        var (cache, uri) = LspTestSupport.Open("file:///semantic-tokens.spek", source);
        var handler = new SpekSemanticTokensHandler(cache);
        var result = await handler.Handle(new SemanticTokensParams
        {
            TextDocument = new TextDocumentIdentifier { Uri = uri },
        }, CancellationToken.None);

        Assert.NotNull(result);
        var data = result!.Data.ToArray();
        Assert.Equal(0, data.Length % 5);

        // Decode LSP delta encoding: (deltaLine, deltaChar, length, type, modifiers).
        var legend = SemanticTokenType.Defaults.ToArray();
        var tokens = new List<DecodedToken>();
        int line = 0, ch = 0;
        for (var i = 0; i < data.Length; i += 5)
        {
            line += data[i];
            ch = data[i] == 0 ? ch + data[i + 1] : data[i + 1];
            tokens.Add(new DecodedToken(line, ch, data[i + 2], legend[data[i + 3]]));
            Assert.Equal(0, data[i + 4]);   // no modifiers are emitted
        }
        return tokens;
    }

    private static DecodedToken ExpectedAt(string lineContains, string token, SemanticTokenType type)
    {
        var line = LspTestSupport.LineOf(Doc, lineContains);
        var ch = LspTestSupport.Normalize(Doc).Split('\n')[line].IndexOf(token, StringComparison.Ordinal);
        return new DecodedToken(line, ch, token.Length, type);
    }

    [Fact]
    public async Task DeclaredTypeNames_GetKindSpecificTokensAsync()
    {
        var tokens = await TokensAsync(Doc);
        Assert.NotEmpty(tokens);

        Assert.Contains(ExpectedAt("message Ping()", "Ping", SemanticTokenType.Struct),    tokens);
        Assert.Contains(ExpectedAt("enum Color",     "Color", SemanticTokenType.Enum),      tokens);
        Assert.Contains(ExpectedAt("channel Host",   "Host", SemanticTokenType.Interface),  tokens);
        Assert.Contains(ExpectedAt("module MathUtil", "MathUtil", SemanticTokenType.Namespace), tokens);
        Assert.Contains(ExpectedAt("actor Echo",     "Echo", SemanticTokenType.Class),      tokens);
    }

    [Fact]
    public async Task UsageSites_AreColouredLikeTheirDeclarationsAsync()
    {
        var tokens = await TokensAsync(Doc);

        // `on Ping` in the handler and `spawn<Echo>` both resolve.
        Assert.Contains(ExpectedAt("on Ping => {", "Ping", SemanticTokenType.Struct), tokens);
        Assert.Contains(ExpectedAt("spawn<Echo>", "Echo", SemanticTokenType.Class),   tokens);
    }

    [Fact]
    public async Task UndeclaredIdentifiers_GetNoTokenAsync()
    {
        var tokens = await TokensAsync(Doc);

        // Enum members, parameters, and locals are left to the TextMate grammar.
        var enumLine = LspTestSupport.LineOf(Doc, "enum Color");
        var redChar = LspTestSupport.Normalize(Doc).Split('\n')[enumLine].IndexOf("Red", StringComparison.Ordinal);
        Assert.DoesNotContain(tokens, t => t.Line == enumLine && t.Char == redChar);

        var behaviorLine = LspTestSupport.LineOf(Doc, "behavior Idle");
        Assert.DoesNotContain(tokens, t => t.Line == behaviorLine);
    }

    [Fact]
    public async Task TokenStream_IsInSourceOrderAsync()
    {
        var tokens = await TokensAsync(Doc);
        for (var i = 1; i < tokens.Count; i++)
        {
            var ordered = tokens[i].Line > tokens[i - 1].Line
                || (tokens[i].Line == tokens[i - 1].Line && tokens[i].Char > tokens[i - 1].Char);
            Assert.True(ordered, $"token {i} out of order: {tokens[i - 1]} then {tokens[i]}");
        }
    }

    [Fact]
    public async Task UnparseableDocument_YieldsAnEmptyTokenStreamAsync()
    {
        var tokens = await TokensAsync("actor A { behavior Idle { on Tick => {  ");
        Assert.Empty(tokens);
    }
}
