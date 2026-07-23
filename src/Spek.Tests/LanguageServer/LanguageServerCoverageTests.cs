using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using Spek.Compiler.Parser;
using Spek.LanguageServer;
using Xunit;
using LspRange = OmniSharp.Extensions.LanguageServer.Protocol.Models.Range;

namespace Spek.Tests.LanguageServer;

/// <summary>
/// Coverage for the language-server feature handlers that the existing
/// per-file suites leave under-tested:
///
/// <list type="bullet">
///   <item>The <b>live diagnostic pipeline</b> — existing CodeAction tests
///         hand-build synthetic <see cref="Diagnostic"/> objects with
///         carefully-placed ranges. These tests instead run real Spek
///         source through <see cref="DocumentCache"/> (which runs full
///         semantic analysis), map the resulting diagnostic with
///         <see cref="DiagnosticMapper"/>, and feed it to the
///         <see cref="SpekCodeActionHandler"/> — the same flow the editor
///         drives. This is where the quick-fix range math meets the spans
///         the analyzer actually emits.</item>
///   <item><b>Completion handler request→response</b> — existing tests only
///         exercise <see cref="Spek.LanguageServer.CompletionContext.Collect"/>; here we drive
///         <see cref="SpekCompletionHandler.Handle(CompletionParams,
///         System.Threading.CancellationToken)"/> end to end.</item>
///   <item><b>CodeAction handler edge cases</b> — empty / null diagnostics,
///         unmatched CE codes, and the instance-shaped CE0083 that
///         intentionally gets no fix.</item>
/// </list>
///
/// Three scenarios once surfaced real production bugs in the quick-fix logic
/// (two range-math bugs on live CE0083/CE0115 spans, one wrong-name bug on the
/// CE0093 channel fix). Those are fixed now; these tests stand as regression
/// coverage, each noted inline.
/// </summary>
public class LanguageServerCoverageTests
{
    // ----- shared helpers ---------------------------------------------------

    private static (DocumentCache Cache, DocumentUri Uri) Live(string sourceUri, string source)
    {
        var cache = new DocumentCache();
        var uri   = DocumentUri.From(sourceUri);
        cache.Update(uri, source);   // runs SpekCompiler.Parse → full semantic analysis
        return (cache, uri);
    }

    /// <summary>Maps every cached diagnostic to LSP form, the way
    /// <c>SpekTextDocumentHandler.RefreshDocument</c> does.</summary>
    private static IReadOnlyList<Diagnostic> LiveLspDiagnostics(DocumentCache cache, DocumentUri uri)
    {
        var entry = cache.Get(uri);
        Assert.NotNull(entry);
        return entry!.Diagnostics.Select(DiagnosticMapper.ToLsp).ToList();
    }

    private static async Task<List<CodeAction>> RequestActionsAsync(
        SpekCodeActionHandler handler, DocumentUri uri,
        IEnumerable<Diagnostic> diagnostics, LspRange range)
    {
        var result = await handler.Handle(new CodeActionParams
        {
            TextDocument = new TextDocumentIdentifier { Uri = uri },
            Range        = range,
            Context      = new CodeActionContext { Diagnostics = new Container<Diagnostic>(diagnostics) },
        }, CancellationToken.None);

        return (result ?? Array.Empty<CommandOrCodeAction>().AsEnumerable())
            .Select(a => a.CodeAction)
            .Where(a => a is not null)
            .Select(a => a!)
            .ToList();
    }

    // ======================================================================
    //  Live diagnostic pipeline — DocumentCache + DiagnosticMapper
    // ======================================================================

    [Fact]
    public void DocumentCache_RunsSemanticAnalysis_AndSurfacesCe0083()
    {
        // The cache feeds the live-squiggle path. Confirm it actually runs
        // the semantic pass (not just a parse) so blocking-call diagnostics
        // reach the editor.
        const string src = """
            message Tick();
            actor A { behavior Idle { on Tick => { Thread.Sleep(100); } } }
            """;
        var (cache, uri) = Live("file:///live-ce0083.spek", src);

        var entry = cache.Get(uri);
        Assert.NotNull(entry);
        Assert.NotNull(entry!.Tree);                               // parse succeeded
        Assert.Contains(entry.Diagnostics, d => d.Code == "CE0083");
    }

    [Fact]
    public void DiagnosticMapper_MapsLiveCe0115_AsWarningSquiggle()
    {
        // CE0115 (sync I/O with an async sibling) is emitted at Warning
        // severity by the analyzer. The whole live chain — analysis →
        // DiagnosticMapper — must preserve that as a warning squiggle, not
        // an error one.
        const string src = """
            message Load(string path);
            actor A { behavior Idle { on Load p => { File.ReadAllText(p.path); } } }
            """;
        var (cache, uri) = Live("file:///live-ce0115.spek", src);

        var lspDiags = LiveLspDiagnostics(cache, uri);
        var ce0115 = Assert.Single(lspDiags, d => d.Code?.String == "CE0115");
        Assert.Equal(DiagnosticSeverity.Warning, ce0115.Severity);
        Assert.Equal("spek", ce0115.Source);
        // Parse still succeeds — a warning does not block compilation.
        Assert.NotNull(cache.Get(uri)!.Tree);
    }

    [Fact]
    public void DiagnosticMapper_MapsLiveSyntaxError_AsErrorSquiggle()
    {
        // A lexer/parser failure (CE0001) leaves no tree but still produces
        // an error-severity squiggle the editor can render.
        const string src = "actor A { behavior Idle { on Tick => {  ";  // unterminated
        var (cache, uri) = Live("file:///live-syntax.spek", src);

        var entry = cache.Get(uri);
        Assert.NotNull(entry);
        Assert.Null(entry!.Tree);                                  // parse failed → no tree
        var lspDiags = LiveLspDiagnostics(cache, uri);
        Assert.NotEmpty(lspDiags);
        Assert.All(lspDiags, d => Assert.Equal(DiagnosticSeverity.Error, d.Severity));
    }

    // ======================================================================
    //  Live pipeline → CodeAction — the flow the editor actually drives
    // ======================================================================

    [Fact]
    public async Task LivePipeline_Ce0011_OffersClosestBehaviorRenameAsync()
    {
        // `become Bsy;` is a typo for `Busy`. Running the *real* analyzer
        // (which positions CE0011 on the BecomeStmt span) and feeding that
        // real diagnostic to the handler exercises PositionResolver.FindChain
        // over genuine spans — the failure mode synthetic tests can't catch.
        const string src = """
            message Tick();
            actor Toggle
            {
                behavior Idle { on Tick => { become Bsy;  } }
                behavior Busy { on Tick => { become Idle; } }
            }
            """;
        var (cache, uri) = Live("file:///live-ce0011.spek", src);
        var lspDiags = LiveLspDiagnostics(cache, uri);
        var ce0011 = Assert.Single(lspDiags, d => d.Code?.String == "CE0011");

        var handler = new SpekCodeActionHandler(cache);
        var actions = await RequestActionsAsync(handler, uri, new[] { ce0011 }, ce0011.Range);

        Assert.Contains(actions, a =>
            a.Title is not null && a.Title.Contains("Busy", StringComparison.Ordinal));
        // The rename edit re-emits a complete `become Busy;` statement.
        var fix = actions.First(a => a.Title!.Contains("Busy", StringComparison.Ordinal));
        var edit = fix.Edit!.Changes!.Values.First().First();
        Assert.Equal("become Busy;", edit.NewText);
    }

    [Fact]
    public async Task LivePipeline_Ce0091_DeclaresTheActuallyMissingChannelAsync()
    {
        // CE0091's message quotes the missing name FIRST
        // ("Unknown channel or base actor 'NoSuchChannel' …"), so the
        // first-quote extraction in BuildMissingChannelFixes picks the
        // right name. End-to-end through the real analyzer.
        const string src = """
            message Ping();
            actor Foo : NoSuchChannel { behavior Idle { on Ping => { } } }
            """;
        var (cache, uri) = Live("file:///live-ce0091.spek", src);
        var lspDiags = LiveLspDiagnostics(cache, uri);
        var ce0091 = Assert.Single(lspDiags, d => d.Code?.String == "CE0091");

        var handler = new SpekCodeActionHandler(cache);
        var actions = await RequestActionsAsync(handler, uri, new[] { ce0091 }, ce0091.Range);

        var fix = Assert.Single(actions);
        Assert.Equal("Declare 'channel NoSuchChannel'", fix.Title);
        var edit = fix.Edit!.Changes!.Values.First().First();
        Assert.Contains("channel NoSuchChannel", edit.NewText, StringComparison.Ordinal);
    }

    // Fixed: BuildMissingChannelFixes now extracts the LAST quoted token (the
    // missing base channel), not the first (the inheriting channel).
    [Fact]
    public async Task LivePipeline_Ce0093_DeclaresMissingBaseChannelAsync()
    {
        const string src = """
            message Ping();
            channel ServerHost : NoSuchBase { }
            actor S : ServerHost { behavior Idle { on Ping => { } } }
            """;
        var (cache, uri) = Live("file:///live-ce0093.spek", src);
        var lspDiags = LiveLspDiagnostics(cache, uri);
        var ce0093 = Assert.Single(lspDiags, d => d.Code?.String == "CE0093");

        var handler = new SpekCodeActionHandler(cache);
        var actions = await RequestActionsAsync(handler, uri, new[] { ce0093 }, ce0093.Range);

        // The fix must declare the MISSING base channel, NoSuchBase —
        // not the already-declared inheriting channel ServerHost.
        var fix = Assert.Single(actions);
        Assert.Equal("Declare 'channel NoSuchBase'", fix.Title);
        var edit = fix.Edit!.Changes!.Values.First().First();
        Assert.Contains("channel NoSuchBase", edit.NewText, StringComparison.Ordinal);
    }

    // Fixed: CE0083 now spans from the call receiver (Thread), not the '.', so the
    // quick-fix replaces exactly "Thread.Sleep".
    [Fact]
    public async Task LivePipeline_Ce0083_ThreadSleep_ReplacesExactlyTheCallAsync()
    {
        const string src = """
            message Tick();
            actor A { behavior Idle { on Tick => { Thread.Sleep(100); } } }
            """;
        var (cache, uri) = Live("file:///live-ce0083-fix.spek", src);
        var lspDiags = LiveLspDiagnostics(cache, uri);
        var ce0083 = Assert.Single(lspDiags, d => d.Code?.String == "CE0083");

        var handler = new SpekCodeActionHandler(cache);
        var actions = await RequestActionsAsync(handler, uri, new[] { ce0083 }, ce0083.Range);

        var fix = Assert.Single(actions, a =>
            a.Title is not null && a.Title.Contains("Task.Delay", StringComparison.Ordinal));
        var edit = fix.Edit!.Changes!.Values.First().First();
        Assert.Equal("System.Threading.Tasks.Task.Delay", edit.NewText);

        // The replaced range must cover exactly "Thread.Sleep" in the source.
        var srcLine = src.Split('\n')[edit.Range.Start.Line];
        var replaced = srcLine.Substring(
            edit.Range.Start.Character,
            edit.Range.End.Character - edit.Range.Start.Character);
        Assert.Equal("Thread.Sleep", replaced);
    }

    // Fixed: CE0115 now spans from the call receiver (File), so the quick-fix
    // inserts 'Async' right after "File.ReadAllText".
    [Fact]
    public async Task LivePipeline_Ce0115_FileRead_InsertsAsyncAfterMethodNameAsync()
    {
        const string src = """
            message Load(string path);
            actor A { behavior Idle { on Load p => { File.ReadAllText(p.path); } } }
            """;
        var (cache, uri) = Live("file:///live-ce0115-fix.spek", src);
        var lspDiags = LiveLspDiagnostics(cache, uri);
        var ce0115 = Assert.Single(lspDiags, d => d.Code?.String == "CE0115");

        var handler = new SpekCodeActionHandler(cache);
        var actions = await RequestActionsAsync(handler, uri, new[] { ce0115 }, ce0115.Range);

        var fix = Assert.Single(actions);
        var edit = fix.Edit!.Changes!.Values.First().First();
        Assert.Equal("Async", edit.NewText);

        // The zero-width insert must land right after "File.ReadAllText".
        var srcLine = src.Split('\n')[edit.Range.Start.Line];
        var prefixEnd = srcLine.IndexOf("File.ReadAllText", StringComparison.Ordinal)
                        + "File.ReadAllText".Length;
        Assert.Equal(prefixEnd, edit.Range.Start.Character);
        Assert.Equal(edit.Range.Start, edit.Range.End);  // zero-width insert
    }

    // ======================================================================
    //  CodeAction handler — edge cases / branch coverage
    // ======================================================================

    [Fact]
    public async Task CodeAction_InstanceShapedCe0083_OffersNoFixAsync()
    {
        // The instance-shaped blocking call `tasks.WaitAny()` produces a
        // CE0083 whose message starts with '.WaitAny()' (leading dot). The
        // rewrite table is keyed on bare static names like "Task.WaitAll",
        // so the dotted-instance name matches nothing → no quick-fix. This
        // guards against an accidental rewrite that would drop the receiver.
        const string src = """
            message Go();
            actor A { behavior Idle { on Go => { tasks.WaitAny(); } } }
            """;
        var (cache, uri) = Live("file:///instance-waitany.spek", src);
        var lspDiags = LiveLspDiagnostics(cache, uri);
        var ce0083 = Assert.Single(lspDiags, d => d.Code?.String == "CE0083");
        Assert.StartsWith("'.WaitAny()'", ce0083.Message, StringComparison.Ordinal);

        var handler = new SpekCodeActionHandler(cache);
        var actions = await RequestActionsAsync(handler, uri, new[] { ce0083 }, ce0083.Range);

        Assert.Empty(actions);
    }

    [Fact]
    public async Task CodeAction_UnknownCeCode_ProducesNoActionsAsync()
    {
        const string src = """
            message Ping();
            actor A { behavior Idle { on Ping => { } } }
            """;
        var (cache, uri) = Live("file:///unknown-code.spek", src);

        // A diagnostic the handler has no fixer for (e.g. CE0014 unused
        // behavior) must yield zero — but a non-null — action container.
        var diag = new Diagnostic
        {
            Code     = new DiagnosticCode("CE0014"),
            Message  = "Behavior 'X' is declared but never reached via 'become'.",
            Range    = new LspRange(new Position(1, 0), new Position(1, 5)),
            Severity = DiagnosticSeverity.Warning,
        };
        var handler = new SpekCodeActionHandler(cache);
        var actions = await RequestActionsAsync(handler, uri, new[] { diag }, diag.Range);

        Assert.Empty(actions);
    }

    [Fact]
    public async Task CodeAction_EmptyDiagnostics_ReturnsEmptyNonNullContainerAsync()
    {
        const string src = "message Ping();\nactor A { behavior Idle { on Ping => { } } }";
        var (cache, uri) = Live("file:///empty-diags.spek", src);

        var handler = new SpekCodeActionHandler(cache);
        var result = await handler.Handle(new CodeActionParams
        {
            TextDocument = new TextDocumentIdentifier { Uri = uri },
            Range        = new LspRange(new Position(0, 0), new Position(0, 0)),
            Context      = new CodeActionContext { Diagnostics = new Container<Diagnostic>() },
        }, CancellationToken.None);

        Assert.NotNull(result);          // empty container, not null
        Assert.Empty(result!);
    }

    [Fact]
    public async Task CodeAction_UnknownDocument_ReturnsNullAsync()
    {
        // No document was cached for this URI → the handler bails out with
        // null (nothing to resolve a fix against).
        var handler = new SpekCodeActionHandler(new DocumentCache());
        var uri = DocumentUri.From("file:///never-opened.spek");

        var result = await handler.Handle(new CodeActionParams
        {
            TextDocument = new TextDocumentIdentifier { Uri = uri },
            Range        = new LspRange(new Position(0, 0), new Position(0, 1)),
            Context      = new CodeActionContext
            {
                Diagnostics = new Container<Diagnostic>(new Diagnostic
                {
                    Code  = new DiagnosticCode("CE0011"),
                    Range = new LspRange(new Position(0, 0), new Position(0, 1)),
                }),
            },
        }, CancellationToken.None);

        Assert.Null(result);
    }

    // ======================================================================
    //  Completion handler — request → response (not just CompletionContext)
    // ======================================================================

    [Fact]
    public async Task Completion_MergesKeywordsAndScopedSymbolsAsync()
    {
        // Cursor inside the `on Ping p => { }` handler body. The response
        // must blend the static keyword set with the in-scope symbols the
        // CompletionContext collects (the `p` binding, the `Ping` message
        // type) for this position.
        const string src = "message Ping();\nactor A { behavior Idle { on Ping p => {  } } }";
        var (cache, uri) = Live("file:///completion.spek", src);
        var handler = new SpekCompletionHandler(cache);

        // 0-based line 1; the body's interior space is around char 41.
        var col = src.Split('\n')[1].IndexOf("{  }", StringComparison.Ordinal) + 2;
        var list = await handler.Handle(new CompletionParams
        {
            TextDocument = new TextDocumentIdentifier { Uri = uri },
            Position     = new Position(1, col),
        }, CancellationToken.None);

        var items = list.Items.ToList();
        // Static keywords are always present.
        Assert.Contains(items, i => i.Label == "actor"  && i.Kind == CompletionItemKind.Keyword);
        Assert.Contains(items, i => i.Label == "become" && i.Kind == CompletionItemKind.Keyword);
        // Scoped symbols layered on for this cursor position.
        Assert.Contains(items, i => i.Label == "p"    && i.Kind == CompletionItemKind.Variable);
        Assert.Contains(items, i => i.Label == "Ping" && i.Kind == CompletionItemKind.Struct);
        // More than the bare keyword list (scoped symbols were appended).
        Assert.True(items.Count > SpekCompletionHandler.Keywords.Length);
    }

    [Fact]
    public async Task Completion_OnUnparseableDocument_StillReturnsKeywordsAsync()
    {
        // A document that fails to parse has a null Tree, so no scoped
        // symbols are collected — but the static keyword list must still be
        // offered so completion never goes dark mid-edit.
        const string src = "actor A { behavior Idle { on Tick => {  ";  // unterminated
        var (cache, uri) = Live("file:///broken-completion.spek", src);
        Assert.Null(cache.Get(uri)!.Tree);   // confirm the precondition

        var handler = new SpekCompletionHandler(cache);
        var list = await handler.Handle(new CompletionParams
        {
            TextDocument = new TextDocumentIdentifier { Uri = uri },
            Position     = new Position(0, 5),
        }, CancellationToken.None);

        var items = list.Items.ToList();
        Assert.Equal(SpekCompletionHandler.Keywords.Length, items.Count);
        Assert.All(items, i => Assert.Equal(CompletionItemKind.Keyword, i.Kind));
        Assert.Contains(items, i => i.Label == "behavior");
    }

    [Fact]
    public async Task Completion_OnUnknownDocument_ReturnsKeywordsOnlyAsync()
    {
        // No cached document at all → fall back to the keyword list only,
        // no exception.
        var handler = new SpekCompletionHandler(new DocumentCache());
        var list = await handler.Handle(new CompletionParams
        {
            TextDocument = new TextDocumentIdentifier
            {
                Uri = DocumentUri.From("file:///does-not-exist.spek"),
            },
            Position = new Position(0, 0),
        }, CancellationToken.None);

        Assert.Equal(SpekCompletionHandler.Keywords.Length, list.Items.Count());
    }
}
