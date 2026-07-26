using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using Spek.Compiler.Parser;
using Spek.LanguageServer;
using Xunit;

namespace Spek.Tests.LanguageServer;

/// <summary>
/// Coverage for <see cref="SpekCodeActionHandler"/>'s quick-fix suggestions.
/// Drives the handler directly (skipping the LSP-server JSON-RPC layer)
/// to assert what edits would land in the editor's lightbulb menu.
/// </summary>
public class CodeActionTests
{
    private static SpekCodeActionHandler BuildHandler(string sourceUri, string source)
    {
        var cache = new DocumentCache();
        var uri   = DocumentUri.From(sourceUri);
        cache.Update(uri, source);
        return new SpekCodeActionHandler(cache);
    }

    private static async Task<List<CodeAction>> RequestActionsAsync(
        SpekCodeActionHandler handler, string sourceUri, IEnumerable<Diagnostic> diagnostics,
        OmniSharp.Extensions.LanguageServer.Protocol.Models.Range range)
    {
        var uri = DocumentUri.From(sourceUri);
        var result = await handler.Handle(new CodeActionParams
        {
            TextDocument = new TextDocumentIdentifier { Uri = uri },
            Range        = range,
            Context      = new CodeActionContext
            {
                Diagnostics = new Container<Diagnostic>(diagnostics),
            },
        }, CancellationToken.None);

        return (result ?? Array.Empty<CommandOrCodeAction>().AsEnumerable())
            .Select(a => a.CodeAction)
            .Where(a => a is not null)
            .Select(a => a!)
            .ToList();
    }

    [Fact]
    public async Task CE0011_SuggestsClosestBehaviorNameAsync()
    {
        // `become Bsy` is a typo for `become Busy`. The fixer should offer
        // "Rename to 'Busy'" as a CodeAction.
        const string src = """
            message Tick();
            actor Toggle
            {
                behavior Idle { on Tick => { become Bsy; } }
                behavior Busy { on Tick => { become Idle; } }
            }
            """;
        var handler = BuildHandler("file:///typo.spek", src);

        // Manufacture a CE0011 diagnostic at `become Bsy;` (line 4 in 1-based, col ~33).
        var diag = new Diagnostic
        {
            Code     = new DiagnosticCode("CE0011"),
            Message  = "Become target 'Bsy' is not a declared behavior.",
            Range    = new OmniSharp.Extensions.LanguageServer.Protocol.Models.Range(
                new Position(line: 3, character: 33),
                new Position(line: 3, character: 41)),
            Severity = DiagnosticSeverity.Error,
        };

        var actions = await RequestActionsAsync(handler, "file:///typo.spek", new[] { diag }, diag.Range);

        // At least one action suggesting "Busy" should be present.
        Assert.NotEmpty(actions);
        Assert.Contains(actions, a =>
            a.Title is not null && a.Title.Contains("Busy", StringComparison.Ordinal));
    }

    [Fact]
    public async Task CE0091_OffersToDeclareMissingChannelAsync()
    {
        const string src = """
            message Ping();
            actor Foo : NoSuchChannel
            {
                behavior Idle { on Ping => { } }
            }
            """;
        var handler = BuildHandler("file:///missing.spek", src);

        var diag = new Diagnostic
        {
            Code     = new DiagnosticCode("CE0091"),
            Message  = "Unknown channel or base actor 'NoSuchChannel' in actor 'Foo'.",
            Range    = new OmniSharp.Extensions.LanguageServer.Protocol.Models.Range(
                new Position(line: 1, character: 13),
                new Position(line: 1, character: 26)),
            Severity = DiagnosticSeverity.Error,
        };

        var actions = await RequestActionsAsync(handler, "file:///missing.spek", new[] { diag }, diag.Range);

        Assert.NotEmpty(actions);
        Assert.Contains(actions, a =>
            a.Title is not null && a.Title.Contains("NoSuchChannel", StringComparison.Ordinal));
    }

    [Fact]
    public async Task CE0083_ThreadSleep_OffersTaskDelayQuickFixAsync()
    {
        // ReSharper-style: a blocking `Thread.Sleep(100)` gets a one-click
        // rewrite to the non-blocking `Task.Delay(100)` (which invisible async
        // awaits). The fix replaces just the `Thread.Sleep` identifier.
        const string src = """
            message Tick();
            actor A { behavior Idle { on Tick => { Thread.Sleep(100); } } }
            """;
        var handler = BuildHandler("file:///sleep.spek", src);

        var diag = new Diagnostic
        {
            Code     = new DiagnosticCode("CE0083"),
            Message  = "'Thread.Sleep' blocks the dispatcher thread; use 'await Task.Delay(...)' or model the wait as a delayed message instead.",
            Range    = new OmniSharp.Extensions.LanguageServer.Protocol.Models.Range(
                new Position(line: 1, character: 39),
                new Position(line: 1, character: 56)),
            Severity = DiagnosticSeverity.Error,
        };

        var actions = await RequestActionsAsync(handler, "file:///sleep.spek", new[] { diag }, diag.Range);

        var fix = Assert.Single(actions, a =>
            a.Title is not null && a.Title.Contains("Task.Delay", StringComparison.Ordinal));
        var edit = fix.Edit!.Changes!.Values.First().First();
        Assert.Equal("System.Threading.Tasks.Task.Delay", edit.NewText);
        // Replaces exactly the 12-char `Thread.Sleep`, keeping `(100)`.
        Assert.Equal(diag.Range.Start, edit.Range.Start);
        Assert.Equal(diag.Range.Start.Character + "Thread.Sleep".Length, edit.Range.End.Character);
    }

    [Fact]
    public async Task CE0083_TaskWaitAll_OffersWhenAllQuickFixAsync()
    {
        const string src = """
            message Tick();
            actor A { behavior Idle { on Tick => Task.WaitAll(a, b); } }
            """;
        var handler = BuildHandler("file:///waitall.spek", src);

        var diag = new Diagnostic
        {
            Code     = new DiagnosticCode("CE0083"),
            Message  = "'Task.WaitAll' synchronously waits on tasks; use 'await Task.WhenAll(...)' instead.",
            Range    = new OmniSharp.Extensions.LanguageServer.Protocol.Models.Range(
                new Position(line: 1, character: 37),
                new Position(line: 1, character: 53)),
            Severity = DiagnosticSeverity.Error,
        };

        var actions = await RequestActionsAsync(handler, "file:///waitall.spek", new[] { diag }, diag.Range);

        var fix = Assert.Single(actions, a =>
            a.Title is not null && a.Title.Contains("Task.WhenAll", StringComparison.Ordinal));
        var edit = fix.Edit!.Changes!.Values.First().First();
        Assert.Equal("Task.WhenAll", edit.NewText);
        Assert.Equal(diag.Range.Start.Character + "Task.WaitAll".Length, edit.Range.End.Character);
    }

    [Fact]
    public async Task CE0115_SyncFileRead_OffersAsyncSiblingQuickFixAsync()
    {
        // Synchronous `File.ReadAllText` → `File.ReadAllTextAsync` by inserting
        // the `Async` suffix right after the method name (args left intact).
        const string src = """
            message Load(string path);
            actor A { behavior Idle { on Load => File.ReadAllText(path); } }
            """;
        var handler = BuildHandler("file:///io.spek", src);

        var diag = new Diagnostic
        {
            Code     = new DiagnosticCode("CE0115"),
            Message  = "'File.ReadAllText' blocks the dispatcher on I/O; use 'File.ReadAllTextAsync(...)' (awaited automatically) instead.",
            Range    = new OmniSharp.Extensions.LanguageServer.Protocol.Models.Range(
                new Position(line: 1, character: 38),
                new Position(line: 1, character: 54)),
            Severity = DiagnosticSeverity.Warning,
        };

        var actions = await RequestActionsAsync(handler, "file:///io.spek", new[] { diag }, diag.Range);

        var fix = Assert.Single(actions, a =>
            a.Title is not null && a.Title.Contains("ReadAllTextAsync", StringComparison.Ordinal));
        var edit = fix.Edit!.Changes!.Values.First().First();
        Assert.Equal("Async", edit.NewText);
        // Zero-width insert right after the 16-char `File.ReadAllText`.
        var insertCol = diag.Range.Start.Character + "File.ReadAllText".Length;
        Assert.Equal(insertCol, edit.Range.Start.Character);
        Assert.Equal(insertCol, edit.Range.End.Character);
    }
}
