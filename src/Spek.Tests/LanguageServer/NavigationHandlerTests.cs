using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using Spek.LanguageServer;
using Xunit;

namespace Spek.Tests.LanguageServer;

/// <summary>
/// Coverage for <see cref="SpekDefinitionHandler"/> — go-to-definition from
/// a usage site (message construction, become target, spawn type argument)
/// to the declaration.
/// </summary>
public class DefinitionHandlerTests
{
    private const string Doc = LspTestSupport.EchoDoc;

    private static async Task<Location?> DefinitionAtAsync(Position position)
    {
        var (cache, uri) = LspTestSupport.Open("file:///definition.spek", Doc);
        var handler = new SpekDefinitionHandler(cache);
        var result = await handler.Handle(new DefinitionParams
        {
            TextDocument = new TextDocumentIdentifier { Uri = uri },
            Position     = position,
        }, CancellationToken.None);

        if (result is null) return null;
        var single = Assert.Single(result);
        Assert.True(single.IsLocation);
        Assert.Equal(uri, single.Location!.Uri);
        return single.Location;
    }

    [Fact]
    public async Task MessageUsage_JumpsToMessageDeclarationAsync()
    {
        // Cursor inside "Pong" of `new Pong(p.count)` → the declaration
        // `message Pong(int count);` on (0-based) line 1.
        var loc = await DefinitionAtAsync(LspTestSupport.CursorOn(Doc, "new Pong", "Pong"));

        Assert.NotNull(loc);
        Assert.Equal(1, loc!.Range.Start.Line);
        Assert.Equal(0, loc.Range.Start.Character);
        Assert.Equal(1, loc.Range.End.Line);
        // The target range covers the whole declaration statement.
        Assert.InRange(loc.Range.End.Character, "message Pong".Length, "message Pong(int count);".Length + 1);
    }

    [Fact]
    public async Task BecomeTarget_JumpsToBehaviorDeclarationAsync()
    {
        var loc = await DefinitionAtAsync(LspTestSupport.CursorOn(Doc, "become Busy", "Busy"));

        Assert.NotNull(loc);
        Assert.Equal(LspTestSupport.LineOf(Doc, "behavior Busy"), loc!.Range.Start.Line);
        Assert.Equal(LspTestSupport.LineOf(Doc, "// end-busy"),   loc.Range.End.Line);
    }

    [Fact]
    public async Task SpawnTypeArgument_JumpsToActorDeclarationAsync()
    {
        var loc = await DefinitionAtAsync(LspTestSupport.CursorOn(Doc, "spawn<Echo>", "Echo"));

        Assert.NotNull(loc);
        Assert.Equal(LspTestSupport.LineOf(Doc, "actor Echo"),  loc!.Range.Start.Line);
        Assert.Equal(LspTestSupport.LineOf(Doc, "// end-echo"), loc.Range.End.Line);
    }

    [Fact]
    public async Task BlankLine_ReturnsNullAsync()
    {
        Assert.Null(await DefinitionAtAsync(new Position(2, 0)));
    }

    [Fact]
    public async Task UnopenedDocument_ReturnsNullAsync()
    {
        var handler = new SpekDefinitionHandler(new DocumentCache());
        var result = await handler.Handle(new DefinitionParams
        {
            TextDocument = new TextDocumentIdentifier { Uri = DocumentUri.From("file:///never-opened.spek") },
            Position     = new Position(0, 0),
        }, CancellationToken.None);

        Assert.Null(result);
    }
}

/// <summary>
/// Coverage for <see cref="SpekHoverHandler"/> — hover markdown for message
/// usages, actor references, become targets, and actor fields.
/// </summary>
public class HoverHandlerTests
{
    private const string Doc = LspTestSupport.EchoDoc;

    private static async Task<Hover?> HoverAtAsync(Position position)
    {
        var (cache, uri) = LspTestSupport.Open("file:///hover.spek", Doc);
        var handler = new SpekHoverHandler(cache);
        return await handler.Handle(new HoverParams
        {
            TextDocument = new TextDocumentIdentifier { Uri = uri },
            Position     = position,
        }, CancellationToken.None);
    }

    private static string MarkdownOf(Hover? hover)
    {
        Assert.NotNull(hover);
        Assert.True(hover!.Contents.HasMarkupContent);
        Assert.Equal(MarkupKind.Markdown, hover.Contents.MarkupContent!.Kind);
        var value = hover.Contents.MarkupContent.Value;
        Assert.False(string.IsNullOrWhiteSpace(value));
        return value;
    }

    [Fact]
    public async Task MessageUsage_ShowsMessageSignatureAsync()
    {
        var hover = await HoverAtAsync(LspTestSupport.CursorOn(Doc, "new Pong", "Pong"));
        Assert.Equal("```spek\nmessage Pong(int count)\n```", MarkdownOf(hover));
    }

    [Fact]
    public async Task ActorReferenceInSpawn_ShowsActorSummaryAsync()
    {
        var hover = await HoverAtAsync(LspTestSupport.CursorOn(Doc, "spawn<Echo>", "Echo"));
        var md = MarkdownOf(hover);
        Assert.Contains("actor Echo", md, StringComparison.Ordinal);
        Assert.Contains("2 behaviors, 1 field.", md, StringComparison.Ordinal);
    }

    [Fact]
    public async Task BecomeTarget_ShowsBehaviorAndOwningActorAsync()
    {
        var hover = await HoverAtAsync(LspTestSupport.CursorOn(Doc, "become Busy", "Busy"));
        Assert.Equal("```spek\nbehavior Busy\n```\n\nDefined on actor `Echo`.", MarkdownOf(hover));
    }

    [Fact]
    public async Task FieldUsage_ShowsFieldTypeAndOwnerAsync()
    {
        // Cursor on the right-hand `hits` of `hits = hits + 1;`. A bare
        // identifier's AST span is a point at its first character (see
        // AstBuilder.Span), so the cursor must sit exactly there.
        var hover = await HoverAtAsync(LspTestSupport.CursorAtStartOf(Doc, "hits = hits + 1", "hits + 1"));
        Assert.Equal("```spek\nint hits\n```\n\nField of actor `Echo`.", MarkdownOf(hover));
    }

    [Fact]
    public async Task FieldUsage_MidToken_AlsoShowsFieldHoverAsync()
    {
        var hover = await HoverAtAsync(LspTestSupport.CursorOn(Doc, "hits = hits + 1", "hits + 1"));
        Assert.Equal("```spek\nint hits\n```\n\nField of actor `Echo`.", MarkdownOf(hover));
    }

    [Fact]
    public async Task KeywordAndBlankPositions_YieldNoHoverAsync()
    {
        // The handler resolves declarations, not keywords: hovering the
        // `actor` keyword itself has nothing to show.
        Assert.Null(await HoverAtAsync(LspTestSupport.CursorOn(Doc, "actor Echo", "actor")));
        Assert.Null(await HoverAtAsync(new Position(2, 0)));
    }

    [Fact]
    public async Task UnopenedDocument_ReturnsNullAsync()
    {
        var handler = new SpekHoverHandler(new DocumentCache());
        var result = await handler.Handle(new HoverParams
        {
            TextDocument = new TextDocumentIdentifier { Uri = DocumentUri.From("file:///never-opened.spek") },
            Position     = new Position(0, 0),
        }, CancellationToken.None);

        Assert.Null(result);
    }
}

/// <summary>
/// Coverage for <see cref="SpekReferencesHandler"/> — find-all-references
/// from both the declaration site and a usage site, with and without the
/// declaration included.
/// </summary>
public class ReferencesHandlerTests
{
    private const string Doc = LspTestSupport.PingPongDoc;

    private static async Task<List<Location>?> ReferencesAtAsync(Position position, bool includeDeclaration)
    {
        var (cache, uri) = LspTestSupport.Open("file:///references.spek", Doc);
        var handler = new SpekReferencesHandler(cache);
        var result = await handler.Handle(new ReferenceParams
        {
            TextDocument = new TextDocumentIdentifier { Uri = uri },
            Position     = position,
            Context      = new ReferenceContext { IncludeDeclaration = includeDeclaration },
        }, CancellationToken.None);

        if (result is null) return null;
        var locations = result.ToList();
        Assert.All(locations, l => Assert.Equal(uri, l.Uri));
        return locations;
    }

    [Fact]
    public async Task FromDeclaration_IncludingDeclaration_FindsDeclarationAndAllUsagesAsync()
    {
        var locations = await ReferencesAtAsync(
            LspTestSupport.CursorOn(Doc, "message Ping()", "Ping"), includeDeclaration: true);

        Assert.NotNull(locations);
        Assert.Equal(3, locations!.Count);

        var expectedLines = new[]
        {
            LspTestSupport.LineOf(Doc, "message Ping()"),
            LspTestSupport.LineOf(Doc, "on Ping =>"),
            LspTestSupport.LineOf(Doc, "on Ping p =>"),
        };
        Assert.Equal(
            expectedLines.OrderBy(l => l),
            locations.Select(l => l.Range.Start.Line).OrderBy(l => l));
    }

    [Fact]
    public async Task FromDeclaration_ExcludingDeclaration_FindsUsagesOnlyAsync()
    {
        var locations = await ReferencesAtAsync(
            LspTestSupport.CursorOn(Doc, "message Ping()", "Ping"), includeDeclaration: false);

        Assert.NotNull(locations);
        Assert.Equal(2, locations!.Count);
        Assert.DoesNotContain(locations, l => l.Range.Start.Line == 0);

        // Each usage range starts on the referencing token.
        Assert.All(locations, l =>
            Assert.StartsWith("Ping", LspTestSupport.Normalize(Doc)
                .Split('\n')[l.Range.Start.Line][l.Range.Start.Character..], StringComparison.Ordinal));
    }

    [Fact]
    public async Task UsageRanges_CoverTheFullReferencedNameAsync()
    {
        var locations = await ReferencesAtAsync(
            LspTestSupport.CursorOn(Doc, "message Ping()", "Ping"), includeDeclaration: false);

        Assert.All(locations!, l => Assert.Equal("Ping", LspTestSupport.TextAt(Doc, l.Range)));
    }

    [Fact]
    public async Task FromUsageSite_FindsTheSameUsagesAsync()
    {
        // Start from the `on Ping p` pattern in Caller instead of the decl.
        var locations = await ReferencesAtAsync(
            LspTestSupport.CursorOn(Doc, "on Ping p =>", "Ping"), includeDeclaration: false);

        Assert.NotNull(locations);
        Assert.Equal(2, locations!.Count);
        Assert.Contains(locations, l => l.Range.Start.Line == LspTestSupport.LineOf(Doc, "on Ping =>"));
        Assert.Contains(locations, l => l.Range.Start.Line == LspTestSupport.LineOf(Doc, "on Ping p =>"));
    }

    [Fact]
    public async Task PositionNotOnASymbol_ReturnsNullAsync()
    {
        Assert.Null(await ReferencesAtAsync(new Position(2, 0), includeDeclaration: true));
    }
}

/// <summary>
/// Coverage for <see cref="SpekRenameHandler"/> — renaming a message
/// rewrites the declaration and every usage in one
/// <see cref="WorkspaceEdit"/>.
/// </summary>
public class RenameHandlerTests
{
    private const string Doc = LspTestSupport.PingPongDoc;

    private static async Task<WorkspaceEdit?> RenameAtAsync(Position position, string newName)
    {
        var (cache, uri) = LspTestSupport.Open("file:///rename.spek", Doc);
        var handler = new SpekRenameHandler(cache);
        return await handler.Handle(new RenameParams
        {
            TextDocument = new TextDocumentIdentifier { Uri = uri },
            Position     = position,
            NewName      = newName,
        }, CancellationToken.None);
    }

    private static List<TextEdit> SingleDocumentEdits(WorkspaceEdit? edit)
    {
        Assert.NotNull(edit);
        Assert.NotNull(edit!.Changes);
        var (changedUri, edits) = Assert.Single(edit.Changes!);
        Assert.Equal(DocumentUri.From("file:///rename.spek"), changedUri);
        return edits.ToList();
    }

    [Fact]
    public async Task FromDeclaration_RenamesDeclarationAndAllUsagesAsync()
    {
        var edit = await RenameAtAsync(
            LspTestSupport.CursorOn(Doc, "message Ping()", "Ping"), "Probe");

        var edits = SingleDocumentEdits(edit);
        Assert.Equal(3, edits.Count);
        Assert.All(edits, e => Assert.Equal("Probe", e.NewText));

        var expectedLines = new[]
        {
            LspTestSupport.LineOf(Doc, "message Ping()"),
            LspTestSupport.LineOf(Doc, "on Ping =>"),
            LspTestSupport.LineOf(Doc, "on Ping p =>"),
        };
        Assert.Equal(
            expectedLines.OrderBy(l => l),
            edits.Select(e => e.Range.Start.Line).OrderBy(l => l));
    }

    [Fact]
    public async Task UsageSiteEdits_CoverExactlyTheReferencedNameAsync()
    {
        var edit = await RenameAtAsync(
            LspTestSupport.CursorOn(Doc, "message Ping()", "Ping"), "Probe");

        var usageEdits = SingleDocumentEdits(edit)
            .Where(e => e.Range.Start.Line != 0)
            .ToList();
        Assert.Equal(2, usageEdits.Count);
        Assert.All(usageEdits, e => Assert.Equal("Ping", LspTestSupport.TextAt(Doc, e.Range)));
    }

    [Fact]
    public async Task DeclarationSiteEdit_CoversExactlyTheDeclaredNameAsync()
    {
        // The rename edit at the declaration must replace exactly "Ping" in
        // `message Ping();` — one character more and the rename corrupts the
        // declaration (e.g. swallowing the open paren).
        var edit = await RenameAtAsync(
            LspTestSupport.CursorOn(Doc, "message Ping()", "Ping"), "Probe");

        var declEdit = Assert.Single(SingleDocumentEdits(edit), e => e.Range.Start.Line == 0);
        Assert.Equal("Ping", LspTestSupport.TextAt(Doc, declEdit.Range));
    }

    [Fact]
    public async Task FromUsageSite_ProducesTheSameEditSetAsync()
    {
        static string Key(TextEdit e) =>
            $"{e.Range.Start.Line}:{e.Range.Start.Character}-{e.Range.End.Line}:{e.Range.End.Character}";

        var fromDecl  = SingleDocumentEdits(await RenameAtAsync(
            LspTestSupport.CursorOn(Doc, "message Ping()", "Ping"), "Probe"));
        var fromUsage = SingleDocumentEdits(await RenameAtAsync(
            LspTestSupport.CursorOn(Doc, "on Ping p =>", "Ping"), "Probe"));

        Assert.Equal(
            fromDecl.Select(Key).OrderBy(k => k),
            fromUsage.Select(Key).OrderBy(k => k));
    }

    [Fact]
    public async Task InvalidNewName_ReturnsNullAsync()
    {
        var position = LspTestSupport.CursorOn(Doc, "message Ping()", "Ping");
        Assert.Null(await RenameAtAsync(position, "9starts_with_digit"));
        Assert.Null(await RenameAtAsync(position, "has space"));
        Assert.Null(await RenameAtAsync(position, ""));
    }

    [Fact]
    public async Task PositionNotOnASymbol_ReturnsNullAsync()
    {
        Assert.Null(await RenameAtAsync(new Position(2, 0), "Probe"));
    }

    [Fact]
    public async Task UnopenedDocument_ReturnsNullAsync()
    {
        var handler = new SpekRenameHandler(new DocumentCache());
        var result = await handler.Handle(new RenameParams
        {
            TextDocument = new TextDocumentIdentifier { Uri = DocumentUri.From("file:///never-opened.spek") },
            Position     = new Position(0, 0),
            NewName      = "Probe",
        }, CancellationToken.None);

        Assert.Null(result);
    }
}
