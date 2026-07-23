using System.Text.Json;
using Spek.Cli;
using Spek.Runtime;
using Xunit;

namespace Spek.Tests.Cli;

/// <summary>
/// Rendering for <c>spekc observe</c> — the table and the per-actor
/// drill-down, driven directly with canned snapshots (the EventPipe
/// transport is exercised by the runtime's EventSource tests).
/// </summary>
public sealed class ObserveRenderTests
{
    private static ActorSnapshot Snap(
        string path, string? behavior = "Active", int mailbox = 0,
        int restarts = 0, string? lastMsg = "Ping", bool stopped = false,
        string[]? head = null, string[]? children = null) =>
        new(path, "Kitchen", behavior, mailbox, head ?? [], restarts, lastMsg,
            DateTimeOffset.UnixEpoch, IsMaterialized: !stopped, IsStopped: stopped,
            children ?? []);

    [Fact]
    public void Table_RendersOneRowPerActor_WithLifecycleMarkers()
    {
        var text = ObserveCommand.Render("myapp",
            [Snap("Kitchen", mailbox: 1204), Snap("Doubler#3", restarts: 2, stopped: true)],
            actorFilter: null, now: DateTimeOffset.UnixEpoch);

        Assert.Contains("[myapp]", text);
        Assert.Contains("Kitchen", text);
        Assert.Contains("1204", text);
        Assert.Contains("Doubler#3", text);
        Assert.Contains("(stopped)", text);
    }

    [Fact]
    public void DrillDown_ShowsHeadTypeCounts_AndChildren()
    {
        var snap = Snap("Kitchen", mailbox: 3,
            head: ["Ping", "Ping", "Deposit"],
            children: ["Doubler#1", "Doubler#2"]);
        var text = ObserveCommand.Render("myapp", [snap],
            actorFilter: "Kitchen", now: DateTimeOffset.UnixEpoch.AddHours(2));

        Assert.Contains("up 02:00:00", text);
        Assert.Contains("Ping x2", text);
        Assert.Contains("Deposit x1", text);
        Assert.Contains("Doubler#1, Doubler#2", text);
    }

    [Fact]
    public void DrillDown_UnknownActor_ListsKnownPaths()
    {
        var text = ObserveCommand.Render("myapp", [Snap("Kitchen")],
            actorFilter: "Nope", now: DateTimeOffset.UnixEpoch);
        Assert.Contains("no actor 'Nope'", text);
        Assert.Contains("Kitchen", text);
    }

    // ─── --json mode ──────────────────────────────────────────────────────

    [Fact]
    public void Json_FullTable_IsOneParseableLine_WithSystemAndSnapshotFields()
    {
        var line = ObserveCommand.RenderJson("myapp",
            [Snap("Kitchen", mailbox: 1204, head: ["Ping", "Deposit"]),
             Snap("Doubler#3", restarts: 2, stopped: true)],
            actorFilter: null);

        // NDJSON contract: exactly one line per sample, no prose.
        Assert.DoesNotContain('\n', line);

        using var doc = JsonDocument.Parse(line);
        var root = doc.RootElement;
        Assert.Equal("myapp", root.GetProperty("system").GetString());

        var actors = root.GetProperty("actors");
        Assert.Equal(2, actors.GetArrayLength());

        // Snapshot members ride the same default System.Text.Json contract
        // as the EventSource wire — PascalCase, all eleven fields present.
        var kitchen = actors[0];
        Assert.Equal("Kitchen", kitchen.GetProperty("Path").GetString());
        Assert.Equal("Kitchen", kitchen.GetProperty("ActorType").GetString());
        Assert.Equal("Active", kitchen.GetProperty("Behavior").GetString());
        Assert.Equal(1204, kitchen.GetProperty("MailboxDepth").GetInt32());
        Assert.Equal(2, kitchen.GetProperty("MailboxHead").GetArrayLength());
        Assert.Equal(0, kitchen.GetProperty("Restarts").GetInt32());
        Assert.Equal("Ping", kitchen.GetProperty("LastMessageType").GetString());
        Assert.Equal(DateTimeOffset.UnixEpoch,
            kitchen.GetProperty("SpawnedAt").GetDateTimeOffset());
        Assert.True(kitchen.GetProperty("IsMaterialized").GetBoolean());
        Assert.False(kitchen.GetProperty("IsStopped").GetBoolean());
        Assert.Equal(0, kitchen.GetProperty("Children").GetArrayLength());

        Assert.True(actors[1].GetProperty("IsStopped").GetBoolean());
    }

    [Fact]
    public void Json_ActorFilter_NarrowsActorsToTheOneMatch()
    {
        var line = ObserveCommand.RenderJson("myapp",
            [Snap("Kitchen"), Snap("Doubler#3")], actorFilter: "Doubler#3");

        using var doc = JsonDocument.Parse(line);
        var actors = doc.RootElement.GetProperty("actors");
        Assert.Equal(1, actors.GetArrayLength());
        Assert.Equal("Doubler#3", actors[0].GetProperty("Path").GetString());
        Assert.Equal("myapp", doc.RootElement.GetProperty("system").GetString());
    }

    [Fact]
    public void Json_UnknownActor_EmitsErrorObject_NotProse()
    {
        var line = ObserveCommand.RenderJson("myapp",
            [Snap("Kitchen")], actorFilter: "Nope");

        Assert.DoesNotContain('\n', line);
        using var doc = JsonDocument.Parse(line);
        var error = doc.RootElement.GetProperty("error").GetString();
        Assert.Contains("no actor 'Nope'", error);
        Assert.Contains("Kitchen", error);   // known paths ride along for repair
        Assert.False(doc.RootElement.TryGetProperty("actors", out _));
    }

    [Fact]
    public void JsonError_QuotesInMessage_StayValidJson()
    {
        var line = ObserveCommand.RenderJsonError("""cannot attach — pipe said "no".""");
        using var doc = JsonDocument.Parse(line);
        Assert.Contains("\"no\"", doc.RootElement.GetProperty("error").GetString());
    }

    [Fact]
    public void ObserveUsage_MentionsJsonFlag()
    {
        var stdout = new StringWriter();
        var stderr = new StringWriter();
        var exit = CliRunner.Run(["observe"], stdout, stderr);

        Assert.Equal(1, exit);
        Assert.Contains("--json", stderr.ToString());
        Assert.True(string.IsNullOrEmpty(stdout.ToString()),
            $"usage must not go to stdout: {stdout}");
    }
}
