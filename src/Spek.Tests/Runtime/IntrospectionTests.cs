using Spek;
using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Spek.Runtime;
using Spek.Testing;
using Spek.Tests.Emit;
using Xunit;

namespace Spek.Tests.Runtime;

/// <summary>
/// Live introspection: <see cref="ActorSystem.SnapshotActors"/> is the
/// read-only, non-perturbing view behind <c>spekc observe</c> — metadata
/// only, actor field contents never included.
/// </summary>
public sealed class IntrospectionTests
{
    private sealed record Ping(int N);
    private sealed record Block();

    private sealed class Sink : ActorBase
    {
        protected override Task DispatchAsync(object message, ActorRef sender)
            => Task.CompletedTask;
    }

    private sealed class Blocking : ActorBase
    {
        public static SemaphoreSlim Gate = new(0);
        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            if (message is Block) Gate.Wait();
            return Task.CompletedTask;
        }
        protected override string? CurrentBehaviorName => "Jammed";
    }

    [Fact]
    public void Snapshot_ListsSpawnedActor_WithIdentityAndLifecycleFacts()
    {
        using var system = new ActorSystem("introspect-basic");
        var before = system.Clock.GetUtcNow();
        system.Spawn<Sink>();

        var snap = Assert.Single(system.SnapshotActors());
        Assert.Equal("Sink", snap.Path);
        Assert.Equal("Sink", snap.ActorType);
        Assert.Equal(0, snap.MailboxDepth);
        Assert.Equal(0, snap.Restarts);
        Assert.True(snap.IsMaterialized);
        Assert.False(snap.IsStopped);
        Assert.Null(snap.LastMessageType);        // nothing dispatched yet
        Assert.True(snap.SpawnedAt >= before);
    }

    [Fact]
    public void AnonymousActors_GetStableInstanceNames()
    {
        using var system = new ActorSystem("introspect-names");
        system.Spawn<Sink>();
        system.Spawn<Sink>();
        system.Spawn<Sink>();

        var paths = system.SnapshotActors().Select(s => s.Path).ToArray();
        Assert.Equal(new[] { "Sink", "Sink#2", "Sink#3" }, paths);
    }

    [Fact]
    public void PersistentActor_UsesItsKeyAsPath()
    {
        using var system = new ActorSystem("introspect-key");
        system.SpawnPersistent<Sink>("ledger-eu-1");

        var snap = Assert.Single(system.SnapshotActors());
        Assert.Equal("ledger-eu-1", snap.Path);
    }

    [Fact]
    public async Task WedgedActor_ShowsItsBacklog_HeadTypes_AndBehaviorAsync()
    {
        Blocking.Gate = new SemaphoreSlim(0);
        using var system = new ActorSystem("introspect-wedged");
        var actor = system.Spawn<Blocking>();

        actor.Tell(new Block());                       // wedges the actor
        for (int i = 0; i < 5; i++) actor.Tell(new Ping(i));

        // Wedged = the Block is being dispatched (LastMessageType set) while
        // the pings pile up behind it.
        await TestActorSystem.WaitUntilAsync(() =>
        {
            var s = system.SnapshotActors()[0];
            return s.MailboxDepth >= 5 && s.LastMessageType == "Block";
        }, description: "wedged with visible backlog");

        var snap = Assert.Single(system.SnapshotActors());
        Assert.True(snap.MailboxDepth >= 5);
        Assert.Contains("Ping", snap.MailboxHead);     // head types name the traffic
        Assert.Equal("Jammed", snap.Behavior);         // hand-written override plumbs through
        Assert.Equal("Block", snap.LastMessageType);   // the message it's stuck on
        Blocking.Gate.Release(10);
    }

    [Fact]
    public async Task LastMessageType_TracksTheMostRecentDispatchAsync()
    {
        using var system = new ActorSystem("introspect-last");
        var actor = system.Spawn<Sink>();
        actor.Tell(new Ping(1));
        await TestActorSystem.WaitUntilAsync(() =>
            system.SnapshotActors()[0].LastMessageType == "Ping",
            description: "last message type recorded");
    }

    // ─── Generated actors carry the behavior-name override ──────────────────

    [Fact]
    public void EmittedActor_OverridesCurrentBehaviorName()
    {
        const string src = """
            namespace X;
            message Go();
            actor Machine
            {
                init() { become Fast; }
                behavior Fast { on Go g => { become Slow; } }
                behavior Slow { on Go g => { become Fast; } }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.True(parsed.Success,
            string.Join("\n", parsed.Diagnostics.Select(d => $"{d.Code} {d.Message}")));
        var code = new FileEmitter().Emit(parsed.Tree!);
        Assert.Contains("CurrentBehaviorName", code);
        Assert.Contains("_HandleAsync", code);

        var (ok, summary, _) = RoslynCompileHelper.TryCompile(code, "BehaviorNameEmit");
        Assert.True(ok, summary);
    }
}
