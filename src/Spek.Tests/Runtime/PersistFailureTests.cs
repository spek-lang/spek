using Spek.Persistence;
using Spek.Runtime;
using Xunit;

namespace Spek.Tests.Runtime;

/// <summary>
/// The failure half of the restart-time persist (ActorSlot.TryPersist):
/// when supervision resolves a crash to Restart, the runtime snapshots the
/// failing instance BEFORE dropping it, and that save can itself fail. The
/// pinned contract:
/// <list type="bullet">
///   <item>A snapshot store that throws on Save does NOT escalate — the
///         failure is swallowed, reported to the dead-letter sink as
///         <c>"persist-before-restart threw"</c> with the store's exception
///         as the cause, and the restart proceeds anyway.</item>
///   <item>The restarted instance rehydrates from whatever the store still
///         holds — nothing, when the save failed and no earlier snapshot
///         exists — so the pre-crash in-memory state is lost. Lossy, loud,
///         alive: the actor keeps serving.</item>
///   <item>Contrast: with a healthy store the same restart preserves the
///         pre-crash state (the happy half of TryPersist).</item>
/// </list>
/// </summary>
public class PersistFailureTests
{
    private static readonly TimeSpan AskTimeout = TimeSpan.FromSeconds(15);

    // ─── messages ────────────────────────────────────────────────────────────
    public record Bump();
    public record Boom();
    public record Get();
    public record Reply(int N);

    /// <summary>Persistent counter that opts into Restart supervision, so a
    /// crash routes through the restart-time persist rather than Stop.</summary>
    private sealed class RestartingCounter : ActorBase
    {
        private int _n;

        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            _currentSender = sender;
            switch (message)
            {
                case Bump: _n++; break;
                case Boom: throw new ApplicationException("handler boom");
                case Get:  sender.Tell(new Reply(_n)); break;
            }
            return Task.CompletedTask;
        }

        protected override FailureDirective OnFailure(Exception exception, object message)
            => FailureDirective.Restart;

        protected override IReadOnlyDictionary<string, object?> CaptureFields() =>
            new Dictionary<string, object?> { ["n"] = _n };

        protected override void OnRestore(Snapshot s) => _n = s.Get<int>("n");
    }

    /// <summary>Store whose Save always fails; Load answers from whatever an
    /// inner in-memory store managed to record (nothing, unless pre-seeded).</summary>
    private sealed class SaveThrowsStore : ISnapshotStore
    {
        private readonly InMemorySnapshotStore _inner = new();
        public int SaveAttempts;

        public Task SaveAsync(string key, Snapshot snapshot)
        {
            Interlocked.Increment(ref SaveAttempts);
            throw new IOException("save boom");
        }

        public Task<Snapshot?> LoadAsync(string key) => _inner.LoadAsync(key);
    }

    [Fact]
    public async Task RestartPersist_StoreSaveThrows_DeadLetters_RestartProceeds_StateIsLostAsync()
    {
        var sink = new RecordingDeadLetterSink();
        var store = new SaveThrowsStore();
        using var system = new ActorSystem("persist-fail", store, sink);
        var actor = system.SpawnPersistent<RestartingCounter>("rc-fail");

        // Build up in-memory state, then crash. FIFO ordering makes the
        // sequence deterministic: Bump, Bump, Boom, then the Get below.
        actor.Tell(new Bump());
        actor.Tell(new Bump());
        actor.Tell(new Boom());

        // The restart proceeded: the actor is alive and serving. The failed
        // save means nothing was restored — the two bumps are gone. Lossy,
        // loud, alive.
        var reply = await actor.AskAsync<Reply>(new Get(), AskTimeout);
        Assert.Equal(0, reply.N);
        Assert.False(actor.IsStopped);
        Assert.True(store.SaveAttempts >= 1, "the restart path must have attempted the save");

        // The failure was reported, not raised: one record for the handler
        // crash itself, one for the failed restart-time persist carrying the
        // store's exception as the cause.
        Assert.Contains(sink.Records, r =>
            r.Reason.Contains("handler threw; restarting") && r.Cause is ApplicationException);

        var persistRecord = sink.Records.Single(r =>
            r.Reason.Contains("persist-before-restart threw"));
        Assert.IsType<IOException>(persistRecord.Cause);
        Assert.Equal("save boom", persistRecord.Cause!.Message);

        // Swallow-and-continue means exactly that: no Stop followed. (The
        // "restart budget exceeded" degradation would show up here too.)
        Assert.DoesNotContain(sink.Records, r => r.Reason.Contains("stopping actor"));
    }

    [Fact]
    public async Task RestartPersist_HealthyStore_PreservesPreCrashStateAcrossTheRestartAsync()
    {
        // The contrast case — same actor, working store: TryPersist snapshots
        // the failing instance before it's dropped, and the rebuilt instance
        // restores the pre-crash count.
        var sink = new RecordingDeadLetterSink();
        using var system = new ActorSystem("persist-ok", new InMemorySnapshotStore(), sink);
        var actor = system.SpawnPersistent<RestartingCounter>("rc-ok");

        actor.Tell(new Bump());
        actor.Tell(new Bump());
        actor.Tell(new Boom());

        var reply = await actor.AskAsync<Reply>(new Get(), AskTimeout);
        Assert.Equal(2, reply.N);
        Assert.False(actor.IsStopped);
        Assert.DoesNotContain(sink.Records, r =>
            r.Reason.Contains("persist-before-restart threw"));
    }
}
