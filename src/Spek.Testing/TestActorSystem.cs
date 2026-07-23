using Spek.Persistence;
using Spek.Runtime;

namespace Spek.Testing;

/// <summary>
/// Thin wrapper over <see cref="ActorSystem"/> with helpers for tests —
/// <see cref="CreateProbe"/> for stand-in actors and <see cref="Spawn{TActor}"/>
/// for the actor under test.
/// </summary>
public sealed class TestActorSystem : IDisposable
{
    private readonly ActorSystem _system;

    /// <summary>
    /// Creates a test system. Pass <paramref name="snapshotStore"/> to share a
    /// persistence backend across test systems — useful for persist/respawn
    /// scenarios where the first system writes and a second reads. Pass
    /// <paramref name="deadLetterSink"/> to assert on dropped / failed
    /// messages (e.g. <see cref="RecordingDeadLetterSink"/>).
    /// </summary>
    public TestActorSystem(
        string name = "test",
        ISnapshotStore? snapshotStore = null,
        IDeadLetterSink? deadLetterSink = null)
    {
        _system = new ActorSystem(name, snapshotStore, deadLetterSink);
    }

    /// <summary>Starts the actor under test and returns its
    /// <see cref="ActorRef"/> — the handle you <c>Tell</c>/<c>Ask</c>
    /// through to drive it. The actor's <c>init</c> has run by the time
    /// this returns.</summary>
    public ActorRef Spawn<TActor>(params object[] args) where TActor : ActorBase
        => _system.Spawn<TActor>(args);

    /// <summary>Non-generic <see cref="Spawn{TActor}"/> — useful when the
    /// actor type is only known at runtime (e.g. dynamically-compiled Spek
    /// code loaded via reflection).</summary>
    public ActorRef Spawn(Type actorType, params object[] args)
        => _system.Spawn(actorType, args);

    /// <summary>Starts a persistent actor bound to
    /// <paramref name="persistenceKey"/>. If the snapshot store already
    /// holds a snapshot for that key, the actor restores from it before
    /// processing messages — combine with a shared
    /// <c>snapshotStore</c> across two test systems to exercise
    /// persist/respawn scenarios.</summary>
    public ActorRef SpawnPersistent<TActor>(string persistenceKey, params object[] args)
        where TActor : ActorBase
        => _system.SpawnPersistent<TActor>(persistenceKey, args);

    /// <summary>Non-generic <see cref="SpawnPersistent{TActor}"/> — useful
    /// when the actor type is only known at runtime.</summary>
    public ActorRef SpawnPersistent(Type actorType, string persistenceKey, params object[] args)
        => _system.SpawnPersistent(actorType, persistenceKey, args);

    /// <summary>Async <see cref="Spawn{TActor}"/> — use when the test
    /// system's <see cref="ISnapshotStore"/> does real I/O, so spawning
    /// doesn't block a pool thread.</summary>
    public Task<ActorRef> SpawnAsync<TActor>(params object[] args) where TActor : ActorBase
        => _system.SpawnAsync<TActor>(args);

    /// <summary>Async <see cref="SpawnPersistent{TActor}"/> — the restore
    /// from the snapshot store happens without blocking a pool
    /// thread.</summary>
    public Task<ActorRef> SpawnPersistentAsync<TActor>(string persistenceKey, params object[] args)
        where TActor : ActorBase
        => _system.SpawnPersistentAsync<TActor>(persistenceKey, args);

    /// <summary>
    /// Spawns a fresh <see cref="TestProbe"/> — a stand-in actor that
    /// captures everything sent to it. Hand its <see cref="TestProbe.Ref"/>
    /// to the code under test wherever an <see cref="ActorRef"/> is
    /// expected, then assert on what arrived with
    /// <see cref="TestProbe.ExpectMsg{T}(TimeSpan?)"/> and friends.
    /// </summary>
    public TestProbe CreateProbe()
    {
        var @ref = _system.Spawn<TestProbeActor>();
        // After Spawn, the slot is materialised — Underlying is non-null.
        var actor = (TestProbeActor)@ref.Underlying!;
        return new TestProbe(actor, @ref);
    }

    // ─── Determinism helpers ─────────────────────────────────────────────
    // Replacements for the Thread.Sleep(N)-and-hope pattern: wait on an
    // observable condition instead of a guessed duration.

    /// <summary>
    /// Blocks until every actor in the system is idle (mailboxes empty,
    /// nothing processing) — "the pipeline has drained". Returns false if
    /// <paramref name="timeout"/> elapses first (default 5s). Signal-driven.
    /// <para>
    /// This parks the calling thread. In an <c>async</c> test prefer
    /// <see cref="WhenIdleAsync"/>, which polls without holding a thread —
    /// under parallel test load, parked threads compete with the actors
    /// that need them to drain (and the pool is slow to grow), so blocking
    /// waits are the main source of flakiness. <c>WaitForIdle</c> stays for
    /// the synchronous <c>program Main</c> case.
    /// </para>
    /// </summary>
    public bool WaitForIdle(TimeSpan? timeout = null) =>
        _system.AwaitTermination(timeout ?? TimeSpan.FromSeconds(5));

    /// <summary>
    /// Awaits quiescence without parking a thread: polls the non-blocking
    /// <see cref="ActorSystem.IsIdle"/> with <c>await Task.Delay</c> between
    /// checks, so the pool thread is free for the actors to make progress.
    /// Throws <see cref="TimeoutException"/> if the system isn't idle within
    /// <paramref name="timeout"/> (default 10s).
    /// </summary>
    public Task WhenIdleAsync(TimeSpan? timeout = null) =>
        WaitUntilAsync(() => _system.IsIdle, timeout ?? TimeSpan.FromSeconds(10), "system idle");

    /// <summary>
    /// Polls <paramref name="condition"/> until it holds, then returns.
    /// Throws <see cref="TimeoutException"/> (naming
    /// <paramref name="description"/>) if it never does within
    /// <paramref name="timeout"/> (default 5s) — a diagnosable failure
    /// instead of a silently-too-short sleep.
    /// </summary>
    public static async Task WaitUntilAsync(
        Func<bool> condition,
        TimeSpan? timeout = null,
        string description = "condition",
        TimeSpan? pollInterval = null)
    {
        var deadline = DateTime.UtcNow + (timeout ?? TimeSpan.FromSeconds(5));
        var interval = pollInterval ?? TimeSpan.FromMilliseconds(10);
        while (!condition())
        {
            if (DateTime.UtcNow >= deadline)
                throw new TimeoutException(
                    $"Condition not met within {(timeout ?? TimeSpan.FromSeconds(5)).TotalMilliseconds:0}ms: {description}");
            await Task.Delay(interval);
        }
    }

    // ─── Supervision observability ───────────────────────────────────────
    // Assert on what supervision did to an actor, as plain method calls
    // (no matcher DSL). The async ones auto-await under invisible async, so a
    // test body writes `sys.ExpectRestart(worker);`.

    /// <summary>How many times <paramref name="actor"/> has been restarted by
    /// supervision (0 if it was stopped, or never failed).</summary>
    public int RestartCountOf(ActorRef actor) => actor.Slot?.RestartCount ?? 0;

    /// <summary>Awaits until <paramref name="actor"/> has restarted at least
    /// <paramref name="count"/> times (default 1); throws <see cref="TimeoutException"/>
    /// otherwise. Pairs with an explicit Restart strategy — the default directive
    /// is Stop, so a crash without supervision stops the actor instead.</summary>
    public Task ExpectRestart(ActorRef actor, int count = 1, TimeSpan? timeout = null) =>
        WaitUntilAsync(() => RestartCountOf(actor) >= count, timeout, $"actor to restart x{count}");

    /// <summary>Awaits until <paramref name="actor"/> is stopped; throws
    /// <see cref="TimeoutException"/> otherwise.</summary>
    public Task ExpectStop(ActorRef actor, TimeSpan? timeout = null) =>
        WaitUntilAsync(() => actor.IsStopped, timeout, "actor to stop");

    /// <summary>Tears down the wrapped <see cref="ActorSystem"/> and every
    /// actor in it. Call at the end of each test so state never leaks
    /// across tests — in a native Spek <c>…Tests</c> container the runner
    /// disposes <see cref="TestActorSystem"/> fields for you.</summary>
    public void Dispose() => _system.Dispose();
}
