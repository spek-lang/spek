using Spek;
using Spek.Runtime;
using Xunit;

namespace Spek.Tests.Runtime;

/// <summary>
/// Red-team V2 (round 2): a crash during RE-materialization — the factory,
/// <c>OnPreStart</c>, or a persistent actor's <c>OnRestore</c> throwing when
/// the dispatch loop rebuilds a dropped instance after a Restart or a
/// passivation-wake — used to escape supervision entirely. EnsureMaterialized
/// ran OUTSIDE the dispatch try, so the throw unwound to DrainGuardedAsync's
/// "dispatch loop crashed; re-arming" catch, which re-armed the loop with
/// <c>_current</c> still null → re-materialize → throw → spin the pool,
/// silently dead-lettering every message: a zombie black hole that bypassed
/// <c>OnFailure</c> and the restart budget. The fix routes re-materialization
/// through the same supervisor chain a handler failure uses (parented actor
/// escalates, root actor stops), bounded by a materialization-retry cap and
/// failing the asker's reply cell (V1). These fences pin that.
/// </summary>
public sealed class RedTeamMaterializationFailureTests
{
    public sealed record Crash();
    public sealed record Ping();
    public sealed record Resp();
    public sealed record SpawnChild();

    /// <summary>
    /// Spawns fine, then throws from <c>OnPreStart</c> on EVERY subsequent
    /// re-materialization. <c>OnFailure</c> returns Restart so a handler crash
    /// clears <c>_current</c> and forces the loop to rebuild — the exact path
    /// that used to hot-loop. (Static counter: the runtime reconstructs the
    /// instance across restarts, so the count must outlive any one instance.
    /// xUnit runs a class's methods serially and no other class references this
    /// type, so it can't race.)
    /// </summary>
    private sealed class RematFailsRoot : ActorBase
    {
        public static int Constructions;

        protected override void OnPreStart()
        {
            if (Interlocked.Increment(ref Constructions) > 1)
                throw new InvalidOperationException("re-materialization boom");
        }

        protected override FailureDirective OnFailure(Exception exception, object message)
            => FailureDirective.Restart;

        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            if (message is Crash)
                throw new InvalidOperationException("handler crash → restart");
            return Task.CompletedTask;
        }
    }

    [Fact]
    public async Task Rematerialization_Crash_On_Root_Stops_The_Slot_Not_Hot_LoopsAsync()
    {
        RematFailsRoot.Constructions = 0;
        var sink = new RecordingDeadLetterSink();
        using var system = new ActorSystem("remat-root", deadLetterSink: sink);

        var actor = system.Spawn<RematFailsRoot>();   // OnPreStart #1 succeeds
        Assert.Equal(1, RematFailsRoot.Constructions);

        actor.Tell(new Crash());   // handler throws → OnFailure Restart → _current = null
        actor.Tell(new Ping());    // rebuild → OnPreStart #2 throws → supervised, not hot-looped

        await WaitUntilAsync(() => actor.IsStopped);

        Assert.True(actor.IsStopped, "the slot must stop, not become a re-materializing zombie");
        // The pre-fix bug reconstructed on every mailbox item forever. Bounded
        // now: spawn (1) + at most MaxMaterializationRetries (3) rebuilds.
        Assert.True(RematFailsRoot.Constructions <= 4,
            $"constructions={RematFailsRoot.Constructions} — a hot-loop would be unbounded");
        Assert.Contains(sink.Records, r =>
            r.Reason.Contains("re-materialization failed") &&
            r.Cause is InvalidOperationException { Message: "re-materialization boom" });
    }

    [Fact]
    public async Task Rematerialization_Crash_Fails_A_Pending_AskAsync()
    {
        RematFailsRoot.Constructions = 0;
        using var system = new ActorSystem("remat-ask");
        var actor = system.Spawn<RematFailsRoot>();

        actor.Tell(new Crash());   // → Restart → _current = null (FIFO: runs before the ask)

        // The ask triggers the rebuild, which throws. Fail-fast (V1) must fault
        // the asker rather than leaving the reply cell to hang forever. A
        // generous window proves the failure is immediate, not a timeout.
        var ex = await Assert.ThrowsAsync<AskException>(() =>
            actor.AskAsync<Resp>(new Ping(), TimeSpan.FromSeconds(5)).AsTask());
        Assert.IsType<InvalidOperationException>(ex.InnerException);
    }

    /// <summary>Child variant: escalates by default (the slot forces Escalate
    /// when <c>_parent</c> is set), so its fate is the parent's to decide.</summary>
    private sealed class RematFailsChild : ActorBase
    {
        public static int Constructions;

        protected override void OnPreStart()
        {
            if (Interlocked.Increment(ref Constructions) > 1)
                throw new InvalidOperationException("child re-materialization boom");
        }

        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            if (message is Crash)
                throw new InvalidOperationException("child handler crash");
            return Task.CompletedTask;
        }
    }

    private sealed class RematParent : ActorBase
    {
        public static readonly List<string> ChildFailures = new();
        private ActorRef? _child;

        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            switch (message)
            {
                case SpawnChild: _child = SpawnChildAsync<RematFailsChild>(); break;
                case Crash:      _child!.Tell(new Crash()); break;
                case Ping:       _child!.Tell(new Ping()); break;
            }
            return Task.CompletedTask;
        }

        protected override FailureDirective OnChildFailure(ActorRef child, Exception cause, object message)
        {
            lock (ChildFailures)
            {
                ChildFailures.Add(cause.Message);
                // 1st failure (the child's handler crash): Restart to clear the
                // child and force a rebuild. 2nd (the rebuild crash, escalated
                // up): Stop to end it.
                return ChildFailures.Count == 1
                    ? FailureDirective.Restart
                    : FailureDirective.Stop;
            }
        }
    }

    [Fact]
    public async Task Rematerialization_Crash_On_Child_Escalates_To_ParentAsync()
    {
        RematFailsChild.Constructions = 0;
        lock (RematParent.ChildFailures) RematParent.ChildFailures.Clear();

        using var system = new ActorSystem("remat-child");
        var parent = system.Spawn<RematParent>();

        parent.Tell(new SpawnChild());
        await WaitUntilAsync(() => Volatile.Read(ref RematFailsChild.Constructions) == 1);

        parent.Tell(new Crash());   // child handler throws → escalate → parent Restart → child _current null
        parent.Tell(new Ping());    // child rebuild → OnPreStart #2 throws → escalate AGAIN to the parent

        await WaitUntilAsync(() => { lock (RematParent.ChildFailures) return RematParent.ChildFailures.Count >= 2; });

        // The SECOND escalation carried the re-materialization exception — proof
        // the rebuild failure entered supervision instead of hot-looping.
        lock (RematParent.ChildFailures)
            Assert.Contains("child re-materialization boom", RematParent.ChildFailures);
    }

    private static async Task WaitUntilAsync(Func<bool> predicate, int timeoutMs = 5000)
    {
        var deadline = DateTime.UtcNow + TimeSpan.FromMilliseconds(timeoutMs);
        while (!predicate() && DateTime.UtcNow < deadline)
            await Task.Delay(15);
        Assert.True(predicate(), "condition not met before timeout");
    }
}
