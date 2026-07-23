using Spek.Persistence;
using Spek.Runtime;
using Spek.Testing;
using Xunit;

namespace Spek.Tests.Runtime;

/// <summary>
/// Failure handling — Resume / Stop directives, dead-letter sink routing for
/// exceptions, unhandled messages, and messages sent to a stopped actor.
/// </summary>
public class SupervisionTests
{
    public record Boom();
    public record Ping();
    public record Count();
    public record CountReply(int N);

    /// <summary>
    /// Default failure directive (Stop). Throws on Boom; counts Pings;
    /// exposes the count via Count.
    /// </summary>
    private sealed class DefaultStopActor : ActorBase
    {
        private int _pings;

        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            _currentSender = sender;
            switch (message)
            {
                case Boom:   throw new InvalidOperationException("boom");
                case Ping:   _pings++; break;
                case Count:  sender.Tell(new CountReply(_pings)); break;
            }
            return Task.CompletedTask;
        }
    }

    /// <summary>Resume directive — skip the bad message and continue.</summary>
    private sealed class ResumeActor : ActorBase
    {
        private int _pings;

        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            _currentSender = sender;
            switch (message)
            {
                case Boom:   throw new InvalidOperationException("boom");
                case Ping:   _pings++; break;
                case Count:  sender.Tell(new CountReply(_pings)); break;
            }
            return Task.CompletedTask;
        }

        protected override FailureDirective OnFailure(Exception exception, object message)
            => FailureDirective.Resume;
    }

    [Fact]
    public async Task Exception_WithDefaultStop_TerminatesActor()
    {
        var sink = new RecordingDeadLetterSink();
        using var system = new TestActorSystem("t", deadLetterSink: sink);

        var actor = system.Spawn<DefaultStopActor>();
        actor.Tell(new Boom());

        await WaitUntil(() => sink.Records.Any(r => r.Cause is not null));

        var failure = sink.Records.Single(r => r.Cause is not null);
        Assert.IsType<Boom>(failure.Message);
        Assert.IsType<InvalidOperationException>(failure.Cause);
        Assert.Contains("stopping", failure.Reason);
    }

    [Fact]
    public async Task AfterStop_FurtherMessagesGoToDeadLetter()
    {
        var sink = new RecordingDeadLetterSink();
        using var system = new TestActorSystem("t", deadLetterSink: sink);

        var actor = system.Spawn<DefaultStopActor>();
        actor.Tell(new Boom());
        await WaitUntil(() => actor.IsStopped);

        // Any message from here on should hit the sink.
        actor.Tell(new Ping());
        actor.Tell(new Ping());

        await WaitUntil(() => sink.Records.Count(r => r.Message is Ping) >= 2);

        var pings = sink.Records.Where(r => r.Message is Ping).ToList();
        Assert.Equal(2, pings.Count);
        Assert.All(pings, r => Assert.Null(r.Cause));
        Assert.All(pings, r => Assert.Contains("stopped", r.Reason));
    }

    [Fact]
    public async Task Resume_SkipsBadMessage_ContinuesProcessing()
    {
        var sink = new RecordingDeadLetterSink();
        using var system = new TestActorSystem("t", deadLetterSink: sink);

        var probe = system.CreateProbe();
        var actor = system.Spawn<ResumeActor>();

        actor.Tell(new Ping());
        actor.Tell(new Boom());   // throws — but Resume continues
        actor.Tell(new Ping());

        probe.Send(actor, new Count());
        var reply = probe.ExpectMsg<CountReply>();

        Assert.Equal(2, reply.N);

        // The Boom was reported, but with a "resuming" reason — not terminal.
        var failure = sink.Records.Single(r => r.Cause is not null);
        Assert.Contains("resuming", failure.Reason);
    }

    /// <summary>Restart directive — replace with fresh instance after failure.</summary>
    private sealed class RestartingActor : ActorBase
    {
        private int _pings;

        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            _currentSender = sender;
            switch (message)
            {
                case Boom:   throw new InvalidOperationException("boom");
                case Ping:   _pings++; break;
                case Count:  sender.Tell(new CountReply(_pings)); break;
            }
            return Task.CompletedTask;
        }

        protected override FailureDirective OnFailure(Exception exception, object message)
            => FailureDirective.Restart;
    }

    /// <summary>Persistent variant — state rides across restart via snapshot.</summary>
    private sealed class PersistentRestartingActor : ActorBase
    {
        private int _pings;

        protected override async Task DispatchAsync(object message, ActorRef sender)
        {
            _currentSender = sender;
            switch (message)
            {
                case Boom:   throw new InvalidOperationException("boom");
                case Ping:   _pings++; await PersistAsync(); break;
                case Count:  sender.Tell(new CountReply(_pings)); break;
            }
        }

        protected override IReadOnlyDictionary<string, object?> CaptureFields() =>
            new Dictionary<string, object?> { ["pings"] = _pings };

        protected override void OnRestore(Snapshot s) => _pings = s.Get<int>("pings");

        protected override FailureDirective OnFailure(Exception exception, object message)
            => FailureDirective.Restart;
    }

    [Fact]
    public async Task Restart_NonPersistent_ResetsInstanceState()
    {
        var sink = new RecordingDeadLetterSink();
        using var system = new TestActorSystem("t", deadLetterSink: sink);

        var probe = system.CreateProbe();
        var actor = system.Spawn<RestartingActor>();

        actor.Tell(new Ping());   // state: 1
        actor.Tell(new Ping());   // state: 2
        actor.Tell(new Boom());   // throws → Restart → fresh instance (state: 0)
        actor.Tell(new Ping());   // state: 1

        probe.Send(actor, new Count());
        var reply = probe.ExpectMsg<CountReply>();

        Assert.Equal(1, reply.N);
        Assert.False(actor.IsStopped);
        Assert.Contains(sink.Records, r => r.Reason.Contains("restarting"));
    }

    [Fact]
    public async Task Restart_Persistent_PreservesStateViaSnapshot()
    {
        var store = new InMemorySnapshotStore();
        var sink = new RecordingDeadLetterSink();
        using var system = new TestActorSystem("t", store, sink);

        var probe = system.CreateProbe();
        var actor = system.SpawnPersistent<PersistentRestartingActor>("p-1");

        actor.Tell(new Ping());   // state: 1, persisted
        actor.Tell(new Ping());   // state: 2, persisted
        actor.Tell(new Boom());   // throws → Restart → state reloads from snapshot (2)
        actor.Tell(new Ping());   // state: 3

        probe.Send(actor, new Count());
        var reply = probe.ExpectMsg<CountReply>();

        Assert.Equal(3, reply.N);
        Assert.False(actor.IsStopped);
    }

    [Fact]
    public async Task Restart_PreservesPendingMailboxMessages()
    {
        var sink = new RecordingDeadLetterSink();
        using var system = new TestActorSystem("t", deadLetterSink: sink);

        var probe = system.CreateProbe();
        var actor = system.Spawn<RestartingActor>();

        actor.Tell(new Boom());   // first message throws
        actor.Tell(new Ping());   // queued; processes after restart
        actor.Tell(new Ping());

        probe.Send(actor, new Count());
        var reply = probe.ExpectMsg<CountReply>();

        Assert.Equal(2, reply.N);
    }

    [Fact]
    public async Task Resume_ActorStaysAliveForFurtherMessages()
    {
        var sink = new RecordingDeadLetterSink();
        using var system = new TestActorSystem("t", deadLetterSink: sink);

        var actor = system.Spawn<ResumeActor>();
        actor.Tell(new Boom());

        await WaitUntil(() => sink.Records.Any(r => r.Cause is not null));

        Assert.False(actor.IsStopped);
    }

    // ─── Retry budget (max-restarts / within-window) ────────────────────────

    /// <summary>
    /// Actor that always restarts but with a hard cap of 3 restarts in any
    /// 10-second window. After the 4th failure, Restart should degrade to Stop.
    /// </summary>
    private sealed class BudgetedActor : ActorBase
    {
        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            _currentSender = sender;
            if (message is Boom) throw new InvalidOperationException("boom");
            return Task.CompletedTask;
        }

        protected override FailureDirective OnFailure(Exception exception, object message)
            => FailureDirective.Restart;

        protected override int MaxRestartsWithinWindow => 3;
        protected override TimeSpan? RestartWindow => TimeSpan.FromSeconds(10);
    }

    [Fact]
    public async Task RestartBudget_ExceededBudget_DegradesToStop()
    {
        var sink = new RecordingDeadLetterSink();
        using var system = new TestActorSystem("t", deadLetterSink: sink);

        var actor = system.Spawn<BudgetedActor>();

        // Four Booms: first 3 should Restart, 4th should Stop.
        actor.Tell(new Boom());
        actor.Tell(new Boom());
        actor.Tell(new Boom());
        actor.Tell(new Boom());

        await WaitUntil(() => actor.IsStopped);

        Assert.Contains(sink.Records, r =>
            r.Reason.Contains("restart budget exceeded", StringComparison.OrdinalIgnoreCase));
    }

    [Fact]
    public async Task RestartBudget_UnderBudget_KeepsRestarting()
    {
        var sink = new RecordingDeadLetterSink();
        using var system = new TestActorSystem("t", deadLetterSink: sink);

        var actor = system.Spawn<BudgetedActor>();

        // Three Booms: should Restart three times, never Stop.
        actor.Tell(new Boom());
        actor.Tell(new Boom());
        actor.Tell(new Boom());

        await WaitUntil(() => sink.Records.Count(r => r.Cause is not null) >= 3);
        Assert.False(actor.IsStopped);
    }

    // ─── Escalate directive + parent OnChildFailure ─────────────────────────

    public record SpawnChild();
    public record TellChild(object Inner);
    public record GetChildStatus();
    public record ChildStatus(bool IsStopped, int FailuresSeen);

    private sealed class EscalatingChild : ActorBase
    {
        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            _currentSender = sender;
            if (message is Boom) throw new InvalidOperationException("boom");
            return Task.CompletedTask;
        }

        protected override FailureDirective OnFailure(Exception exception, object message)
            => FailureDirective.Escalate;
    }

    private sealed class SupervisorParent : ActorBase
    {
        private ActorRef? _child;
        private int _failuresSeen;

        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            _currentSender = sender;
            switch (message)
            {
                case SpawnChild:     _child = SpawnChildAsync<EscalatingChild>(); break;
                case TellChild t:    _child?.Tell(t.Inner); break;
                case GetChildStatus: sender.Tell(new ChildStatus(_child?.IsStopped ?? true, _failuresSeen)); break;
            }
            return Task.CompletedTask;
        }

        protected override FailureDirective OnChildFailure(ActorRef child, Exception cause, object message)
        {
            Interlocked.Increment(ref _failuresSeen);
            return FailureDirective.Stop;
        }
    }

    [Fact]
    public async Task Escalate_ChildFailure_FlowsToParentOnChildFailure_AndStopsChild()
    {
        using var system = new TestActorSystem("t");
        var probe = system.CreateProbe();
        var parent = system.Spawn<SupervisorParent>();

        parent.Tell(new SpawnChild());
        parent.Tell(new TellChild(new Boom()));

        await WaitUntil(async () =>
        {
            probe.Send(parent, new GetChildStatus());
            var s = probe.ExpectMsg<ChildStatus>();
            return s.FailuresSeen == 1 && s.IsStopped;
        });
    }

    private sealed class RootEscalatingActor : ActorBase
    {
        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            _currentSender = sender;
            if (message is Boom) throw new InvalidOperationException("boom");
            return Task.CompletedTask;
        }

        protected override FailureDirective OnFailure(Exception exception, object message)
            => FailureDirective.Escalate;
    }

    [Fact]
    public async Task Escalate_AtRoot_DegradesToStop_WithDeadLetterExplanation()
    {
        var sink = new RecordingDeadLetterSink();
        using var system = new TestActorSystem("t", deadLetterSink: sink);

        var actor = system.Spawn<RootEscalatingActor>();
        actor.Tell(new Boom());

        await WaitUntil(() => actor.IsStopped);
        Assert.Contains(sink.Records, r =>
            r.Reason.Contains("escalate at root", StringComparison.OrdinalIgnoreCase));
    }

    // ─── Helpers ─────────────────────────────────────────────────────────────

    // Generous default so the suite stays green under heavy parallel load
    // (the emit tests run Roslyn compilations that can saturate CPU and
    // delay these wall-clock waits). A genuinely stuck condition still fails,
    // just after the longer budget rather than racing a tight 2s window.
    private static async Task WaitUntil(Func<Task<bool>> predicate, int timeoutMs = 60_000)
    {
        var deadline = DateTime.UtcNow.AddMilliseconds(timeoutMs);
        while (DateTime.UtcNow < deadline)
        {
            if (await predicate()) return;
            await Task.Delay(10);
        }
        throw new TimeoutException($"Condition not met within {timeoutMs}ms.");
    }

    private static async Task WaitUntil(Func<bool> predicate, int timeoutMs = 60_000)
    {
        var deadline = DateTime.UtcNow.AddMilliseconds(timeoutMs);
        while (DateTime.UtcNow < deadline)
        {
            if (predicate()) return;
            await Task.Delay(10);
        }
        throw new TimeoutException($"Condition not met within {timeoutMs}ms.");
    }
}
