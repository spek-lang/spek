using Spek;
using Spek.Runtime;
using Spek.Testing;
using Xunit;

namespace Spek.Tests.Runtime;

/// <summary>
/// Targeted coverage for supervision directives the existing suite under-tests:
/// parent-driven <see cref="FailureDirective.Resume"/> (state preserved, same
/// instance), multi-level escalation through a grandparent → parent → child
/// hierarchy (and the depth-cap degrade-to-Stop), and the per-child restart
/// budget exposed via <see cref="ActorBase.ApplyRestartPolicy"/>.
///
/// All actors are hand-written C# (sealed : ActorBase, override DispatchAsync)
/// for precise control of the failure path — the runtime forces a child WITH a
/// parent onto the Escalate path regardless of the child's own OnFailure, so
/// the parent's OnChildFailure is the steering wheel for every scenario here.
///
/// State and liveness are observed through reply messages (Spek.Tests is not in
/// Spek.Runtime's InternalsVisibleTo list, so ActorRef.Underlying is off-limits);
/// ActorRef.IsStopped is public and read directly.
/// </summary>
public class SupervisionDirectiveTests
{
    // ─── Shared message vocabulary ───────────────────────────────────────────

    public record Crash();
    public record GetCount();
    public record CountReply(int N);

    public record SpawnChild();
    public record TellChild(object Inner);
    public record GetChildRef();
    public record ChildRefReply(ActorRef Child);

    // ─────────────────────────────────────────────────────────────────────────
    // Scenario 1 — Parent-driven RESUME: child mutates in-memory state, then
    // throws. The parent's OnChildFailure returns Resume; the slot's Resume path
    // skips the bad message WITHOUT nulling _current, so the same instance —
    // and its mutated counter — survives, and the child stays alive.
    // ─────────────────────────────────────────────────────────────────────────

    /// <summary>
    /// Increments a counter on every Crash *before* throwing, so a preserved
    /// instance reveals itself by a non-reset count. GetCount replies with the
    /// live counter to whatever sender asked.
    /// </summary>
    private sealed class CountingCrashChild : ActorBase
    {
        private int _counter;

        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            _currentSender = sender;
            switch (message)
            {
                case Crash:
                    _counter++;                                   // mutate, THEN throw
                    throw new InvalidOperationException("boom after mutate");
                case GetCount:
                    sender.Tell(new CountReply(_counter));
                    break;
            }
            return Task.CompletedTask;
        }
    }

    private sealed class ResumingParent : ActorBase
    {
        private ActorRef? _child;

        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            _currentSender = sender;
            switch (message)
            {
                case SpawnChild:   _child = SpawnChildAsync<CountingCrashChild>(); break;
                case TellChild t:  _child?.Tell(t.Inner); break;
                case GetChildRef:  sender.Tell(new ChildRefReply(_child!)); break;
            }
            return Task.CompletedTask;
        }

        protected override FailureDirective OnChildFailure(ActorRef child, Exception cause, object message)
            => FailureDirective.Resume;
    }

    [Fact]
    public async Task ParentResume_PreservesChildInstanceAndState_AndKeepsItAliveAsync()
    {
        var sink = new RecordingDeadLetterSink();
        using var system = new TestActorSystem("resume", deadLetterSink: sink);
        var probe = system.CreateProbe();

        var parent = system.Spawn<ResumingParent>();
        parent.Tell(new SpawnChild());

        // Crash twice. Each crash increments the counter (to 1, then 2) and
        // throws; the parent's Resume keeps the same instance and its counter.
        parent.Tell(new TellChild(new Crash()));
        parent.Tell(new TellChild(new Crash()));

        var child = await AskChildRefAsync(probe, parent);

        // Ask the surviving instance for its counter. If Resume preserved the
        // instance, the count is 2 (both mutations rode through the throws).
        probe.Send(child, new GetCount());
        var reply = probe.ExpectMsg<CountReply>(TimeSpan.FromSeconds(10));

        Assert.Equal(2, reply.N);
        Assert.False(child.IsStopped, "Resume must not stop the child.");

        // Resume is non-terminal: each crash is reported with a "resuming" reason.
        await WaitUntilAsync(() => sink.Records.Count(r => r.Cause is not null) >= 2);
        Assert.True(
            sink.Records.Count(r => r.Reason.Contains("resuming", StringComparison.OrdinalIgnoreCase)) >= 2,
            "Both crashes should be dead-lettered with a 'resuming' reason.");
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Scenario 2 — Multi-level escalation in a grandparent → parent → child
    // hierarchy.
    //
    // Variant A (supported path): the child escalates (forced by having a
    //   parent) and the IMMEDIATE supervisor returns Restart → the child is
    //   restarted (fresh instance, stays alive).
    //
    // Variant B (depth-cap): every supervisor in reach returns Escalate → the
    //   escalation walk in ActorSlot.ResolveEscalation hits its depth cap (8)
    //   and degrades to Stop with an "escalation chain exceeded max depth"
    //   reason.
    //
    // Variant C (true climb to grandparent): documents a runtime gap —
    //   ResolveEscalation re-invokes the failing child's IMMEDIATE parent on
    //   every Escalate instead of climbing to the grandparent, so a
    //   grandparent that would return Restart is never consulted. Skipped +
    //   recorded as a bug.
    // ─────────────────────────────────────────────────────────────────────────

    private sealed class EscalatingLeaf : ActorBase
    {
        private int _counter;

        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            _currentSender = sender;
            switch (message)
            {
                case Crash:
                    _counter++;                                   // survives only if NOT restarted
                    throw new InvalidOperationException("leaf boom");
                case GetCount:
                    sender.Tell(new CountReply(_counter));
                    break;
            }
            return Task.CompletedTask;
        }

        // A child WITH a parent is forced onto Escalate by the slot regardless
        // of this; kept explicit to document intent.
        protected override FailureDirective OnChildFailure(ActorRef child, Exception cause, object message)
            => FailureDirective.Escalate;
    }

    /// <summary>Mid-level supervisor whose directive is configurable per instance.</summary>
    private sealed class ConfigurableSupervisor : ActorBase
    {
        private readonly FailureDirective _directive;
        private ActorRef? _child;

        public ConfigurableSupervisor(FailureDirective directive) => _directive = directive;

        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            _currentSender = sender;
            switch (message)
            {
                case SpawnChild:   _child = SpawnChildAsync<EscalatingLeaf>(); break;
                case TellChild t:  _child?.Tell(t.Inner); break;
                case GetChildRef:  sender.Tell(new ChildRefReply(_child!)); break;
            }
            return Task.CompletedTask;
        }

        protected override FailureDirective OnChildFailure(ActorRef child, Exception cause, object message)
            => _directive;
    }

    /// <summary>
    /// Grandparent that spawns a parent (its own configurable directive) and
    /// drills SpawnChild / TellChild / GetChildRef down to that parent so the
    /// test can drive a three-level tree from the grandparent ref alone.
    /// </summary>
    private sealed class GrandparentSupervisor : ActorBase
    {
        private readonly FailureDirective _grandDirective;
        private readonly FailureDirective _parentDirective;
        private ActorRef? _parent;
        private ActorRef? _grandchild;

        public GrandparentSupervisor(FailureDirective grandDirective, FailureDirective parentDirective)
        {
            _grandDirective = grandDirective;
            _parentDirective = parentDirective;
        }

        protected override async Task DispatchAsync(object message, ActorRef sender)
        {
            _currentSender = sender;
            switch (message)
            {
                case SpawnChild:
                    _parent = SpawnChildAsync<ConfigurableSupervisor>(_parentDirective);
                    _parent.Tell(new SpawnChild());
                    break;
                case TellChild t:
                    _parent?.Tell(new TellChild(t.Inner));
                    break;
                case GetChildRef:
                    // Resolve the grandchild ref by asking the parent, then relay.
                    _grandchild = await ResolveGrandchildAsync();
                    sender.Tell(new ChildRefReply(_grandchild!));
                    break;
            }
        }

        private async Task<ActorRef> ResolveGrandchildAsync()
        {
            if (_grandchild is not null) return _grandchild;
            var reply = await _parent!.AskAsync<ChildRefReply>(new GetChildRef(), TimeSpan.FromSeconds(10));
            return reply.Child;
        }

        protected override FailureDirective OnChildFailure(ActorRef child, Exception cause, object message)
            => _grandDirective;
    }

    [Fact]
    public async Task Escalate_ImmediateSupervisorReturnsRestart_RestartsChildAsync()
    {
        var sink = new RecordingDeadLetterSink();
        using var system = new TestActorSystem("escalate-restart", deadLetterSink: sink);
        var probe = system.CreateProbe();

        // grandparent (Restart) -> parent (Restart) -> leaf
        var grandparent = system.Spawn<GrandparentSupervisor>(
            FailureDirective.Restart, FailureDirective.Restart);
        grandparent.Tell(new SpawnChild());

        var leaf = await AskChildRefAsync(probe, grandparent);

        // Prime the leaf's in-memory counter, then crash it. A Restart drops
        // the instance, so the post-restart counter is back to 0 (state reset).
        probe.Send(leaf, new GetCount());                       // counter currently 0
        Assert.Equal(0, probe.ExpectMsg<CountReply>(TimeSpan.FromSeconds(10)).N);

        leaf.Tell(new Crash());                                  // counter→1 in old instance, then throw → Restart

        await WaitUntilAsync(() => sink.Records.Any(r =>
            r.Reason.Contains("restarting", StringComparison.OrdinalIgnoreCase)));

        // Fresh instance: counter is 0 again, and the leaf is alive.
        probe.Send(leaf, new GetCount());
        var reply = probe.ExpectMsg<CountReply>(TimeSpan.FromSeconds(10));

        Assert.Equal(0, reply.N);
        Assert.False(leaf.IsStopped, "A Restart directive must keep the child alive.");
    }

    [Fact]
    public async Task Escalate_EveryLevelEscalates_ClimbsToRoot_DegradesToStopAsync()
    {
        var sink = new RecordingDeadLetterSink();
        using var system = new TestActorSystem("escalate-all", deadLetterSink: sink);
        var probe = system.CreateProbe();

        // grandparent (Escalate, and is the root) -> parent (Escalate) -> leaf.
        // The escalation climbs leaf -> parent -> grandparent and then runs out of
        // supervisors at the root, so it degrades to Stop. (The depth-8 cap is a
        // separate backstop only reachable by a pathologically deep chain.)
        var grandparent = system.Spawn<GrandparentSupervisor>(
            FailureDirective.Escalate, FailureDirective.Escalate);
        grandparent.Tell(new SpawnChild());

        var leaf = await AskChildRefAsync(probe, grandparent);

        leaf.Tell(new Crash());

        // Wait for BOTH terminal conditions — the leaf stopping and the
        // root-reached dead-letter — which settle on slightly different timings.
        await WaitUntilAsync(() => leaf.IsStopped && sink.Records.Any(r =>
            r.Reason.Contains("escalate at root", StringComparison.OrdinalIgnoreCase)));
        Assert.True(leaf.IsStopped, "When every level escalates up to the root, the child must end up stopped.");
        Assert.Contains(sink.Records, r =>
            r.Reason.Contains("escalate at root", StringComparison.OrdinalIgnoreCase));
    }

    // Fixed: ResolveEscalation now climbs the supervisor chain, so the grandparent's
    // Restart is consulted past the escalating parent and the leaf is restarted (alive).
    [Fact]
    public async Task Escalate_ParentEscalates_GrandparentRestarts_ClimbsAndRestartsChildAsync()
    {
        var sink = new RecordingDeadLetterSink();
        using var system = new TestActorSystem("escalate-climb", deadLetterSink: sink);
        var probe = system.CreateProbe();

        // grandparent (Restart) -> parent (Escalate) -> leaf.
        // Intended: leaf escalates → parent escalates → grandparent restarts → leaf alive.
        var grandparent = system.Spawn<GrandparentSupervisor>(
            FailureDirective.Restart, FailureDirective.Escalate);
        grandparent.Tell(new SpawnChild());

        var leaf = await AskChildRefAsync(probe, grandparent);

        leaf.Tell(new Crash());

        // The climb-to-grandparent surfaces a "restarting" dead-letter. Use the
        // default (generous) timeout — under a saturated thread pool the
        // crash → escalate → climb → restart chain can take a while to dispatch.
        await WaitUntilAsync(() => sink.Records.Any(r =>
            r.Reason.Contains("restarting", StringComparison.OrdinalIgnoreCase)));

        Assert.False(leaf.IsStopped,
            "Grandparent's Restart should climb past the escalating parent and keep the leaf alive.");
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Scenario 3 — Per-child restart budget via ActorBase.ApplyRestartPolicy.
    // The parent's OnChildFailure runs ApplyRestartPolicy(child, Restart,
    // maxRetries: 2, window). With maxRetries 2: the 1st and 2nd failures
    // restart, and the 3rd — log already at the budget — degrades to Stop.
    // ─────────────────────────────────────────────────────────────────────────

    private sealed class CrashOnlyChild : ActorBase
    {
        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            _currentSender = sender;
            if (message is Crash) throw new InvalidOperationException("crash");
            return Task.CompletedTask;
        }
    }

    private sealed class BudgetedRestartParent : ActorBase
    {
        private ActorRef? _child;

        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            _currentSender = sender;
            switch (message)
            {
                case SpawnChild:   _child = SpawnChildAsync<CrashOnlyChild>(); break;
                case TellChild t:  _child?.Tell(t.Inner); break;
                case GetChildRef:  sender.Tell(new ChildRefReply(_child!)); break;
            }
            return Task.CompletedTask;
        }

        protected override FailureDirective OnChildFailure(ActorRef child, Exception cause, object message)
            => ApplyRestartPolicy(
                child,
                FailureDirective.Restart,
                maxRetries: 2,
                window: TimeSpan.FromSeconds(30));
    }

    [Fact]
    public async Task PerChildBudget_RestartsTwiceThenStopsOnThirdCrashAsync()
    {
        var sink = new RecordingDeadLetterSink();
        using var system = new TestActorSystem("budget", deadLetterSink: sink);
        var probe = system.CreateProbe();

        var parent = system.Spawn<BudgetedRestartParent>();
        parent.Tell(new SpawnChild());

        var child = await AskChildRefAsync(probe, parent);

        // First crash: budget log empty (0 < 2) → Restart; child stays alive.
        child.Tell(new Crash());
        await WaitUntilAsync(() => CountReason(sink, "restarting") >= 1);
        Assert.False(child.IsStopped, "First crash should restart, not stop.");

        // Second crash: log at 1 (< 2) → Restart; still alive.
        child.Tell(new Crash());
        await WaitUntilAsync(() => CountReason(sink, "restarting") >= 2);
        Assert.False(child.IsStopped, "Second crash should restart, not stop.");

        // Third crash: log at 2 (>= 2) → budget exhausted → degrade to Stop.
        child.Tell(new Crash());
        await WaitUntilAsync(() => child.IsStopped);

        Assert.True(child.IsStopped, "Third crash should exceed the budget and stop the child.");
        Assert.Equal(2, CountReason(sink, "restarting"));
        Assert.Contains(sink.Records, r =>
            r.Reason.Contains("stopping", StringComparison.OrdinalIgnoreCase));
    }

    // ─── Helpers ─────────────────────────────────────────────────────────────

    private static int CountReason(RecordingDeadLetterSink sink, string fragment) =>
        sink.Records.Count(r => r.Reason.Contains(fragment, StringComparison.OrdinalIgnoreCase));

    /// <summary>Asks a parent for its child's ref and returns it.</summary>
    private static async Task<ActorRef> AskChildRefAsync(TestProbe probe, ActorRef parent)
    {
        // The parent may not have spawned its child yet (SpawnChild is queued).
        // Retry the request until a non-null child ref comes back.
        var deadline = DateTime.UtcNow.AddSeconds(10);
        while (true)
        {
            probe.Send(parent, new GetChildRef());
            var reply = probe.ExpectMsg<ChildRefReply>(TimeSpan.FromSeconds(10));
            if (reply.Child is not null) return reply.Child;
            if (DateTime.UtcNow >= deadline)
                throw new TimeoutException("Parent never reported a child ref.");
            await Task.Delay(20);
        }
    }

    private static async Task WaitUntilAsync(Func<bool> predicate, int timeoutMs = 30_000)
    {
        var deadline = DateTime.UtcNow.AddMilliseconds(timeoutMs);
        while (DateTime.UtcNow < deadline)
        {
            if (predicate()) return;
            await Task.Delay(15);
        }
        throw new TimeoutException($"Condition not met within {timeoutMs}ms.");
    }
}
