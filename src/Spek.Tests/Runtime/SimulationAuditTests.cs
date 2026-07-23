using Spek;
using Spek.Runtime;
using Spek.Testing;
using Xunit;

namespace Spek.Tests.Runtime;

/// <summary>
/// Negative tests for simulator audit mode
/// (<c>new SimulatedActorSystem(seed, audit: true)</c>): each seeds a real
/// defect class — mail parked in a mailbox, an ask that can never complete,
/// a one-shot timer left armed on the virtual clock — and proves the
/// end-of-test sweep turns the silent leak into a loud, seed-quoting
/// assertion. The positive direction (a clean run audits clean) is covered
/// by the simulator suites themselves, which all run with audit on.
/// </summary>
public sealed class SimulationAuditTests
{
    private sealed record Ping();

    /// <summary>Handles everything, replies to nothing.</summary>
    private sealed class Silent : ActorBase
    {
        protected override Task DispatchAsync(object message, ActorRef sender)
            => Task.CompletedTask;
    }

    private sealed class Wedged : ActorBase
    {
        // The handler never returns — a genuinely hung dispatch. Fail-fast's
        // no-reply check runs only AFTER the handler returns, so it can't
        // resolve an ask parked on this; the reply cell stays open for the
        // audit to catch. (A handler that merely returns without replying now
        // fails the ask fast and would never reach the audit.)
        protected override Task DispatchAsync(object message, ActorRef sender)
            => new TaskCompletionSource<object>().Task;
    }

    [Fact]
    public void ParkedMail_UndrainedMailbox_FailsTheAuditAtDispose()
    {
        var sim = new SimulatedActorSystem(seed: 1, audit: true);
        var actor = sim.Spawn<Silent>();
        actor.Tell(new Ping());
        // Seeded defect: no Run() — the message never dispatches. Disposal
        // must surface it instead of silently discarding the mailbox.
        var ex = Assert.Throws<SimulationAuditException>(sim.Dispose);
        Assert.Contains("parked mail", ex.Message);
        Assert.Contains("Silent", ex.Message);
        Assert.Contains("seed 1", ex.Message);
        // Conservation still balances (enqueued 1 == dispatched 0 + dropped 0
        // + parked 1): parked mail is its own violation, not a broken count.
        Assert.DoesNotContain("conservation", ex.Message);
    }

    [Fact]
    public void IncompleteAsk_TargetNeverReplies_FailsTheAuditAtDispose()
    {
        var sim = new SimulatedActorSystem(seed: 2, audit: true);
        var actor = sim.Spawn<Wedged>();
        // Seeded defect: an ask with no timeout against a handler that never
        // returns — the dispatch itself wedges, so the reply cell can never
        // resolve (fail-fast's no-reply check never runs). The audit flags
        // both the in-flight dispatch and the incomplete ask; we assert on
        // the latter.
        var pending = actor.AskAsync<object>(new Ping());
        sim.Run();
        Assert.False(pending.IsCompleted);

        var ex = Assert.Throws<SimulationAuditException>(sim.Dispose);
        Assert.Contains("incomplete asks: 1 issued but 0 completed", ex.Message);
    }

    [Fact]
    public void DanglingOneShotTimer_ChaosDelayNeverAdvancedPast_FailsTheAuditAtDispose()
    {
        var chaos = new ChaosPlan().Delay<Silent>(TimeSpan.FromSeconds(5));
        var sim = new SimulatedActorSystem(seed: 3, chaos: chaos, audit: true);
        var actor = sim.Spawn<Silent>();
        // Seeded defect: the delayed message is parked on the virtual clock;
        // no Advance() ever releases it, so disposal would strand it on a
        // timer that fires into a dead system.
        actor.Tell(new Ping());
        sim.Run();

        var ex = Assert.Throws<SimulationAuditException>(sim.Dispose);
        Assert.Single(ex.Violations);
        Assert.Contains("armed one-shot timer", ex.Message);
    }

    [Fact]
    public void TimeParkedWork_DrainedByAdvance_AuditsClean()
    {
        // The positive twin of the dangling-timer test: Advance() past the
        // delay releases the parked message, and the same audit passes.
        var chaos = new ChaosPlan().Delay<Silent>(TimeSpan.FromSeconds(5));
        using var sim = new SimulatedActorSystem(seed: 3, chaos: chaos, audit: true);
        var actor = sim.Spawn<Silent>();
        actor.Tell(new Ping());
        sim.Run();
        sim.Advance(TimeSpan.FromSeconds(6));
        Assert.Equal(1, sim.DispatchCountOf(actor));
        // `using` disposes — the audit runs and must find nothing.
    }

    [Fact]
    public void ExplicitAssert_UsableWithoutOptingIntoDisposeTimeAudit()
    {
        // audit: false — Dispose stays silent; the explicit call still sweeps.
        var sim = new SimulatedActorSystem(seed: 4);
        var actor = sim.Spawn<Silent>();
        actor.Tell(new Ping());
        var ex = Assert.Throws<SimulationAuditException>(sim.AssertQuiescentAndAccounted);
        Assert.Contains("parked mail", ex.Message);
        sim.Dispose();   // audit off: teardown does not re-throw
    }
}
