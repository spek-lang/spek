using BenchmarkDotNet.Attributes;
using Spek;
using Spek.Resilience;
using Spek.Runtime;

namespace Spek.Benchmarks;

/// <summary>
/// The runtime's clock-driven machinery, priced from the surfaces plain C#
/// can reach. Two shapes live here: defer→timer→re-admission (via an ingress
/// policy returning Defer — the runtime's only defer surface; there is no
/// handler-level defer) and the passivation arm tax. A TimerArmDisarm shape
/// does NOT exist to measure: ActorBase exposes no arm/cancel timer API —
/// every runtime timer is internal (passivation checks, chaos delays, defer
/// re-admission). Rows pair as documented per benchmark; there is no single
/// class-wide baseline because the two pairs use different message counts.
/// </summary>
[MemoryDiagnoser]
public class TimerBenchmarks
{
    private const int DeferMessages = 10_000;
    private const int ThroughputMessages = 100_000;

    /// <summary>The reference for <see cref="DeferOnceThenAdmit"/>: the same
    /// policy object with deferral switched off, so every message is admitted
    /// first try and the pair's delta isolates the defer machinery from the
    /// policy-evaluation floor (which <c>IngressPolicyBenchmarks</c> prices
    /// separately).</summary>
    [Benchmark]
    public async Task AdmitFirstTry()
    {
        using var system = new ActorSystem("bench");
        var done = new TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously);
        var actor = system.Spawn<CountdownActor>(DeferMessages, done);
        actor.AttachIngressPolicy(new DeferOncePolicy { DeferEnabled = false });
        for (int i = 0; i < DeferMessages; i++)
            actor.Tell(new DeferrablePing());
        await done.Task;
    }

    /// <summary>
    /// Every message is deferred exactly once at first admission (1ms
    /// RetryAfter) and admitted on re-admission — the full park cycle per
    /// message: policy verdict, a one-shot clock timer plus its callback
    /// closure, the defer-budget entry (ConditionalWeakTable + StrongBox),
    /// and a second trip through enqueue and the policy chain. Wall time
    /// includes the 1ms park (timers overlap across the burst, so it shows
    /// up once, not 10k times) — read <c>Allocated</c>, not the mean, and
    /// compare against <see cref="AdmitFirstTry"/>. Completion is detected by
    /// processed-message count, not idleness: parked messages leave every
    /// mailbox empty, so AwaitTermination would return early.
    /// </summary>
    [Benchmark]
    public async Task DeferOnceThenAdmit()
    {
        using var system = new ActorSystem("bench");
        var done = new TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously);
        var actor = system.Spawn<CountdownActor>(DeferMessages, done);
        actor.AttachIngressPolicy(new DeferOncePolicy { DeferEnabled = true });
        for (int i = 0; i < DeferMessages; i++)
            actor.Tell(new DeferrablePing());
        await done.Task;
    }

    /// <summary>The reference for <see cref="PassivationArmed_TellThroughput"/>:
    /// the standard 100k Tell-to-idle burst against a counter with no
    /// passivation configured.</summary>
    [Benchmark]
    public int PassivationOff_TellThroughput()
    {
        using var system = new ActorSystem("bench");
        var actor = system.Spawn<CounterActor>();
        for (int i = 0; i < ThroughputMessages; i++)
            actor.Tell(new Ping());
        system.AwaitTermination();
        return ThroughputMessages;
    }

    /// <summary>
    /// The same burst against an actor with <c>passivate after 250ms</c>
    /// configured — the per-message price of having passivation at all. In
    /// this runtime that price should be ~zero by construction: the timeout
    /// arms one periodic idle-check timer at materialization (period
    /// timeout/4, floored at 50ms), and the per-message idle timestamp is
    /// written for every actor whether or not passivation is armed. The
    /// benchmark pins that claim to a measured delta against
    /// <see cref="PassivationOff_TellThroughput"/>.
    /// </summary>
    [Benchmark]
    public int PassivationArmed_TellThroughput()
    {
        using var system = new ActorSystem("bench");
        var actor = system.Spawn<PassivationArmedCounterActor>();
        for (int i = 0; i < ThroughputMessages; i++)
            actor.Tell(new Ping());
        system.AwaitTermination();
        return ThroughputMessages;
    }
}

/// <summary>
/// Defers each message exactly once — first admission parks it for 1ms, the
/// re-admitted instance is allowed through. The message carries its own
/// re-admission flag so the policy needs no out-of-band tracking; with
/// <see cref="DeferEnabled"/> off the same policy allows everything, keeping
/// the two arms' policy-evaluation cost identical.
/// </summary>
internal sealed class DeferOncePolicy : IngressPolicy
{
    public bool DeferEnabled;

    public override ValueTask<PolicyDecision> EvaluateAsync(
        ResilienceContext context, CancellationToken cancellationToken = default)
    {
        if (DeferEnabled && context.Message is DeferrablePing { Readmitted: false } ping)
        {
            ping.Readmitted = true;
            return new(PolicyDecision.Defer(TimeSpan.FromMilliseconds(1), "bench: defer once"));
        }
        return new(PolicyDecision.Allow());
    }
}

/// <summary>Mutable-flag message so <see cref="DeferOncePolicy"/> can tell a
/// first admission from a re-admission. A class (not a record) on purpose:
/// the defer budget is tracked per message INSTANCE.</summary>
internal sealed class DeferrablePing
{
    public bool Readmitted;
}
