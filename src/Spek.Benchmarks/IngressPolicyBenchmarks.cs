using BenchmarkDotNet.Attributes;
using Spek;
using Spek.Resilience;
using Spek.Resilience.RateLimiting;
using Spek.Runtime;

namespace Spek.Benchmarks;

/// <summary>
/// Per-message overhead of the ingress-policy chain. Policies run on the
/// dispatch path for every message, so attaching one buys admission control
/// at the price measured here: a <c>ResilienceContext</c> per message plus
/// one <c>EvaluateAsync</c> per attached policy. The token-bucket arm is
/// sized so no message is ever rejected or deferred — it measures the
/// limiter's permit check, not the rejection path.
/// </summary>
[MemoryDiagnoser]
public class IngressPolicyBenchmarks
{
    [Params(100_000)]
    public int Messages;

    /// <summary>No policies attached — the dispatch loop's policy check is a
    /// single array-length test per message. The reference cost.</summary>
    [Benchmark(Baseline = true)]
    public int NoPolicy()
    {
        using var system = new ActorSystem("bench");
        var actor = system.Spawn<CounterActor>();
        for (int i = 0; i < Messages; i++)
            actor.Tell(new Ping());
        system.AwaitTermination();
        return Messages;
    }

    /// <summary>One always-allow policy: the fixed floor of having any policy
    /// attached — context construction, the chain walk, and one synchronously
    /// completing ValueTask per message.</summary>
    [Benchmark]
    public int OneAllowPolicy()
    {
        using var system = new ActorSystem("bench");
        var actor = system.Spawn<CounterActor>();
        actor.AttachIngressPolicy(new AlwaysAllowPolicy());
        for (int i = 0; i < Messages; i++)
            actor.Tell(new Ping());
        system.AwaitTermination();
        return Messages;
    }

    /// <summary>A chain of three always-allow policies. The delta over
    /// <see cref="OneAllowPolicy"/> is the marginal cost per additional link,
    /// separating chain-walk overhead from the fixed attach cost.</summary>
    [Benchmark]
    public int ThreeAllowPolicies()
    {
        using var system = new ActorSystem("bench");
        var actor = system.Spawn<CounterActor>();
        actor.AttachIngressPolicy(new AlwaysAllowPolicy());
        actor.AttachIngressPolicy(new AlwaysAllowPolicy());
        actor.AttachIngressPolicy(new AlwaysAllowPolicy());
        for (int i = 0; i < Messages; i++)
            actor.Tell(new Ping());
        system.AwaitTermination();
        return Messages;
    }

    /// <summary>The shipped token-bucket policy with a bucket deeper than the
    /// whole run (every message admitted): dominated by the BCL rate limiter's
    /// AttemptAcquire — a lock plus token accounting — and the lease dispose,
    /// per message.</summary>
    [Benchmark]
    public async Task<int> TokenBucket_NeverRejects()
    {
        await using var policy = RateLimitIngressPolicy.TokenBucket(
            permitsPerSecond: Messages,
            burstCapacity: Messages * 2);
        using var system = new ActorSystem("bench");
        var actor = system.Spawn<CounterActor>();
        actor.AttachIngressPolicy(policy);
        for (int i = 0; i < Messages; i++)
            actor.Tell(new Ping());
        system.AwaitTermination();
        return Messages;
    }
}

/// <summary>The cheapest possible policy — allows everything, synchronously.
/// Isolates the runtime's chain-walk overhead from any real policy work.</summary>
internal sealed class AlwaysAllowPolicy : IngressPolicy
{
    public override ValueTask<PolicyDecision> EvaluateAsync(
        ResilienceContext context, CancellationToken cancellationToken = default)
        => new(PolicyDecision.Allow());
}
