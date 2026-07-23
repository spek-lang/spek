using Spek;
using Spek.Resilience;
using Spek.Resilience.RateLimiting;
using Spek.Runtime;
using Xunit;

namespace Spek.Tests.Resilience;

/// <summary>
/// End-to-end coverage that an attached <see cref="IngressPolicy"/>
/// actually gates message dispatch through the live ActorSlot loop.
/// </summary>
public sealed class IngressPolicyIntegrationTests
{
    private sealed record Ping();

    private sealed class CountingActor : ActorBase
    {
        // Static so tests can read the count without needing internal
        // access to ActorRef.Underlying. Each test resets it.
        public static int Handled;
        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            Interlocked.Increment(ref Handled);
            return Task.CompletedTask;
        }
    }

    /// <summary>Always-deny policy — proves the gate fires.</summary>
    private sealed class DenyAll : IngressPolicy
    {
        public override ValueTask<PolicyDecision> EvaluateAsync(
            ResilienceContext context, CancellationToken cancellationToken = default)
            => ValueTask.FromResult(PolicyDecision.Reject("test deny-all"));
    }

    [Fact]
    public async Task Reject_decision_blocks_dispatch_and_dead_letters()
    {
        CountingActor.Handled = 0;
        var sink = new RecordingDeadLetterSink();
        using var system = new ActorSystem("ingress-test", deadLetterSink: sink);
        var actor = system.Spawn<CountingActor>();
        actor.AttachIngressPolicy(new DenyAll());

        actor.Tell(new Ping());
        actor.Tell(new Ping());
        actor.Tell(new Ping());

        // Wait for the dispatch loop to run the policy on each message.
        var deadline = DateTime.UtcNow + TimeSpan.FromSeconds(2);
        while (sink.Records.Count < 3 && DateTime.UtcNow < deadline)
            await Task.Delay(20);

        Assert.Equal(0, CountingActor.Handled);
        Assert.Equal(3, sink.Records.Count);
        Assert.All(sink.Records, r =>
        {
            Assert.Contains("rejected by ingress policy", r.Reason);
            Assert.Contains("test deny-all", r.Reason);
        });
    }

    [Fact]
    public async Task TokenBucket_throttles_burst_traffic_to_the_actor()
    {
        CountingActor.Handled = 0;
        var sink = new RecordingDeadLetterSink();
        using var system = new ActorSystem("rate-test", deadLetterSink: sink);
        var actor = system.Spawn<CountingActor>();

        // Capacity 3, refill 1/sec — burst of 5 → 3 handled, 2 throttled.
        actor.AttachIngressPolicy(RateLimitIngressPolicy.TokenBucket(
            permitsPerSecond: 1,
            burstCapacity:    3));

        for (int i = 0; i < 5; i++)
            actor.Tell(new Ping());

        var deadline = DateTime.UtcNow + TimeSpan.FromSeconds(2);
        while (DateTime.UtcNow < deadline)
        {
            if (CountingActor.Handled + sink.Records.Count >= 5) break;
            await Task.Delay(20);
        }

        Assert.Equal(3, CountingActor.Handled);
        Assert.Equal(2, sink.Records.Count);
        Assert.All(sink.Records, r => Assert.Contains("rate limit exceeded", r.Reason));
    }

    [Fact]
    public async Task Multiple_policies_are_chained_in_order()
    {
        CountingActor.Handled = 0;
        var sink = new RecordingDeadLetterSink();
        using var system = new ActorSystem("chain-test", deadLetterSink: sink);
        var actor = system.Spawn<CountingActor>();

        // First-attached deny-all wins regardless of what follows;
        // proves chain order short-circuits on the first non-Allow.
        actor.AttachIngressPolicy(new DenyAll());
        actor.AttachIngressPolicy(RateLimitIngressPolicy.TokenBucket(100, 200));

        actor.Tell(new Ping());

        var deadline = DateTime.UtcNow + TimeSpan.FromSeconds(2);
        while (sink.Records.Count < 1 && DateTime.UtcNow < deadline)
            await Task.Delay(20);

        Assert.Single(sink.Records);
        Assert.Contains("test deny-all", sink.Records[0].Reason);
        Assert.Equal(0, CountingActor.Handled);
    }
}
