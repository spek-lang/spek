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
    public async Task Reject_decision_blocks_dispatch_and_dead_lettersAsync()
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
    public async Task TokenBucket_throttles_burst_traffic_to_the_actorAsync()
    {
        CountingActor.Handled = 0;
        var sink = new RecordingDeadLetterSink();
        using var system = new ActorSystem("rate-test", deadLetterSink: sink);
        var actor = system.Spawn<CountingActor>();

        // Capacity 3, refill 1/sec — burst of 5 → 3 admitted immediately,
        // 2 DEFERRED. Deferral is delayed re-admission, not rejection: the
        // runtime re-enqueues after the policy's RetryAfter, so with a live
        // refill every message is eventually handled and nothing
        // dead-letters. (Rejection semantics are covered by the deny-all
        // test above; defer-budget exhaustion by the unit tests.)
        actor.AttachIngressPolicy(RateLimitIngressPolicy.TokenBucket(
            permitsPerSecond: 1,
            burstCapacity:    3));

        for (int i = 0; i < 5; i++)
            actor.Tell(new Ping());

        // Immediate burst: exactly the bucket's capacity gets through fast.
        var deadline = DateTime.UtcNow + TimeSpan.FromSeconds(2);
        while (DateTime.UtcNow < deadline && CountingActor.Handled < 3)
            await Task.Delay(20);
        Assert.Equal(3, CountingActor.Handled);
        Assert.Empty(sink.Records);   // deferred, not dead-lettered

        // Re-admission: the two deferred messages land as tokens refill.
        deadline = DateTime.UtcNow + TimeSpan.FromSeconds(10);
        while (DateTime.UtcNow < deadline && CountingActor.Handled < 5)
            await Task.Delay(50);
        Assert.Equal(5, CountingActor.Handled);
        Assert.Empty(sink.Records);
    }

    /// <summary>Defers every Ping; everything else passes.</summary>
    private sealed class DeferPings : IngressPolicy
    {
        public override ValueTask<PolicyDecision> EvaluateAsync(
            ResilienceContext context, CancellationToken cancellationToken = default)
            => ValueTask.FromResult(context.Message is Ping
                ? PolicyDecision.Defer(TimeSpan.FromSeconds(5), "parked for test")
                : PolicyDecision.Allow());
    }

    private sealed record StopMe();

    private sealed class StoppableActor : ActorBase
    {
        public static int PingsHandled;
        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            if (message is StopMe) StopSelf();
            else Interlocked.Increment(ref PingsHandled);
            return Task.CompletedTask;
        }
    }

    [Fact]
    public async Task Deferred_message_dead_letters_if_the_actor_stops_before_readmissionAsync()
    {
        StoppableActor.PingsHandled = 0;
        var sink = new RecordingDeadLetterSink();
        var clock = new Spek.Testing.ManualTimeProvider();
        using var system = new ActorSystem("defer-stop-test",
            deadLetterSink: sink, timeProvider: clock);
        var actor = system.Spawn<StoppableActor>();
        actor.AttachIngressPolicy(new DeferPings());

        // Mailbox FIFO makes this deterministic: the Ping is parked (its
        // re-admission timer created inside the dispatch loop) strictly
        // before the StopMe is dispatched.
        actor.Tell(new Ping());
        actor.Tell(new StopMe());

        var deadline = DateTime.UtcNow + TimeSpan.FromSeconds(5);
        while (!actor.IsStopped && DateTime.UtcNow < deadline)
            await Task.Delay(20);
        Assert.True(actor.IsStopped);

        // Parking is not a dead letter.
        Assert.Empty(sink.Records);

        // Fire the re-admission timer (synchronous under the manual clock).
        // The parked message must reach a terminal state — dead-lettered by
        // Enqueue's stopped path, never silently dropped.
        clock.Advance(TimeSpan.FromSeconds(6));

        var record = Assert.Single(sink.Records);
        Assert.Contains("target actor is stopped", record.Reason);
        Assert.Equal(0, StoppableActor.PingsHandled);
    }

    [Fact]
    public async Task Multiple_policies_are_chained_in_orderAsync()
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
