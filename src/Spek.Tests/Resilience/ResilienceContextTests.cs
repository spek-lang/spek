using System.Collections.Concurrent;
using Spek.Resilience;
using Spek.Runtime;
using Xunit;

namespace Spek.Tests.Resilience;

/// <summary>
/// Coverage for <see cref="ResilienceContext"/> — both its plain value
/// semantics and, more importantly, the shape of the context the live
/// dispatch loop actually hands to an <see cref="IngressPolicy"/>:
/// actor identity, channel name derived from the message type, the
/// message instance itself, and a sane wall-clock timestamp.
/// </summary>
public sealed class ResilienceContextTests
{
    private sealed record Probe(string Tag);
    private sealed record OtherProbe(int N);

    private sealed class ProbeActor : ActorBase
    {
        protected override Task DispatchAsync(object message, ActorRef sender)
            => Task.CompletedTask;
    }

    /// <summary>Allows everything; records each context it evaluates.</summary>
    private sealed class RecordingPolicy : IngressPolicy
    {
        public ConcurrentQueue<ResilienceContext> Seen { get; } = new();

        public override ValueTask<PolicyDecision> EvaluateAsync(
            ResilienceContext context, CancellationToken cancellationToken = default)
        {
            Seen.Enqueue(context);
            return ValueTask.FromResult(PolicyDecision.Allow());
        }
    }

    [Fact]
    public void Construction_ExposesEveryMember()
    {
        var message = new Probe("direct");
        var timestamp = DateTimeOffset.UtcNow;

        var ctx = new ResilienceContext("user/orders-1", "PlaceOrder", message, timestamp);

        Assert.Equal("user/orders-1", ctx.ActorPath);
        Assert.Equal("PlaceOrder", ctx.ChannelName);
        Assert.Same(message, ctx.Message);
        Assert.Equal(timestamp, ctx.Timestamp);
    }

    [Fact]
    public void ValueSemantics_EqualityFollowsMembers()
    {
        var message = new Probe("eq");
        var timestamp = DateTimeOffset.UtcNow;

        var ctx = new ResilienceContext("a", "Probe", message, timestamp);

        Assert.Equal(ctx, new ResilienceContext("a", "Probe", message, timestamp));
        Assert.NotEqual(ctx, ctx with { ChannelName = "OtherChannel" });
        Assert.NotEqual(ctx, ctx with { ActorPath = "b" });
    }

    [Fact]
    public async Task IngressPolicy_ReceivesFullyPopulatedContext_FromLiveDispatchAsync()
    {
        var policy = new RecordingPolicy();
        using var system = new ActorSystem("resilience-context-test");
        var actor = system.Spawn<ProbeActor>();
        actor.AttachIngressPolicy(policy);

        var before = DateTimeOffset.UtcNow;
        var message = new Probe("live");
        actor.Tell(message);

        var deadline = DateTime.UtcNow + TimeSpan.FromSeconds(2);
        while (policy.Seen.IsEmpty && DateTime.UtcNow < deadline)
            await Task.Delay(20);
        var after = DateTimeOffset.UtcNow;

        Assert.True(policy.Seen.TryDequeue(out var ctx),
            "Ingress policy was never evaluated within 2s.");

        Assert.Equal(typeof(ProbeActor).FullName, ctx.ActorPath);
        Assert.Equal(nameof(Probe), ctx.ChannelName);
        Assert.Same(message, ctx.Message);
        Assert.InRange(ctx.Timestamp, before, after);
    }

    [Fact]
    public async Task ChannelName_TracksEachMessageTypeIndividuallyAsync()
    {
        var policy = new RecordingPolicy();
        using var system = new ActorSystem("resilience-context-channels");
        var actor = system.Spawn<ProbeActor>();
        actor.AttachIngressPolicy(policy);

        actor.Tell(new Probe("one"));
        actor.Tell(new OtherProbe(2));

        var deadline = DateTime.UtcNow + TimeSpan.FromSeconds(2);
        while (policy.Seen.Count < 2 && DateTime.UtcNow < deadline)
            await Task.Delay(20);

        var channels = policy.Seen.Select(c => c.ChannelName).ToArray();
        Assert.Equal(new[] { nameof(Probe), nameof(OtherProbe) }, channels);
    }
}
