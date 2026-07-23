using Spek.Resilience;
using Spek.Resilience.RateLimiting;
using Xunit;

namespace Spek.Tests.Resilience;

public sealed class PartitionedRateLimitIngressPolicyTests
{
    private sealed record TenantMessage(string TenantId);

    private static ResilienceContext Context(string tenant) =>
        new("test/actor", "Channel", new TenantMessage(tenant), DateTimeOffset.UtcNow);

    [Fact]
    public async Task Per_key_buckets_isolate_tenantsAsync()
    {
        await using var policy = PartitionedRateLimitIngressPolicy<string>.PerKeyTokenBucket(
            keySelector:      ctx => ((TenantMessage)ctx.Message).TenantId,
            permitsPerSecond: 1,
            burstCapacity:    2);

        // Drain tenant-A's bucket (capacity 2).
        Assert.Equal(PolicyDecisionKind.Allow, (await policy.EvaluateAsync(Context("A"))).Kind);
        Assert.Equal(PolicyDecisionKind.Allow, (await policy.EvaluateAsync(Context("A"))).Kind);

        // A is exhausted.
        var aDenied = await policy.EvaluateAsync(Context("A"));
        Assert.NotEqual(PolicyDecisionKind.Allow, aDenied.Kind);

        // B has its own bucket — still full.
        Assert.Equal(PolicyDecisionKind.Allow, (await policy.EvaluateAsync(Context("B"))).Kind);
        Assert.Equal(PolicyDecisionKind.Allow, (await policy.EvaluateAsync(Context("B"))).Kind);
    }

    [Fact]
    public void Per_key_factory_rejects_null_selector()
    {
        Assert.Throws<ArgumentNullException>(() =>
            PartitionedRateLimitIngressPolicy<string>.PerKeyTokenBucket(
                keySelector: null!, permitsPerSecond: 1, burstCapacity: 1));
    }
}
