using Spek.Resilience;
using Spek.Resilience.RateLimiting;
using Xunit;

namespace Spek.Tests.Resilience;

public sealed class RateLimitIngressPolicyTests
{
    private static ResilienceContext Context() =>
        new("test/actor", "Channel", new object(), DateTimeOffset.UtcNow);

    [Fact]
    public async Task TokenBucket_allows_within_burst_then_defers()
    {
        await using var policy = RateLimitIngressPolicy.TokenBucket(
            permitsPerSecond: 1,
            burstCapacity:    3);

        // Drain the burst — three Allows in quick succession.
        for (int i = 0; i < 3; i++)
        {
            var d = await policy.EvaluateAsync(Context());
            Assert.Equal(PolicyDecisionKind.Allow, d.Kind);
        }

        // Fourth must be denied. Token-bucket surfaces RetryAfter only
        // when AutoReplenishment is on and the bucket is non-empty
        // soon — for a freshly-drained bucket, accept either Defer
        // (with hint) or Reject (no hint), both signal "not now".
        var denied = await policy.EvaluateAsync(Context());
        Assert.NotEqual(PolicyDecisionKind.Allow, denied.Kind);
        Assert.NotNull(denied.Reason);
    }

    [Fact]
    public async Task FixedWindow_rejects_after_quota_exhausted()
    {
        await using var policy = RateLimitIngressPolicy.FixedWindow(
            permits: 2,
            window:  TimeSpan.FromSeconds(60));

        Assert.Equal(PolicyDecisionKind.Allow, (await policy.EvaluateAsync(Context())).Kind);
        Assert.Equal(PolicyDecisionKind.Allow, (await policy.EvaluateAsync(Context())).Kind);

        var third = await policy.EvaluateAsync(Context());
        Assert.NotEqual(PolicyDecisionKind.Allow, third.Kind);
    }

    [Fact]
    public async Task Concurrency_allows_up_to_permit_count()
    {
        await using var policy = RateLimitIngressPolicy.Concurrency(permits: 2);

        // Concurrency policy releases on lease dispose (immediate) so
        // calls in serial all succeed — this exercises the "permit
        // available" path.
        for (int i = 0; i < 5; i++)
        {
            var d = await policy.EvaluateAsync(Context());
            Assert.Equal(PolicyDecisionKind.Allow, d.Kind);
        }
    }

    [Fact]
    public void Constructor_rejects_null_limiter()
    {
        Assert.Throws<ArgumentNullException>(() => new RateLimitIngressPolicy(null!));
    }
}
