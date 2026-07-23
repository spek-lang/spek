using System.Threading.RateLimiting;

namespace Spek.Resilience.RateLimiting;

/// <summary>
/// Ingress policy that picks a separate rate-limit bucket per key
/// derived from the <see cref="ResilienceContext"/>. The classic use
/// case is "100 req/sec per tenant" — derive the tenant id from the
/// inbound message and let the partitioned limiter manage one bucket
/// per tenant under the hood.
///
/// Wraps <see cref="PartitionedRateLimiter{TResource}"/> so all the
/// BCL's partition factories are available.
/// </summary>
public sealed class PartitionedRateLimitIngressPolicy<TKey> : IngressPolicy, IAsyncDisposable
    where TKey : notnull
{
    private readonly PartitionedRateLimiter<ResilienceContext> _limiter;
    private readonly bool _ownsLimiter;

    public PartitionedRateLimitIngressPolicy(
        PartitionedRateLimiter<ResilienceContext> limiter,
        bool ownsLimiter = true)
    {
        ArgumentNullException.ThrowIfNull(limiter);
        _limiter = limiter;
        _ownsLimiter = ownsLimiter;
    }

    public override async ValueTask<PolicyDecision> EvaluateAsync(
        ResilienceContext context,
        CancellationToken cancellationToken = default)
    {
        using var lease = _limiter.AttemptAcquire(context, permitCount: 1);

        if (lease.IsAcquired)
            return PolicyDecision.Allow();

        if (lease.TryGetMetadata(MetadataName.RetryAfter, out TimeSpan retryAfter))
            return PolicyDecision.Defer(retryAfter, "rate limit exceeded");

        return PolicyDecision.Reject("rate limit exceeded");
    }

    /// <summary>
    /// Convenience factory for "N permits/sec per key, derived from
    /// the message via <paramref name="keySelector"/>". Builds a
    /// token-bucket-per-partition under the hood.
    /// </summary>
    public static PartitionedRateLimitIngressPolicy<TKey> PerKeyTokenBucket(
        Func<ResilienceContext, TKey> keySelector,
        int permitsPerSecond,
        int burstCapacity,
        IEqualityComparer<TKey>? keyComparer = null)
    {
        ArgumentNullException.ThrowIfNull(keySelector);

        var limiter = PartitionedRateLimiter.Create<ResilienceContext, TKey>(
            ctx =>
            {
                var key = keySelector(ctx);
                return RateLimitPartition.GetTokenBucketLimiter(
                    key,
                    _ => new TokenBucketRateLimiterOptions
                    {
                        TokenLimit          = burstCapacity,
                        TokensPerPeriod     = permitsPerSecond,
                        ReplenishmentPeriod = TimeSpan.FromSeconds(1),
                        QueueLimit          = 0,
                        QueueProcessingOrder = QueueProcessingOrder.OldestFirst,
                        AutoReplenishment   = true,
                    });
            },
            keyComparer ?? EqualityComparer<TKey>.Default);

        return new PartitionedRateLimitIngressPolicy<TKey>(limiter);
    }

    public async ValueTask DisposeAsync()
    {
        if (_ownsLimiter)
            await _limiter.DisposeAsync().ConfigureAwait(false);
    }
}
