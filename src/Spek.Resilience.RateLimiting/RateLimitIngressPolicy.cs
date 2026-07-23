using System.Threading.RateLimiting;

namespace Spek.Resilience.RateLimiting;

/// <summary>
/// Ingress policy that gates messages on a single
/// <see cref="RateLimiter"/>. The policy attempts to acquire one
/// permit per evaluation; if the limiter denies, the policy returns
/// <see cref="PolicyDecision.Defer"/> when the limiter surfaces a
/// <see cref="MetadataName.RetryAfter"/> hint, otherwise
/// <see cref="PolicyDecision.Reject"/>.
///
/// Use the static factories (<see cref="TokenBucket"/>,
/// <see cref="FixedWindow"/>, <see cref="SlidingWindow"/>,
/// <see cref="Concurrency"/>) for the common shapes, or pass any
/// pre-built <see cref="RateLimiter"/> to the constructor for full
/// control.
///
/// For per-key partitioning (per-tenant, per-actor-instance), use
/// <see cref="PartitionedRateLimitIngressPolicy{TKey}"/> instead.
/// </summary>
public sealed class RateLimitIngressPolicy : IngressPolicy, IAsyncDisposable
{
    private readonly RateLimiter _limiter;
    private readonly bool _ownsLimiter;

    /// <summary>
    /// Wrap any pre-built <see cref="RateLimiter"/>. The policy takes
    /// ownership of the limiter and disposes it when the policy is
    /// disposed unless <paramref name="ownsLimiter"/> is false.
    /// </summary>
    public RateLimitIngressPolicy(RateLimiter limiter, bool ownsLimiter = true)
    {
        ArgumentNullException.ThrowIfNull(limiter);
        _limiter = limiter;
        _ownsLimiter = ownsLimiter;
    }

    public override async ValueTask<PolicyDecision> EvaluateAsync(
        ResilienceContext context,
        CancellationToken cancellationToken = default)
    {
        // AttemptAcquire is the non-blocking variant — we want to
        // surface backoff as Defer rather than block the dispatcher
        // thread waiting in the queue. (The runtime will re-tell the
        // message after the RetryAfter window elapses.)
        using var lease = _limiter.AttemptAcquire(permitCount: 1);

        if (lease.IsAcquired)
            return PolicyDecision.Allow();

        if (lease.TryGetMetadata(MetadataName.RetryAfter, out TimeSpan retryAfter))
            return PolicyDecision.Defer(retryAfter, "rate limit exceeded");

        return PolicyDecision.Reject("rate limit exceeded");
    }

    /// <summary>
    /// Token bucket: refills <paramref name="permitsPerSecond"/>
    /// tokens per second, up to a ceiling of
    /// <paramref name="burstCapacity"/>. Good for "smooth average,
    /// occasional spikes" traffic shapes (most APIs).
    /// </summary>
    public static RateLimitIngressPolicy TokenBucket(
        int permitsPerSecond,
        int burstCapacity,
        int? queueLimit = null)
    {
        var limiter = new TokenBucketRateLimiter(new TokenBucketRateLimiterOptions
        {
            TokenLimit          = burstCapacity,
            TokensPerPeriod     = permitsPerSecond,
            ReplenishmentPeriod = TimeSpan.FromSeconds(1),
            QueueLimit          = queueLimit ?? 0,
            QueueProcessingOrder = QueueProcessingOrder.OldestFirst,
            AutoReplenishment   = true,
        });
        return new RateLimitIngressPolicy(limiter);
    }

    /// <summary>
    /// Fixed window: <paramref name="permits"/> per
    /// <paramref name="window"/>, no smoothing. Cheap; rough at
    /// window boundaries.
    /// </summary>
    public static RateLimitIngressPolicy FixedWindow(
        int permits,
        TimeSpan window,
        int? queueLimit = null)
    {
        var limiter = new FixedWindowRateLimiter(new FixedWindowRateLimiterOptions
        {
            PermitLimit = permits,
            Window      = window,
            QueueLimit  = queueLimit ?? 0,
            QueueProcessingOrder = QueueProcessingOrder.OldestFirst,
            AutoReplenishment    = true,
        });
        return new RateLimitIngressPolicy(limiter);
    }

    /// <summary>
    /// Sliding window: <paramref name="permits"/> per
    /// <paramref name="window"/>, divided into
    /// <paramref name="segments"/> sub-windows for smoother boundary
    /// behavior.
    /// </summary>
    public static RateLimitIngressPolicy SlidingWindow(
        int permits,
        TimeSpan window,
        int segments = 8,
        int? queueLimit = null)
    {
        var limiter = new SlidingWindowRateLimiter(new SlidingWindowRateLimiterOptions
        {
            PermitLimit       = permits,
            Window            = window,
            SegmentsPerWindow = segments,
            QueueLimit        = queueLimit ?? 0,
            QueueProcessingOrder = QueueProcessingOrder.OldestFirst,
            AutoReplenishment    = true,
        });
        return new RateLimitIngressPolicy(limiter);
    }

    /// <summary>
    /// Bounded concurrency: at most <paramref name="permits"/>
    /// in-flight at any moment. Permits release when the lease is
    /// disposed — this policy releases immediately on acquisition,
    /// so it functions as a flat admission gate rather than a true
    /// concurrency limiter. Use <c>ConcurrencyExecutionPolicy</c> in
    /// <c>Spek.Resilience.Bulkheads</c> (when shipped) for leases
    /// held across handler execution.
    /// </summary>
    public static RateLimitIngressPolicy Concurrency(int permits)
    {
        var limiter = new ConcurrencyLimiter(new ConcurrencyLimiterOptions
        {
            PermitLimit = permits,
            QueueLimit  = 0,
            QueueProcessingOrder = QueueProcessingOrder.OldestFirst,
        });
        return new RateLimitIngressPolicy(limiter);
    }

    public async ValueTask DisposeAsync()
    {
        if (_ownsLimiter)
            await _limiter.DisposeAsync().ConfigureAwait(false);
    }
}
