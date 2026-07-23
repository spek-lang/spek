namespace RateLimiter.Contracts;

/// <summary>
/// What both panes implement: a per-API-key rate limiter (token bucket,
/// capacity 10, refill 5/second). The harness drives both through this one
/// interface with the identical key traffic, so any asymmetry in the table
/// belongs to the implementations.
/// </summary>
public interface IRateLimiter : IAsyncDisposable
{
    /// <summary>Pane label ("Spek" / "C#").</summary>
    string Name { get; }

    /// <summary>One rate-limit decision for one API key.</summary>
    Task<bool> AllowAsync(string apiKey);

    /// <summary>Keys currently resident in memory — the working set, not
    /// the key cardinality. This is the number the idle phase watches.</summary>
    long ResidentKeys();

    /// <summary>Allowed/throttled totals since start.</summary>
    LimiterStats Stats();
}

/// <summary>The verdict counters.</summary>
public sealed record LimiterStats(long Allowed, long Throttled);

/// <summary>The bucket parameters both panes share.</summary>
public static class Limits
{
    public const double Capacity = 10.0;
    public const double RefillPerSecond = 5.0;
    /// <summary>Idle window after which a key's state may be discarded.
    /// Chosen so a reset equals a natural full refill: Capacity /
    /// RefillPerSecond = 2s — an evicted-then-recreated bucket behaves
    /// exactly like one that idled and refilled.</summary>
    public static readonly TimeSpan IdleEviction = TimeSpan.FromSeconds(2);
}
