using System.Collections.Concurrent;
using RateLimiter.Contracts;

namespace RateLimiter.CSharp;

/// <summary>
/// The C# twin: a per-key token bucket the way a careful C# developer
/// builds one today — a <see cref="ConcurrentDictionary{TKey,TValue}"/> of
/// buckets, a lock per bucket around the refill-and-take, and a sweeper
/// timer for eviction, because without one the dictionary grows with key
/// cardinality forever. This twin is written to be defended; the subtle
/// parts are called out where they live. If it can be written better,
/// improve it — the comparison only means something if this side is as
/// good as it can be.
/// </summary>
public sealed class ShardedLimiter : IRateLimiter
{
    private sealed class Bucket
    {
        public double Tokens = Limits.Capacity;
        public long LastTouchTicks = Environment.TickCount64;
        public readonly object Gate = new();
    }

    private readonly ConcurrentDictionary<string, Bucket> _buckets = new();
    private readonly Timer _sweeper;
    private long _allowed;
    private long _throttled;

    public ShardedLimiter()
    {
        // The eviction story, by hand: a sweeper that scans every bucket.
        // Its period, its scan cost at high cardinality, and its race with
        // in-flight requests are all ours to own now.
        _sweeper = new Timer(_ => Sweep(), null,
            Limits.IdleEviction, TimeSpan.FromMilliseconds(500));
    }

    public string Name => "C#";

    public Task<bool> AllowAsync(string apiKey)
    {
        var bucket = _buckets.GetOrAdd(apiKey, _ => new Bucket());
        bool allowed;
        lock (bucket.Gate)
        {
            // Refill under the same lock as the take, or two concurrent
            // requests can both observe the pre-refill balance.
            var now = Environment.TickCount64;
            var elapsedSeconds = (now - bucket.LastTouchTicks) / 1000.0;
            bucket.Tokens = Math.Min(Limits.Capacity,
                bucket.Tokens + elapsedSeconds * Limits.RefillPerSecond);
            bucket.LastTouchTicks = now;

            allowed = bucket.Tokens >= 1.0;
            if (allowed) bucket.Tokens -= 1.0;
        }

        if (allowed) Interlocked.Increment(ref _allowed);
        else Interlocked.Increment(ref _throttled);
        return Task.FromResult(allowed);
    }

    private void Sweep()
    {
        var cutoff = Environment.TickCount64 - (long)Limits.IdleEviction.TotalMilliseconds;
        foreach (var (key, bucket) in _buckets)
        {
            // The evict-vs-refill race: a request may be acquiring this
            // bucket right now. Re-check idleness under the bucket's lock,
            // and accept the residual race where a request that already
            // fetched the reference keeps using a bucket we just removed —
            // its next request recreates the bucket full, which is the
            // same semantics as a natural 2-second refill. That argument
            // is subtle enough to need this comment; the other pane's
            // equivalent is one passivate clause.
            if (Volatile.Read(ref bucket.LastTouchTicks) < cutoff)
            {
                lock (bucket.Gate)
                {
                    if (bucket.LastTouchTicks < cutoff)
                        _buckets.TryRemove(key, out Bucket? _);
                }
            }
        }
    }

    public long ResidentKeys() => _buckets.Count;

    public LimiterStats Stats()
        => new(Interlocked.Read(ref _allowed), Interlocked.Read(ref _throttled));

    public ValueTask DisposeAsync()
    {
        _sweeper.Dispose();
        return ValueTask.CompletedTask;
    }
}
