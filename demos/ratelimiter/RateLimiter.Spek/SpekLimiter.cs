using System.Collections.Concurrent;
using RateLimiter.Contracts;
using Spek;
using Spek.Runtime;

namespace RateLimiter;

/// <summary>
/// Host adapter for the Spek pane: routes each key's checks to that key's
/// actor. This dictionary is only routing (key -> ref) — all state,
/// timing, and eviction live in the actor. Residency is read from
/// <see cref="ActorSystem.SnapshotActors"/>: a passivated key's slot stays
/// (the ref keeps working) but its instance is gone, which is exactly the
/// "working set, not cardinality" number the demo is about.
/// </summary>
public sealed class SpekLimiter : IRateLimiter
{
    private readonly ActorSystem _system = new("ratelimiter-spek");
    private readonly ConcurrentDictionary<string, ActorRef> _keys = new();
    private long _allowed;
    private long _throttled;

    public string Name => "Spek";

    public async Task<bool> AllowAsync(string apiKey)
    {
        var actor = _keys.GetOrAdd(apiKey, _ => _system.Spawn<ApiKey>());
        var verdict = await actor.AskAsync<Verdict>(new Check());
        if (verdict.allowed) Interlocked.Increment(ref _allowed);
        else Interlocked.Increment(ref _throttled);
        return verdict.allowed;
    }

    public long ResidentKeys()
        => _system.SnapshotActors().Count(s => s.IsMaterialized);

    public LimiterStats Stats()
        => new(Interlocked.Read(ref _allowed), Interlocked.Read(ref _throttled));

    public ValueTask DisposeAsync()
    {
        _system.Dispose();
        return ValueTask.CompletedTask;
    }
}
