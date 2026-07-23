using System.Diagnostics;
using RateLimiter.Contracts;

// The rate-limiter demo harness: identical key traffic through both panes,
// in three acts. Act 1 proves the bucket (hammer one key, check the
// arithmetic). Act 2 is the load: a large keyspace with a hot working set.
// Act 3 is the point: everyone goes home, and the working set — not the
// key cardinality — is what stays in memory.

var keys = ArgInt("--keys", 20_000);
var requests = ArgInt("--requests", 200_000);
var parallelism = ArgInt("--parallelism", 256);

Console.WriteLine($"rate limiter demo — {requests:N0} checks across {keys:N0} API keys " +
                  $"(bucket: {Limits.Capacity:0} burst, {Limits.RefillPerSecond:0}/s refill, " +
                  $"{Limits.IdleEviction.TotalSeconds:0}s idle eviction)\n");

var results = new List<Pane>();
var failures = new List<string>();

foreach (var limiter in new IRateLimiter[] { new RateLimiter.SpekLimiter(), new RateLimiter.CSharp.ShardedLimiter() })
{
    await using var _ = limiter;

    // ── Act 1: the bucket arithmetic, one key, sequential hammer ─────────
    var sanitySw = Stopwatch.StartNew();
    var sanityAllowed = 0;
    for (var i = 0; i < 100; i++)
        if (await limiter.AllowAsync("sanity-key")) sanityAllowed++;
    sanitySw.Stop();

    // Exactly Capacity immediately, plus whatever refilled while we
    // hammered, with one token of rounding slack.
    var refillBudget = (int)Math.Ceiling(sanitySw.Elapsed.TotalSeconds * Limits.RefillPerSecond) + 1;
    var lo = (int)Limits.Capacity;
    var hi = (int)Limits.Capacity + refillBudget;
    if (sanityAllowed < lo || sanityAllowed > hi)
        failures.Add($"{limiter.Name}: sanity key allowed {sanityAllowed}, expected {lo}..{hi}");

    // ── Act 2: the load — hot working set over a large keyspace ─────────
    // 1% of keys receive 80% of traffic (every fifth request goes to a
    // cold key), the shape API traffic actually has. Deterministic stream.
    var rng = new Random(2026);
    var hotKeys = Math.Max(1, keys / 100);
    var loadSw = Stopwatch.StartNew();
    var gate = new SemaphoreSlim(parallelism);
    var inFlight = new List<Task>(requests);
    for (var i = 0; i < requests; i++)
    {
        var key = i % 5 == 0
            ? $"key-{rng.Next(hotKeys, keys)}"        // the long cold tail
            : $"key-{rng.Next(hotKeys)}";             // the hot set
        await gate.WaitAsync();
        inFlight.Add(Check(limiter, key, gate));
    }
    await Task.WhenAll(inFlight);
    loadSw.Stop();

    var residentAfterLoad = limiter.ResidentKeys();

    // ── Act 3: everyone goes home ────────────────────────────────────────
    await Task.Delay(Limits.IdleEviction + TimeSpan.FromSeconds(2));
    var residentAfterIdle = limiter.ResidentKeys();
    if (residentAfterIdle > residentAfterLoad / 20)
        failures.Add($"{limiter.Name}: {residentAfterIdle:N0} keys still resident after idle " +
                     $"(was {residentAfterLoad:N0})");

    results.Add(new Pane(limiter.Name, limiter.Stats(), sanityAllowed,
        residentAfterLoad, residentAfterIdle, loadSw.Elapsed));
}

var (s, c) = (results[0], results[1]);
Console.WriteLine($"{"",-26}{s.Name,14}{c.Name,14}");
Row("checks sent", requests + 100, requests + 100);
Row("allowed", s.Stats.Allowed, c.Stats.Allowed);
Row("throttled", s.Stats.Throttled, c.Stats.Throttled);
Row("sanity key allowed", s.SanityAllowed, c.SanityAllowed);
Row("resident after load", s.ResidentAfterLoad, c.ResidentAfterLoad);
Row("resident after idle", s.ResidentAfterIdle, c.ResidentAfterIdle);
Console.WriteLine($"{"check throughput",-26}{Rate(s),12:N0}/s{Rate(c),12:N0}/s");

Console.WriteLine();
Console.WriteLine("Eviction on the Spek pane is the passivate clause in Limiter.spek —");
Console.WriteLine("idle keys leave memory on their own, and the window equals the refill");
Console.WriteLine("time so a reset is indistinguishable from a refill. The C# pane earns");
Console.WriteLine("the same row with a sweeper timer, a lock per bucket, and an");
Console.WriteLine("evict-vs-refill race argument (ShardedLimiter.cs).");

foreach (var f in failures) Console.Error.WriteLine($"FAIL: {f}");
return failures.Count == 0 ? 0 : 1;

static async Task Check(IRateLimiter limiter, string key, SemaphoreSlim gate)
{
    try { await limiter.AllowAsync(key); }
    finally { gate.Release(); }
}

void Row(string label, long a, long b) =>
    Console.WriteLine($"{label,-26}{a,14:N0}{b,14:N0}");

double Rate(Pane p) => (requests + 100) / Math.Max(0.001, p.LoadElapsed.TotalSeconds);

int ArgInt(string name, int fallback)
{
    var i = Array.IndexOf(args, name);
    return i >= 0 && i + 1 < args.Length && int.TryParse(args[i + 1], out var v) ? v : fallback;
}

internal sealed record Pane(
    string Name, LimiterStats Stats, int SanityAllowed,
    long ResidentAfterLoad, long ResidentAfterIdle, TimeSpan LoadElapsed);
