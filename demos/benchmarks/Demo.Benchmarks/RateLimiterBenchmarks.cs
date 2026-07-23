using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Configs;
using RateLimiter.Contracts;

namespace Demo.Benchmarks;

/// <summary>
/// The rate-limiter panes under BenchmarkDotNet, two shapes per pane.
/// <c>*Check</c> is the steady-state cost of ONE decision on a hot key —
/// on the Spek side that is a full ask round-trip through a mailbox
/// (MemoryDiagnoser shows the reply-slot allocation this era pays per
/// ask), on the C# side a lock-protected dictionary read. <c>*Burst</c>
/// is 10,000 decisions across a mixed hot/cold keyspace with bounded
/// concurrency, per-check via OperationsPerInvoke. The C# twin is the
/// baseline; the Ratio column is the price of serialized-by-mailbox
/// decisions, virtualizable time, and passivation-as-eviction.
/// </summary>
[MemoryDiagnoser]
[GroupBenchmarksBy(BenchmarkLogicalGroupRule.ByCategory)]
[CategoriesColumn]
public class RateLimiterBenchmarks
{
    private const int BurstChecks = 10_000;
    private const int BurstKeys = 500;
    private const int Parallelism = 64;

    private RateLimiter.SpekLimiter _spek = null!;
    private RateLimiter.CSharp.ShardedLimiter _csharp = null!;

    [GlobalSetup]
    public async Task Setup()
    {
        _spek = new RateLimiter.SpekLimiter();
        _csharp = new RateLimiter.CSharp.ShardedLimiter();
        // Materialize the hot key on both panes so the first measured
        // check isn't a spawn.
        await _spek.AllowAsync("hot");
        await _csharp.AllowAsync("hot");
    }

    [GlobalCleanup]
    public async Task Cleanup()
    {
        await _spek.DisposeAsync();
        await _csharp.DisposeAsync();
    }

    // ── one decision, hot key, steady state ─────────────────────────────

    [BenchmarkCategory("Check"), Benchmark(Baseline = true)]
    public Task<bool> CSharpCheck() => _csharp.AllowAsync("hot");

    [BenchmarkCategory("Check"), Benchmark]
    public Task<bool> SpekCheck() => _spek.AllowAsync("hot");

    // ── a burst across a mixed keyspace ─────────────────────────────────

    [BenchmarkCategory("Burst"), Benchmark(Baseline = true, OperationsPerInvoke = BurstChecks)]
    public Task CSharpBurst() => BurstAsync(_csharp);

    [BenchmarkCategory("Burst"), Benchmark(OperationsPerInvoke = BurstChecks)]
    public Task SpekBurst() => BurstAsync(_spek);

    private static async Task BurstAsync(IRateLimiter limiter)
    {
        // Deterministic key stream, same shape as the demo harness:
        // every fifth check goes to the cold tail.
        var rng = new Random(2026);
        var gate = new SemaphoreSlim(Parallelism);
        var inFlight = new Task[BurstChecks];
        for (var i = 0; i < BurstChecks; i++)
        {
            var key = i % 5 == 0
                ? $"key-{rng.Next(BurstKeys / 10, BurstKeys)}"
                : $"key-{rng.Next(BurstKeys / 10)}";
            await gate.WaitAsync();
            inFlight[i] = Check(limiter, key, gate);
        }
        await Task.WhenAll(inFlight);
    }

    private static async Task Check(IRateLimiter limiter, string key, SemaphoreSlim gate)
    {
        try { await limiter.AllowAsync(key); }
        finally { gate.Release(); }
    }
}
