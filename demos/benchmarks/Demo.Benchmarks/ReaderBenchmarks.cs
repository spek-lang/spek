using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Configs;

namespace Demo.Benchmarks;

/// <summary>
/// The reader-path pair (added in perf r8; existing pairs untouched, so
/// prior rounds stay comparable — this pair's first run is its own
/// mini-baseline). One board actor, 10,000 concurrent lookups against a
/// seeded table: on the Spek side each lookup is an ask into a
/// <c>reader on</c> handler (readers overlap by construction, serialized
/// only against the writer); the C# twin is the honest hand-rolled
/// equivalent, a <see cref="ReaderWriterLockSlim"/> around a dictionary.
/// </summary>
[MemoryDiagnoser]
[GroupBenchmarksBy(BenchmarkLogicalGroupRule.ByCategory)]
[CategoriesColumn]
public class ReaderBenchmarks
{
    private const int Lookups = 10_000;
    private const int Keys = 1_000;
    private const int Parallelism = 64;

    private Readers.SpekBoard _spek = null!;
    private RwLockBoard _csharp = null!;

    [GlobalSetup]
    public async Task Setup()
    {
        _spek = new Readers.SpekBoard();
        _csharp = new RwLockBoard();
        for (var k = 0; k < Keys; k++)
        {
            _spek.Post(k, k * 7);
            _csharp.Post(k, k * 7);
        }
        await _spek.DrainAsync();
    }

    [GlobalCleanup]
    public async Task Cleanup()
    {
        await _spek.DisposeAsync();
        _csharp.Dispose();
    }

    [BenchmarkCategory("Lookup"), Benchmark(Baseline = true, OperationsPerInvoke = Lookups)]
    public Task CSharpLookups() => BurstAsync(k => Task.FromResult(_csharp.Lookup(k)));

    [BenchmarkCategory("Lookup"), Benchmark(OperationsPerInvoke = Lookups)]
    public Task SpekLookups() => BurstAsync(k => _spek.LookupAsync(k));

    private static async Task BurstAsync(Func<int, Task<int>> lookup)
    {
        var rng = new Random(2026);
        var gate = new SemaphoreSlim(Parallelism);
        var inFlight = new Task[Lookups];
        for (var i = 0; i < Lookups; i++)
        {
            var key = rng.Next(Keys);
            await gate.WaitAsync();
            inFlight[i] = Run(lookup, key, gate);
        }
        await Task.WhenAll(inFlight);

        static async Task Run(Func<int, Task<int>> lookup, int key, SemaphoreSlim gate)
        {
            try { await lookup(key); }
            finally { gate.Release(); }
        }
    }
}

/// <summary>
/// The C# twin: what a careful developer writes for read-heavy shared
/// state without an actor runtime — a dictionary behind a
/// ReaderWriterLockSlim. Written to be defended; the subtleties (lock
/// recursion policy, dispose while readers drain, upgrade deadlocks it
/// carefully avoids by never upgrading) are the demo content.
/// </summary>
public sealed class RwLockBoard : IDisposable
{
    private readonly Dictionary<int, int> _scores = new();
    private readonly ReaderWriterLockSlim _lock = new(LockRecursionPolicy.NoRecursion);

    public void Post(int key, int value)
    {
        _lock.EnterWriteLock();
        try { _scores[key] = value; }
        finally { _lock.ExitWriteLock(); }
    }

    public int Lookup(int key)
    {
        _lock.EnterReadLock();
        try { return _scores.TryGetValue(key, out var v) ? v : 0; }
        finally { _lock.ExitReadLock(); }
    }

    public void Dispose() => _lock.Dispose();
}
