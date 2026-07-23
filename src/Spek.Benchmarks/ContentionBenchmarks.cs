using BenchmarkDotNet.Attributes;
using Spek;
using Spek.Runtime;

namespace Spek.Benchmarks;

/// <summary>
/// Quantifies the thread-pool dispatch model's behaviour under handlers that
/// occupy their thread. Each actor receives ONE message whose handler waits
/// ~20ms; <see cref="ActorCount"/> actors are fanned out at once.
///
/// - <see cref="AsyncWait_FanOut"/> models the wait with <c>await Task.Delay</c>:
///   handlers yield their pool thread, so all N waits overlap and the batch
///   finishes in roughly one wait (~20ms) regardless of N.
/// - <see cref="BlockingWait_FanOut"/> models it with <c>Thread.Sleep</c>:
///   each handler parks a pool thread. Once N exceeds the pool's ready threads,
///   later actors wait for thread injection (~1–2/sec), so the batch time grows
///   far past one wait. The ThreadingDiagnoser delta between the two makes the
///   cost concrete.
///
/// This is the runtime characteristic behind the test-suite flakiness: a
/// blocking handler starves siblings. CE0083 flags <c>Thread.Sleep</c> /
/// <c>.Wait()</c> / <c>.GetResult()</c> in Spek source for exactly this reason;
/// the numbers here are why.
/// </summary>
[MemoryDiagnoser]
public class ContentionBenchmarks
{
    [Params(64)]
    public int ActorCount;

    [GlobalSetup]
    public void Setup() { BlockingActor.BlockMs = 20; AsyncWaitActor.WaitMs = 20; }

    [Benchmark(Baseline = true)]
    public async Task AsyncWait_FanOut()
    {
        using var system = new ActorSystem("bench");
        var tasks = new Task<Pong>[ActorCount];
        for (int i = 0; i < ActorCount; i++)
        {
            var actor = system.Spawn<AsyncWaitActor>();
            tasks[i] = actor.AskAsync<Pong>(new Ping());
        }
        await Task.WhenAll(tasks);
    }

    [Benchmark]
    public async Task BlockingWait_FanOut()
    {
        using var system = new ActorSystem("bench");
        var tasks = new Task<Pong>[ActorCount];
        for (int i = 0; i < ActorCount; i++)
        {
            var actor = system.Spawn<BlockingActor>();
            tasks[i] = actor.AskAsync<Pong>(new Ping());
        }
        await Task.WhenAll(tasks);
    }
}
