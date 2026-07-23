using BenchmarkDotNet.Attributes;
using Spek;
using Spek.Runtime;

namespace Spek.Benchmarks;

/// <summary>
/// Cost of the failure path: a handler that throws under a Restart directive,
/// measured as whole-run throughput including the restarts. A crash costs an
/// exception throw/catch, a dead-letter report, restart bookkeeping, and a
/// fresh instance via the slot factory on the next dispatch. All three
/// benchmarks run the same actor with the same quiet dead-letter sink — the
/// default sink writes a Console.Error line per crash, which would otherwise
/// drown the supervision machinery in console I/O.
/// </summary>
[MemoryDiagnoser]
public class SupervisionBenchmarks
{
    [Params(10_000)]
    public int Messages;

    private static readonly NoopDeadLetterSink Sink = new();

    /// <summary>Same actor, crashes disabled — the healthy-path reference the
    /// crash arms are compared against.</summary>
    [Benchmark(Baseline = true)]
    public int NoCrash_Baseline()
    {
        CrashingCounterActor.CrashEvery = 0;
        return Run();
    }

    /// <summary>One crash→restart cycle per 100 messages (1% failure rate).
    /// The delta over baseline, divided by the number of restarts, is the
    /// per-restart cost: throw/catch, supervision resolution, dead-letter,
    /// and re-materialisation of the replacement instance.</summary>
    [Benchmark]
    public int CrashEvery100th_Restart()
    {
        CrashingCounterActor.CrashEvery = 100;
        return Run();
    }

    /// <summary>The restart storm: every message crashes, so every dispatch
    /// pays the full failure path and re-materialises the actor. This is the
    /// worst case — an actor stuck in a crash loop under an unlimited restart
    /// budget — and puts an upper bound on what supervision can cost.</summary>
    [Benchmark]
    public int RestartStorm_EveryMessage()
    {
        CrashingCounterActor.CrashEvery = 1;
        return Run();
    }

    private int Run()
    {
        using var system = new ActorSystem("bench", deadLetterSink: Sink);
        var actor = system.Spawn<CrashingCounterActor>();
        for (int i = 0; i < Messages; i++)
            actor.Tell(new Ping());
        system.AwaitTermination();
        return Messages;
    }
}

/// <summary>Swallows dead letters so the crash benchmarks measure the
/// supervision machinery rather than Console.Error I/O. Attached to the
/// baseline too, keeping the comparison apples-to-apples.</summary>
internal sealed class NoopDeadLetterSink : IDeadLetterSink
{
    public void DeadLetter(object message, string reason, Exception? cause) { }
}
