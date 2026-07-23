using BenchmarkDotNet.Attributes;
using Spek;
using Spek.Runtime;

namespace Spek.Benchmarks;

/// <summary>
/// The ask path beyond the plain sequential round trip: what the timeout
/// overload's wrapper costs per call, what hand-rolling request-reply with
/// Tell plus your own TaskCompletionSource costs against the built-in ask,
/// and how reply-cell allocation and sender routing hold up under 64-way
/// asker contention. <see cref="SequentialAsk"/> is the reference row the
/// two other sequential arms compare against (same 1000 round trips).
/// </summary>
[MemoryDiagnoser]
public class AskBenchmarks
{
    private const int SequentialRoundTrips = 1_000;
    private const int Askers = 64;
    private const int Waves = 1_000;

    /// <summary>The plain ValueTask-shaped ask (perf r7), 1000 sequential
    /// round trips: one reply cell per ask, no Task&lt;T&gt;. The reference
    /// for the timeout and Tell-reply arms below.</summary>
    [Benchmark]
    public async Task SequentialAsk()
    {
        using var system = new ActorSystem("bench");
        var actor = system.Spawn<EchoActor>();
        for (int i = 0; i < SequentialRoundTrips; i++)
            await actor.AskAsync<Pong>(new Ping());
    }

    /// <summary>
    /// The deadline overload — since perf r11 the timeout rides the reply
    /// cell (one entry on the system's shared deadline sweeper) instead of
    /// wrapping the ValueTask in <c>AsTask().WaitAsync(timeout)</c>, so the
    /// delta over <see cref="SequentialAsk"/> should be near zero. The
    /// timeout (30s) never fires; before r11 this row cost ~2x the plain
    /// ask's allocation (the bridging Task plus WaitAsync's timer).
    /// </summary>
    [Benchmark]
    public async Task SequentialAskWithTimeout()
    {
        using var system = new ActorSystem("bench");
        var actor = system.Spawn<EchoActor>();
        for (int i = 0; i < SequentialRoundTrips; i++)
            await actor.AskAsync<Pong>(new Ping(), TimeSpan.FromSeconds(30));
    }

    /// <summary>
    /// The idiom users weigh against ask: Tell with a probe actor as sender,
    /// the target replying via <c>sender.Tell</c>, the caller awaiting its
    /// own armed TaskCompletionSource. Costs TWO mailbox hops per round trip
    /// (target, then probe) plus a fresh TCS, where ask pays one hop and a
    /// reply cell that completes the awaiter directly — the structural reason
    /// the reply idiom favors <c>return new X(...)</c> over probe plumbing.
    /// </summary>
    [Benchmark]
    public async Task SequentialTellWithReplyProbe()
    {
        using var system = new ActorSystem("bench");
        var echo = system.Spawn<EchoActor>();
        var sink = new ReplySink();
        var probe = system.Spawn<ReplySinkActor>(sink);
        for (int i = 0; i < SequentialRoundTrips; i++)
        {
            var reply = sink.Arm();
            echo.Tell(new Ping(), probe);
            await reply;
        }
    }

    /// <summary>
    /// 64 concurrent askers against ONE actor, 1000 waves (64k asks total):
    /// each wave fires 64 asks and awaits their Task.WhenAll. Measures reply-
    /// cell allocation and sender routing under contention — 64 enqueues race
    /// for the mailbox, the writer path serializes the dispatches, and 64
    /// reply cells complete against concurrently-registered awaiters. Divide
    /// by 64,000 for per-ask figures; not comparable to the sequential rows
    /// (the actor never parks between asks here).
    /// </summary>
    [Benchmark]
    public async Task ConcurrentAskFanIn()
    {
        using var system = new ActorSystem("bench");
        var actor = system.Spawn<EchoActor>();
        var inFlight = new Task<Pong>[Askers];
        for (int wave = 0; wave < Waves; wave++)
        {
            for (int i = 0; i < Askers; i++)
                inFlight[i] = actor.AskAsync<Pong>(new Ping()).AsTask();
            await Task.WhenAll(inFlight);
        }
    }
}
