using BenchmarkDotNet.Attributes;
using Spek;
using Spek.Runtime;

namespace Spek.Benchmarks;

/// <summary>
/// The mailbox's distinct operating shapes — the per-primitive costs that
/// compound in large systems. <see cref="MessagingBenchmarks.TellThroughput"/>
/// races producer against consumer; the shapes here isolate the paths that
/// blend together there: the cold wake out of a parked mailbox (the latency
/// number for request-reply services, target of the perf-r10 second-chance
/// round), one continuous drain of a deep pre-filled backlog, and the
/// two-actor ping-pong where every hop wakes an idle peer.
/// </summary>
[MemoryDiagnoser]
public class MailboxBenchmarks
{
    private const int ColdAsks = 1_000;
    private const int PingPongRounds = 10_000;
    private const int BacklogDepth = 100_000;

    /// <summary>
    /// One ask at a time against an actor whose mailbox is verifiably PARKED
    /// (1000 round trips): park → work-item queue → drain start → handler →
    /// reply → awaiter resumed. Back-to-back asks land inside the drain
    /// loop's second-chance spin window and skip the pool hop, so between
    /// asks this spins until <see cref="ActorSystem.IsIdle"/> confirms the
    /// loop parked — the idle poll (a lock plus a one-slot scan, typically
    /// one or two iterations) is included in the mean. Compare against
    /// <see cref="MessagingBenchmarks.AskRoundTrip"/>: that delta is what the
    /// second-chance window saves.
    /// </summary>
    [Benchmark]
    public async Task ColdWakeAsk()
    {
        using var system = new ActorSystem("bench");
        var actor = system.Spawn<EchoActor>();
        for (int i = 0; i < ColdAsks; i++)
        {
            await actor.AskAsync<Pong>(new Ping());
            // The drain loop is still in its pure-spin second chance when the
            // await resumes; wait for it to park so the next ask pays the
            // full cold-wake path instead of being caught by the spin.
            while (!system.IsIdle) Thread.SpinWait(20);
        }
    }

    /// <summary>
    /// One continuous drain of a 100k-deep backlog. The first message parks
    /// the dispatch loop on a gate task, the backlog is enqueued behind it
    /// (each Tell's wake attempt fails fast — the loop is already live), then
    /// the gate opens and the loop drains everything in a single run.
    /// Honestly stated: the mean includes the gated enqueue phase (as
    /// TellThroughput's does), but unlike TellThroughput the drain never
    /// interleaves with producers or re-arms. The deep backlog also keeps
    /// mailbox queue segments alive across Gen0 while draining — the
    /// survivor pressure the two-ref mailbox tuple (perf r5) trimmed.
    /// </summary>
    [Benchmark]
    public int DeepDrain()
    {
        using var system = new ActorSystem("bench");
        var gate = new TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously);
        var actor = system.Spawn<GatedCounterActor>(gate.Task);
        actor.Tell(new HoldGate());          // dispatch loop parks on the gate
        for (int i = 0; i < BacklogDepth; i++)
            actor.Tell(new Ping());          // piles up behind the held gate
        gate.SetResult();                    // one continuous drain of the backlog
        system.AwaitTermination();
        return BacklogDepth;
    }

    /// <summary>
    /// Two actors exchanging one message back and forth, 10k round trips: the
    /// initiator Tells the echo, goes idle, and is woken by the reply — so
    /// every hop pays an idle peer's wake (or its second-chance catch; this
    /// alternating shape is exactly what the perf-r10 spin targets).
    /// Dominated by two wake-or-spin transitions plus two dispatches per
    /// round; divide the mean by 10k for the round-trip figure.
    /// </summary>
    [Benchmark]
    public async Task PingPongLatency()
    {
        using var system = new ActorSystem("bench");
        var echo = system.Spawn<EchoActor>();
        var initiator = system.Spawn<PingPongInitiatorActor>();
        var done = new TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously);
        initiator.Tell(new StartVolley(echo, PingPongRounds, done));
        await done.Task;
    }
}
