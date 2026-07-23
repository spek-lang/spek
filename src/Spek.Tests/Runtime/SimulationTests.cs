using Spek;
using Spek.Runtime;
using Spek.Testing;
using Xunit;

namespace Spek.Tests.Runtime;

/// <summary>
/// Deterministic simulation (D1): execution is a pure function of
/// (program, inputs, seed). Same seed → identical interleaving, message
/// for message; virtual time makes idle windows free; chaos faults become
/// seeded decisions.
/// </summary>
public sealed class SimulationTests
{
    private sealed record Nudge();
    private sealed record Item(string From, int N);
    private sealed record GetLog();
    private sealed record LogReply(string Joined);

    /// <summary>Records arrival order — the interleaving made visible.</summary>
    private sealed class Collector : ActorBase
    {
        private readonly List<string> _log = [];
        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            switch (message)
            {
                case Item i: _log.Add($"{i.From}{i.N}"); break;
                case GetLog: sender.Tell(new LogReply(string.Join(",", _log)), _selfRef!); break;
            }
            return Task.CompletedTask;
        }
    }

    /// <summary>On each Nudge, sends the next item to the collector.</summary>
    private sealed class Feeder : ActorBase
    {
        private readonly ActorRef _collector;
        private readonly string _name;
        private int _n;
        public Feeder(ActorRef collector, string name) { _collector = collector; _name = name; }
        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            _collector.Tell(new Item(_name, ++_n), _selfRef!);
            return Task.CompletedTask;
        }
    }

    private static string InterleavingFor(int seed)
    {
        using var sim = new SimulatedActorSystem(seed, audit: true);
        var collector = sim.Spawn<Collector>();
        var a = sim.Spawn<Feeder>(collector, "a");
        var b = sim.Spawn<Feeder>(collector, "b");
        for (int i = 0; i < 5; i++) { a.Tell(new Nudge()); b.Tell(new Nudge()); }
        sim.Run();   // the seeded interleaving happens here…
        return sim.Ask<LogReply>(collector, new GetLog()).Joined;   // …the probe reads it after
    }

    [Fact]
    public void SameSeed_ReplaysTheIdenticalInterleaving()
    {
        var first = InterleavingFor(20260710);
        var second = InterleavingFor(20260710);
        Assert.Equal(first, second);

        // And every message arrived — the schedule varies, the work doesn't.
        Assert.Equal(10, first.Split(',').Length);
    }

    [Fact]
    public void DifferentSeeds_ExploreDifferentLegalSchedules()
    {
        // Not guaranteed for any single pair, so probe a few: at least two
        // distinct interleavings should appear across five seeds.
        var runs = Enumerable.Range(1, 5).Select(InterleavingFor).Distinct().ToArray();
        Assert.True(runs.Length >= 2,
            $"five seeds produced a single interleaving: {runs[0]}");
    }

    // ─── virtual time ────────────────────────────────────────────────────

    private sealed class IdleSession : ActorBase
    {
        protected override Task DispatchAsync(object message, ActorRef sender)
            => Task.CompletedTask;
        protected override TimeSpan? PassivationTimeout => TimeSpan.FromMinutes(10);
    }

    [Fact]
    public void PassivationWindow_ElapsesInMicroseconds()
    {
        using var sim = new SimulatedActorSystem(seed: 1, audit: true);
        var session = sim.SpawnPersistent<IdleSession>("session-1");
        session.Tell(new Nudge());
        sim.Run();

        Assert.True(session.IsMaterialized);
        sim.Advance(TimeSpan.FromMinutes(11));   // ten idle minutes, one call
        Assert.False(session.IsMaterialized);    // passivated
        Assert.False(session.IsStopped);         // not terminated
    }

    // ─── asks inside handlers (the pending-dispatch path) ────────────────

    private sealed record Double(int N);
    private sealed record Doubled(int N);
    private sealed record Compute(int N);
    private sealed record Computed(int N);

    private sealed class Doubler : ActorBase
    {
        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            if (message is Double d) sender.Tell(new Doubled(d.N * 2), _selfRef!);
            return Task.CompletedTask;
        }
    }

    private sealed class Orchestrator : ActorBase
    {
        private readonly ActorRef _doubler;
        public Orchestrator(ActorRef doubler) => _doubler = doubler;
        protected override async Task DispatchAsync(object message, ActorRef sender)
        {
            if (message is Compute c)
            {
                // Parks this handler mid-dispatch until the doubler replies.
                var d = await AskAsync<Doubled>(_doubler, new Double(c.N));
                sender.Tell(new Computed(d.N + 1), _selfRef!);
            }
        }
        private static ValueTask<T> AskAsync<T>(ActorRef target, object msg) => target.AskAsync<T>(msg);
    }

    [Fact]
    public void HandlerAwaitingAnAsk_ParksAndResumes_Deterministically()
    {
        using var sim = new SimulatedActorSystem(seed: 7, audit: true);
        var doubler = sim.Spawn<Doubler>();
        var orch = sim.Spawn<Orchestrator>(doubler);

        var reply = sim.Ask<Computed>(orch, new Compute(20));
        Assert.Equal(41, reply.N);
    }

    // ─── chaos under the seed ────────────────────────────────────────────

    private sealed class Fragile : ActorBase
    {
        public static int Handled;
        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            Interlocked.Increment(ref Handled);
            return Task.CompletedTask;
        }
    }

    [Fact]
    public void ChaosCrash_UnderSimulation_IsSeedDeterministic()
    {
        string RunOnce()
        {
            Fragile.Handled = 0;
            var chaos = new ChaosPlan().CrashOnNth<Fragile>(n: 3);
            using var sim = new SimulatedActorSystem(seed: 41, chaos: chaos, audit: true);
            var actor = sim.Spawn<Fragile>();
            for (int i = 0; i < 5; i++) actor.Tell(new Nudge());
            sim.Run();
            return $"handled={Volatile.Read(ref Fragile.Handled)} stopped={actor.IsStopped} " +
                   $"deadletters={sim.DeadLetters.Records.Count}";
        }

        var first = RunOnce();
        var second = RunOnce();
        Assert.Equal(first, second);
        Assert.Contains("stopped=True", first);   // default supervision: Stop at the 3rd dispatch
    }
}
