using Spek.Runtime;
using Spek.Testing;
using Xunit;
using Xunit.Abstractions;

namespace Spek.Tests.Runtime;

/// <summary>
/// Systematic schedule exploration over the deterministic simulator: the
/// mailbox pick is a pluggable <see cref="ISchedulePolicy"/>. Three policies,
/// three value propositions — the default seeded policy replays exactly what
/// it always has (pinned pre-refactor captures prove byte-compatibility), PCT
/// finds priority-inversion-shaped bugs with a probabilistic bound instead of
/// a coin-flip prayer, and bounded exhaustive enumeration turns a small
/// scenario's green run into a proof over its whole schedule space.
/// </summary>
public sealed class ScheduleExplorationTests
{
    private readonly ITestOutputHelper _output;
    public ScheduleExplorationTests(ITestOutputHelper output) => _output = output;

    // ─── shared plumbing ─────────────────────────────────────────────────

    private sealed record Nudge();
    private sealed record Item(string From, int N);
    private sealed record GetLog();
    private sealed record LogReply(string Joined);

    /// <summary>Records arrival order and replies on request — the exact
    /// scenario shape the pre-refactor interleavings were captured from.</summary>
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

    /// <summary>Ask-free collector for exploration scenarios: writes into an
    /// injected list so the schedule tree stays purely Tell-driven.</summary>
    private sealed class ListCollector : ActorBase
    {
        private readonly List<string> _log;
        public ListCollector(List<string> log) => _log = log;
        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            if (message is Item i) _log.Add($"{i.From}{i.N}");
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

    // ─── 1. default policy: byte-compatibility with the pre-refactor code ─

    private static string TwoFeederInterleavingFor(int seed)
    {
        using var sim = new SimulatedActorSystem(seed, audit: true);
        var collector = sim.Spawn<Collector>();
        var a = sim.Spawn<Feeder>(collector, "a");
        var b = sim.Spawn<Feeder>(collector, "b");
        for (int i = 0; i < 5; i++) { a.Tell(new Nudge()); b.Tell(new Nudge()); }
        sim.Run();
        return sim.Ask<LogReply>(collector, new GetLog()).Joined;
    }

    private static string ThreeFeederInterleavingFor(int seed)
    {
        using var sim = new SimulatedActorSystem(seed, audit: true);
        var collector = sim.Spawn<Collector>();
        var a = sim.Spawn<Feeder>(collector, "a");
        var b = sim.Spawn<Feeder>(collector, "b");
        var c = sim.Spawn<Feeder>(collector, "c");
        for (int i = 0; i < 4; i++) { a.Tell(new Nudge()); b.Tell(new Nudge()); c.Tell(new Nudge()); }
        sim.Run();
        return sim.Ask<LogReply>(collector, new GetLog()).Joined;
    }

    /// <summary>
    /// The byte-compatibility proof: these interleavings were captured from
    /// the PRE-refactor simulator (the inline <c>_choices.Next</c> pick,
    /// commit b5cbf2b) and pinned verbatim. The extracted
    /// <see cref="RandomSchedulePolicy"/> must reproduce every one — same
    /// seed, same draw sequence, same schedule — so every seed ever quoted in
    /// a failure report or doc stays replayable.
    /// </summary>
    [Fact]
    public void DefaultPolicy_ReproducesThePinnedPreRefactorInterleavings()
    {
        Assert.Equal("b1,b2,b3,a1,a2,a3,a4,a5,b4,b5", TwoFeederInterleavingFor(20260712));
        Assert.Equal("a1,b1,a2,b2,a3,b3,b4,a4,a5,b5", TwoFeederInterleavingFor(424242));
        Assert.Equal("b1,b2,b3,a1,a2,b4,b5,a3,a4,a5", TwoFeederInterleavingFor(777));
        Assert.Equal("b1,c1,a1,c2,a2,b2,b3,a3,a4,c3,b4,c4", ThreeFeederInterleavingFor(20260712));
        Assert.Equal("c1,b1,c2,a1,c3,b2,b3,c4,b4,a2,a3,a4", ThreeFeederInterleavingFor(31337));
    }

    [Fact]
    public void SeedCtor_IsPureDelegation_ToTheDefaultPolicy()
    {
        // The (int seed) constructor and an explicitly-wired
        // RandomSchedulePolicy over the same seed are the same simulator.
        string Explicit(int seed)
        {
            var choices = new ChoiceStream(seed);
            using var sim = new SimulatedActorSystem(
                new RandomSchedulePolicy(choices), seed, audit: true);
            var collector = sim.Spawn<Collector>();
            var a = sim.Spawn<Feeder>(collector, "a");
            var b = sim.Spawn<Feeder>(collector, "b");
            for (int i = 0; i < 5; i++) { a.Tell(new Nudge()); b.Tell(new Nudge()); }
            sim.Run();
            return sim.Ask<LogReply>(collector, new GetLog()).Joined;
        }

        Assert.Equal(TwoFeederInterleavingFor(20260712), Explicit(20260712));
        Assert.Equal(TwoFeederInterleavingFor(777), Explicit(777));
    }

    // ─── 2. the planted depth-2 ordering bug ─────────────────────────────
    //
    // Use-before-init across two actors racing into a third: Initializer
    // delivers Init in one dispatch; Slowpoke burns HOPS self-dispatches
    // before delivering Use. The target's mailbox is FIFO, so the violation
    // fires exactly when EVERY Slowpoke dispatch precedes the Initializer's
    // single one — an eight-step priority inversion. Uniform random sampling
    // hits it with probability (1/2)^8 = 1/256 per seed; PCT hits it whenever
    // the priority draw puts the Slowpoke above the Initializer (~1/2 per
    // run); exhaustive enumeration cannot miss it.

    private const int Hops = 7;

    private sealed class RaceBox
    {
        public bool UseSeen;
        public bool UseBeforeInit;
    }

    private sealed record Go();
    private sealed record InitMsg();
    private sealed record UseMsg();
    private sealed record HopMsg(int N);

    private sealed class Guarded : ActorBase
    {
        private readonly RaceBox _box;
        private bool _initialized;
        public Guarded(RaceBox box) => _box = box;
        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            switch (message)
            {
                case InitMsg: _initialized = true; break;
                case UseMsg:
                    _box.UseSeen = true;
                    if (!_initialized) _box.UseBeforeInit = true;
                    break;
            }
            return Task.CompletedTask;
        }
    }

    private sealed class Initializer : ActorBase
    {
        private readonly ActorRef _target;
        public Initializer(ActorRef target) => _target = target;
        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            _target.Tell(new InitMsg(), _selfRef!);
            return Task.CompletedTask;
        }
    }

    private sealed class Slowpoke : ActorBase
    {
        private readonly ActorRef _target;
        private readonly int _hops;
        public Slowpoke(ActorRef target, int hops) { _target = target; _hops = hops; }
        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            switch (message)
            {
                case Go: _selfRef!.Tell(new HopMsg(1), _selfRef); break;
                case HopMsg h when h.N < _hops: _selfRef!.Tell(new HopMsg(h.N + 1), _selfRef); break;
                case HopMsg: _target.Tell(new UseMsg(), _selfRef!); break;
            }
            return Task.CompletedTask;
        }
    }

    /// <summary>Runs the race under <paramref name="sim"/>'s schedule;
    /// returns whether the planted ordering bug fired.</summary>
    private static bool UseBeforeInitHappens(SimulatedActorSystem sim)
    {
        var box = new RaceBox();
        var target = sim.Spawn<Guarded>(box);
        var init = sim.Spawn<Initializer>(target);
        var racer = sim.Spawn<Slowpoke>(target, Hops);
        init.Tell(new Go());
        racer.Tell(new Go());
        sim.Run();
        if (!box.UseSeen)
            throw new InvalidOperationException("scenario broken: Use never arrived");
        return box.UseBeforeInit;
    }

    private static void UseBeforeInitScenario(SimulatedActorSystem sim)
    {
        if (UseBeforeInitHappens(sim))
            throw new InvalidOperationException(
                "use-before-init: Use dispatched before Init reached the target");
    }

    [Fact]
    public void PlantedBug_Exhaustive_FindsIt_AndReportsTheExactSchedule()
    {
        var result = ScheduleExplorer.ExploreAll(UseBeforeInitScenario, maxSchedules: 5_000, audit: true);

        // Found, with the violation and its exact schedule in hand.
        Assert.NotNull(result.Failure);
        Assert.Contains("use-before-init", result.Failure!.Message);
        Assert.NotNull(result.FailingSchedule);

        // The exact schedule IS the bug's anatomy: the racer (pick index 1)
        // wins all eight decisions before the initializer ever runs.
        Assert.Equal(Enumerable.Repeat(1, Hops + 1), result.FailingSchedule!.Take(Hops + 1));

        // DFS order over a deterministic tree is itself deterministic, so
        // the run count at first failure is exact: 44 clean schedules, then
        // the violating one.
        Assert.Equal(PinnedExhaustiveBugRunCount, result.SchedulesExplored);

        // And the reported schedule replays: same picks, same violation.
        var replay = new ReplaySchedulePolicy(result.FailingSchedule);
        using var sim = new SimulatedActorSystem(replay, seed: 0, audit: true);
        Assert.True(UseBeforeInitHappens(sim),
            "the reported failing schedule did not reproduce the violation on replay");
    }

    /// <summary>Empirically verified before pinning (see the sweep in this
    /// test): with base seed <see cref="PctBaseSeed"/>, PCT(depth 2) first
    /// hits the planted bug at this run index. Deterministic — each PCT
    /// schedule is a pure function of (seed, depth, maxSteps).</summary>
    private const int PctBaseSeed = 9_000;
    private const int PctPinnedFirstHitRun = 0;
    private const int PinnedExhaustiveBugRunCount = 45;
    private const int PinnedExhaustiveCleanRunCount = 84;

    [Fact]
    public void PlantedBug_Pct_Depth2_FindsIt_WithinABoundedRunCount()
    {
        var found = -1;
        for (var run = 0; run < 64 && found < 0; run++)
        {
            var policy = new PctSchedulePolicy(seed: PctBaseSeed + run, depth: 2, maxSteps: 32);
            using var sim = new SimulatedActorSystem(policy, seed: policy.Seed, audit: true);
            if (UseBeforeInitHappens(sim)) found = run;
        }

        // No probabilistic flakiness: the first-hit index was verified
        // empirically and pinned. A change here means the PCT schedule
        // derivation changed — re-verify before re-pinning.
        Assert.Equal(PctPinnedFirstHitRun, found);
    }

    [Fact]
    public void PlantedBug_FiftyRandomSeeds_NotedForComparison()
    {
        // The baseline the systematic policies are measured against — noted,
        // deliberately NOT asserted: uniform random sampling owes us nothing
        // here (per-seed hit probability is (1/2)^8 ≈ 0.4%).
        var hits = new List<int>();
        for (var seed = 0; seed < 50; seed++)
        {
            using var sim = new SimulatedActorSystem(seed, audit: true);
            if (UseBeforeInitHappens(sim)) hits.Add(seed);
        }
        _output.WriteLine(
            $"random-seed baseline: {hits.Count}/50 seeds hit the planted use-before-init" +
            (hits.Count > 0 ? $" (seeds: {string.Join(", ", hits)})" : "") +
            " — PCT and exhaustive exploration found it above; random sampling may or may not.");
    }

    // ─── 3. bug-free scenario: full coverage is a proof ──────────────────

    private static void PerSenderFifoScenario(SimulatedActorSystem sim)
    {
        var log = new List<string>();
        var collector = sim.Spawn<ListCollector>(log);
        var a = sim.Spawn<Feeder>(collector, "a");
        var b = sim.Spawn<Feeder>(collector, "b");
        for (int i = 0; i < 2; i++) { a.Tell(new Nudge()); b.Tell(new Nudge()); }
        sim.Run();

        // The invariant that must hold under EVERY schedule: nothing lost,
        // per-sender FIFO preserved (cross-sender order is schedule's choice).
        if (log.Count != 4)
            throw new InvalidOperationException($"expected 4 items, saw {log.Count}");
        if (string.Join(",", log.Where(x => x.StartsWith('a'))) != "a1,a2" ||
            string.Join(",", log.Where(x => x.StartsWith('b'))) != "b1,b2")
            throw new InvalidOperationException($"per-sender order broken: {string.Join(",", log)}");
    }

    [Fact]
    public void BugFreeScenario_Exhaustive_CompletesWithFullCoverage()
    {
        var result = ScheduleExplorer.ExploreAll(PerSenderFifoScenario, maxSchedules: 5_000, audit: true);

        Assert.Null(result.Failure);
        Assert.True(result.FullyExplored,
            $"expected full coverage, got: {result}");

        // Full coverage over a deterministic tree is an exact number — and a
        // PROOF: every schedule the simulator can produce for this scenario
        // ran, and the invariant held in all of them.
        Assert.Equal(PinnedExhaustiveCleanRunCount, result.SchedulesExplored);
        _output.WriteLine($"proof over the space: {result}");
    }
}
