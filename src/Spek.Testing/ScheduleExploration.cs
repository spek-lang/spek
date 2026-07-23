namespace Spek.Testing;

/// <summary>
/// The simulator's one scheduling decision, made pluggable: at every dispatch
/// step <see cref="SimulatedActorSystem.Run"/> asks the policy which of the
/// ready actors runs next. Everything else — virtual time, chaos, dispatch —
/// is unchanged, so a policy swaps HOW the schedule space is sampled without
/// touching what a schedule means.
///
/// <para>Contract: <c>readyActorIds</c> lists the actors that
/// have mail and no dispatch in flight, in deterministic spawn order; ids are
/// stable for the simulation's lifetime (assigned on first readiness, in
/// encounter order — spawn order, in practice). The policy returns an index
/// INTO THE LIST. It is called once per dispatch step, including forced steps
/// where only one actor is ready — step counting stays uniform across
/// policies. Policies are stateful and single-use: one instance per
/// <see cref="SimulatedActorSystem"/>.</para>
/// </summary>
internal interface ISchedulePolicy
{
    /// <summary>Picks which ready actor dispatches next; returns an index
    /// into <paramref name="readyActorIds"/>.</summary>
    int PickNext(IReadOnlyList<int> readyActorIds);
}

/// <summary>
/// The default policy: uniform seeded sampling, byte-identical to the
/// pre-policy simulator. Each pick is exactly one
/// <see cref="ChoiceStream.Next"/> draw over the ready count — and
/// <c>Next(1)</c> consumes no entropy, matching the old
/// <c>ready.Length == 1 ? 0 : …</c> bypass — so every seed replays the same
/// interleaving it always has, and property-test shrinking still sees one
/// choice per real decision.
/// </summary>
internal sealed class RandomSchedulePolicy(ChoiceStream choices) : ISchedulePolicy
{
    public int PickNext(IReadOnlyList<int> readyActorIds)
        => choices.Next(readyActorIds.Count);
}

/// <summary>
/// Probabilistic concurrency testing (Burckhardt, Kothari, Musuvathi,
/// Nagarakatte — "A Randomized Scheduler with Probabilistic Guarantees of
/// Finding Bugs", ASPLOS 2010), adapted from threads to actors: each actor
/// gets a random distinct priority on first readiness (all at least
/// <see cref="Depth"/>), and every step runs the highest-priority ready
/// actor. The seed also draws <c>depth − 1</c> priority-change points over
/// <c>[1, maxSteps]</c>; when the step counter hits the i-th, the actor about
/// to run has its priority dropped to <c>i</c> — below every initial
/// priority — forcing the schedule to switch away at exactly that point.
///
/// <para>The informal guarantee: for a scenario with <c>n</c> actors and at
/// most <c>k</c> steps, a single run finds any bug of depth <c>d</c> (one
/// needing <c>d</c> ordering constraints) with probability at least
/// <c>1/(n·k^(d−1))</c>. Uniform random sampling has no such bound — a bug
/// hidden behind one long priority inversion (lose <c>m</c> coin flips in a
/// row) costs random <c>2^m</c> runs but PCT only <c>1/n</c>-ish, because a
/// single priority draw decides the whole inversion.</para>
///
/// <para>Deterministic per (seed, depth, maxSteps): rerunning the same
/// parameters replays the same schedule. Quote all three in failure
/// reports.</para>
/// </summary>
internal sealed class PctSchedulePolicy : ISchedulePolicy
{
    private readonly Random _rng;
    private readonly Dictionary<int, long> _priorities = [];
    private readonly Dictionary<long, long> _changePoints = [];
    private long _step;
    private int _seen;

    /// <summary>Creates a PCT schedule. <paramref name="maxSteps"/> is the
    /// step-count estimate <c>k</c> the change points are drawn over — an
    /// overestimate dilutes (weakens the bound), an underestimate leaves late
    /// steps free of change points; neither breaks determinism.</summary>
    public PctSchedulePolicy(int seed, int depth, int maxSteps = 1_000)
    {
        if (depth < 1)
            throw new ArgumentOutOfRangeException(nameof(depth), depth, "PCT depth is at least 1");
        if (maxSteps < depth)
            throw new ArgumentOutOfRangeException(nameof(maxSteps), maxSteps,
                $"maxSteps must admit depth − 1 = {depth - 1} distinct change points");
        Seed = seed;
        Depth = depth;
        _rng = new Random(seed);
        for (long i = 1; i < depth; i++)
        {
            long step;
            do { step = _rng.Next(1, maxSteps + 1); } while (_changePoints.ContainsKey(step));
            _changePoints[step] = i;
        }
    }

    /// <summary>The seed this schedule derives from — quote it in failure reports.</summary>
    public int Seed { get; }

    /// <summary>The bug depth this run is tuned for (d ordering constraints ⇒ d − 1 change points).</summary>
    public int Depth { get; }

    public int PickNext(IReadOnlyList<int> readyActorIds)
    {
        _step++;

        // First sight of an actor: a random priority, distinct by
        // construction (unique low bits) and at least Depth — so every
        // change-point priority (1 … d−1) sits below every initial one.
        foreach (var id in readyActorIds)
            if (!_priorities.ContainsKey(id))
                _priorities[id] = (((long)_rng.Next() << 20) | (uint)(_seen++ & 0xFFFFF)) + Depth;

        var best = HighestPriorityIndex(readyActorIds);
        if (_changePoints.TryGetValue(_step, out var demoted))
        {
            // The change point: the actor about to run is demoted to the
            // point's priority, and the pick re-resolves under the new order.
            _priorities[readyActorIds[best]] = demoted;
            best = HighestPriorityIndex(readyActorIds);
        }
        return best;
    }

    private int HighestPriorityIndex(IReadOnlyList<int> readyActorIds)
    {
        var best = 0;
        for (var i = 1; i < readyActorIds.Count; i++)
            if (_priorities[readyActorIds[i]] > _priorities[readyActorIds[best]])
                best = i;
        return best;
    }
}

/// <summary>
/// Deterministic replay-and-diverge: replays a forced prefix of picks, then
/// follows the lowest-index branch, recording the pick and the branching
/// factor at every step. One instance drives one schedule of an exhaustive
/// enumeration (<see cref="ScheduleExplorer.ExploreAll"/>); the recorded
/// (<see cref="Chosen"/>, <see cref="Bounds"/>) pair is both the schedule's
/// exact identity and the frontier the next schedule diverges from. Replay
/// validates the branching factor against the run that produced the prefix,
/// so hidden nondeterminism (foreign threading, real IO) fails loudly instead
/// of silently corrupting the enumeration.
/// </summary>
internal sealed class ReplaySchedulePolicy(
    IReadOnlyList<int>? prefix = null,
    IReadOnlyList<int>? expectedBounds = null) : ISchedulePolicy
{
    private readonly IReadOnlyList<int> _prefix = prefix ?? [];

    /// <summary>The pick taken at each step — the schedule, exactly.</summary>
    public List<int> Chosen { get; } = [];

    /// <summary>The number of ready actors at each step — the branching factor.</summary>
    public List<int> Bounds { get; } = [];

    public int PickNext(IReadOnlyList<int> readyActorIds)
    {
        var bound = readyActorIds.Count;
        var at = Chosen.Count;
        var pick = 0;
        if (at < _prefix.Count)
        {
            if (expectedBounds is not null && expectedBounds[at] != bound)
                throw new InvalidOperationException(
                    $"schedule replay diverged at step {at}: {bound} actor(s) ready, " +
                    $"expected {expectedBounds[at]} — the scenario is not schedule-" +
                    "deterministic (foreign threading or IO outside the simulator's control?)");
            pick = _prefix[at];
            if (pick >= bound)
                throw new InvalidOperationException(
                    $"schedule replay diverged at step {at}: pick {pick} of {bound} ready actor(s)");
        }
        Bounds.Add(bound);
        Chosen.Add(pick);
        return pick;
    }
}

/// <summary>The outcome of <see cref="ScheduleExplorer.ExploreAll"/>.</summary>
/// <param name="SchedulesExplored">How many complete schedules ran.</param>
/// <param name="FullyExplored">True when the enumeration exhausted the whole
/// schedule tree within budget. A fully-explored clean run is a PROOF over
/// the bounded space: every schedule the simulator can produce for this
/// scenario — every interleaving at mailbox-pick granularity — executed, and
/// none violated the scenario's assertions. (A budget-capped run is only a
/// sample; treat it like fuzzing.)</param>
/// <param name="FailingSchedule">The exact schedule (pick per step) of the
/// first violating run, replayable via <see cref="ReplaySchedulePolicy"/>;
/// null when none failed.</param>
/// <param name="Failure">What the violating run threw; null when none failed.</param>
internal sealed record ScheduleExplorationResult(
    int SchedulesExplored,
    bool FullyExplored,
    IReadOnlyList<int>? FailingSchedule,
    Exception? Failure)
{
    /// <summary>One-line summary for logs and failure messages.</summary>
    public override string ToString()
        => Failure is null
            ? $"{SchedulesExplored} schedule(s) explored, " +
              (FullyExplored ? "space fully covered, no violation — proof over the space"
                             : "budget exhausted before full coverage, no violation")
            : $"violation on schedule [{string.Join(",", FailingSchedule!)}] " +
              $"after {SchedulesExplored} schedule(s): {Failure.Message}";
}

/// <summary>
/// Bounded exhaustive schedule exploration: enumerates EVERY schedule of a
/// small scenario by depth-first search over the schedule tree — each run is
/// a fresh <see cref="SimulatedActorSystem"/> driven by a
/// <see cref="ReplaySchedulePolicy"/> that replays a prefix and diverges at
/// the deepest step with an untaken branch. Exploration stops at the first
/// violating schedule (reported exactly) or when the tree is exhausted.
///
/// <para>Scope: schedule count is factorial in scenario size, so this is for
/// SMALL scenarios — a handful of actors, tens of steps. The known upgrades,
/// deliberately not in this round: state hashing to merge converged prefixes,
/// and dynamic partial-order reduction (DPOR) to skip interleavings that only
/// permute commuting dispatches. Both shrink the tree without shrinking the
/// proof.</para>
/// </summary>
internal static class ScheduleExplorer
{
    /// <summary>
    /// Runs <paramref name="scenario"/> under every schedule (up to
    /// <paramref name="maxSchedules"/>). The scenario receives a fresh
    /// simulated system per schedule; it spawns, sends, drains, and THROWS to
    /// signal a violation (assertion libraries welcome). With
    /// <paramref name="audit"/> set, every schedule also runs the
    /// end-of-run accounting sweep
    /// (<see cref="SimulatedActorSystem.AssertQuiescentAndAccounted"/>).
    /// </summary>
    public static ScheduleExplorationResult ExploreAll(
        Action<SimulatedActorSystem> scenario,
        int maxSchedules = 10_000,
        bool audit = false)
    {
        var prefix = new List<int>();
        var prefixBounds = new List<int>();
        var explored = 0;

        while (explored < maxSchedules)
        {
            var policy = new ReplaySchedulePolicy(prefix, prefixBounds);
            var sim = new SimulatedActorSystem(policy, seed: 0, audit: audit);
            explored++;
            Exception? failure = null;
            try
            {
                scenario(sim);
                sim.Dispose();   // the audit sweep (when enabled) throws here
            }
            catch (Exception ex)
            {
                failure = ex;
                try { sim.System.Dispose(); } catch { /* best-effort teardown */ }
            }

            if (failure is not null)
                return new ScheduleExplorationResult(
                    explored, FullyExplored: false, policy.Chosen.ToArray(), failure);

            // Backtrack to the deepest step with an unexplored sibling: the
            // next schedule replays up to it and takes the next branch.
            var at = -1;
            for (var i = policy.Chosen.Count - 1; i >= 0; i--)
                if (policy.Chosen[i] + 1 < policy.Bounds[i]) { at = i; break; }
            if (at < 0)
                return new ScheduleExplorationResult(
                    explored, FullyExplored: true, null, null);

            prefix = [.. policy.Chosen.Take(at), policy.Chosen[at] + 1];
            prefixBounds = [.. policy.Bounds.Take(at + 1)];
        }

        return new ScheduleExplorationResult(
            explored, FullyExplored: false, null, null);
    }
}
