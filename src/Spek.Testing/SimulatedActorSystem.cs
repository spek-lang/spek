using Spek.Runtime;

namespace Spek.Testing;

/// <summary>
/// Deterministic whole-system simulation: the actor system under a
/// simulated scheduler, clock, and RNG, so execution is a pure function of
/// (program, inputs, seed). A failing run reproduces exactly from its seed
/// on any machine — with logging added or a debugger attached.
///
/// <para>Mechanics: slots never self-schedule (dispatch is external); the
/// simulator repeatedly picks one mailbox among those ready — the pick is
/// the schedule policy's decision (default: the seeded RNG,
/// <see cref="RandomSchedulePolicy"/>; alternatives:
/// <see cref="PctSchedulePolicy"/>, <see cref="ScheduleExplorer"/>) — and
/// single-steps it through the SAME
/// dispatch pipeline production runs (policies, chaos, supervision), via
/// <c>ActorSlot.DispatchItemAsync</c>. The clock is a
/// <see cref="ManualTimeProvider"/>: no timer fires and no idle window
/// elapses until <see cref="Advance"/> moves time, so a five-minute
/// passivation window costs microseconds. Chaos faults
/// (<see cref="ChaosPlan"/>) become deterministic under the same seed,
/// because fault ordering follows arrival ordering.</para>
///
/// <para>The guarantee extends exactly as far as CE0119's: pure Spek code
/// is deterministic by construction; foreign libraries doing their own
/// threading or IO reintroduce nondeterminism the simulator cannot
/// control.</para>
///
/// <para>Audit mode (<c>audit: true</c>) turns the end of the test into an
/// accounting sweep: disposal first runs
/// <see cref="AssertQuiescentAndAccounted"/>, which verifies that every ask
/// completed exactly once, every mailbox drained, per-slot message
/// conservation holds, no handler is still in flight, and no one-shot timer
/// is left armed to fire into the disposed system. Every simulator test
/// becomes a wedge/stuck/dropped detector for free.</para>
/// </summary>
public sealed class SimulatedActorSystem : IDisposable
{
    private readonly ActorSystem _system;
    private readonly ISchedulePolicy _policy;
    private readonly Dictionary<ActorSlot, int> _actorIds = [];
    private readonly Dictionary<ActorSlot, Task> _inflight = [];
    private readonly bool _audit;

    /// <summary>Creates a simulated system. The seed IS the schedule:
    /// rerunning with the same seed replays the identical interleaving.
    /// With <paramref name="audit"/> set, disposal runs
    /// <see cref="AssertQuiescentAndAccounted"/> first — leaks the test's
    /// own assertions never looked at become failures.</summary>
    public SimulatedActorSystem(int seed, ChaosPlan? chaos = null, bool audit = false)
        : this(new ChoiceStream(seed), chaos, audit) { }

    /// <summary>
    /// Property-testing entry: scheduling decisions draw from
    /// <paramref name="choices"/> — the same stream the generators consumed
    /// — so one integrated shrinker minimizes data and schedule together.
    /// </summary>
    internal SimulatedActorSystem(ChoiceStream choices, ChaosPlan? chaos = null, bool audit = false)
        : this(new RandomSchedulePolicy(choices), choices.Seed, chaos, audit) { }

    /// <summary>
    /// Schedule-exploration entry: every mailbox pick is
    /// <paramref name="policy"/>'s decision — PCT sampling, exhaustive
    /// replay, or the default seeded stream, all through the same single
    /// point. <paramref name="seed"/> is only quoted in failure reports;
    /// pass the policy's own seed when it has one.
    /// </summary>
    internal SimulatedActorSystem(ISchedulePolicy policy, int seed, ChaosPlan? chaos = null, bool audit = false)
    {
        Seed = seed;
        _policy = policy;
        _audit = audit;
        Clock = new ManualTimeProvider();
        DeadLetters = new RecordingDeadLetterSink();
        _system = new ActorSystem($"sim-{Seed}",
            deadLetterSink: DeadLetters, timeProvider: Clock, chaos: chaos)
        {
            ExternalDispatch = true,
        };
    }

    /// <summary>The seed this run's schedule derives from — quote it in failure reports.</summary>
    public int Seed { get; }

    /// <summary>
    /// Total dispatch steps executed so far — every message the simulator has
    /// dequeued and pushed through the pipeline, accumulated across
    /// <see cref="Run"/> and <see cref="Advance"/> calls (the same counter the
    /// livelock guard bounds per drain). Exact under a fixed seed, so tests
    /// can assert precise work budgets — counting work where wall-clock
    /// benchmarks would flake on loaded machines.
    /// </summary>
    public long DispatchSteps { get; private set; }

    /// <summary>The simulated clock; moved only by <see cref="Advance"/>.</summary>
    public ManualTimeProvider Clock { get; }

    /// <summary>Every dead-letter of the run, for assertions.</summary>
    public RecordingDeadLetterSink DeadLetters { get; }

    /// <summary>The underlying system — for <c>Observe</c>, <c>SnapshotActors</c>, etc.</summary>
    public ActorSystem System => _system;

    /// <summary>Spawns the actor under simulation (same contract as <see cref="ActorSystem.Spawn{TActor}"/>).</summary>
    public ActorRef Spawn<TActor>(params object?[] args) where TActor : ActorBase
        => _system.Spawn<TActor>(args);

    /// <summary>Persistent spawn under simulation.</summary>
    public ActorRef SpawnPersistent<TActor>(string persistenceKey, params object?[] args)
        where TActor : ActorBase
        => _system.SpawnPersistent<TActor>(persistenceKey, args);

    /// <summary>Supervision restarts observed for <paramref name="actor"/>.</summary>
    public int RestartCountOf(ActorRef actor) => actor.Slot?.RestartCount ?? 0;

    /// <summary>Messages that fully entered dispatch on <paramref name="actor"/> —
    /// its share of <see cref="DispatchSteps"/> (mirrors
    /// <see cref="TestActorSystem.DispatchCountOf"/>). Pairs with
    /// <see cref="DeadLetters"/> for "every message reached a terminal state"
    /// assertions.</summary>
    public long DispatchCountOf(ActorRef actor) => actor.Slot?.DispatchedCount ?? 0;

    /// <summary>
    /// Drains every mailbox in a seed-determined order until the system is
    /// quiescent — nothing left to dispatch and no handler still running.
    /// Work that is waiting on time (timers, passivation windows) stays
    /// parked; move it with <see cref="Advance"/>.
    /// </summary>
    public void Run(int maxSteps = 100_000)
    {
        var steps = 0;
        while (true)
        {
            // Reap finished in-flight dispatches; surface unexpected faults
            // (handler failures route to supervision, never here).
            foreach (var done in _inflight.Where(kv => kv.Value.IsCompleted).ToArray())
            {
                done.Value.GetAwaiter().GetResult();
                _inflight.Remove(done.Key);
            }

            var ready = _system.SlotsSnapshot()
                .Where(s => s.Self is not null && s.HasMail && !_inflight.ContainsKey(s))
                .ToArray();

            if (ready.Length == 0)
            {
                if (_inflight.Count == 0) return;      // quiescent
                if (!SettleInflight()) return;         // parked on time/external only
                continue;
            }

            if (++steps > maxSteps)
                throw new InvalidOperationException(
                    $"simulation exceeded {maxSteps} dispatch steps (seed {Seed}) — livelock?");
            DispatchSteps++;

            // The scheduling decision: which mailbox runs next. The candidate
            // list is in spawn order and actor ids are stable, so the pick is
            // reproducible; the default policy draws it from the seeded
            // choice stream, byte-for-byte as it always has.
            var ids = new int[ready.Length];
            for (var i = 0; i < ready.Length; i++)
            {
                if (!_actorIds.TryGetValue(ready[i], out var id))
                    _actorIds[ready[i]] = id = _actorIds.Count;
                ids[i] = id;
            }
            var pick = _policy.PickNext(ids);
            if ((uint)pick >= (uint)ready.Length)
                throw new InvalidOperationException(
                    $"schedule policy picked index {pick} with {ready.Length} actor(s) ready (seed {Seed})");
            var slot = ready[pick];
            var task = slot.SimDispatchOneAsync();
            if (task.IsCompleted)
                task.GetAwaiter().GetResult();
            else
                _inflight[slot] = task;                // handler awaiting (e.g. an ask)
        }
    }

    /// <summary>
    /// Moves simulated time forward — due timers (passivation checks, defer
    /// re-admissions, chaos delays) fire synchronously, in due order — then
    /// drains the resulting work via <see cref="Run"/>.
    /// </summary>
    public void Advance(TimeSpan by)
    {
        Clock.Advance(by);
        Run();
    }

    /// <summary>
    /// Request-reply from the host: sends the ask, drains the system, and
    /// returns the reply. Throws (quoting the seed) if the ask cannot
    /// complete without more input or time.
    /// </summary>
    public TReply Ask<TReply>(ActorRef target, object message)
    {
        var pending = target.AskAsync<TReply>(message);
        // The reply completes on an untracked reply slot moments after the
        // target's dispatch — settle briefly between drains rather than
        // sampling once. Happy path: the first iteration.
        for (var quiet = 0; quiet < 1000; quiet++)
        {
            Run();
            if (pending.IsCompleted) return pending.GetAwaiter().GetResult();
            Thread.Sleep(1);
        }
        throw new InvalidOperationException(
            $"ask did not complete under simulation (seed {Seed}) — " +
            "the target never replied; does the handler return a message?");
    }

    /// <summary>
    /// Ingress replay (D2): re-feeds a recorded trace's external inputs, in
    /// recorded arrival order, into this simulation — production incident,
    /// local repro. The host must first re-create the recorded topology
    /// (same spawn order gives the same display identities). Each event
    /// drains fully before the next feeds, honoring the recorded order;
    /// scheduling within a drain is this run's seed. Ingress replay
    /// survives code change — replaying an incident's inputs against a
    /// candidate fix is how the fix is validated — so the build fingerprint
    /// mismatch is a warning surface, not a refusal: pass
    /// <paramref name="allowFingerprintMismatch"/> deliberately.
    /// </summary>
    public void ReplayIngress(SpekTrace trace, bool allowFingerprintMismatch = false)
    {
        var current = FlightRecorder.BuildFingerprint();
        if (trace.Fingerprint != current && !allowFingerprintMismatch)
            throw new InvalidOperationException(
                $"trace was recorded on '{trace.Fingerprint}' but this build is " +
                $"'{current}'. Replaying inputs across builds is legitimate when " +
                "validating a fix — opt in with allowFingerprintMismatch: true.");

        var slots = _system.SlotsSnapshot()
            .Where(s => s.Self is not null)
            .ToDictionary(s => s.DisplayName);

        foreach (var e in trace.Events)
        {
            if (!slots.TryGetValue(e.Target, out var slot))
                throw new InvalidOperationException(
                    $"trace event #{e.Seq} targets '{e.Target}', which this " +
                    "simulation has not spawned — re-create the recorded " +
                    "topology (same spawn order) before replaying.");
            slot.Self!.Tell(trace.Rehydrate(e));
            Run();   // recorded arrival order is a causal boundary
        }
    }

    // A pending handler (parked on an ask or an await) completes shortly
    // after the dispatch it waits for; its continuations are pure compute
    // under the simulator's contract (no real IO in Spek code — CE0119).
    // "Stable" therefore means: a full quiet window with no in-flight
    // completion and no new mail. The window is ~3 orders of magnitude
    // above continuation cost, so classification is deterministic in
    // practice; genuinely-parked work (waiting on the clock) stays parked.
    private bool SettleInflight()
    {
        for (var quiet = 0; quiet < 25; quiet++)
        {
            if (_inflight.Any(kv => kv.Value.IsCompleted)) return true;
            if (_system.SlotsSnapshot().Any(s => s.HasMail && !_inflight.ContainsKey(s)))
                return true;
            Thread.Sleep(1);
        }
        return false;
    }

    /// <summary>
    /// The audit-mode sweep: verifies the run left nothing wedged, stuck, or
    /// silently dropped, and throws one aggregate
    /// <see cref="SimulationAuditException"/> naming every violation (and the
    /// seed) otherwise. Callable explicitly from any test; with
    /// <c>audit: true</c>, <see cref="Dispose"/> calls it automatically.
    ///
    /// <para>The invariants, exactly:</para>
    /// <list type="number">
    ///   <item>No dispatch still in flight — a pending handler at end of test
    ///   is parked on work that will never arrive (a wedged await).</item>
    ///   <item>No mailbox holds mail — everything sent was dispatched or
    ///   dead-lettered, never silently parked. Time-parked messages (chaos
    ///   delays, defer re-admissions, debounce quiet-windows) live on the
    ///   clock, not in mailboxes, and are accounted by the timer check.</item>
    ///   <item>Per-slot conservation: enqueued == dispatched +
    ///   dead-lettered-on-dequeue + still-parked.</item>
    ///   <item>Every ask completed exactly once: issued == delivered + failed
    ///   and duplicate-drops == 0, on THIS system's own counters — per-system
    ///   scoping keeps the check sound while other test collections run
    ///   asks in parallel (the process-global counters see everyone's).</item>
    ///   <item>No one-shot timer left armed on the simulated clock — it
    ///   would fire into a disposed system, losing the message it parks.
    ///   Periodic timers (passivation checks) are slot infrastructure and
    ///   legitimately armed until slot disposal; timer callbacks are opaque,
    ///   so entries are counted and timestamped, not attributed.</item>
    /// </list>
    /// </summary>
    /// <exception cref="SimulationAuditException">At least one invariant is violated.</exception>
    public void AssertQuiescentAndAccounted()
    {
        var violations = new List<string>();

        // 1. Wedged handlers: a dispatch Run() could never reap. A pending
        // task gets a bounded settle window first — NOT leniency, the
        // simulator's documented soft edge: in-handler ask continuations
        // resume on the thread pool (see SettleInflight above), so Ask() can
        // return the moment the reply cell completes while the asking
        // handler's own dispatch task is still mid-epilogue (state-machine
        // completion + ExitWriter, each a pool hop) — microseconds unloaded,
        // tens of milliseconds under full-suite CPU load. Wedged means
        // NEVER-completing, so the window changes only how fast a genuine
        // wedge reports, never whether.
        var settleDeadline = Environment.TickCount64 + 1000;
        foreach (var (slot, task) in _inflight)
        {
            while (!task.IsCompleted && Environment.TickCount64 < settleDeadline)
                Thread.Sleep(1);
            if (!task.IsCompleted)
                violations.Add(
                    $"dispatch still in flight on '{slot.DisplayName}' — the handler is " +
                    "parked on an await that never completed (an ask with no reply?)");
        }

        foreach (var slot in _system.SlotsSnapshot())
        {
            if (slot.Self is null) continue;

            // 2. Parked mail: enqueued but neither dispatched nor dead-lettered.
            var parked = slot.MailboxDepth;
            if (parked > 0)
                violations.Add(
                    $"parked mail on '{slot.DisplayName}': {parked} message(s) " +
                    $"[{string.Join(", ", slot.Snapshot().MailboxHead)}] never dispatched " +
                    "— missing Run()/Advance(), or the system was abandoned mid-flood");

            // 3. Conservation: every admitted message reached exactly one exit.
            var enqueued = slot.EnqueuedCount;
            var dispatched = slot.DispatchedCount;
            var stoppedDrops = slot.StoppedDropCount;
            if (enqueued != dispatched + stoppedDrops + parked)
                violations.Add(
                    $"message conservation broken on '{slot.DisplayName}': " +
                    $"enqueued {enqueued} != dispatched {dispatched} + " +
                    $"dead-lettered-on-dequeue {stoppedDrops} + parked {parked}");
        }

        // 4. Ask accounting, on this system's own scope.
        var asks = _system.ReplyScope;
        var completed = asks.Delivered + asks.Failed;
        if (asks.Issued != completed)
            violations.Add(
                $"incomplete asks: {asks.Issued} issued but {completed} completed " +
                $"({asks.Delivered} delivered + {asks.Failed} failed) — " +
                $"{asks.Issued - completed} ask(s) will never resolve (no reply, no timeout)");
        if (asks.DupDropped != 0)
            violations.Add(
                $"duplicate ask completions: {asks.DupDropped} repl(ies) raced an " +
                "already-completed ask — a reply-routing bug (two replies to one ask, " +
                "or a reply crossed to the wrong asker)");

        // 5. Dangling one-shot timers on the simulated clock.
        foreach (var (due, periodic) in Clock.ArmedTimersSnapshot())
            if (!periodic)
                violations.Add(
                    $"armed one-shot timer due {due:O} — a chaos delay, defer " +
                    "re-admission, or debounce quiet-window is still parked on the " +
                    "clock and would fire into a disposed system; Advance() past it " +
                    "or assert on its effect before disposing");

        if (violations.Count > 0)
            throw new SimulationAuditException(Seed, violations);
    }

    /// <summary>
    /// Tears the simulation down. In audit mode the accounting sweep runs
    /// FIRST and throws before the underlying system is disposed — teardown
    /// consumes the evidence (deadline asks get failed, slots stop), so the
    /// audit must read the world the test actually left behind. The
    /// underlying dispose still runs (finally), and the one aggregate
    /// exception surfaces through the test's <c>using</c> scope as an
    /// ordinary assertion failure. Accepted caveat of that choice: when the
    /// test body itself threw, <c>using</c> semantics let a Dispose-time
    /// audit failure replace the body's exception — when diagnosing an
    /// already-failing test, call <see cref="AssertQuiescentAndAccounted"/>
    /// explicitly at the end of the body instead.
    /// </summary>
    public void Dispose()
    {
        try
        {
            if (_audit) AssertQuiescentAndAccounted();
        }
        finally
        {
            _system.Dispose();
        }
    }
}

/// <summary>
/// Thrown by <see cref="SimulatedActorSystem.AssertQuiescentAndAccounted"/>
/// when the end-of-run sweep finds work that never reached a terminal state —
/// parked mail, a broken conservation count, an ask that never completed, a
/// duplicated reply, or a one-shot timer still armed. One exception carries
/// every violation (and the seed), so a single failure report shows the whole
/// leak surface of the run.
/// </summary>
public sealed class SimulationAuditException : Exception
{
    internal SimulationAuditException(int seed, IReadOnlyList<string> violations)
        : base($"simulation audit failed (seed {seed}) — {violations.Count} violation(s):\n"
               + string.Join("\n", violations.Select((v, i) => $"  {i + 1}. {v}")))
        => Violations = violations;

    /// <summary>The individual violations, one message each.</summary>
    public IReadOnlyList<string> Violations { get; }
}
