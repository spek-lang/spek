using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Spek.Resilience;
using Spek.Runtime;
using Spek.Testing;
using Spek.Tests.Emit;
using Xunit;

namespace Spek.Tests.Runtime;

/// <summary>
/// Work-budget tests: deterministic performance regression guards. Under
/// the simulator, execution is a pure function of (program, inputs, seed) —
/// so instead of timing anything, these tests COUNT the work each core
/// construct costs and assert the exact number. A regression that adds a
/// hop, a re-dispatch, or a hidden retry changes the count and fails
/// exactly, on any machine, at any CI load.
///
/// Every expected number is derived from first principles in a comment
/// beside its assertion — these are contracts, not snapshots. A future
/// change that legitimately alters a budget must consciously update the
/// arithmetic here.
/// </summary>
public sealed class WorkBudgetTests
{
    private sealed record Ping();

    /// <summary>Counts handled messages. The simulator dispatches on the
    /// calling thread (writer arm, inline readers), so a plain increment is
    /// race-free; the static resets per test and the class is private to
    /// this test class, whose tests xUnit runs sequentially.</summary>
    private sealed class TallyActor : ActorBase
    {
        public static int Handled;
        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            Handled++;
            return Task.CompletedTask;
        }
    }

    // ─── Budget 1: tells ─────────────────────────────────────────────────

    [Fact]
    public void Tells_CostExactlyOneDispatchStepEach()
    {
        const int N = 100;
        TallyActor.Handled = 0;
        using var sim = new SimulatedActorSystem(seed: 4242, audit: true);
        var counter = sim.Spawn<TallyActor>();

        for (int i = 0; i < N; i++) counter.Tell(new Ping());
        sim.Run();

        // Budget: one Tell = one mailbox entry = one seeded pick = one trip
        // through the dispatch pipeline. Nothing else runs, so the whole
        // simulation costs exactly N steps, all of them on the counter, and
        // every message reaches its handler (none dead-letter).
        Assert.Equal(N, sim.DispatchSteps);
        Assert.Equal(N, sim.DispatchCountOf(counter));
        Assert.Equal(N, TallyActor.Handled);
        Assert.Empty(sim.DeadLetters.Records);
    }

    // ─── Budget 2: asks ──────────────────────────────────────────────────

    private sealed record Double(int N);
    private sealed record Doubled(int N);

    private sealed class Doubler : ActorBase
    {
        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            if (message is Double d) sender.Tell(new Doubled(d.N * 2), _selfRef!);
            return Task.CompletedTask;
        }
    }

    [Fact]
    public async Task Asks_CostExactlyOneDispatchStepEach_RepliesAreFreeAsync()
    {
        const int N = 50;
        using var sim = new SimulatedActorSystem(seed: 777, audit: true);
        var doubler = sim.Spawn<Doubler>();

        var pending = new ValueTask<Doubled>[N];
        for (int i = 0; i < N; i++)
            pending[i] = doubler.AskAsync<Doubled>(new Double(i));
        sim.Run();

        // Budget: N asks = exactly N dispatches, all on the TARGET. The
        // reply never touches a mailbox: an ask's sender ref wraps a reply
        // cell (a slotless IValueTaskSource), and the handler's sender.Tell
        // completes the asker's ValueTask directly inside the target's own
        // dispatch — so replies cost zero additional steps.
        Assert.Equal(N, sim.DispatchSteps);
        Assert.Equal(N, sim.DispatchCountOf(doubler));
        Assert.Empty(sim.DeadLetters.Records);

        for (int i = 0; i < N; i++)
        {
            Assert.True(pending[i].IsCompleted,
                $"ask {i} did not complete during Run() (seed {sim.Seed})");
            Assert.Equal(i * 2, (await pending[i]).N);
        }
    }

    // ─── Budget 3: stream-shaped handler (the two-hop model) ─────────────

    // `distinct` is the timer-free operator — the two-hop arithmetic here
    // has no time axis at all. The timer-based operators are budgeted
    // separately: debounce in budget 6 (its quiet-window timer rides the
    // simulated clock) and throttle in budget 7 (its window math reads the
    // simulated clock's timestamps). The docs' qualified form is used
    // because bare `distinct(by:)` needs two type arguments (streams.md).
    private const string DedupeSource = """
        using Spek.Streams;

        message Reading(int id);
        message GetSeen();
        message Seen(int count, int lastId);

        actor Dedupe
        {
            int count = 0;
            int lastId = 0;

            on Reading r
                => StreamOperators.distinct<Reading, int>(by: x => x.id)
                => {
                    count = count + 1;
                    lastId = r.id;
                }

            on GetSeen => return new Seen(count, lastId);
        }
        """;

    [Fact]
    public void StreamShapedHandler_CostsTwoStepsPerChainPassingMessage()
    {
        var parse = SpekCompiler.Parse(DedupeSource);
        Assert.True(parse.Success, string.Join("\n",
            parse.Diagnostics.Select(d => $"  {d.Code} {d.Line}:{d.Column} {d.Message}")));
        var assembly = RoslynCompileHelper.CompileAndLoad(
            new FileEmitter().Emit(parse.Tree!), "WorkBudgetDedupe");
        var actorType   = assembly.GetTypes().Single(t => t.Name == "Dedupe");
        var readingType = assembly.GetTypes().Single(t => t.Name == "Reading");
        var getType     = assembly.GetTypes().Single(t => t.Name == "GetSeen");

        using var sim = new SimulatedActorSystem(seed: 11, audit: true);
        var actor = sim.System.Spawn(actorType);

        // ids 1,1,2,2,3 — distinct-until-changed passes 1, 2, 3 (P = 3).
        int[] ids = [1, 1, 2, 2, 3];
        foreach (var id in ids)
            actor.Tell(Activator.CreateInstance(readingType, id)!);
        sim.Run();

        // Budget: EVERY message costs one step for its first hop (the
        // dispatch arm feeds the operator chain via OfferAsync). A
        // chain-PASSING message costs one more: the chain's emit callback
        // self-Tells a synthetic __FireBody_* trigger, and the user's body
        // runs in that second dispatch. 5 first hops + 3 FireBody hops = 8.
        // This pins the two-hop model: a future change that collapses the
        // second hop must consciously rewrite this arithmetic to N + 0.
        Assert.Equal(ids.Length + 3, sim.DispatchSteps);
        Assert.Equal(ids.Length + 3, sim.DispatchCountOf(actor));
        Assert.Empty(sim.DeadLetters.Records);

        // The probe ask is one more step on the actor; its reply is free
        // (budget 2). Body ran exactly 3 times, last with the final id.
        var seen = sim.Ask<object>(actor, Activator.CreateInstance(getType)!);
        Assert.Equal(ids.Length + 3 + 1, sim.DispatchSteps);
        var t = seen.GetType();
        Assert.Equal(3, (int)t.GetProperty("count")!.GetValue(seen)!);
        Assert.Equal(3, (int)t.GetProperty("lastId")!.GetValue(seen)!);
    }

    // ─── Budget 4: defer-once ingress flood ──────────────────────────────

    /// <summary>Mutable-flag message: the defer budget is tracked per message
    /// INSTANCE, so a class (not a record) lets the policy tell a first
    /// admission from a re-admission without out-of-band state.</summary>
    private sealed class DeferrableJob
    {
        public bool Readmitted;
    }

    /// <summary>Defers each message exactly once (first admission parks it;
    /// the re-admitted instance passes) and counts every evaluation. The
    /// counter is a plain increment: policies run inside the simulator's
    /// single-stepped dispatch.</summary>
    private sealed class DeferOncePolicy : IngressPolicy
    {
        public int Evaluations;

        public override ValueTask<PolicyDecision> EvaluateAsync(
            ResilienceContext context, CancellationToken cancellationToken = default)
        {
            Evaluations++;
            if (context.Message is DeferrableJob { Readmitted: false } job)
            {
                job.Readmitted = true;
                return new(PolicyDecision.Defer(
                    TimeSpan.FromMilliseconds(5), "work budget: defer once"));
            }
            return new(PolicyDecision.Allow());
        }
    }

    [Fact]
    public void DeferOnceFlood_EachMessageIsReadmittedExactlyOnce()
    {
        const int N = 40;
        TallyActor.Handled = 0;
        using var sim = new SimulatedActorSystem(seed: 99, audit: true);
        var actor = sim.Spawn<TallyActor>();
        var policy = new DeferOncePolicy();
        actor.AttachIngressPolicy(policy);

        for (int i = 0; i < N; i++) actor.Tell(new DeferrableJob());
        sim.Run();

        // First admission: each message costs one dispatch step (the
        // dispatched-count increments before the policy chain runs), one
        // policy evaluation ends in Defer, and a one-shot re-admission timer
        // parks the message on the virtual clock. Nothing reaches the
        // handler and nothing dead-letters — deferral is not failure.
        Assert.Equal(N, sim.DispatchSteps);
        Assert.Equal(N, policy.Evaluations);
        Assert.Equal(0, TallyActor.Handled);
        Assert.Empty(sim.DeadLetters.Records);

        // Advance past the 5ms RetryAfter: every parked timer fires
        // (synchronously, in due order), re-enqueues its message, and the
        // Advance-internal Run drains — one more step and one more (now
        // allowing) evaluation per message. Totals: dispatches = 2N, policy
        // evaluations = 2N, handled = N, dead letters = 0.
        sim.Advance(TimeSpan.FromMilliseconds(10));

        Assert.Equal(2L * N, sim.DispatchSteps);
        Assert.Equal(2 * N, policy.Evaluations);
        Assert.Equal(2L * N, sim.DispatchCountOf(actor));
        Assert.Equal(N, TallyActor.Handled);
        Assert.Empty(sim.DeadLetters.Records);
    }

    // ─── Budget 5: crash-every-Nth under Restart ─────────────────────────

    private sealed record Job(int Id, bool Poison);

    private sealed class RestartingWorker : ActorBase
    {
        public static int Processed;

        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            if (message is Job { Poison: true } j)
                throw new InvalidOperationException($"poison job {j.Id}");
            Processed++;
            return Task.CompletedTask;
        }

        protected override FailureDirective OnFailure(Exception exception, object message)
            => FailureDirective.Restart;
    }

    [Fact]
    public void CrashEveryThird_UnderRestart_ExactDispatchAndDeadLetterArithmetic()
    {
        const int N = 12;   // poison at 3, 6, 9, 12 → 4 crashes
        RestartingWorker.Processed = 0;
        using var sim = new SimulatedActorSystem(seed: 5, audit: true);
        var worker = sim.Spawn<RestartingWorker>();

        for (int i = 1; i <= N; i++) worker.Tell(new Job(i, Poison: i % 3 == 0));
        sim.Run();

        // Budget: a crash consumes its message, so 12 sends are still
        // exactly 12 dispatch steps. Each of the 4 poison jobs dead-letters
        // once with the Restart directive's reason and costs one observed
        // restart; the instance is dropped and the NEXT message
        // re-materializes it inside that message's own step — restarts add
        // zero steps. The 8 non-poison jobs each process exactly once.
        Assert.Equal(N, sim.DispatchSteps);
        Assert.Equal(N, sim.DispatchCountOf(worker));
        Assert.Equal(N - 4, RestartingWorker.Processed);
        Assert.Equal(4, sim.RestartCountOf(worker));
        Assert.Equal(4, sim.DeadLetters.Records.Count);
        Assert.All(sim.DeadLetters.Records,
            r => Assert.Equal("handler threw; restarting", r.Reason));
        Assert.False(worker.IsStopped);
    }

    // ─── Budget 6: debounce chain (timer rides the simulated clock) ───────

    private const string DebounceSource = """
        using Spek.Streams;

        message Nudge(int id);
        message GetFired();
        message Fired(int count, int lastId);

        actor Debouncer
        {
            int fired = 0;
            int lastId = 0;

            on Nudge n
                => debounce(500)
                => {
                    fired = fired + 1;
                    lastId = n.id;
                }

            on GetFired => return new Fired(fired, lastId);
        }
        """;

    [Fact]
    public void DebounceChain_BurstCostsOneFireBodyStep_ExactlyAtTheWindow()
    {
        var parse = SpekCompiler.Parse(DebounceSource);
        Assert.True(parse.Success, string.Join("\n",
            parse.Diagnostics.Select(d => $"  {d.Code} {d.Line}:{d.Column} {d.Message}")));
        var assembly = RoslynCompileHelper.CompileAndLoad(
            new FileEmitter().Emit(parse.Tree!), "WorkBudgetDebounce");
        var actorType = assembly.GetTypes().Single(t => t.Name == "Debouncer");
        var nudgeType = assembly.GetTypes().Single(t => t.Name == "Nudge");
        var getType   = assembly.GetTypes().Single(t => t.Name == "GetFired");

        using var sim = new SimulatedActorSystem(seed: 23, audit: true);
        var actor = sim.System.Spawn(actorType);

        // A 5-message burst at one virtual instant. Each message costs one
        // first-hop step (dispatch arm → OfferAsync); the offer records the
        // latest and re-arms the quiet-window timer on the SIMULATED clock —
        // time is frozen, so nothing else runs.
        for (int i = 1; i <= 5; i++)
            actor.Tell(Activator.CreateInstance(nudgeType, i)!);
        sim.Run();
        Assert.Equal(5, sim.DispatchSteps);

        // One tick short of the window: the timer is not due — zero steps.
        sim.Advance(TimeSpan.FromMilliseconds(499));
        Assert.Equal(5, sim.DispatchSteps);

        // The tick that completes the window fires the timer synchronously
        // inside Advance: one self-Tell of the FireBody trigger, one more
        // dispatch step, and the body runs once with the burst's last id.
        // Total: 5 first hops + 1 FireBody hop = 6.
        sim.Advance(TimeSpan.FromMilliseconds(1));
        Assert.Equal(6, sim.DispatchSteps);
        Assert.Equal(6, sim.DispatchCountOf(actor));
        Assert.Empty(sim.DeadLetters.Records);

        // The probe ask is one more step; its reply is free (budget 2).
        var fired = sim.Ask<object>(actor, Activator.CreateInstance(getType)!);
        Assert.Equal(7, sim.DispatchSteps);
        var t = fired.GetType();
        Assert.Equal(1, (int)t.GetProperty("count")!.GetValue(fired)!);
        Assert.Equal(5, (int)t.GetProperty("lastId")!.GetValue(fired)!);
    }

    // ─── Budget 7: throttle chain (window math reads the simulated clock) ─

    private const string ThrottleSource = """
        using Spek.Streams;

        message Beat(int id);
        message GetBeats();
        message Beats(int count, int lastId);

        actor Meter
        {
            int count = 0;
            int lastId = 0;

            on Beat b
                => throttle(200)
                => {
                    count = count + 1;
                    lastId = b.id;
                }

            on GetBeats => return new Beats(count, lastId);
        }
        """;

    [Fact]
    public void ThrottleChain_WindowIsExactOnTheSimulatedClock()
    {
        var parse = SpekCompiler.Parse(ThrottleSource);
        Assert.True(parse.Success, string.Join("\n",
            parse.Diagnostics.Select(d => $"  {d.Code} {d.Line}:{d.Column} {d.Message}")));
        var assembly = RoslynCompileHelper.CompileAndLoad(
            new FileEmitter().Emit(parse.Tree!), "WorkBudgetThrottle");
        var actorType = assembly.GetTypes().Single(t => t.Name == "Meter");
        var beatType  = assembly.GetTypes().Single(t => t.Name == "Beat");
        var getType   = assembly.GetTypes().Single(t => t.Name == "GetBeats");

        using var sim = new SimulatedActorSystem(seed: 31, audit: true);
        var actor = sim.System.Spawn(actorType);

        // Four beats at one virtual instant: the first opens the window and
        // passes (leading edge), the other three read zero elapsed and drop.
        // 4 first hops + 1 FireBody hop = 5 steps.
        for (int i = 1; i <= 4; i++)
            actor.Tell(Activator.CreateInstance(beatType, i)!);
        sim.Run();
        Assert.Equal(5, sim.DispatchSteps);

        // One tick inside the window still drops: first hop only (+1 = 6).
        sim.Advance(TimeSpan.FromMilliseconds(199));
        actor.Tell(Activator.CreateInstance(beatType, 5)!);
        sim.Run();
        Assert.Equal(6, sim.DispatchSteps);

        // Elapsed == interval reopens the window: first hop + FireBody
        // (+2 = 8). Throttle has no timer, so Advance itself costs nothing.
        sim.Advance(TimeSpan.FromMilliseconds(1));
        actor.Tell(Activator.CreateInstance(beatType, 6)!);
        sim.Run();
        Assert.Equal(8, sim.DispatchSteps);
        Assert.Equal(8, sim.DispatchCountOf(actor));
        Assert.Empty(sim.DeadLetters.Records);

        // Probe: bodies ran for beats 1 and 6 only.
        var beats = sim.Ask<object>(actor, Activator.CreateInstance(getType)!);
        Assert.Equal(9, sim.DispatchSteps);
        var t = beats.GetType();
        Assert.Equal(2, (int)t.GetProperty("count")!.GetValue(beats)!);
        Assert.Equal(6, (int)t.GetProperty("lastId")!.GetValue(beats)!);
    }

    // Not budgeted here, deliberately:
    //   - asks issued INSIDE a handler: the await parks the dispatch and
    //     resumes on a thread-pool continuation, which Run() reaps through
    //     the SettleInflight quiet window. The step count stays exact, but
    //     the reap is wall-clock-bounded, not schedule-driven — the
    //     simulator's one soft edge.
    //
    // (Debounce / throttle chains used to sit in this list: their operators
    // armed raw System.Threading.Timers the simulated clock could not
    // control, so no exact count existed. Since the operators took the
    // system clock through StreamOperator.Configure, their FireBody hops
    // land inside Advance deterministically — budgets 6 and 7 above.)
}
