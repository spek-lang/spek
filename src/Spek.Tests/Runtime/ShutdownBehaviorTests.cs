using System.Runtime.CompilerServices;   // StrongBox
using Spek;                 // SharedRegion, ActorBase, ActorRef
using Spek.Compiler.Emit;   // FileEmitter
using Spek.Compiler.Parser; // SpekCompiler
using Spek.Persistence;     // ISnapshotStore, Snapshot
using Spek.Runtime;         // ActorSystem, IDeadLetterSink, RecordingDeadLetterSink, InMemorySnapshotStore
using Spek.Tests.Emit;      // RoslynCompileHelper
using Xunit;

namespace Spek.Tests.Runtime;

/// <summary>
/// Behavior tests for the shutdown machinery — the cleanup that
/// runs when an <see cref="ActorSystem"/> tears down: shared-region
/// <c>term { }</c> blocks (reverse construction order, isolated failures,
/// dead-lettered throws), the actor stop-hook double-fire guard, and the
/// durable flush of every <c>: Persisted</c> region. These paths are
/// emit-tested elsewhere; here we drive the runtime end-to-end and assert on
/// observed behavior.
///
/// Scenarios 1 and 2 hand-write <see cref="SharedRegion"/> subclasses (the same
/// shape the compiler emits for a `shared X { term { ... } }` declaration) and
/// materialise them through the public <see cref="ActorSystem.GetSharedRegion{T}"/>
/// — which is exactly what an actor's `use X;` lowers to, and what records the
/// region in the system's LIFO construction order. Scenarios 3 and 4 use a
/// hand-written actor / compiled Spek respectively.
/// </summary>
public sealed class ShutdownBehaviorTests
{
    // ─────────────────────────────────────────────────────────────────────────
    // Scenario 1 — term blocks run in REVERSE construction order on shutdown.
    // ─────────────────────────────────────────────────────────────────────────

    // The sink every Order* region appends to as its term block runs. xUnit runs
    // the tests in one class serially, so a per-test reset is enough; we also key
    // each scenario to its own region types so they can't cross-contaminate.
    private static readonly List<string> OrderSink = new();

    private abstract class OrderRegion : SharedRegion
    {
        protected abstract string Marker { get; }
        // The compiler emits the user's `term { }` body as an OnTerm override; this
        // mirrors that — append our marker so the disposal order is observable.
        protected override void OnTerm()
        {
            lock (OrderSink) OrderSink.Add(Marker);
        }
    }

    private sealed class OrderRegionA : OrderRegion { protected override string Marker => "A"; }
    private sealed class OrderRegionB : OrderRegion { protected override string Marker => "B"; }
    private sealed class OrderRegionC : OrderRegion { protected override string Marker => "C"; }

    [Fact]
    public void TermBlocks_RunInReverseConstructionOrder_OnShutdown()
    {
        lock (OrderSink) OrderSink.Clear();

        var system = new ActorSystem("order");

        // Materialise A, then B, then C — the construction order the runtime
        // records. (This is what `use OrderRegionA;` etc. lowers to inside an
        // actor; calling it directly keeps the order deterministic.)
        system.GetSharedRegion<OrderRegionA>();
        system.GetSharedRegion<OrderRegionB>();
        system.GetSharedRegion<OrderRegionC>();

        // Shutdown disposes regions LIFO — like C# `using` blocks unwinding.
        system.Dispose();

        List<string> observed;
        lock (OrderSink) observed = OrderSink.ToList();

        Assert.Equal(new[] { "C", "B", "A" }, observed);
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Scenario 2 — a throwing term block is isolated + dead-lettered, doesn't
    //              block the other regions, and Dispose does not propagate.
    // ─────────────────────────────────────────────────────────────────────────

    private static readonly List<string> ThrowSink = new();

    private sealed class ThrowTermBoom : Exception
    {
        public ThrowTermBoom() : base("term block blew up") { }
    }

    private sealed class ThrowRegionFirst : SharedRegion
    {
        protected override void OnTerm() { lock (ThrowSink) ThrowSink.Add("First"); }
    }

    // Constructed second → disposed second (middle of the LIFO unwind). Its throw
    // must not stop "First" (constructed first, disposed last) from running.
    private sealed class ThrowRegionMiddle : SharedRegion
    {
        protected override void OnTerm()
        {
            lock (ThrowSink) ThrowSink.Add("Middle-entered");
            throw new ThrowTermBoom();
        }
    }

    private sealed class ThrowRegionLast : SharedRegion
    {
        protected override void OnTerm() { lock (ThrowSink) ThrowSink.Add("Last"); }
    }

    [Fact]
    public void ThrowingTermBlock_IsIsolated_DeadLettered_AndDoesNotBlockOthers()
    {
        lock (ThrowSink) ThrowSink.Clear();

        var sink = new RecordingDeadLetterSink();
        var system = new ActorSystem("throw-term", deadLetterSink: sink);

        // Construction order First, Middle, Last → disposal order Last, Middle, First.
        system.GetSharedRegion<ThrowRegionFirst>();
        system.GetSharedRegion<ThrowRegionMiddle>();
        system.GetSharedRegion<ThrowRegionLast>();

        // The middle region's term block throws; the runtime must swallow it,
        // dead-letter it, and keep disposing the rest. Dispose itself must not
        // propagate the throw — this call returning normally is part of the assert.
        var ex = Record.Exception(() => system.Dispose());
        Assert.Null(ex);

        List<string> observed;
        lock (ThrowSink) observed = ThrowSink.ToList();

        // The two non-throwing term blocks both ran, in LIFO order, and the
        // throwing one was actually entered (so it really did throw, not skip).
        Assert.Contains("Last", observed);
        Assert.Contains("First", observed);
        Assert.Contains("Middle-entered", observed);
        // LIFO: Last ran before First even though Middle blew up between them.
        Assert.True(observed.IndexOf("Last") < observed.IndexOf("First"),
            $"expected Last to run before First; observed [{string.Join(", ", observed)}]");

        // Exactly one dead-letter recorded the term-block throw, carrying the
        // original exception as its cause.
        var boom = sink.Records.Where(r => r.Cause is ThrowTermBoom).ToList();
        Assert.Single(boom);
        Assert.Contains("term block threw", boom[0].Reason);
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Scenario 3 — OnPostStop / OnTerm fire EXACTLY ONCE under the double-fire
    //              guard, even when supervision-Stop and Dispose both run.
    // ─────────────────────────────────────────────────────────────────────────

    public sealed record Detonate();

    /// Throws on its first (and only) message so the supervisor's default
    /// directive (Stop) fires the stop sequence. Counts every OnPostStop / OnTerm
    /// invocation so we can assert the slot's `_stopHooksRan` guard prevents the
    /// later Dispose() from re-running them.
    private sealed class CountingStopActor : ActorBase
    {
        private readonly StrongBox<int> _postStops;
        private readonly StrongBox<int> _terms;

        public CountingStopActor(StrongBox<int> postStops, StrongBox<int> terms)
        {
            _postStops = postStops;
            _terms = terms;
        }

        protected override Task DispatchAsync(object message, ActorRef sender)
            => throw new InvalidOperationException("boom — force a supervision Stop");

        protected override void OnPostStop() => Interlocked.Increment(ref _postStops.Value);
        protected override void OnTerm() => Interlocked.Increment(ref _terms.Value);
    }

    [Fact]
    public async Task StopHooks_FireExactlyOnce_AcrossSupervisionStopAndDispose()
    {
        var postStops = new StrongBox<int>(0);
        var terms = new StrongBox<int>(0);

        var system = new ActorSystem("double-fire");
        var actor = system.Spawn<CountingStopActor>(postStops, terms);

        // Make the handler throw → default OnFailure returns Stop → RunStopHooks
        // fires OnPostStop + OnTerm once (guarded by _stopHooksRan).
        actor.Tell(new Detonate());

        var deadline = DateTime.UtcNow + TimeSpan.FromSeconds(10);
        while (!actor.IsStopped && DateTime.UtcNow < deadline)
            await Task.Delay(25);
        Assert.True(actor.IsStopped, "actor should have been stopped by supervision after throwing");

        // Give any racing hook a moment, then confirm the supervision path fired once.
        await Task.Delay(50);
        Assert.Equal(1, postStops.Value);
        Assert.Equal(1, terms.Value);

        // Now tear the system down. StopGracefully → RunStopHooks again, but the
        // _stopHooksRan guard must short-circuit it: the counters stay at 1.
        system.Dispose();

        Assert.Equal(1, postStops.Value);
        Assert.Equal(1, terms.Value);
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Scenario 4 — every ": Persisted" region is flushed on shutdown.
    // ─────────────────────────────────────────────────────────────────────────

    // Two distinct persisted regions, each mutated by a writer handler on the same
    // actor; the actor then triggers self.System.Shutdown(). Both regions' final
    // state must be flushed to the shared store before teardown completes. The
    // `program` block exists only to satisfy CE0098 (a `: Persisted` region needs a
    // registered provider somewhere in the compilation); the test drives its own
    // system and wires the real store via the ActorSystem ctor.
    private const string MultiPersistSrc = """
        namespace MultiPersistDemo;

        shared Alpha : Persisted
        {
            int Count = 0;
        }

        shared Beta : Persisted
        {
            int Count = 0;
        }

        message Go();

        actor Writer
        {
            use Alpha alpha;
            use Beta beta;

            writer on Go =>
            {
                alpha.Count = alpha.Count + 7;     // writer mutation → region save on exit
                beta.Count = beta.Count + 11;
                self.System.Shutdown();            // leaf/driver triggers shutdown
            }
        }

        program Main
        {
            var system = new ActorSystem("demo");
            system.RegisterPersistenceProvider<Alpha>(new InMemorySnapshotStore());
            system.RegisterPersistenceProvider<Beta>(new InMemorySnapshotStore());
        }
        """;

    [Fact]
    public async Task MultiplePersistedRegions_AreAllFlushed_OnShutdown()
    {
        var parsed = SpekCompiler.Parse(MultiPersistSrc);
        Assert.True(parsed.Success,
            string.Join("\n", parsed.Diagnostics.Select(d => $"{d.Code} {d.Line}:{d.Column} {d.Message}")));
        var csharp = new FileEmitter().Emit(parsed.Tree!);
        var asm = RoslynCompileHelper.CompileAndLoad(csharp, "MultiPersistShutdown");

        var writerType = asm.GetType("MultiPersistDemo.Writer")!;
        var goType = asm.GetType("MultiPersistDemo.Go")!;

        // One real store, shared by both regions. The region's snapshot key is its
        // type name ("Alpha" / "Beta").
        var store = new InMemorySnapshotStore();

        var system = new ActorSystem("multi", snapshotStore: store);
        try
        {
            var writer = system.Spawn(writerType, Array.Empty<object>());
            writer.Tell(Activator.CreateInstance(goType)!);   // mutates both → triggers shutdown

            // The hostless self.System.Shutdown() ends in a background
            // GracefulShutdown → Dispose, which flushes the persisted regions.
            Assert.True(system.AwaitTermination(TimeSpan.FromSeconds(10)),
                "system should terminate after the writer-initiated shutdown");
        }
        finally
        {
            system.Dispose();   // idempotent; also flushes if the shutdown path didn't
        }

        // Both regions' final state landed in the store, keyed by region type name.
        var alpha = await store.LoadAsync("Alpha");
        var beta = await store.LoadAsync("Beta");

        Assert.NotNull(alpha);
        Assert.NotNull(beta);
        Assert.Equal(7, alpha!.Get<int>("Count"));
        Assert.Equal(11, beta!.Get<int>("Count"));
    }
}

