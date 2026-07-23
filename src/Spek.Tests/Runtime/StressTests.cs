using Spek;
using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Spek.Runtime;
using Spek.Testing;
using Spek.Tests.Emit;
using Xunit;

namespace Spek.Tests.Runtime;

/// <summary>
/// The wedge/stuck/dropped suite (r8's lesson made systematic): each test
/// floods a runtime feature PAIR with true parallelism and asserts the
/// terminal-state invariants — every message handled or dead-lettered,
/// every ask completed exactly once, no reply duplicated, nothing wedged.
/// The deterministic simulator can't see these (it linearizes readers);
/// only real parallelism exercises the runtime's own machinery.
/// </summary>
[Trait("Category", "Stress")]
public sealed class StressTests
{
    private static object Load(string source, string fixture, string typeName, out System.Reflection.Assembly asm)
    {
        var parse = SpekCompiler.Parse(source);
        Assert.True(parse.Success,
            string.Join("\n", parse.Diagnostics.Select(d => $"{d.Code} {d.Message}")));
        asm = RoslynCompileHelper.CompileAndLoad(new FileEmitter().Emit(parse.Tree!), fixture);
        return asm.GetType(typeName)!;
    }

    // ─── readers × writers × asks, all overlapping ───────────────────────

    [Fact]
    public async Task ReaderWriterAskFlood_CountsExact_NoReplyLostOrDuplicatedAsync()
    {
        const string source = """
            namespace RwFlood;

            message Add(int n);
            message Get();
            message Total(int value);

            actor Counter
            {
                int total = 0;

                behavior Default
                {
                    on Add a => { total = total + a.n; }
                    public reader on Get => { return new Total(total); }
                }
            }
            """;
        var counterType = (Type)Load(source, "RwFloodFixture", "RwFlood.Counter", out var asm);
        var addCtor = asm.GetType("RwFlood.Add")!.GetConstructor([typeof(int)])!;
        var getCtor = asm.GetType("RwFlood.Get")!.GetConstructor(Type.EmptyTypes)!;
        var totalValue = asm.GetType("RwFlood.Total")!.GetProperty("value")!;

        TestActorSystem.ResetReplyDiagnostics();
        using var system = new TestActorSystem("stress-rw");
        var counter = system.Spawn(counterType);

        var askMethod = typeof(ActorRef).GetMethods()
            .Single(m => m.Name == nameof(ActorRef.AskAsync) && m.GetParameters().Length == 1)
            .MakeGenericMethod(typeof(object));

        const int WritersPerTask = 250, GetsPerTask = 250, Tasks = 4;
        var reads = new List<Task<object>>();
        var work = new List<Task>();
        for (var t = 0; t < Tasks; t++)
        {
            work.Add(Task.Run(() =>
            {
                for (var i = 0; i < WritersPerTask; i++)
                    counter.Tell(addCtor.Invoke([1]));
            }));
            work.Add(Task.Run(() =>
            {
                var mine = new List<Task<object>>();
                for (var i = 0; i < GetsPerTask; i++)
                    mine.Add(((ValueTask<object>)askMethod.Invoke(counter, [getCtor.Invoke(null)])!).AsTask());
                lock (reads) reads.AddRange(mine);
            }));
        }
        await Task.WhenAll(work);

        var allReads = Task.WhenAll(reads);
        Assert.True(await Task.WhenAny(allReads, Task.Delay(TimeSpan.FromSeconds(30))) == allReads,
            $"reads wedged: {reads.Count(r => r.IsCompleted)}/{reads.Count} completed");

        await system.WhenIdleAsync(TimeSpan.FromSeconds(30));
        var final = (ValueTask<object>)askMethod.Invoke(counter, [getCtor.Invoke(null)])!;
        Assert.Equal(Tasks * WritersPerTask, (int)totalValue.GetValue(await final)!);

        var (delivered, dup, failed) = TestActorSystem.ReplyDiagnostics;
        Assert.Equal(0, dup);                              // no cross-routed replies
        Assert.Equal(Tasks * GetsPerTask + 1, delivered);  // one reply per ask
        Assert.Equal(0, failed);
    }

    // ─── asks × supervision restarts ─────────────────────────────────────

    private sealed record Poke(int N);

    private sealed class CrashEveryFifth : ActorBase
    {
        private int _seen;
        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            _seen++;
            if (_seen % 5 == 0) throw new InvalidOperationException("scripted crash");
            sender.Tell(_seen, _selfRef!);
            return Task.CompletedTask;
        }
        protected override FailureDirective OnFailure(Exception ex, object message)
            => FailureDirective.Resume;
    }

    [Fact]
    public async Task AsksAgainstACrashingActor_AllTerminate_NoneWedgeAsync()
    {
        using var system = new TestActorSystem("stress-crash");
        var actor = system.Spawn<CrashEveryFifth>();

        // Every ask must terminate: a reply, or a timeout — never a hang.
        const int Asks = 200;
        var outcomes = new Task[Asks];
        for (var i = 0; i < Asks; i++)
            outcomes[i] = actor.AskAsync<object>(new Poke(i), TimeSpan.FromSeconds(15))
                .AsTask().ContinueWith(t => { _ = t.Exception; }, TaskScheduler.Default);

        var all = Task.WhenAll(outcomes);
        Assert.True(await Task.WhenAny(all, Task.Delay(TimeSpan.FromSeconds(40))) == all,
            $"asks wedged: {outcomes.Count(t => t.IsCompleted)}/{Asks} terminated");
    }

    // ─── defer × flood: every parked message reaches a terminal state ────

    private sealed record Job(int N);

    private sealed class Sink : ActorBase
    {
        public static int Handled;
        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            Interlocked.Increment(ref Handled);
            return Task.CompletedTask;
        }
    }

    [Fact]
    public async Task DeferFlood_EveryMessageTerminal_ProcessedOrDeadLetteredAsync()
    {
        Sink.Handled = 0;
        var sink = new RecordingDeadLetterSink();
        using var system = new ActorSystem("stress-defer", deadLetterSink: sink);
        var actor = system.Spawn<Sink>();
        actor.AttachIngressPolicy(Spek.Resilience.RateLimiting.RateLimitIngressPolicy.TokenBucket(
            permitsPerSecond: 50, burstCapacity: 10));

        const int Sent = 120;
        var senders = Enumerable.Range(0, 4).Select(t => Task.Run(() =>
        {
            for (var i = 0; i < Sent / 4; i++) actor.Tell(new Job(i));
        }));
        await Task.WhenAll(senders);

        // Terminal-state invariant: handled + dead-lettered == sent, within
        // a window generous enough for re-admission cycles under suite load.
        await TestActorSystem.WaitUntilAsync(
            () => Volatile.Read(ref Sink.Handled) + sink.Records.Count >= Sent,
            timeout: TimeSpan.FromSeconds(60), description: "all messages terminal");
        Assert.Equal(Sent, Volatile.Read(ref Sink.Handled) + sink.Records.Count);
    }

    // ─── passivation churn × persistent state: nothing lost ──────────────

    [Fact]
    public async Task PassivationChurn_UnderBursts_LosesNothingAsync()
    {
        const string source = """
            namespace Churn;

            message Bump();
            message Read();
            message Count(int value);

            actor Tally
            {
                int total = 0;

                passivate after System.TimeSpan.FromMilliseconds(120);

                behavior Default
                {
                    on Bump b => { total = total + 1; persist; }
                    on Read r => { return new Count(total); }
                }
            }
            """;
        var tallyType = (Type)Load(source, "ChurnFixture", "Churn.Tally", out var asm);
        var bumpCtor = asm.GetType("Churn.Bump")!.GetConstructor(Type.EmptyTypes)!;
        var readCtor = asm.GetType("Churn.Read")!.GetConstructor(Type.EmptyTypes)!;
        var countValue = asm.GetType("Churn.Count")!.GetProperty("value")!;

        using var system = new TestActorSystem("stress-churn");
        var tally = system.SpawnPersistent(tallyType, "tally-1");

        // Bursts separated by gaps longer than the passivation window, so
        // the actor materializes and unloads repeatedly mid-run.
        const int Bursts = 5, PerBurst = 40;
        for (var b = 0; b < Bursts; b++)
        {
            for (var i = 0; i < PerBurst; i++) tally.Tell(bumpCtor.Invoke(null));
            await system.WhenIdleAsync(TimeSpan.FromSeconds(30));
            await Task.Delay(300);   // outlast the passivation window
        }

        var askMethod = typeof(ActorRef).GetMethods()
            .Single(m => m.Name == nameof(ActorRef.AskAsync) && m.GetParameters().Length == 1)
            .MakeGenericMethod(typeof(object));
        var final = await ((ValueTask<object>)askMethod.Invoke(tally, [readCtor.Invoke(null)])!).AsTask();
        Assert.Equal(Bursts * PerBurst, (int)countValue.GetValue(final)!);
    }

    // ─── stop mid-flood: everything terminal ─────────────────────────────

    private sealed record Halt();

    private sealed class Stoppable : ActorBase
    {
        public static int Handled;
        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            if (message is Halt) StopSelf();
            else Interlocked.Increment(ref Handled);
            return Task.CompletedTask;
        }
    }

    [Fact]
    public async Task StopMidFlood_EveryMessageHandledOrDeadLetteredAsync()
    {
        Stoppable.Handled = 0;
        var sink = new RecordingDeadLetterSink();
        using var system = new ActorSystem("stress-stop", deadLetterSink: sink);
        var actor = system.Spawn<Stoppable>();

        const int Before = 150, After = 150;
        for (var i = 0; i < Before; i++) actor.Tell(new Job(i));
        actor.Tell(new Halt());
        for (var i = 0; i < After; i++) actor.Tell(new Job(i));

        await TestActorSystem.WaitUntilAsync(
            () => Volatile.Read(ref Stoppable.Handled) + sink.Records.Count >= Before + After,
            timeout: TimeSpan.FromSeconds(30), description: "all messages terminal");
        // The Halt is consumed (not counted); everything else is handled or
        // dead-lettered — exactly no silent drops.
        Assert.Equal(Before + After, Volatile.Read(ref Stoppable.Handled) + sink.Records.Count);
        Assert.True(actor.IsStopped);
    }
}
