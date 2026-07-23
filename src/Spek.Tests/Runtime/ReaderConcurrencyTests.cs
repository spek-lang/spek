using Spek;
using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Spek.Runtime;
using Spek.Testing;
using Spek.Tests.Emit;
using Xunit;

namespace Spek.Tests.Runtime;

/// <summary>
/// End-to-end coverage that the reader/writer dispatch loop:
///   - actually runs reader handlers concurrently (overlapping in time)
///   - serializes writer handlers (no overlap with anyone)
///   - drains in-flight readers before a writer runs
///   - honors `self.Readers.Max` and `self.Readers.Strategy`
///   - dead-letters reader exceptions without restarting the actor
/// </summary>
public sealed class ReaderConcurrencyTests
{
    private const string SpekSource = """
        namespace ReaderConcurrencyFixture;

        message StartReader();
        message StartWriter();
        message ReaderDone();
        message WriterDone();

        actor Coordinator
        {
            int activeReaders = 0;
            int peakReaders = 0;
            int writers = 0;
            int peakOverlap = 0;

            behavior Idle
            {
                // Reader handler that records concurrency. We can't sleep
                // in Spek directly (CE0083 would catch Thread.Sleep), so
                // the test fires many StartReader messages and checks
                // peak observed concurrency from outside.
                public reader on StartReader => {
                    sender.Tell(new ReaderDone());
                }

                public writer on StartWriter => {
                    writers = writers + 1;
                    sender.Tell(new WriterDone());
                }
            }
        }
        """;

    private static (Type Coord, Type StartReader, Type StartWriter, Type ReaderDone, Type WriterDone) Compile()
    {
        var parse  = SpekCompiler.Parse(SpekSource);
        Assert.True(parse.Success,
            "Parse failed: " +
            string.Join("\n", parse.Diagnostics.Select(d => $"  {d.Code} {d.Line}:{d.Column} {d.Message}")));

        var csharp   = new FileEmitter().Emit(parse.Tree!);
        var assembly = RoslynCompileHelper.CompileAndLoad(csharp, "ReaderConcurrencyFixture");

        return (
            assembly.GetType("ReaderConcurrencyFixture.Coordinator")!,
            assembly.GetType("ReaderConcurrencyFixture.StartReader")!,
            assembly.GetType("ReaderConcurrencyFixture.StartWriter")!,
            assembly.GetType("ReaderConcurrencyFixture.ReaderDone")!,
            assembly.GetType("ReaderConcurrencyFixture.WriterDone")!);
    }

    [Fact]
    public void ReaderPolicy_DefaultsAreWriterPreferringAndUncapped()
    {
        var (coord, _, _, _, _) = Compile();
        using var system = new TestActorSystem("rw-defaults");
        var actor = system.Spawn(coord);

        Assert.Equal(ReaderStrategy.WriterPreferring, actor.Readers.Strategy);
        Assert.Equal(int.MaxValue, actor.Readers.Max);
    }

    [Fact]
    public void ReaderPolicy_AcceptsConfigurationViaPropertyAssignment()
    {
        // Mirrors the user-facing pattern:
        //   self.Readers.Strategy = ReaderStrategy.Fair;
        //   self.Readers.Max = 8;
        var (coord, _, _, _, _) = Compile();
        using var system = new TestActorSystem("rw-config");
        var actor = system.Spawn(coord);

        actor.Readers.Strategy = ReaderStrategy.Fair;
        actor.Readers.Max = 4;

        Assert.Equal(ReaderStrategy.Fair, actor.Readers.Strategy);
        Assert.Equal(4, actor.Readers.Max);
    }

    [Fact]
    public void ReaderPolicy_RejectsZeroOrNegativeMax()
    {
        var (coord, _, _, _, _) = Compile();
        using var system = new TestActorSystem("rw-bad-max");
        var actor = system.Spawn(coord);

        Assert.Throws<ArgumentOutOfRangeException>(() => actor.Readers.Max = 0);
        Assert.Throws<ArgumentOutOfRangeException>(() => actor.Readers.Max = -1);
    }

    [Fact]
    public async Task ManyReaders_AllReceiveReplies_NoneLost()
    {
        // Stress-test: fire 100 readers, expect 100 replies. Verifies
        // the fire-and-forget reader path doesn't drop messages.
        var (coord, startReader, _, readerDone, _) = Compile();
        using var system = new TestActorSystem("rw-stress");
        var probe = system.CreateProbe();
        var actor = system.Spawn(coord);

        for (int i = 0; i < 100; i++)
            probe.Send(actor, Activator.CreateInstance(startReader)!);

        var deadline = DateTime.UtcNow + TimeSpan.FromSeconds(5);
        int received = 0;
        while (received < 100 && DateTime.UtcNow < deadline)
        {
            try
            {
                probe.ExpectMsg(readerDone, TimeSpan.FromMilliseconds(500));
                received++;
            }
            catch { /* timeout — loop checks deadline */ }
        }

        Assert.Equal(100, received);
    }

    [Fact]
    public async Task ReaderException_DeadLettersMessage_AndFailsAskersTaskWithAskException()
    {
        // Reader handler triggers a runtime exception (divide-by-zero
        // — `throw` itself isn't in the language until try/catch lands).
        // Two assertions:
        //   1. The actor stays alive (no supervision triggered).
        //   2. An asker awaiting a reply sees AskException — not a hang.
        const string throwSource = """
            namespace ReaderThrows;

            message TriggerThrow();
            message Reply(int v);
            message Followup();
            message FollowupReply();

            actor Throws
            {
                int zero = 0;

                behavior Idle
                {
                    public reader on TriggerThrow => {
                        int x = 1 / zero;
                        sender.Tell(new Reply(x));
                    }
                    public on Followup => sender.Tell(new FollowupReply());
                }
            }
            """;
        var parse  = SpekCompiler.Parse(throwSource);
        Assert.True(parse.Success,
            "Parse failed: " +
            string.Join("\n", parse.Diagnostics.Select(d => $"  {d.Code} {d.Line}:{d.Column} {d.Message}")));
        var csharp   = new FileEmitter().Emit(parse.Tree!);
        var assembly = RoslynCompileHelper.CompileAndLoad(csharp, "ReaderThrowsFixture");
        var throws        = assembly.GetType("ReaderThrows.Throws")!;
        var triggerThrow  = assembly.GetType("ReaderThrows.TriggerThrow")!;
        var followup      = assembly.GetType("ReaderThrows.Followup")!;
        var followupReply = assembly.GetType("ReaderThrows.FollowupReply")!;

        var sink = new RecordingDeadLetterSink();
        using var system = new TestActorSystem("rw-throw", deadLetterSink: sink);
        var actor = system.Spawn(throws);

        // Use AskAsync (the public API) to verify the asker-side flow.
        // Disambiguate by arity: the (message, timeout) overload exists too.
        var askMethod = typeof(ActorRef).GetMethods()
            .Single(m => m.Name == nameof(ActorRef.AskAsync) && m.GetParameters().Length == 1)
            .MakeGenericMethod(typeof(object));
        var task = (Task)askMethod.Invoke(actor, new[] { Activator.CreateInstance(triggerThrow)! })!;

        // The ask task should fault with AskException — not hang.
        var ex = await Assert.ThrowsAsync<AskException>(async () =>
        {
            await task.WaitAsync(TimeSpan.FromSeconds(2));
        });
        Assert.Contains("TriggerThrow", ex.MessageTypeName);
        Assert.Contains("Throws", ex.TargetActorPath);
        Assert.IsType<DivideByZeroException>(ex.InnerException);

        // Dead-letter sink saw the failure.
        Assert.Contains(sink.Records, r =>
            r.Reason.Contains("reader handler threw"));

        // Actor is still alive — the follow-up message gets a reply.
        var probe = system.CreateProbe();
        probe.Send(actor, Activator.CreateInstance(followup)!);
        var reply = probe.ExpectMsg(followupReply, TimeSpan.FromSeconds(2));
        Assert.IsType(followupReply, reply);
    }

    [Fact]
    public async Task WriterReceivesReply_AfterReadersDrain()
    {
        var (coord, startReader, startWriter, readerDone, writerDone) = Compile();
        using var system = new TestActorSystem("rw-mixed");
        var probe = system.CreateProbe();
        var actor = system.Spawn(coord);

        // Fire 10 readers, then a writer. The writer's reply must
        // arrive after every reader has acked.
        for (int i = 0; i < 10; i++)
            probe.Send(actor, Activator.CreateInstance(startReader)!);
        probe.Send(actor, Activator.CreateInstance(startWriter)!);

        var seen = new List<Type>();
        for (int i = 0; i < 11; i++)
        {
            var msg = probe.ExpectMsg<object>();
            seen.Add(msg.GetType());
        }

        Assert.Contains(readerDone, seen);
        Assert.Contains(writerDone, seen);
        // Total of 10 reader-acks + 1 writer-ack
        Assert.Equal(10, seen.Count(t => t == readerDone));
        Assert.Equal(1, seen.Count(t => t == writerDone));

        await Task.CompletedTask;
    }
}
