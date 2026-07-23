using Spek;
using Spek.Runtime;
using Spek.Testing;
using Xunit;

namespace Spek.Tests.Runtime;

/// <summary>
/// The inbox observer — tcpdump for a mailbox. Passive (can't perturb the
/// actor), isolated (a throwing observer dead-letters, the actor never
/// sees it), and lossy under pressure (bounded buffer, counted drops)
/// rather than ever stalling the enqueue path.
/// </summary>
public sealed class InboxObserverTests
{
    private sealed record Ping(int N);
    private sealed record Nudge();

    private sealed class Sink : ActorBase
    {
        public static int Handled;
        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            Interlocked.Increment(ref Handled);
            return Task.CompletedTask;
        }
    }

    private sealed class Forwarder : ActorBase
    {
        private readonly ActorRef _target;
        public Forwarder(ActorRef target) => _target = target;
        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            _target.Tell(message, _selfRef!);
            return Task.CompletedTask;
        }
    }

    [Fact]
    public async Task Observe_SeesMessages_InOrder_WithNullSenderForProgramSendsAsync()
    {
        using var system = new ActorSystem("tap-order");
        var actor = system.Spawn<Sink>();
        var recorder = new RecordingObserver();
        using var tap = system.Observe(actor, recorder.OnMessage);

        actor.Tell(new Ping(1));
        actor.Tell(new Ping(2));
        actor.Tell(new Ping(3));

        await TestActorSystem.WaitUntilAsync(
            () => recorder.Count == 3, description: "three observed messages");

        var seen = recorder.Messages;
        Assert.Equal(new[] { 1, 2, 3 }, seen.Select(m => ((Ping)m.Message).N));
        Assert.All(seen, m => Assert.Null(m.Sender));           // program sends carry no sender
        Assert.All(seen, m => Assert.NotEqual(default, m.EnqueuedAt));
        Assert.Equal(0, tap.Dropped);
    }

    [Fact]
    public async Task Observe_ActorToActorSend_CarriesTheSenderAsync()
    {
        using var system = new ActorSystem("tap-sender");
        var target = system.Spawn<Sink>();
        var forwarder = system.Spawn<Forwarder>(target);
        var recorder = new RecordingObserver();
        using var tap = system.Observe(target, recorder.OnMessage);

        forwarder.Tell(new Nudge());

        await TestActorSystem.WaitUntilAsync(
            () => recorder.Count == 1, description: "forwarded message observed");
        Assert.NotNull(recorder.Messages[0].Sender);
    }

    [Fact]
    public async Task Dispose_DetachesTheTapAsync()
    {
        Sink.Handled = 0;
        using var system = new ActorSystem("tap-detach");
        var actor = system.Spawn<Sink>();
        var recorder = new RecordingObserver();
        var tap = system.Observe(actor, recorder.OnMessage);

        actor.Tell(new Ping(1));
        await TestActorSystem.WaitUntilAsync(
            () => recorder.Count == 1, description: "first message observed");

        tap.Dispose();
        actor.Tell(new Ping(2));
        await TestActorSystem.WaitUntilAsync(
            () => Volatile.Read(ref Sink.Handled) == 2, description: "second message handled");

        Assert.Equal(1, recorder.Count);   // nothing observed after detach
    }

    [Fact]
    public async Task ThrowingObserver_IsIsolated_DeadLettersAndActorKeepsWorkingAsync()
    {
        Sink.Handled = 0;
        var sink = new RecordingDeadLetterSink();
        using var system = new ActorSystem("tap-throw", deadLetterSink: sink);
        var actor = system.Spawn<Sink>();
        using var tap = system.Observe(actor,
            _ => throw new InvalidOperationException("observer bug"));

        actor.Tell(new Ping(1));
        actor.Tell(new Ping(2));

        await TestActorSystem.WaitUntilAsync(
            () => Volatile.Read(ref Sink.Handled) == 2, description: "actor handled both");
        await TestActorSystem.WaitUntilAsync(
            () => sink.Records.Count == 2, description: "both observer throws dead-lettered");

        Assert.All(sink.Records, r =>
        {
            Assert.Contains("inbox observer threw", r.Reason);
            Assert.IsType<InvalidOperationException>(r.Cause);
        });
        // The actor and its supervision never saw the observer's exception.
        Assert.Equal(2, Volatile.Read(ref Sink.Handled));
    }

    [Fact]
    public async Task SlowObserver_ShedsLoad_IntoTheDropCounterAsync()
    {
        using var system = new ActorSystem("tap-slow");
        var actor = system.Spawn<Sink>();
        using var gate = new SemaphoreSlim(0);
        // Capacity 1 and a blocked consumer: the first event occupies the
        // pump, the second fills the buffer, everything after that drops.
        using var tap = system.Observe(actor, _ => gate.Wait(), bufferCapacity: 1);

        for (int i = 0; i < 20; i++) actor.Tell(new Ping(i));

        await TestActorSystem.WaitUntilAsync(
            () => tap.Dropped > 0, description: "overflow counted as drops");
        gate.Release(20);
    }

    [Fact]
    public void Observe_GuardsItsArguments()
    {
        using var system = new ActorSystem("tap-args");
        var actor = system.Spawn<Sink>();
        Assert.Throws<ArgumentNullException>(() => system.Observe(null!, _ => { }));
        Assert.Throws<ArgumentNullException>(() => system.Observe(actor, null!));
        Assert.Throws<ArgumentOutOfRangeException>(
            () => system.Observe(actor, _ => { }, bufferCapacity: 0));
    }

    [Fact]
    public async Task VirtualTime_EnqueuedAt_ReadsTheManualClockAsync()
    {
        using var sys = new TestActorSystem("tap-clock", virtualTime: true);
        var actor = sys.Spawn<Sink>();
        var recorder = new RecordingObserver();
        using var tap = sys.Observe(actor, recorder.OnMessage);

        var before = sys.Clock!.GetUtcNow();
        actor.Tell(new Ping(1));
        await TestActorSystem.WaitUntilAsync(
            () => recorder.Count == 1, description: "message observed");

        Assert.Equal(before, recorder.Messages[0].EnqueuedAt);  // frozen clock: exact
    }
}
