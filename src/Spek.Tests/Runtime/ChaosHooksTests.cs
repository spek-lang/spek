using Spek;
using Spek.Runtime;
using Spek.Testing;
using Xunit;

namespace Spek.Tests.Runtime;

/// <summary>
/// Chaos hooks (D4): drop / delay / duplicate at the enqueue path,
/// crash-on-nth at the dispatch path — attach-only-at-construction, and
/// an injected crash unwinds through the real supervision machinery.
/// </summary>
public sealed class ChaosHooksTests
{
    private sealed record Ping(int N);

    private sealed class Counter : ActorBase
    {
        public static int Handled;
        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            Interlocked.Increment(ref Handled);
            return Task.CompletedTask;
        }
    }

    [Fact]
    public async Task Drop_EveryThird_LosesExactlyThoseMessagesAsync()
    {
        Counter.Handled = 0;
        var chaos = new ChaosPlan().Drop<Ping>(every: 3);
        using var system = new ActorSystem("chaos-drop", chaos: chaos);
        var actor = system.Spawn<Counter>();

        for (int i = 1; i <= 6; i++) actor.Tell(new Ping(i));

        await TestActorSystem.WaitUntilAsync(
            () => Volatile.Read(ref Counter.Handled) == 4, description: "4 of 6 handled");
        await Task.Delay(100);                            // grace: nothing extra arrives
        Assert.Equal(4, Volatile.Read(ref Counter.Handled));
        Assert.Equal(2, chaos.Fires);                     // messages 3 and 6
    }

    [Fact]
    public async Task Delay_UnderVirtualTime_HoldsTheMessageUntilAdvanceAsync()
    {
        Counter.Handled = 0;
        var chaos = new ChaosPlan();
        using var sys = new TestActorSystem("chaos-delay", virtualTime: true, chaos: chaos);
        var actor = sys.Spawn<Counter>();
        chaos.Delay(actor, by: TimeSpan.FromSeconds(30));

        actor.Tell(new Ping(1));
        await Task.Delay(100);
        Assert.Equal(0, Volatile.Read(ref Counter.Handled));   // held by the fault

        sys.AdvanceClock(TimeSpan.FromSeconds(31));            // fault elapses, message lands
        await TestActorSystem.WaitUntilAsync(
            () => Volatile.Read(ref Counter.Handled) == 1, description: "delayed message arrived");
    }

    [Fact]
    public async Task Duplicate_DeliversTheMessageTwiceAsync()
    {
        Counter.Handled = 0;
        var chaos = new ChaosPlan().Duplicate<Ping>();
        using var system = new ActorSystem("chaos-dup", chaos: chaos);
        var actor = system.Spawn<Counter>();

        actor.Tell(new Ping(1));

        await TestActorSystem.WaitUntilAsync(
            () => Volatile.Read(ref Counter.Handled) == 2, description: "duplicate delivered");
        Assert.Equal(1, chaos.Fires);
    }

    [Fact]
    public async Task CrashOnNth_UnwindsThroughRealSupervision_DefaultStopAsync()
    {
        Counter.Handled = 0;
        var sink = new RecordingDeadLetterSink();
        var chaos = new ChaosPlan().CrashOnNth<Counter>(n: 3);
        using var system = new ActorSystem("chaos-crash", deadLetterSink: sink, chaos: chaos);
        var actor = system.Spawn<Counter>();

        for (int i = 1; i <= 3; i++) actor.Tell(new Ping(i));

        // Default supervision is Stop: dispatches 1 and 2 succeed, the third
        // crashes before the handler runs and the real failure path stops
        // the actor and dead-letters the message with the injected cause.
        await TestActorSystem.WaitUntilAsync(() => actor.IsStopped, description: "actor stopped");
        Assert.Equal(2, Volatile.Read(ref Counter.Handled));
        Assert.Contains(sink.Records, r => r.Cause is ChaosInjectedException);
        Assert.Equal(1, chaos.Fires);
    }

    [Fact]
    public void ChaosEnabled_IsVisible_AndOffByDefault()
    {
        using var plain = new ActorSystem("no-chaos");
        Assert.False(plain.ChaosEnabled);

        using var chaotic = new ActorSystem("with-chaos", chaos: new ChaosPlan());
        Assert.True(chaotic.ChaosEnabled);
    }
}
