using Spek;
using Spek.Runtime;
using Spek.Testing;
using Xunit;

namespace Spek.Tests.Runtime;

/// <summary>
/// Trace replay (D2): the flight recorder journals ingress only (bounded,
/// serializable, reported gaps), and a dumped trace re-executes under the
/// deterministic simulator — production incident, local repro.
/// </summary>
public sealed class TraceReplayTests
{
    private sealed record Add(int N);
    private sealed record GetSum();
    private sealed record Sum(int Value);
    private sealed record Opaque(Func<int> NotSerializable);

    private sealed class SumActor : ActorBase
    {
        private int _sum;
        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            switch (message)
            {
                case Add a: _sum += a.N; break;
                case GetSum: sender.Tell(new Sum(_sum), _selfRef!); break;
            }
            return Task.CompletedTask;
        }
    }

    private sealed class Forwarder : ActorBase
    {
        private readonly ActorRef _target;
        public Forwarder(ActorRef target) => _target = target;
        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            _target.Tell(message, _selfRef!);   // actor-to-actor: not journaled
            return Task.CompletedTask;
        }
    }

    [Fact]
    public async Task Recorder_JournalsHostIngressOnly_NotInternalTrafficAsync()
    {
        var recorder = new FlightRecorder();
        using var system = new ActorSystem("trace-ingress", trace: recorder);
        var sum = system.Spawn<SumActor>();
        var fwd = system.Spawn<Forwarder>(sum);

        fwd.Tell(new Add(5));                       // ingress → journaled (target: Forwarder)
        await TestActorSystem.WaitUntilAsync(
            () => recorder.Snapshot().Events.Length >= 1, description: "ingress recorded");
        await Task.Delay(100);                      // let the internal forward happen

        var events = recorder.Snapshot().Events;
        var e = Assert.Single(events);              // the forwarded copy was NOT journaled
        Assert.Equal("Forwarder", e.Target);
        Assert.Contains("Add", e.MessageType);
    }

    [Fact]
    public void Recorder_IsARingBuffer_KeepingTheLastWindow()
    {
        var recorder = new FlightRecorder(capacity: 3);
        using var system = new ActorSystem("trace-ring", trace: recorder);
        var sum = system.Spawn<SumActor>();

        for (int i = 1; i <= 5; i++) sum.Tell(new Add(i));

        var events = recorder.Snapshot().Events;
        Assert.Equal(3, events.Length);
        Assert.Equal(3, events[0].Seq);             // oldest kept is #3 of 5
        Assert.Equal(5, events[^1].Seq);
    }

    [Fact]
    public void UnserializablePayloads_AreReported_NeverSilentlyDropped()
    {
        var recorder = new FlightRecorder();
        using var system = new ActorSystem("trace-unser", trace: recorder);
        var sum = system.Spawn<SumActor>();

        sum.Tell(new Opaque(() => 1));

        Assert.Contains(recorder.UnserializableTypes, t => t.Contains("Opaque"));
        Assert.Empty(recorder.Snapshot().Events);
    }

    [Fact]
    public async Task CapturedTrace_ReplaysIntoTheSimulator_SameOutcomeAsync()
    {
        // Capture: a live (parallel) system takes four deposits.
        var recorder = new FlightRecorder();
        using (var live = new ActorSystem("trace-capture", trace: recorder))
        {
            var sum = live.Spawn<SumActor>();
            foreach (var n in new[] { 1, 2, 3, 4 }) sum.Tell(new Add(n));
            await TestActorSystem.WaitUntilAsync(
                () => recorder.Snapshot().Events.Length == 4, description: "all ingress recorded");
        }

        var path = Path.Combine(Path.GetTempPath(), $"spek-trace-{Guid.NewGuid():N}.trace");
        recorder.Dump(path);
        try
        {
            // Replay: same topology, deterministic scheduler, recorded inputs.
            var trace = SpekTrace.Load(path);
            using var sim = new SimulatedActorSystem(seed: 11);
            var sum = sim.Spawn<SumActor>();
            sim.ReplayIngress(trace);

            Assert.Equal(10, sim.Ask<Sum>(sum, new GetSum()).Value);
        }
        finally { File.Delete(path); }
    }

    [Fact]
    public void FingerprintMismatch_RefusesUnlessDeliberate()
    {
        var recorder = new FlightRecorder();
        using (var live = new ActorSystem("trace-fp", trace: recorder))
        {
            live.Spawn<SumActor>().Tell(new Add(1));
            SpinWait.SpinUntil(() => recorder.Snapshot().Events.Length == 1, 5000);
        }

        var doctored = recorder.Snapshot() with { Fingerprint = "otherapp/9.9 spek/0.0" };

        using var sim = new SimulatedActorSystem(seed: 1);
        var sum = sim.Spawn<SumActor>();

        var ex = Assert.Throws<InvalidOperationException>(() => sim.ReplayIngress(doctored));
        Assert.Contains("fingerprint", ex.Message, StringComparison.OrdinalIgnoreCase);

        sim.ReplayIngress(doctored, allowFingerprintMismatch: true);   // the deliberate path
        Assert.Equal(1, sim.Ask<Sum>(sum, new GetSum()).Value);
    }

    [Fact]
    public void UnknownTarget_ThrowsWithTopologyGuidance()
    {
        var recorder = new FlightRecorder();
        using (var live = new ActorSystem("trace-topo", trace: recorder))
        {
            live.Spawn<SumActor>().Tell(new Add(1));
            SpinWait.SpinUntil(() => recorder.Snapshot().Events.Length == 1, 5000);
        }

        using var sim = new SimulatedActorSystem(seed: 1);   // nothing spawned
        var ex = Assert.Throws<InvalidOperationException>(
            () => sim.ReplayIngress(recorder.Snapshot()));
        Assert.Contains("topology", ex.Message);
    }
}
