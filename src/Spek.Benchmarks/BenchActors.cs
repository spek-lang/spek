using Spek;
using Spek.Observability;
using Spek.Persistence;
using Spek.Runtime;

namespace Spek.Benchmarks;

// Messages — plain records (a real Spek program would declare these as
// `message` types; the runtime doesn't care, it dispatches on object).
public sealed record Ping;
public sealed record Pong;

/// <summary>Tells the receiving actor to stop itself — the poison-pill idiom.</summary>
public sealed record PoisonPill;

/// <summary>First message into a <see cref="GatedCounterActor"/>; its handler
/// parks on the benchmark-owned gate task until the backlog is pre-filled.</summary>
public sealed record HoldGate;

/// <summary>Kicks off a ping-pong volley: <paramref name="Rounds"/> round trips
/// against <paramref name="Peer"/>, completing <paramref name="Done"/> when the
/// budget is spent.</summary>
public sealed record StartVolley(ActorRef Peer, int Rounds, TaskCompletionSource Done);

/// <summary>Countdown for a parent→child spawn chain: each link decrements
/// <paramref name="Remaining"/>; the last completes <paramref name="Done"/>.</summary>
public sealed record GrowChain(int Remaining, TaskCompletionSource Done);

/// <summary>Never sent by any benchmark. Chaos rules target it so the
/// rule-scan cost is measured without a fault ever firing.</summary>
public sealed record NeverSentMessage;

/// <summary>Wire-shaped payload for the envelope-serialization benchmarks:
/// the smallest realistic cross-node message (one field).</summary>
public sealed record SmallWireMessage(string Name);

/// <summary>Wire-shaped payload for the envelope-serialization benchmarks:
/// a wider, still-flat message (eight mixed-type fields) — the shape of a
/// typical domain event crossing nodes.</summary>
public sealed record EightFieldWireMessage(
    long Id,
    string Name,
    string Region,
    int Count,
    double Score,
    bool Active,
    DateTimeOffset CreatedAt,
    string Notes);

/// <summary>Counts messages; a trivial synchronous writer handler.</summary>
public sealed class CounterActor : ActorBase
{
    public long Count;
    protected override Task DispatchAsync(object message, ActorRef sender)
    {
        _currentSender = sender;
        Count++;
        return Task.CompletedTask;
    }
}

/// <summary>Replies <see cref="Pong"/> to the sender — the ask round-trip target.</summary>
public sealed class EchoActor : ActorBase
{
    protected override Task DispatchAsync(object message, ActorRef sender)
    {
        _currentSender = sender;
        sender.Tell(new Pong());
        return Task.CompletedTask;
    }
}

/// <summary>
/// Handler that performs a SYNCHRONOUS blocking wait — the anti-pattern.
/// Each message parks the actor's thread-pool pump thread for <see cref="BlockMs"/>,
/// so concurrent siblings can't make progress until the pool injects more
/// threads (~1–2/sec). CE0083 flags this in Spek source; the benchmark
/// quantifies the cost.
/// </summary>
public sealed class BlockingActor : ActorBase
{
    public static int BlockMs = 20;
    protected override Task DispatchAsync(object message, ActorRef sender)
    {
        _currentSender = sender;
        Thread.Sleep(BlockMs);          // blocks the pool thread
        sender.Tell(new Pong());
        return Task.CompletedTask;
    }
}

/// <summary>
/// The correct shape: the same wait modeled with <c>await Task.Delay</c>, which
/// releases the pool thread so siblings run. Directly comparable to
/// <see cref="BlockingActor"/>.
/// </summary>
public sealed class AsyncWaitActor : ActorBase
{
    public static int WaitMs = 20;
    protected override async Task DispatchAsync(object message, ActorRef sender)
    {
        _currentSender = sender;
        await Task.Delay(WaitMs).ConfigureAwait(false);   // yields the thread
        sender.Tell(new Pong());
    }
}

/// <summary>Never spawned by any benchmark. Chaos crash rules target it so
/// the dispatch-path rule check runs without ever matching.</summary>
public sealed class NeverSpawnedActor : ActorBase
{
    protected override Task DispatchAsync(object message, ActorRef sender) =>
        Task.CompletedTask;
}

/// <summary>
/// Persistent counter: mutates state and writes a snapshot on every message —
/// what a Spek actor with a <c>persist</c> in its handler compiles to. The
/// snapshot round-trips through <see cref="CaptureFields"/> / <see cref="OnRestore"/>
/// so each save carries a realistic (small) field map.
/// </summary>
public sealed class PersistingCounterActor : ActorBase
{
    public long Count;
    protected override async Task DispatchAsync(object message, ActorRef sender)
    {
        _currentSender = sender;
        Count++;
        await PersistAsync().ConfigureAwait(false);
    }
    protected override IReadOnlyDictionary<string, object?> CaptureFields() =>
        new Dictionary<string, object?> { ["Count"] = Count };
    protected override void OnRestore(Snapshot s) => Count = s.Get<long>("Count");
}

/// <summary>
/// Counter that opts into passivation with a short idle window — what
/// <c>passivate after TimeSpan.FromMilliseconds(50)</c> compiles to. The runtime's
/// idle timer persists its state and drops the live instance; the next
/// message re-materialises it and <see cref="OnRestore"/> reloads the count.
/// </summary>
public sealed class PassivatingCounterActor : ActorBase
{
    public long Count;
    protected override Task DispatchAsync(object message, ActorRef sender)
    {
        _currentSender = sender;
        Count++;
        return Task.CompletedTask;
    }
    protected override TimeSpan? PassivationTimeout => TimeSpan.FromMilliseconds(50);
    protected override IReadOnlyDictionary<string, object?> CaptureFields() =>
        new Dictionary<string, object?> { ["Count"] = Count };
    protected override void OnRestore(Snapshot s) => Count = s.Get<long>("Count");
}

/// <summary>
/// One-round-trip-at-a-time reply future for the cluster benchmarks.
/// Remote asks aren't a runtime primitive yet (<c>AskAsync</c> on a remote
/// ref throws <see cref="NotSupportedException"/>), so the cross-node ask
/// benchmarks use the wire request/reply idiom instead: Tell with a
/// named-root sender, reply routed back over the transport. The benchmark
/// arms a fresh <see cref="TaskCompletionSource{T}"/> before each Tell and
/// awaits it; <see cref="ReplySinkActor"/> completes it when the reply
/// lands. Round trips are strictly sequential, so one pending cell is
/// enough. Continuations run asynchronously so the benchmark loop never
/// runs inline on the actor's pump thread.
/// </summary>
public sealed class ReplySink
{
    private TaskCompletionSource<object>? _pending;

    public Task<object> Arm()
    {
        var tcs = new TaskCompletionSource<object>(
            TaskCreationOptions.RunContinuationsAsynchronously);
        Volatile.Write(ref _pending, tcs);
        return tcs.Task;
    }

    public void Complete(object message) =>
        Volatile.Read(ref _pending)?.TrySetResult(message);
}

/// <summary>Named-root reply target for cross-node round trips: every
/// message it receives completes the shared <see cref="ReplySink"/>.</summary>
public sealed class ReplySinkActor : ActorBase
{
    private readonly ReplySink _sink;
    public ReplySinkActor(ReplySink sink) => _sink = sink;
    protected override Task DispatchAsync(object message, ActorRef sender)
    {
        _currentSender = sender;
        _sink.Complete(message);
        return Task.CompletedTask;
    }
}

/// <summary>
/// Counter whose handler throws on every <see cref="CrashEvery"/>th message
/// of each incarnation, under a <see cref="FailureDirective.Restart"/>
/// directive — the supervision failure path, driven from plain C# the way
/// the emitter wires <c>supervise OneForOne(on Failure: Restart)</c>. Set
/// <see cref="CrashEvery"/> to 0 to never crash (the baseline arm).
/// </summary>
public sealed class CrashingCounterActor : ActorBase
{
    public static int CrashEvery;   // 0 = never crash
    public long Count;
    private int _sinceRestart;
    protected override Task DispatchAsync(object message, ActorRef sender)
    {
        _currentSender = sender;
        Count++;
        if (CrashEvery > 0 && ++_sinceRestart >= CrashEvery)
            throw new InvalidOperationException("bench: injected handler crash");
        return Task.CompletedTask;
    }
    protected override FailureDirective OnFailure(Exception exception, object message)
        => FailureDirective.Restart;
}

/// <summary>
/// Counts messages until a <see cref="PoisonPill"/> arrives, then stops
/// voluntarily via <c>StopSelf</c> — the runtime's public stop surface (there
/// is no external <c>ActorRef.Stop</c>; actors stop themselves, mirroring
/// Akka's <c>Context.Stop(Self)</c>). Drives the spawn→message→stop cycle.
/// </summary>
public sealed class StopOnPoisonActor : ActorBase
{
    public long Count;
    protected override Task DispatchAsync(object message, ActorRef sender)
    {
        _currentSender = sender;
        if (message is PoisonPill) StopSelf();
        else Count++;
        return Task.CompletedTask;
    }
}

/// <summary>
/// Counter whose first message (<see cref="HoldGate"/>) awaits a
/// benchmark-owned task, suspending the slot's dispatch loop mid-drain with
/// the writer lock held. Messages enqueued behind the held gate pile into a
/// deep backlog; releasing the gate lets the loop consume it in one
/// continuous drain — the shape <c>MailboxBenchmarks.DeepDrain</c> measures.
/// </summary>
public sealed class GatedCounterActor : ActorBase
{
    private readonly Task _gate;
    public long Count;
    public GatedCounterActor(Task gate) => _gate = gate;
    protected override async Task DispatchAsync(object message, ActorRef sender)
    {
        _currentSender = sender;
        if (message is HoldGate)
        {
            await _gate.ConfigureAwait(false);   // parks the dispatch loop
            return;
        }
        Count++;
    }
}

/// <summary>
/// Round-trip initiator for the ping-pong latency benchmark. On
/// <see cref="StartVolley"/> it sends the first <see cref="Ping"/> to its
/// peer (an <see cref="EchoActor"/>) with itself as sender; every
/// <see cref="Pong"/> that comes back triggers the next Ping until the round
/// budget is spent, then completes the volley's TaskCompletionSource. Both
/// mailboxes go empty between hops, so every send pays the peer's wake path.
/// </summary>
public sealed class PingPongInitiatorActor : ActorBase
{
    private ActorRef? _peer;
    private TaskCompletionSource? _done;
    private int _remaining;
    protected override Task DispatchAsync(object message, ActorRef sender)
    {
        _currentSender = sender;
        switch (message)
        {
            case StartVolley volley:
                _peer = volley.Peer;
                _done = volley.Done;
                _remaining = volley.Rounds;
                _peer.Tell(new Ping(), _selfRef);
                break;
            case Pong:
                if (--_remaining <= 0) _done?.TrySetResult();
                else _peer?.Tell(new Ping(), _selfRef);
                break;
        }
        return Task.CompletedTask;
    }
}

/// <summary>
/// Grows a parent→child chain: each link spawns one child through the
/// protected <c>SpawnChildAsync</c> surface (what <c>spawn</c> inside a
/// handler compiles to) and forwards the countdown; the last link completes
/// the chain's task. Each hop pays child-slot wiring: factory invocation,
/// parent link, RegisterChild, TrackSlot.
/// </summary>
public sealed class ChildChainActor : ActorBase
{
    protected override Task DispatchAsync(object message, ActorRef sender)
    {
        _currentSender = sender;
        if (message is GrowChain chain)
        {
            if (chain.Remaining <= 0)
                chain.Done.TrySetResult();
            else
                SpawnChildAsync<ChildChainActor>()
                    .Tell(new GrowChain(chain.Remaining - 1, chain.Done), _selfRef);
        }
        return Task.CompletedTask;
    }
}

/// <summary>
/// Counter that completes a benchmark-owned task after processing its
/// expected message total. Used where <c>AwaitTermination</c> can't detect
/// completion — deferred messages parked in re-admission timers leave every
/// mailbox momentarily empty, so idleness is not "all messages processed."
/// </summary>
public sealed class CountdownActor : ActorBase
{
    private readonly int _expected;
    private readonly TaskCompletionSource _done;
    public long Count;
    public CountdownActor(int expected, TaskCompletionSource done)
    {
        _expected = expected;
        _done = done;
    }
    protected override Task DispatchAsync(object message, ActorRef sender)
    {
        _currentSender = sender;
        if (++Count >= _expected) _done.TrySetResult();
        return Task.CompletedTask;
    }
}

/// <summary>
/// Counter with <c>passivate after 250ms</c> armed but — unlike
/// <see cref="PassivatingCounterActor"/> — no persistence overrides, and an
/// idle window a throughput burst never reaches. In this runtime the timeout
/// arms one PERIODIC idle-check timer at materialization (period timeout/4,
/// floored at 50ms); nothing is re-armed per message, so the arm-tax
/// benchmark exists to verify the per-message delta really is ~zero.
/// </summary>
public sealed class PassivationArmedCounterActor : ActorBase
{
    public long Count;
    protected override Task DispatchAsync(object message, ActorRef sender)
    {
        _currentSender = sender;
        Count++;
        return Task.CompletedTask;
    }
    protected override TimeSpan? PassivationTimeout => TimeSpan.FromMilliseconds(250);
}

/// <summary>
/// Counter that writes one structured-log event per message in the guarded
/// shape emitted for <c>self.Log</c>: an <c>IsEnabled</c> check, then — only
/// when the level is on — a property-list allocation and the Log call. With
/// the default <see cref="NullStructuredLogger"/> the guard is the whole
/// cost; with an enabled sink every message also pays the KeyValuePair array
/// plus the boxed value.
/// </summary>
public sealed class LoggingCounterActor : ActorBase
{
    public long Count;
    protected override Task DispatchAsync(object message, ActorRef sender)
    {
        _currentSender = sender;
        Count++;
        if (Log.IsEnabled(StructuredLogLevel.Information))
            Log.Log(StructuredLogLevel.Information, "bench.counted",
                new[] { new KeyValuePair<string, object?>("count", Count) });
        return Task.CompletedTask;
    }
}

/// <summary>
/// Reads <c>self.Clock</c> once per message — the virtual-time-safe
/// timestamp: a null-coalesced property walk to the system's
/// <see cref="TimeProvider"/> plus a virtual <c>GetTimestamp</c> call.
/// </summary>
public sealed class ClockReadCounterActor : ActorBase
{
    public long LastTimestamp;
    protected override Task DispatchAsync(object message, ActorRef sender)
    {
        _currentSender = sender;
        LastTimestamp = Clock.GetTimestamp();
        return Task.CompletedTask;
    }
}

/// <summary>
/// Reference twin of <see cref="ClockReadCounterActor"/>: the same handler
/// reading <see cref="Environment.TickCount64"/> — the cheapest monotonic
/// read the BCL offers — so the delta prices the Clock abstraction itself.
/// </summary>
public sealed class TickCountCounterActor : ActorBase
{
    public long LastTimestamp;
    protected override Task DispatchAsync(object message, ActorRef sender)
    {
        _currentSender = sender;
        LastTimestamp = Environment.TickCount64;
        return Task.CompletedTask;
    }
}
