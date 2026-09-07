---
title: Runtime
layout: default
parent: Reference
nav_order: 3
permalink: /reference/runtime/
---

# Runtime reference

The core of the Spek runtime lives in two NuGet packages: `Spek.Runtime`
for the scheduler, mailbox, supervision, and persistence primitives; and
`Spek.Testing` for actor-aware xUnit helpers.

This page is the pragmatic overview. For the fully documented surface,
browse the C# source in `src/Spek.Runtime/` and `src/Spek.Testing/`, where
every public type carries XML doc comments.

## `ActorSystem`

The root of an actor hierarchy. You construct one per application (or one
per test, in `TestActorSystem` form).

```csharp
var system = new ActorSystem("HelloBank");
```

The default constructor wires up an `InMemorySnapshotStore` and a
`ConsoleDeadLetterSink`. Pass explicit implementations to swap either:

```csharp
var system = new ActorSystem(
    name:           "prod",
    snapshotStore:  new MyDatabaseSnapshotStore(connectionString),
    deadLetterSink: new MyMetricsDeadLetterSink());
```

### Spawning

Two spawn entry points, plus equivalents on `TestActorSystem`.

```csharp
// Session-scoped — no persistence key, persist; is a no-op.
ActorRef root = system.Spawn<Root>();

// Persistent — bound to the key. If the store already has a snapshot for
// this key, on Restore fires before the first message is delivered.
ActorRef account = system.SpawnPersistent<Account>(
    persistenceKey: "account-Alice",
    "Alice");   // constructor args for init(string ownerName)
```

Both return an `ActorRef` that is safe to share. The underlying actor
instance can be swapped by the runtime (during a `Restart`) without
invalidating the ref.

Each has an async variant, `SpawnAsync<T>(args)` and
`SpawnPersistentAsync<T>(key, args)`, that returns `Task<ActorRef>`. The
difference matters for persistent spawns: the synchronous form blocks while
the snapshot loads from the store, the async form doesn't hold the calling
thread. Inside Spek source the distinction disappears (invisible async awaits
either), so the async variants exist for C# hosts that spawn actors from
async code paths. Non-generic overloads taking a `Type` serve reflection-driven
hosts.

### Waiting for work to finish

```csharp
root.Tell(new Start());
system.AwaitTermination();
```

`AwaitTermination()` blocks until every tracked actor is idle (mailbox
empty, not currently processing). It is used at the end of a `program Main`
and in tests where you want to let the pipeline drain.

`GracefulShutdown(timeout?)` is the one-call form: it drains
(`AwaitTermination`) and then disposes the system, so actors release their
resources and shared regions run their `term { }` blocks in reverse
construction order. It returns `false` if the timeout elapsed before the
system went idle (shutdown still proceeds), and skips the wait entirely on
a system that never spawned an actor.

```csharp
root.Tell(new Start());
system.GracefulShutdown(TimeSpan.FromSeconds(30));
```

### Deterministic waits in tests

`TestActorSystem` (in `Spek.Testing`) adds helpers so tests wait on an
*observable condition* instead of a guessed `Thread.Sleep`:

```csharp
using var system = new TestActorSystem("t");
var actor = system.Spawn<Worker>();
actor.Tell(new Crunch());

await system.WhenIdleAsync();                   // async: releases the thread

await TestActorSystem.WaitUntilAsync(           // poll an arbitrary condition
    () => probe.Received.Count == 3,
    description: "probe got all three replies");
```

Prefer the async `WhenIdleAsync` / `WaitUntilAsync` (they poll without parking
a thread) over the blocking `WaitForIdle`: under fully-parallel test
execution a parked thread competes with the actors that need it. With async
waits the whole suite runs **fully in parallel** (no serialized collections,
no thread cap). The only accommodations are `TestThreadPoolConfig` raising the
thread-pool floor and generous timeouts on the few tests that verify
time-based behavior (passivation timers, shutdown grace), so a CPU-saturation
burst delays them rather than failing them.

### Slow-handler watchdog

Every system watches its own handlers. When one has been running longer than
`ActorSystem.SlowHandlerThreshold` (default: 30 seconds), the runtime writes
a single entry to the dead-letter sink naming the actor and how long the
handler has been running, and increments the `spek.actor.handler.slow`
counter. Detection is the whole feature: the runtime never cancels or kills
the suspect handler, because a watchdog that guesses wrong about "stuck"
would corrupt exactly the state it means to protect. Each occurrence is
reported once, so a handler wedged forever produces one entry rather than
one per sweep.

The threshold is wall-clock time. A wedge is a real-time phenomenon, so a
virtual-time test that advances the manual clock by hours does not trip it,
and detection is suspended while a debugger is attached (a breakpoint is not
a wedge). The default is deliberately generous because the target is
handlers stuck forever: awaiting a reply that cannot come, blocked on a
dead external resource. It is not for merely slow work. Set the property to `null`
to disable detection entirely.

```csharp
system.SlowHandlerThreshold = TimeSpan.FromSeconds(5);
system.SlowHandlerThreshold = null;                      // detection off
```

## `ActorRef`

A stable handle to an actor. `ActorRef` is untyped
([CE0030](/reference/errors/#ce0030)).

```csharp
public sealed class ActorRef
{
    public void         Tell(object message);
    public ValueTask<T> AskAsync<T>(object message);
    public ValueTask<T> AskAsync<T>(object message, TimeSpan timeout);
    public bool         IsStopped { get; }
}
```

An ask fails fast on every terminal delivery failure: a target that is
already stopped, a handler that throws, a message no behavior handles, and
a handler that returns without replying all fault the asker immediately
with `AskException` (the handler's own exception rides along as
`InnerException` when there is one). An unbounded ask therefore cannot
hang on any of those; the one case it cannot detect is a handler that
never returns at all. The timeout overload covers that case, faulting
with `TimeoutException` when the deadline passes with the handler still
running.

`Tell` enqueues a message and returns immediately. `AskAsync<T>` is the
low-level target of Spek's `.Ask(...)` sugar. You rarely write it yourself
from Spek source; the compiler emits the `await ... AskAsync<T>(...)` call
for you.

### Bridging .NET events: `Forward<T>()`

`Forward<T>()` returns an `EventHandler<T>` that `Tell`s each event payload to
the actor: the one-line bridge from a .NET event source into a mailbox:

```csharp
watcher.Changed += actorRef.Forward<FileSystemEventArgs>();
```

Each call returns a distinct delegate instance, so to unsubscribe later, keep
the handler in a variable and `-=` that same instance. The payload arrives
like any other message; pair it with a [`private`
handler](/language/messaging/) when the event-args type isn't a declared
`message`.

## `ISnapshotStore` {#isnapshotstore}

The interface for durable persistence back-ends.

```csharp
public interface ISnapshotStore
{
    Task SaveAsync(string key, Snapshot snapshot);
    Task<Snapshot?> LoadAsync(string key);
}
```

Keys identify a persistent actor instance across restarts. The key is
exactly the string passed to `SpawnPersistent<T>(key, …)`, used verbatim
for both save and load; choose keys that are unique per logical actor.

- `InMemorySnapshotStore`: process-local, resets when the process exits.
  The default if no store is passed to `ActorSystem`.
- *(Your own.)* Implement the interface for file, database, or remote
  storage. The interface is deliberately small so rolling your own is a
  short exercise.

## `FailureDirective` {#failuredirective}

See [supervision](/language/supervision/) for the conceptual overview.

```csharp
public enum FailureDirective
{
    Resume,     // skip this message, state unchanged
    Restart,    // rebuild the actor, reload snapshot, continue
    Escalate,   // hand the decision to the parent's OnChildFailure
    Stop,       // drain to dead letter, refuse further sends (default)
}
```

Who decides depends on where the actor sits. A **root** actor decides for
itself: override `ActorBase.OnFailure(Exception, object message)`; the
default is `Stop`. A **child** actor's failure escalates to its parent,
whose `OnChildFailure(ActorRef child, Exception cause, object message)`
returns the directive; a chain of escalating parents is walked upward,
and escalation that reaches the top degrades to `Stop`. A `Restart` that
exceeds the actor's configured restart budget also degrades to `Stop`.

## `Outcome<T, E>` and `Outcome<T>` {#outcome}

A success-or-failure result type for replies that can fail for *expected*
reasons (validation, not-found, insufficient-funds) where throwing (and
triggering supervision) would be the wrong tool. An `Outcome` is an immutable
record, so it rides in `message` fields and `return` replies.

```csharp
public abstract record Outcome<T, E>
{
    public static Outcome<T, E> Success(T value);
    public static Outcome<T, E> Failure(E reason);

    public bool IsSuccess { get; }
    public bool IsFailure { get; }
    public bool TryGetValue(out T value);
    public bool TryGetReason(out E reason);

    public Outcome<TNew, E>  Map<TNew>(Func<T, TNew> map);
    public Outcome<T, ENew>  MapFailure<ENew>(Func<E, ENew> map);
}
```

The failure type `E` is yours; an enum of domain errors reads best. For the
common "code plus message" case, the one-type-parameter shorthand
`Outcome<T>` fixes `E` to the built-in `Reason(string Code, string Message)`
record. The two-parameter form pattern-matches directly on its success and
failure cases; the `Outcome<T>` shorthand is a thin wrapper, so match
through its `Underlying` property:

```spek
on Withdrawn w =>
{
    var text = w.result.Underlying switch
    {
        Outcome<decimal, Reason>.SuccessCase s => $"new balance {s.Value}",
        Outcome<decimal, Reason>.FailureCase f => $"declined: {f.Reason.Code}",
    };
}
```

Reserve exceptions (and [supervision](/language/supervision/)) for the
*unexpected*: bugs, I/O faults, violated invariants. Expected, in-protocol
failure is data, and `Outcome` is its shape.

## Logical clocks: `LamportClock` and `VectorClock` {#logical-clocks}

Building blocks for causal ordering in distributed or event-sourced designs.
Both are immutable records in the `Spek` namespace, safe in `message` fields;
neither is wired into the runtime's own delivery (which stays
at-most-once with per-pair ordering; see
[clustering](/language/clustering/)). They exist for *application-level*
protocols that need happens-before reasoning.

```csharp
public readonly record struct LamportClock(long Counter)
{
    public static readonly LamportClock Zero;
    public LamportClock Tick();                        // local event
    public LamportClock Receive(LamportClock received); // merge + tick
    public bool HappensBefore(LamportClock other);
}

public sealed record VectorClock(ImmutableDictionary<Guid, long> Entries)
{
    public static readonly VectorClock Empty;
    public VectorClock Tick(Guid localNodeId);
    public VectorClock Receive(VectorClock received, Guid localNodeId);
    public VectorClock Merge(VectorClock other);
    public bool HappensBefore(VectorClock other);
    public bool IsConcurrentWith(VectorClock other);   // parallel edits
}
```

A `LamportClock` gives a single total-orderable counter: cheap, but it can't
distinguish concurrency from ordering. A `VectorClock` tracks one counter per
node, so it can also answer "did these two updates race?"
(`IsConcurrentWith`) at the cost of one dictionary entry per participant.
Stamp outgoing messages with `Tick()`, fold incoming stamps with
`Receive(...)`, and compare with `HappensBefore`.

## `IDeadLetterSink` {#ideadlettersink}

Where unhandled messages, messages to stopped actors, and failure-related
drops are routed.

```csharp
public interface IDeadLetterSink
{
    void DeadLetter(object message, string reason, Exception? cause);
}
```

`cause` is non-null when the failure was an exception from the handler,
and null when the message was unhandled or the target actor had
already stopped.

Shipped implementations:

- `ConsoleDeadLetterSink`: the default, logs one line per event to stderr.
- `RecordingDeadLetterSink`: captures in-memory for test assertions.
  Exposes a `Records` list of `DeadLetterRecord(Message, Reason, Cause)`
  snapshots in arrival order.
- Your own.

## `IngressPolicy` {#ingresspolicy}

A gate the runtime consults before a message reaches its handler. Attach
one or more to any `ActorRef`. The dispatch loop evaluates the chain in
attachment order, and the first non-Allow decision wins. Attaching to a
remote ref is a no-op: apply ingress policies on the actor's home node,
not on the caller's side.

```csharp
public abstract class IngressPolicy
{
    public abstract ValueTask<PolicyDecision> EvaluateAsync(
        ResilienceContext context,
        CancellationToken cancellationToken = default);
}
```

The base class and `PolicyDecision` live in `Spek.Resilience.Abstractions`.
The `ResilienceContext` argument carries the actor path, the message type
name, the message itself, and a timestamp. Implementations are typically
stateful (token buckets, sliding windows) and must be thread-safe, because
the runtime calls `EvaluateAsync` from arbitrary dispatcher threads.

Ready-made rate limiters built on `System.Threading.RateLimiting` ship in
`Spek.Resilience.RateLimiting`: `RateLimitIngressPolicy.TokenBucket(...)`,
`.FixedWindow(...)`, `.SlidingWindow(...)`, and `.Concurrency(...)`, plus
`PartitionedRateLimitIngressPolicy<TKey>` for per-key (per-tenant,
per-sender) partitioning.

```csharp
var actor = system.Spawn<Teller>();
actor.AttachIngressPolicy(RateLimitIngressPolicy.TokenBucket(
    permitsPerSecond: 100,
    burstCapacity:    200));
```

### Admission decisions: Allow, Reject, Defer {#policydecision}

A policy renders its verdict through the three `PolicyDecision` factories.
`Allow()` dispatches the message normally. `Reject(reason)` is terminal:
the runtime dead-letters the message immediately, and the reason arrives
at the sink as `rejected by ingress policy: {reason}`. Choose Reject when
retrying later cannot help, or when ordering matters more than throughput.

`Defer(retryAfter, reason)` is delayed re-admission, not a politer
rejection. The runtime re-enqueues the message after `retryAfter`,
scheduling the delay on the actor system's clock (`ActorSystem.Clock`), so
deferral is deterministic under a virtual-time test clock: advance the
clock and the parked message re-enters. The budget is bounded. Each
message instance gets at most three defer attempts, tracked on the object
itself so the count follows it through re-admission; when a policy defers
the same instance a fourth time, the runtime gives up and dead-letters it
with `defer budget exhausted after 3 attempts: {reason}`.

A re-admitted message enters at the tail of the mailbox and runs the full
policy chain again, which means arrival order across a deferral is not
preserved. The message yielded its slot, and anything admitted during the
park window overtakes it. That is the honest cost of deferral: Defer
trades ordering for smoothing. Senders notice none of this. `Tell` keeps its fire-and-forget contract, and an `Ask` that would have timed out
against a hard drop instead gets a real chance to complete.

The shipped rate limiters pick the verdict from limiter metadata: a denial
carrying a `RetryAfter` hint (the windowed shapes: token bucket, fixed
window, sliding window) becomes Defer, while a denial with no scheduled
replenishment (`Concurrency`) becomes Reject. Under a live refill, nothing
dead-letters:

```csharp
// Capacity 3, refill 1/sec. A burst of 5 admits 3 immediately and
// defers 2; as tokens refill, the deferred pair is re-admitted, so
// all 5 are handled and the dead-letter sink stays empty.
actor.AttachIngressPolicy(RateLimitIngressPolicy.TokenBucket(
    permitsPerSecond: 1,
    burstCapacity:    3));

for (int i = 0; i < 5; i++)
    actor.Tell(new Ping());
```

## Inbox observers: `ActorSystem.Observe` {#inbox-observers}

An inbox observer is tcpdump for a mailbox: a passive tap on one local
actor's message stream, attached at runtime with no recompile and no
redeploy. Passivity is by construction rather than by discipline: Spek
messages are immutable records, so holding an observed reference cannot
perturb the actor, reply on its behalf, or reorder delivery.

```csharp
public InboxObserverHandle Observe(
    ActorRef actor, Action<ObservedMessage> onMessage, int bufferCapacity = 1024);

public sealed record ObservedMessage(
    object Message,             // the actual message instance
    ActorRef? Sender,           // null for host- and timer-originated sends
    DateTimeOffset EnqueuedAt); // mailbox arrival, on the system clock
```

The callback never runs on the dispatch path. Observed events queue into a
bounded buffer consumed by a background pump; the actor's enqueue side only
ever performs a non-blocking write. When the observer falls behind, the
overflow is dropped and counted in the handle's `Dropped` property, because
a diagnostic tap that can stall a production actor is a worse defect than a
gap in diagnostics. The same isolation covers bugs in the callback itself:
an observer that throws has the exception routed to the dead-letter sink
(reason: `inbox observer threw`), and the actor and its supervisor never
see it.

```csharp
using var tap = system.Observe(teller, m =>
    Console.WriteLine($"{m.EnqueuedAt:HH:mm:ss.fff} {m.Message.GetType().Name}"));
```

Dispose the handle to detach. Detaching is graceful: events already
buffered still drain to the callback before the pump exits. Observers are local-node only. A tap on a remote ref would observe nothing, so `Observe` throws
`InvalidOperationException` instead; attach on the actor's home node.

For tests, `Spek.Testing` ships `RecordingObserver`, the tap-side mirror of
`RecordingDeadLetterSink`: pass its `OnMessage` callback to `Observe`, then
assert on `Messages` and `Count` without instrumenting the actor under
test.

```csharp
var recorder = new RecordingObserver();
using var tap = system.Observe(actor, recorder.OnMessage);

actor.Tell(new Ping(1));
await TestActorSystem.WaitUntilAsync(
    () => recorder.Count == 1, description: "message observed");

Assert.Null(recorder.Messages[0].Sender);   // host sends carry no sender
Assert.Equal(0, tap.Dropped);
```

`EnqueuedAt` reads the system clock, so under a virtual-time
`TestActorSystem` it reports the manual clock's frozen instant exactly.

## Live introspection: `SnapshotActors` and `spekc observe` {#live-introspection}

Live introspection answers "what is this system doing right now," asked of
a running process. Two surfaces serve the question: an in-process API for
dashboards and tests, and an out-of-process attach for production.

```csharp
public IReadOnlyList<ActorSnapshot> SnapshotActors();

public sealed record ActorSnapshot(
    string Path,               // stable display identity
    string ActorType,          // "(unmaterialized)" when passivated
    string? Behavior,          // active behavior name; null when unmaterialized
    int MailboxDepth,          // messages pending right now
    string[] MailboxHead,      // type names of the first pending messages (up to 8)
    int Restarts,              // supervision restarts observed
    string? LastMessageType,   // most recent dispatch; null before the first
    DateTimeOffset SpawnedAt,  // on the system clock
    bool IsMaterialized,       // false when passivated
    bool IsStopped,
    string[] Children);        // child display identities, for tree rendering
```

Sampling is non-perturbing: counters and cheap queue reads only, with no
mailbox locks taken and no messages injected. The view is metadata only. Actor field contents are
deliberately absent, because a state dump leaks whatever the actor happens
to hold and needs a redaction story first; until that story exists, the
runtime refuses to be the leak.

`Path` is a stable display identity: the persistence key when the actor has
one, otherwise the type name for the first anonymous instance and
`TypeName#N` for later ones, assigned in spawn order. Stability is what
lets successive samples correlate, and the same identities address replay
targets in [flight-recorder traces](#flight-recorder).

The out-of-process transport is an `EventSource` named `Spek-Introspection`.
While at least one EventPipe session is attached, a pump emits one
`ActorTable` event per system per second carrying the JSON-serialized
snapshot table; while nobody is attached, the entire cost is one weak
reference per system. The channel is the .NET diagnostics IPC transport
that `dotnet-counters` rides: no listening port opens, and the attach
boundary is the OS user: anyone who can attach could already read the
process's memory.

`spekc observe <pid>` is the reader:

```text
$ spekc observe 41283
[bank]
ACTOR                    BEHAVIOR      MAILBOX  RESTARTS  LAST MSG
ledger-eu-1              Open                0         0  Deposit
Teller                   Default            12         1  Withdraw
Teller#2                 -                   0         0  Ping (passivated)
```

Pass `--actor <path>` to drill into a single actor (behavior, mailbox depth
with head types, restarts, children, uptime), and `--once` to print one
sample and exit instead of refreshing until Ctrl-C.

Read together, `MailboxDepth`, `LastMessageType`, and `MailboxHead` are the
wedge diagnosis: a stuck actor shows a growing depth, `LastMessageType`
names the message it is stuck on, and `MailboxHead` names the traffic
piling up behind it.

## The flight recorder: `FlightRecorder` and trace replay {#flight-recorder}

`FlightRecorder` is a bounded ring buffer at the runtime's ingress
boundary: it journals the messages that enter the system from outside the
actor world (host sends and channel adapters, recognized as enqueues that
carry no actor sender). Ingress is enough because execution is
deterministic: pure Spek code has no raw concurrency
([CE0119](/reference/errors/#ce0119)), so re-executing the program against
the recorded inputs re-derives every internal message. Journaling
actor-to-actor traffic would persist what replay can recompute, and that
economy is what keeps the recorder cheap enough to stay attached in
production.

```csharp
public sealed class FlightRecorder
{
    public FlightRecorder(int capacity = 10_000);

    public IReadOnlyCollection<string> UnserializableTypes { get; }
    public SpekTrace Snapshot();      // the buffered window, oldest first
    public void Dump(string path);    // the window, as indented JSON
}

public sealed record TraceEvent(
    long Seq, string Target, string MessageType, string PayloadJson);

public sealed record SpekTrace(string Fingerprint, TraceEvent[] Events)
{
    public static SpekTrace Load(string path);
}
```

Attach at system construction, dump on incident:

```csharp
var recorder = new FlightRecorder(capacity: 50_000);
using var system = new ActorSystem("prod", trace: recorder);
// ... the incident happens ...
recorder.Dump("incident-4411.trace");
```

The ring keeps the last `capacity` events, so a dump is the window leading
up to the incident rather than an unbounded log. Payloads are
JSON-serialized at capture, the same contract clustering already imposes on
remote messages. A message type that fails to serialize is recorded as a
gap and reported in `UnserializableTypes`, so a
hole in the journal is visible before conclusions get built on top of it.

### Replay: `SimulatedActorSystem.ReplayIngress`

```csharp
public void ReplayIngress(SpekTrace trace, bool allowFingerprintMismatch = false);
```

`SimulatedActorSystem` (in `Spek.Testing`) runs an actor system under a
seeded deterministic scheduler and a manual clock; `ReplayIngress` feeds a
recorded trace's inputs into it, turning a production incident into a
local repro:

```csharp
var trace = SpekTrace.Load("incident-4411.trace");

using var sim = new SimulatedActorSystem(seed: 11);
var ledger = sim.Spawn<Ledger>();   // re-create the recorded topology,
                                    // in the recorded spawn order
sim.ReplayIngress(trace);

var balance = sim.Ask<Balance>(ledger, new GetBalance());
```

Events feed in recorded arrival order, and each drains fully before the
next enters: recorded order is a causal boundary, while scheduling inside a
drain belongs to this run's seed. Targets are matched by display identity,
the same `Path` that [introspection](#live-introspection) shows, so the
host must first re-create the recorded topology. An event addressed to an actor the simulation has not spawned throws with that guidance.

Every trace is pinned to a build fingerprint (entry-assembly name and
version plus the Spek runtime version), and replaying against a different
build throws unless the caller passes `allowFingerprintMismatch: true`. The
escape hatch is deliberate: replaying an incident's inputs against a
candidate fix is precisely how the fix gets validated, so a fingerprint
mismatch is a warning surface, not a refusal.

One caution outlasts the mechanics: traces contain real payloads. Treat a
trace file as production data, subject to the same access and retention
rules as a database dump, not as a build artifact to attach to a ticket.

## Chaos plans: `ChaosPlan` {#chaos-plans}

A `ChaosPlan` injects faults at the runtime's own enqueue and dispatch
choke points, per actor and per message type, so a resilience claim is
tested on the same enqueue and dispatch path production uses.

```csharp
public sealed class ChaosPlan
{
    public long Fires { get; }   // total faults injected, across all rules

    public ChaosPlan CrashOnNth<TActor>(int n) where TActor : ActorBase;
    public ChaosPlan CrashOnNth(ActorRef actor, int n);
    public ChaosPlan Drop<TMessage>(int every = 1);
    public ChaosPlan Drop(ActorRef to, int every = 1);
    public ChaosPlan Delay<TActor>(TimeSpan by) where TActor : ActorBase;
    public ChaosPlan Delay(ActorRef to, TimeSpan by);
    public ChaosPlan Duplicate<TMessage>(int every = 1);
    public ChaosPlan Duplicate(ActorRef to, int every = 1);
}
```

A plan attaches only at system construction, and a chaos-enabled system
announces itself on stderr the moment it starts:

```csharp
var chaos = new ChaosPlan().Drop<Ping>(every: 3);
using var system = new ActorSystem("soak", chaos: chaos);
```

```text
[spek] CHAOS ENABLED on system 'soak' — fault injection is active. This
configuration must never reach production.
```

Both constraints serve one goal: a fault plan must be impossible to leave
enabled by accident. There is no ambient static to flip and no
configuration-file backdoor to forget, so enabling chaos takes a code
change at the composition root, and the announcement makes an accidental
deploy visible in the first lines of the log. Rules may still be added to
an attached plan while the system runs; that is how a soak harness flips
knobs live. Only the attachment itself is fixed. `ActorSystem.ChaosEnabled`
reports the state programmatically, `Fires` counts every injected fault so
a test can assert the chaos actually happened, and ref-targeted rules match
local actors only.

### Fault semantics: what each fault models

`Drop` models delivery loss: the message never enqueues. It is counted in
`Fires` but not dead-lettered, and the distinction is deliberate. A dead
letter is the runtime keeping its promise ("I could not deliver this, and I
am telling you"); a drop is the modeled fault itself, and a lossy network
files no report. If drops appeared in the dead-letter sink, every recovery
strategy built on watching dead letters would pass chaos tests it deserves
to fail.

`Delay` re-enqueues the message after the given time on the system clock,
so it is deterministic under virtual time: advance the clock and the held
message lands. A delay also reorders the message past its successors, which
is today's only reordering knob. Systematic reordering is the simulator's
job (seeded mailbox selection explores schedules methodically), not a fault
a plan injects.

`Duplicate` enqueues a second copy of the matched message, the direct test
of handler idempotency.

`CrashOnNth` throws `ChaosInjectedException` at the target's nth dispatch,
before the handler runs, and the exception unwinds through the real
supervision machinery. Nothing on the recovery side is simulated, so the
recovery a chaos test certifies is the recovery production runs:

```csharp
var sink = new RecordingDeadLetterSink();
var chaos = new ChaosPlan().CrashOnNth<Teller>(n: 3);
using var system = new ActorSystem("drill", deadLetterSink: sink, chaos: chaos);
var teller = system.Spawn<Teller>();

for (int i = 1; i <= 3; i++) teller.Tell(new Ping(i));

// Default supervision is Stop: dispatches 1 and 2 succeed, the third
// crashes before its handler runs, the actor stops through the real
// failure path, and the message dead-letters with the injected
// ChaosInjectedException as its cause.
```

Pass the same plan to `new SimulatedActorSystem(seed, chaos)` and fault
ordering follows arrival ordering, so a chaos run reproduces exactly from
its seed.

## Testing

`Spek.Testing` is a C# library that depends on `xunit.assert` (the
assertion library alone, not the runner). Tests written in C# use xUnit's
`[Fact]` and `Assert.*` as normal, plus the Testing types below for actor
wiring. Tests written in Spek itself use the `…Tests` naming convention
instead, where each public method of a `…Tests` module or class is a test
under `dotnet test`. [Testing actors](/language/testing/) covers that
story end to end, and it rides on the same types documented here.

### `TestActorSystem`

```csharp
public sealed class TestActorSystem : IDisposable
{
    public TestActorSystem(
        string name = "test",
        ISnapshotStore? snapshotStore = null,
        IDeadLetterSink? deadLetterSink = null,
        bool virtualTime = false,
        ChaosPlan? chaos = null);

    public ActorRef Spawn<TActor>(params object[] args);
    public ActorRef SpawnPersistent<TActor>(string key, params object[] args);
    public TestProbe CreateProbe();
}
```

A thin wrapper over `ActorSystem` that adds `CreateProbe()` for stand-in
actors. Passing `virtualTime: true` installs a manual clock, exposed as
`Clock`, that timers and `passivate after` windows follow instead of wall
time; `chaos` attaches a [chaos plan](#chaos-plans) at construction, the
only point where one can attach. Dispose at the end of each test to
release resources.

### `TestProbe`

A stand-in actor. Hand its `.Ref` to code under test wherever an
`ActorRef` is expected, and use the assertion helpers to verify what it
received.

```csharp
public sealed class TestProbe
{
    public ActorRef Ref { get; }

    public void Send(ActorRef target, object message);

    public T    ExpectMsg<T>(TimeSpan? timeout = null);
    public T    ExpectMsg<T>(Func<T,bool> predicate, TimeSpan? timeout = null);
    public void ExpectNoMsg(TimeSpan? within = null);
}
```

Failures throw `Xunit.Sdk.XunitException`, so xUnit's test explorer
renders them as normal test failures with stack traces.

### Example test

```csharp
using Spek.Testing;
using Xunit;

public class EchoTests
{
    [Fact]
    public void Replies_with_pong_to_ping()
    {
        using var sys = new TestActorSystem();
        var probe = sys.CreateProbe();
        var echo  = sys.Spawn<Echo>();

        probe.Send(echo, new Ping());

        probe.ExpectMsg<Pong>();
    }

    [Fact]
    public async Task Does_nothing_for_unknown_messagesAsync()
    {
        var sink = new RecordingDeadLetterSink();
        using var sys = new TestActorSystem(deadLetterSink: sink);
        var echo = sys.Spawn<Echo>();

        echo.Tell(new SomeUnknownMessage());

        await TestActorSystem.WaitUntilAsync(
            () => sink.Records.Count > 0, description: "dead letter recorded");
        Assert.Contains(sink.Records, r => r.Message is SomeUnknownMessage);
    }
}
```

The pattern mirrors `Akka.TestKit` deliberately: it is a proven .NET
convention with low language-design overhead.

## Spek-to-C# mappings {#spek-to-c-mappings}

Handy if you're reading emitted `.g.cs` or writing code that straddles
both languages:

| Spek construct                     | C# output                                                   |
|------------------------------------|-------------------------------------------------------------|
| `message Foo(T x)`                 | `public record Foo(T x);`                                   |
| `actor Foo`                        | `internal sealed class Foo : ActorBase`                 |
| `public actor Foo`                 | `public sealed class Foo : ActorBase`                   |
| `abstract actor Foo`               | `public abstract class Foo : ActorBase`                 |
| `behavior NormalOperation { ... }` | Method group / delegate field on the class                  |
| `become NormalOperation`           | `_behavior = NormalOperation_HandleAsync;`                  |
| `on Deposit d => { ... }`          | Case arm in a type-switch dispatched by the mailbox loop    |
| `init(args) { ... }`               | Constructor wired to the runtime spawn lifecycle            |
| `target.Tell(msg)` (inside an actor) | `target.Tell(msg, _selfRef);`                             |
| `target.Ask(new Foo(args))`             | `await target.AskAsync<FooResponse>(new Foo(args))`         |
| `spawn<Foo>(args)`                 | `SpawnChildAsync<Foo>(args)`                                |
| `persist;`                         | `await PersistAsync();`                                     |
| `passivate after System.TimeSpan.FromMinutes(30)`       | Runtime registration with a 30-minute inactivity timer      |
| `on PreStart => ...`               | `ActorBase.OnPreStart()` override                       |
| `on PostStop => ...`               | `ActorBase.OnPostStop()` override                       |
| `on Restore(Snapshot s) => ...`    | `ActorBase.OnRestore(Snapshot s)` override              |
| `program Main { ... }`             | `static async Task Main(string[] args) { ... }`             |
| `self`                             | `_selfRef`                                                  |
| `sender`                           | `_sender`                                                   |
