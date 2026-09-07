---
title: Testing actors
layout: default
parent: Language
nav_order: 19
permalink: /language/testing/
description: "Test actors in Spek: TestActorSystem to spawn the system under test, TestProbe with ExpectMsg/ExpectNoMsg to assert on messages, ExpectStop/ExpectRestart for supervision outcomes, virtual time to test passivation and timeouts without sleeping, deterministic simulation that replays a whole system from a seed, property-based testing with integrated shrinking, chaos fault injection, and the …Tests convention that runs under dotnet test."
---

# Testing actors

You have now built the whole surface: [actors](/language/actors/) that hold
state, [messages](/language/messages/) that carry data, [supervision](/language/supervision/)
that handles failure, [persistence](/language/persistence/) that survives a
restart. This chapter answers the obvious next question: **how do you test any
of it?**

An actor is awkward to test the way you'd test a plain object. You can't call a
handler directly and read a return value; a message goes into a mailbox, gets
processed on some pool thread, and the reply (if any) comes back *later*, to
whoever sent it. The unit under test is concurrent and asynchronous by
construction. So Spek ships a small test kit that gives you back the two things
ordinary unit tests rely on: a way to **drive** the actor (send it messages as
if you were another actor) and a way to **observe** it (catch what it sends back
and assert on it).

There is no new language feature here. Spek tests are written **in Spek**, as
ordinary actor-facing code, and they run under `dotnet test` like any xUnit
test. This chapter introduces the test kit (`TestActorSystem`, `TestProbe`,
the `Expect…` assertions, and the virtual clock) and the one convention that
turns a Spek type into a test. Its back half belongs with the debugging tools:
deterministic simulation that replays a whole actor system from a single seed,
property-based testing that shrinks every failure to a minimal case, and chaos
rules that inject the faults supervision exists to survive.

{: .note }
> **Where this comes from.** `TestProbe`, `ExpectMsg`, and driving an actor from
> the outside are Akka TestKit. Spek's spin: a `…Tests` type *is* the suite (no
> attributes), and the kit rides on xUnit, so `dotnet test` just runs it.

## Tests are `…Tests` types

There is no `test` keyword and no attribute to write. In a **test project**, a
`module` or `class` whose name ends in **`Tests`** is a test container, and each
of its **public** methods is a single test. The name is the entire signal.

<!-- spek-test: compile -->
```spek
message Deposit(decimal amount);
message GetBalance();
message Balance(decimal amount);

actor Wallet
{
    decimal balance = 0m;

    init() { become Active; }

    behavior Active
    {
        on Deposit d  => { balance += d.amount; }
        on GetBalance => return new Balance(balance);
    }
}

class WalletTests                          // name ends in `Tests` → a test container
{
    TestActorSystem sys;

    init() { sys = new TestActorSystem(); } // runs before each test

    public void DepositIncreasesBalance()   // public method = one test
    {
        var wallet = sys.Spawn<Wallet>();
        wallet.Tell(new Deposit(100m));

        Balance b = wallet.Ask(new GetBalance());
        Xunit.Assert.Equal(100m, b.amount);
    }
}
```

The actor under test and its tests can share a file, as above, and three things
about that example are worth pulling out. The `init` block runs before each
test: the runner builds a fresh instance of the `…Tests` class every time, so fields reset between tests, which makes `init`
the place for shared setup like constructing the `TestActorSystem`. That
`TestActorSystem` field is then disposed for you after each test, so there is no
teardown method to write. And the body is ordinary Spek:
`wallet.Ask(new GetBalance())` is the same [Ask](/language/messaging/) you already use,
`Xunit.Assert` is just the xUnit assertion library called by its full name, and
`using Spek.Testing;` is added for you in a test project so `TestActorSystem`
resolves unqualified.

There are two container shapes, and you pick based on whether tests share setup:

- **`class …Tests`**: per-test state in fields, set in `init`. Reach for this
  whenever your tests share a `TestActorSystem` (almost always).
- **`module …Tests`**: stateless test methods, each fully isolated, with no
  shared fields. Reach for this when a test stands entirely on its own.

A non-public method in either container is an ordinary helper, callable from
the tests, but never run as one.

{: .note }
> The `…Tests` convention only kicks in for a **test project**, one that
> references `Microsoft.NET.Test.Sdk`. In a normal project the very same type is
> just an ordinary [module](/language/modules/) or [class](/language/classes/).
> The [Running tests](#running-tests) section below covers the project setup.

## `TestActorSystem`: spawn the system under test

`TestActorSystem` is a thin, test-scoped [actor system](/reference/runtime/).
It's where the actors you want to exercise come to life, and it's the handle you
use to assert on supervision outcomes. The methods you'll use most:

- `Spawn<TActor>(args)`: start the actor under test and get its `ActorRef`.
- `SpawnPersistent<TActor>(key, args)`: start a [persistent](/language/persistence/)
  actor under a persistence key.
- `CreateProbe()`: make a [`TestProbe`](#testprobe-a-stand-in-actor) (next
  section).
- `WhenIdleAsync()`: await until every actor has drained (mailboxes empty,
  nothing processing).
- `ExpectStop(actor)` / `ExpectRestart(actor)`: await a
  [supervision](#asserting-on-supervision) outcome.

The simplest possible test spawns an actor, tells it something, and asks for the
result. Because [Ask](/language/messaging/) blocks until the reply arrives, you
often don't need any further synchronization:

<!-- spek-test: compile -->
```spek
message Inc();
message Read();
message Count(int n);

actor Tally
{
    int n = 0;

    init() { become Live; }

    behavior Live
    {
        on Inc  => { n = n + 1; }
        on Read => return new Count(n);
    }
}

class TallyTests
{
    TestActorSystem sys;

    init() { sys = new TestActorSystem(); }

    public void CountsEachIncrement()
    {
        var tally = sys.Spawn<Tally>();
        tally.Tell(new Inc());
        tally.Tell(new Inc());
        tally.Tell(new Inc());

        Count c = tally.Ask(new Read());
        Xunit.Assert.Equal(3, c.n);
    }
}
```

The three `Tell`s land in the mailbox in order and are processed one at a time
(an actor never runs two handlers at once, the [isolation
guarantee](/language/isolation/)), so by the time the `Ask` is processed,
all three increments have already run. The Ask gives you a natural barrier: its
reply can't arrive until everything queued before it has been handled.

## `TestProbe`: a stand-in actor

Ask is enough when the handler *replies*. But plenty of actors don't reply;
they `Tell` a message onward to some collaborator. To test that, you need to
stand in for the collaborator: receive what the actor sends, and assert on it. A
**`TestProbe`** is exactly that: a real actor you control, with an inbox you can
inspect.

You create one with `sys.CreateProbe()`. It gives you:

- `Ref`: the probe's `ActorRef`. Hand this to the actor under test anywhere an
  `ActorRef` is expected, and the actor will send to the probe.
- `Send(target, message)`: send a message to `target` **as the probe**, so any
  reply routes back to the probe's inbox.
- `ExpectMsg<T>()`: wait for the next message and assert it is a `T`, returning
  it typed.
- `ExpectMsg<T>(predicate)`: same, but also assert the message satisfies a
  predicate.
- `ExpectNoMsg()`: assert that *nothing* arrives within a short window.

Each `Expect…` throws on failure, which the runner reports as a failed test.

Here's a probe standing in for a downstream collaborator. The `Notifier`
forwards every alert to a sink it was handed at spawn; the probe *is* that sink,
so the test can assert the forward happened:

<!-- spek-test: compile -->
```spek
message Alert(string text);

actor Notifier
{
    ActorRef sink;

    init(ActorRef s) { sink = s; become Live; }

    behavior Live
    {
        on Alert a => { sink.Tell(a); }
    }
}

class NotifierTests
{
    TestActorSystem sys;

    init() { sys = new TestActorSystem(); }

    public void ForwardsAlertsToTheSink()
    {
        var probe    = sys.CreateProbe();
        var notifier = sys.Spawn<Notifier>(probe.Ref);   // the probe is the sink

        notifier.Tell(new Alert("disk full"));

        Alert got = probe.ExpectMsg<Alert>();
        Xunit.Assert.Equal("disk full", got.text);
    }
}
```

`ExpectMsg<Alert>()` blocks until a message arrives (with a short default
timeout), checks its type, and returns it typed so you can read its fields. If
the wrong type arrives, or nothing arrives in time, it throws and the test fails.

### Asserting on content with a predicate

When you care about *what's* in the message, pass a predicate to `ExpectMsg`. It
asserts the type and the predicate in one call, and still returns the typed
message:

<!-- spek-test: compile -->
```spek
message Quote(string symbol, decimal price);

actor PriceFeed
{
    ActorRef sink;

    init(ActorRef s) { sink = s; become Live; }

    behavior Live
    {
        on Quote q => { sink.Tell(q); }
    }
}

class PriceFeedTests
{
    TestActorSystem sys;

    init() { sys = new TestActorSystem(); }

    public void ForwardsAGoodQuote()
    {
        var probe = sys.CreateProbe();
        var feed  = sys.Spawn<PriceFeed>(probe.Ref);

        feed.Tell(new Quote("SPEK", 42.50m));

        Quote q = probe.ExpectMsg<Quote>(x => x.price > 0m);
        Xunit.Assert.Equal("SPEK", q.symbol);
    }
}
```

### Driving replies through the probe

`probe.Send(target, msg)` sends *as the probe*, so a handler's `return reply`
routes its answer back to the probe's inbox instead of to your test thread. Use
it to test the [return-to-reply idiom](/language/messaging/) without an Ask:
send, then `ExpectMsg` the reply:

<!-- spek-test: compile -->
```spek
message Ping();
message Pong();

actor Echo
{
    init() { become Live; }

    behavior Live
    {
        on Ping => return new Pong();
    }
}

class EchoTests
{
    TestActorSystem sys;

    init() { sys = new TestActorSystem(); }

    public void RepliesWithPong()
    {
        var echo  = sys.Spawn<Echo>();
        var probe = sys.CreateProbe();

        probe.Send(echo, new Ping());   // reply routes back to the probe

        probe.ExpectMsg<Pong>();
    }
}
```

### Asserting that *nothing* happens

Some of the most valuable tests are the ones that prove a message was *not*
sent: a filter that drops an event, or a guard that swallows an invalid command.
`ExpectNoMsg()` waits a short window and fails if anything arrives. Here a
`Gate` only forwards values above a threshold:

<!-- spek-test: compile -->
```spek
message Value(int n);

actor Gate
{
    ActorRef sink;

    init(ActorRef s) { sink = s; become Live; }

    behavior Live
    {
        on Value v => { if (v.n > 10) { sink.Tell(v); } }   // drop the small ones
    }
}

class GateTests
{
    TestActorSystem sys;

    init() { sys = new TestActorSystem(); }

    public void DropsValuesBelowTheThreshold()
    {
        var probe = sys.CreateProbe();
        var gate  = sys.Spawn<Gate>(probe.Ref);

        gate.Tell(new Value(3));      // below threshold → dropped

        probe.ExpectNoMsg();          // nothing should reach the sink
    }

    public void ForwardsValuesAboveTheThreshold()
    {
        var probe = sys.CreateProbe();
        var gate  = sys.Spawn<Gate>(probe.Ref);

        gate.Tell(new Value(42));     // above threshold → forwarded

        Value got = probe.ExpectMsg<Value>();
        Xunit.Assert.Equal(42, got.n);
    }
}
```

{: .note }
> `ExpectNoMsg` proves a *negative*, so it can only ever wait a bounded window
> (a few hundred milliseconds by default), long enough to catch a message
> that's already on its way, short enough not to slow the suite. It can't prove a
> message will *never* come, only that none came in the window.

## Asserting on supervision

[Supervision](/language/supervision/) is failure handling, and failure handling
deserves tests. `TestActorSystem` lets you assert on what supervision *did* to a
child after it crashed:

- `ExpectStop(actor)`: await until the actor is stopped.
- `ExpectRestart(actor, count)`: await until it has restarted at least `count`
  times (default 1).
- `RestartCountOf(actor)`: the raw restart count, if you'd rather assert it
  directly.

Both `Expect…` calls poll until the condition holds and throw a
`TimeoutException` if it never does. They auto-await under [invisible
async](/language/async/), so you write them as plain statements.

This test crashes a child two ways and checks the parent's strategy did the
right thing each time. The parent supervises one child with `Restart` and the
other, a per-child override, with `Stop`:

<!-- spek-test: compile -->
```spek
message Boom();
message Spawn();
message GetChildren();
message Children(ActorRef restarting, ActorRef stopping);

actor Worker
{
    init() { become Live; }

    behavior Live
    {
        on Boom => { throw new System.InvalidOperationException("boom"); }
    }
}

actor Manager
{
    ActorRef restarting;
    ActorRef stopping;

    init() { become Live; }

    behavior Live
    {
        on Spawn       => { restarting = spawn<Worker>(); stopping = spawn<Worker>(); }
        on GetChildren => return new Children(restarting, stopping);
    }

    supervise OneForOne(on Failure: Restart);                 // default — restart
    supervise(stopping, strategy: OneForOne(on Failure: Stop)); // one child — stop
}

class ManagerTests
{
    TestActorSystem sys;

    init() { sys = new TestActorSystem(); }

    public void RestartingChildRestartsStoppingChildStops()
    {
        var probe   = sys.CreateProbe();
        var manager = sys.Spawn<Manager>();
        manager.Tell(new Spawn());

        probe.Send(manager, new GetChildren());
        Children refs = probe.ExpectMsg<Children>();

        refs.restarting.Tell(new Boom());     // default strategy → Restart
        sys.ExpectRestart(refs.restarting);
        Xunit.Assert.False(refs.restarting.IsStopped);

        refs.stopping.Tell(new Boom());       // per-child override → Stop
        sys.ExpectStop(refs.stopping);
        Xunit.Assert.True(refs.stopping.IsStopped);
    }
}
```

{: .warning }
> `ExpectRestart` pairs with an **explicit** `Restart` strategy. The default
> supervision directive is `Stop`, so an actor that crashes *without* a
> matching `supervise … Restart` arm is stopped, and `ExpectRestart` will time
> out waiting for a restart that never comes. If you're asserting a restart,
> make sure the child is actually supervised with `Restart`.

## Synchronizing on a drained system

Most tests synchronize naturally: an Ask blocks for its reply, an `ExpectMsg`
blocks for its message. But some don't have a reply to wait on: an actor that
fires off side effects, or a fan-out you only want to check *after* it has
settled. For those, `WhenIdleAsync()` awaits until every actor in the system has
gone quiet: mailboxes empty, nothing processing. It auto-awaits, so it reads as
a plain statement:

<!-- spek-test: compile -->
```spek
message Work(int n);
message Done();

actor Batch
{
    ActorRef sink;
    int seen = 0;

    init(ActorRef s) { sink = s; become Live; }

    behavior Live
    {
        on Work w =>
        {
            seen = seen + w.n;
            if (seen >= 6) { sink.Tell(new Done()); }
        }
    }
}

class BatchTests
{
    TestActorSystem sys;

    init() { sys = new TestActorSystem(); }

    public void SignalsDoneAfterEnoughWork()
    {
        var probe = sys.CreateProbe();
        var batch = sys.Spawn<Batch>(probe.Ref);

        batch.Tell(new Work(2));
        batch.Tell(new Work(2));
        batch.Tell(new Work(2));

        sys.WhenIdleAsync();          // wait for the three Works to drain
        probe.ExpectMsg<Done>();
    }
}
```

`WhenIdleAsync` is the right tool whenever you'd otherwise be tempted to sleep
for a fixed duration and hope. It waits on an *observable condition* (the system
being idle) rather than a guessed interval, so it's both faster and not flaky.
For a condition the system can't express on its own, `TestActorSystem` also
offers a static `WaitUntilAsync(() => …)` that polls your own predicate and
throws a descriptive `TimeoutException` if it never holds.

Both helpers wait for *work* to finish. They can't help when the behavior under
test is triggered by nothing but the passage of time: an idle window, a restart
budget. For that, keep reading:
[virtual time](#virtual-time-controlling-the-clock) advances the clock instead
of waiting on it.

## Testing persistent actors

A [persistent](/language/persistence/) actor's whole point is that its state
survives a restart, so the test that matters spans *two* lifetimes: write with
one system, then bring up a second system over the same store and prove the
state came back. Share a single snapshot store between two `TestActorSystem`s to
simulate the restart:

<!-- spek-test: compile -->
```spek
message Add(int n);
message Total();
message Sum(int n);

actor Ledger
{
    int total = 0;

    init() { become Live; }

    behavior Live
    {
        on Add a   => { total = total + a.n; persist; }
        on Total   => return new Sum(total);
    }

    // No `on Restore` needed — the runtime auto-restores `total` from the snapshot.
}

class LedgerTests
{
    public void StateSurvivesARestart()
    {
        var store = new Spek.Runtime.InMemorySnapshotStore();

        // First lifetime: add up, drain so the snapshot flushes, dispose.
        var first  = new TestActorSystem("svc-1", store);
        var ledger = first.SpawnPersistent<Ledger>("ledger-A");
        ledger.Tell(new Add(10));
        ledger.Tell(new Add(5));
        first.WhenIdleAsync();
        first.Dispose();

        // Second lifetime: fresh system, same store — the total restores to 15.
        var second  = new TestActorSystem("svc-2", store);
        var ledger2 = second.SpawnPersistent<Ledger>("ledger-A");

        Sum s = ledger2.Ask(new Total());
        Xunit.Assert.Equal(15, s.n);

        second.Dispose();
    }
}
```

This is a `class …Tests` without a `TestActorSystem` field, because the test
manages two systems by hand and disposes them explicitly. `SpawnPersistent`
takes the persistence key that ties an actor to its snapshot. Spawn under the same key in the second system and [auto-restore](/language/persistence/) brings
the fields back with no `on Restore` handler in sight.

## Virtual time: controlling the clock

Every synchronization tool so far waits on an observable condition: an Ask
blocks for its reply, `ExpectMsg` for its message, `WhenIdleAsync` for the
drain. Time-driven behavior offers no such condition. A `passivate after`
window of ten minutes elapses whether or not any message arrives, and a test of
it faces a bad trade. Shrink the window to milliseconds and you are no longer
testing the configuration production runs; keep it honest and sleep the window
out, and the test is slow on a good day and flaky under suite load. The test
kit's answer is to take the clock away from the machine and hand it to the
test.

### `self.Clock`: the actor's time source

Every actor system owns a clock. It is a standard .NET
[`TimeProvider`](https://learn.microsoft.com/en-us/dotnet/api/system.timeprovider),
`TimeProvider.System` by default, and the runtime routes every semantic use of
time through it: how long an actor has sat idle before `passivate after`
unloads it, the window a restart budget counts failures in, the delay before a
deferred message is re-admitted. An actor reaches the same clock as
`self.Clock`, one of the ambient accessors alongside `self.Log`.
`self.Clock.GetUtcNow()` reads wall time; `self.Clock.GetTimestamp()` starts a
monotonic measurement.

<!-- spek-test: compile -->
```spek
message Record(string what);

actor Audit
{
    System.DateTimeOffset lastSeen;

    init() { become Live; }

    behavior Live
    {
        on Record r => { lastSeen = self.Clock.GetUtcNow(); }
    }
}
```

On a production system this returns exactly what `DateTime.UtcNow` would have.
The difference appears in a test that virtualizes the clock: a read through
`self.Clock` moves with the test's clock, while a direct read keeps consulting
the machine.

Two things deliberately stay on real time. The dispatcher's internal
scheduling micro-backoffs never route through the system clock, so a virtual
clock that nobody advances cannot deadlock the runtime. And the test kit's
wait windows (`ExpectMsg` timeouts, `WhenIdleAsync` polling, `WaitUntilAsync`
deadlines) are bounds on the *test's* patience, not statements about the
system's time, so they keep working unchanged when the clock is virtual.

### Opting in: `virtualTime: true` and `AdvanceClock`

Virtual time is opt-in, per test system. Construct the `TestActorSystem` with
`virtualTime: true` and its clock stands still: no idle window elapses, no
timer fires, `self.Clock` returns the same instant on every read, until the
test moves time forward with `AdvanceClock`. Here is a ten-minute
[passivation](/language/persistence/) window, tested in milliseconds:

<!-- spek-test: compile -->
```spek
message Hit();

actor Session
{
    int hits = 0;

    init() { become Live; }

    behavior Live
    {
        on Hit => { hits = hits + 1; persist; }
    }

    passivate after System.TimeSpan.FromMinutes(10);
}

class SessionTests
{
    TestActorSystem sys;

    init() { sys = new TestActorSystem(virtualTime: true); }

    public void IdleSessionPassivates()
    {
        var session = sys.SpawnPersistent<Session>("session-A");
        session.Tell(new Hit());
        sys.WhenIdleAsync();                                // the Hit has drained; idleness begins

        sys.AdvanceClock(System.TimeSpan.FromMinutes(11));  // ten idle minutes, one call

        TestActorSystem.WaitUntilAsync(() => !session.IsMaterialized, description: "session passivated");
        Xunit.Assert.False(session.IsStopped);   // passivated, not terminated
    }
}
```

`AdvanceClock` is not a fast sleep. Inside the call, every timer that comes due
fires synchronously on the calling thread, in due order, and a periodic timer
fires once per period the advance spans. The runtime checks passivation
idleness at a quarter of the declared window, so this single call runs that
check four times; the fourth finds ten idle minutes on the clock and unloads
the actor. The unload itself completes asynchronously after its timer fires,
which is why the test still ends by waiting on the observable condition rather
than asserting on the very next line.

A standing clock also turns wall-time assertions from tolerance bands into
equalities:

<!-- spek-test: compile -->
```spek
message WhatTime();
message ItIs(System.DateTimeOffset now);

actor ClockReader
{
    init() { become Live; }

    behavior Live
    {
        on WhatTime => return new ItIs(self.Clock.GetUtcNow());
    }
}

class ClockReaderTests
{
    TestActorSystem sys;

    init() { sys = new TestActorSystem(virtualTime: true); }

    public void ClockReadsAreDeterministic()
    {
        var clock = sys.Spawn<ClockReader>();

        ItIs first = clock.Ask(new WhatTime());
        sys.AdvanceClock(System.TimeSpan.FromHours(1));
        ItIs second = clock.Ask(new WhatTime());

        Xunit.Assert.Equal(System.TimeSpan.FromHours(1), second.now - first.now);
    }
}
```

The two reads are exactly an hour apart. Not roughly an hour with an allowance
for scheduler jitter: exactly, because between the two Asks the clock moved
only when the test moved it.

{: .note }
> Virtual time is opt-in rather than the test default for a simple reason:
> under a clock nobody advances, nothing time-driven ever happens. A test that
> synchronizes against real timing keeps working exactly as before; only a
> test where time itself is the behavior under test should take the clock. On
> a default (real-time) system, `AdvanceClock` throws.

### CE0134: time reads that dodge the clock

Virtual time is only as honest as the reads that route through it. A handler
that calls `DateTime.UtcNow` directly consults the machine, so under virtual
time it diverges silently from every timer and every `self.Clock` read in the
same system: advance the clock an hour and the direct read still reports
lunchtime. The compiler flags exactly this. Inside an actor body, a direct
read of `DateTime.Now` / `UtcNow` / `Today` (or the `DateTimeOffset`
equivalents), `Environment.TickCount`, or `Stopwatch.StartNew` /
`Stopwatch.GetTimestamp` draws [CE0134](/reference/errors/#ce0134). This is
the `Audit` actor from above with one line changed back:

<!-- spek-test: compile -->
```spek
message Record(string what);

actor Audit
{
    System.DateTimeOffset lastSeen;

    init() { become Live; }

    behavior Live
    {
        on Record r => { lastSeen = System.DateTimeOffset.UtcNow; }   // CE0134
    }
}
```

```text
warning[CE0134]: 'DateTimeOffset.UtcNow' reads time directly, bypassing the system clock; under virtual time (tests) it silently diverges from timers and other clock reads. Use 'self.Clock.GetUtcNow()' / 'self.Clock.GetTimestamp()' instead.
  --> Audit.spek:11:37
   |
11 |         on Record r => { lastSeen = System.DateTimeOffset.UtcNow; }   // CE0134
   |                                     ^^^^^^^^^^^^^^^^^^^^^^
   |
```

It is a warning, not an error. The code compiles and behaves correctly in
production; what it puts at risk is the virtual-time guarantee, and only tests
exercise that. The fix is the one-line swap back to `self.Clock`.

The lint is scoped to actor bodies on purpose. A `program` block, a module, or
a class is host-side code with no `self.Clock`, no passivation window, and no
virtual-time guarantee to uphold, so reading real time there is legitimate and
stays silent. The stance mirrors [CE0119](/reference/errors/#ce0119) one layer
up: the compiler defends a runtime guarantee exactly where the guarantee
applies, and nowhere else.

### Any `TimeProvider`: the seam is standard .NET

Nothing about the clock is test-kit magic. `TimeProvider` is the BCL's time
abstraction, `ActorSystem` accepts one at construction, and
`virtualTime: true` merely installs `Spek.Testing`'s `ManualTimeProvider` in
that slot. A host embedding Spek can hand the system any provider it likes:

```csharp
using Spek.Runtime;
using Spek.Testing;

var clock  = new ManualTimeProvider();                   // any TimeProvider works here
var system = new ActorSystem("sim", timeProvider: clock);

clock.Advance(TimeSpan.FromMinutes(5));                  // due timers fire, in order
```

`ManualTimeProvider` starts at a fixed instant (January 1, 2000, UTC) and
moves only on `Advance`. `FakeTimeProvider` from
`Microsoft.Extensions.TimeProvider.Testing` slots in the same way, and a
custom provider can do whatever your host requires. Because the seam is the
standard one, a C# host that already schedules with
`Task.Delay(span, provider)` shares a single notion of time with the Spek
actors it embeds.

## Deterministic simulation: the seed is the schedule

Every tool in this chapter so far has tamed *asynchrony*: an Ask blocks for
its reply, a probe catches what was sent, virtual time advances instead of
waiting. None of it has touched *nondeterminism*. On a live system the thread
pool decides which actor runs next, so two actors racing toward a third
interleave differently from one run to the next, and a bug that needs one
interleaving in ten thousand will pass CI for a month before it fires. When it
finally does, rerunning the test tells you nothing, because the schedule that
produced the failure is gone.

`SimulatedActorSystem` treats the schedule as an input rather than as weather.
Construct it with an integer seed and the whole run becomes a pure function of
three things: the program, the messages fed in, and the seed. Same three in,
same execution out, message for message, on any machine.

{: .note }
> **Where this comes from.** Whole-system deterministic simulation is the
> discipline FoundationDB and TigerBeetle built their reliability stories on;
> shrinking a recorded choice sequence (next section) is Hypothesis's internal
> shrinking. Spek's spin: [CE0119](/reference/errors/#ce0119) makes the whole
> language simulable by construction, so the guarantee needs no carefully
> disciplined subset of it.

The mechanism is a change of who drives. In production, every actor schedules
its own mailbox onto the thread pool. Under simulation nothing self-schedules:
the simulator holds every mailbox, and in a loop it collects the actors that
have mail, picks one, and single-steps it through the same dispatch pipeline
production runs, supervision and ingress policies and chaos rules included.
That pick is the only scheduling freedom in the system, and each pick is one
draw from a random generator seeded at construction. Determinism follows:
replay the draws and you have replayed the run.

Here are two feeders racing into one collector, a shape whose arrival order a
real scheduler decides differently on every run:

<!-- spek-test: compile -->
```spek
message Nudge();
message Item(string tag);
message GetStory();
message Story(string order);

actor Collector
{
    string seen = "";

    init() { become Live; }

    behavior Live
    {
        on Item i   => { seen = seen + i.tag; }
        on GetStory => return new Story(seen);
    }
}

actor Feeder
{
    ActorRef collector;
    string tag;

    init(ActorRef c, string t) { collector = c; tag = t; become Live; }

    behavior Live
    {
        on Nudge => { collector.Tell(new Item(tag)); }
    }
}
```

The simulator's surface is host-side C#. It replaces the scheduler, so it
stands where the scheduler stands, outside the actor world. Its natural home is a C# test file next to your `.spek` sources in the same test project, where
every Spek message and actor is visible and `dotnet test` runs it beside your
Spek tests:

```csharp
using Spek.Testing;
using Xunit;

public sealed class InterleavingTests
{
    private static string StoryFor(int seed)
    {
        using var sim = new SimulatedActorSystem(seed);
        var collector = sim.Spawn<Collector>();
        var a = sim.Spawn<Feeder>(collector, "a");
        var b = sim.Spawn<Feeder>(collector, "b");

        for (int i = 0; i < 5; i++) { a.Tell(new Nudge()); b.Tell(new Nudge()); }
        sim.Run();                                    // the seeded interleaving happens here

        return sim.Ask<Story>(collector, new GetStory()).order;
    }

    [Fact]
    public void SameSeed_TellsTheSameStory()
    {
        Assert.Equal(StoryFor(20260826), StoryFor(20260826));
    }
}
```

`Run()` drains mailboxes in seed-determined order until the system is
quiescent: nothing left to dispatch, no handler still running. The host-side
`Ask` sends its message, drains, and hands back the reply, so a simulated test
needs no probe and no timeout. By the time `Ask` returns, everything the seed
had to say has been said. For seed 1 the collector's story reads
`aabaaabbbb`. For seed 3 it reads `abbabaaabb`. For the same seed twice it
reads identically, down to the last message, which is what the test above
proves. And because each fresh seed explores another legal schedule, a loop
over seeds is a schedule fuzzer with a property no amount of rerunning on a
real scheduler can offer: a real scheduler's choices cluster around whatever
timing your hardware favors, while seeds spread deliberately across the space.

For assertions beyond replies, the simulator exposes `DeadLetters` (a
recording sink of every dropped or failed message), `RestartCountOf(actor)`
for supervision outcomes, and the underlying `System` for everything else.

### Time under simulation: `Advance`

The simulated clock is the same `ManualTimeProvider` that `virtualTime: true`
installs, so everything [virtual time](#virtual-time-controlling-the-clock)
established carries over. `Run()` deliberately leaves time-parked work parked:
no timer fires and no idle window elapses until the test says so.
`Advance(by)` moves the clock, fires every timer that comes due synchronously
and in due order, then drains the fallout. The ten-minute passivation test
from earlier collapses even further here, because the unload that had to be
awaited as an observable condition under `TestActorSystem` now completes
inside the `Advance` call:

```csharp
using var sim = new SimulatedActorSystem(seed: 1);
var session = sim.SpawnPersistent<Session>("session-A");
session.Tell(new Hit());
sim.Run();                                   // the Hit drains; idleness begins

Assert.True(session.IsMaterialized);
sim.Advance(TimeSpan.FromMinutes(11));       // due timers fire, in order, then drain
Assert.False(session.IsMaterialized);        // passivated, assertable on the next line
```

There are no pool threads for the unload to complete on. The timer fires
inside `Advance`, its work drains inside the same call, and the assertion
holds on the very next line.

### Failure reports carry the seed

A deterministic failure is only worth having if its inputs can be found again,
so every failure surface quotes the seed. An `Ask` whose target never replies
throws `ask did not complete under simulation (seed 41)`, with a hint to check
that the handler actually returns a message. A run that exceeds its step
budget trips the livelock guard: `simulation exceeded 100000 dispatch steps
(seed 41); livelock?`. The seed is the entire reproduction recipe. Paste it
back into the constructor and the failing run replays exactly, and it keeps
replaying exactly after you add logging or attach a debugger. That last
property is the one a real race never grants: instrumenting a timing-sensitive
bug usually moves the timing and hides the bug.

### The boundary of the guarantee: CE0119

A simulator can only replay decisions it owns, and Spek can promise it owns
all of them because [CE0119](/reference/errors/#ce0119) forbids raw
concurrency in Spek source: no `Task.Run`, no threads, no raw timers. In pure
Spek, every scheduling decision therefore belongs to the runtime, and under
simulation the runtime hands each one to the seed. The guarantee extends
exactly as far as that rule does. A foreign .NET library that spins up its own
threads, or does real IO on its own schedule, reintroduces timing the
simulator can neither see nor replay. Determinism by construction reaches as
far as Spek source reaches, and stops at the interop boundary.

### Replaying a recorded trace

The simulator is also the replay half of the runtime's flight recorder. A
production system constructed with `new ActorSystem(name, trace: recorder)`
journals its *ingress*, the messages entering from outside the actor world,
into a bounded ring buffer. Since deterministic re-execution re-derives all
internal traffic, ingress is all a replay needs. After an incident: dump the
recorder, load the resulting `SpekTrace`, re-create the recorded topology in
a simulator, and call `ReplayIngress(trace)` to feed the recorded inputs back in arrival order. A build-fingerprint check refuses cross-build replays unless
you pass `allowFingerprintMismatch: true`, the deliberate path when validating
a candidate fix against the incident's own inputs. The recorder itself
(`FlightRecorder`, `SpekTrace`) belongs to the
[runtime reference](/reference/runtime/#flight-recorder).

## Property-based testing: shrinking data and schedule together

An example-based test checks the cases you thought of. A property-based test
states an invariant, generates hundreds of cases, and hunts for the one you
did not. Actors add a twist that ordinary property frameworks miss: whether an
invariant holds can depend on the delivery schedule as much as on the data, so
generating clever inputs searches only half the space. Spek's `Prop` searches
both halves at once, and the design that makes this possible is one shared
stream of choices.

### Generators: pure functions of a choice stream

A `Gen<T>` produces values by consuming *choices*: bounded integers drawn from
a recorded stream, seeded per run. `Gen.Int(min, max)` consumes one choice.
`Gen.Bool()` consumes one. `Gen.OneOf(...)` consumes one to pick among
constant values or among nested generators. `Gen.Sequence(element, upTo)`
consumes one for the length, then whatever its elements consume. `Select` maps
a generated value and consumes nothing. Beneath the combinators sits one
deliberate invariant: smaller choices produce simpler values. `Int` shrinks
toward its minimum, `Bool` toward `false`, `OneOf` toward its first
alternative, `Sequence` toward empty.

When `Prop.ForAll` runs a case, the generator consumes the front of the
stream, and then the property runs under a fresh `SimulatedActorSystem` whose
schedule picks draw from the *same* stream, right where the generator stopped.
One list of integers now describes the case completely: what data was
generated and how every message interleaved. That sharing is the point of the
design. A failure's entire identity, data and schedule both, fits in one
replayable recording.

### Integrated shrinking: edit the recording, replay it

When a property is falsified, the shrinker does not manipulate values; it
edits the recorded choice list and replays. It deletes chunks first, largest
first, which removes messages and the preemptions between them; then it lowers
individual choices, halving before stepping down, which simplifies values.
Every probe replays deterministically from its edited list, so a probe either
still fails and becomes the new best case, or passes and is discarded. The shrinker never has to guess whether a change mattered. Choices past the edited
list read as zero rather than as fresh randomness, because deleting a choice
must genuinely simplify the case. Re-rolling the tail from the seed would turn
each probe into a *different* case rather than a smaller one. And since
schedule picks live in the same list, shrinking minimizes the interleaving
alongside the data. A framework that shrinks only data routinely hands back a
smaller input that no longer fails because the schedule moved beneath it; here
the schedule cannot move.

### A model against the actor

The strongest property shape for a stateful actor is model-based: run the real
actor and an obviously-correct model side by side, feed both the same
generated messages, and demand they agree. For a counter, the model is a plain
integer fold, exactly the kind of code you can trust by reading it:

<!-- spek-test: compile -->
```spek
message Add(int n);
message Reset();
message GetTotal();
message Total(int value);

actor Counter
{
    int total = 0;

    init() { become Live; }

    behavior Live
    {
        on Add a    => { total = total + a.n; }
        on Reset    => { total = 0; }
        on GetTotal => return new Total(total);
    }
}
```

```csharp
using Spek.Testing;
using Xunit;

public sealed class CounterProperties
{
    private static readonly Gen<object> AnyMsg =
        Gen.OneOf(
            Gen.Int(0, 50).Select(n => (object)new Add(n)),
            Gen.OneOf<object>(new Reset()));

    [Fact]
    public void TotalAgreesWithAPlainFold()
    {
        var result = Prop.ForAll(
            Gen.Sequence(AnyMsg, upTo: 30),
            (msgs, sim) =>
            {
                var counter = sim.Spawn<Counter>();
                var model = 0;

                foreach (var m in msgs)
                {
                    counter.Tell(m);
                    if (m is Add a) model += a.n;
                    if (m is Reset) model = 0;
                }
                sim.Run();

                return sim.Ask<Total>(counter, new GetTotal()).value == model;
            });

        result.Assert();
    }
}
```

The property receives the generated input and a fresh simulated system, and
returns `true` to pass; returning `false` or throwing falsifies it. The
defaults are 200 runs, a shrink budget of 500 probes, and a fixed seed of
20260826. The fixed default is a deliberate stance: a property suite that
draws fresh entropy on every run turns CI red one day and green the next with
nothing changed, so Spek makes reproducibility the default and exploration
explicit (pass a different `seed`). Give the counter a saturation bug, one
that silently ignores any deposit pushing the total past 100, and the
property falsifies:

```text
property falsified (run 14, seed 20260839)
shrunk: 48 choices -> 7, in 78 probes
minimal repro: [Add { n = 31 }, Add { n = 38 }, Add { n = 32 }] — rerun with seed 20260839
```

None of it needs decoding. The minimal repro is readable on sight, three
deposits totalling 101, the shortest road past the saturation point, where the
falsifying original consumed 48 choices' worth of messages and schedule. And
the seed makes the report a coordinate rather than an anecdote: rerun with
`seed: 20260839` and the identical failure, shrinking included, reproduces
probe for probe. `PropResult.Assert()` throws a `PropertyFailedException`
carrying exactly this text, so under xUnit the report surfaces as an ordinary
test failure.

## Chaos in tests: faults on purpose

[Supervision](/language/supervision/) is a set of promises about failure, and
promises deserve hostile tests. Real deployments drop messages, deliver
duplicates, delay traffic, and crash actors on the unluckiest message of the
week. A `ChaosPlan` makes each of those faults happen deliberately, at the
runtime's own choke points, so a test can watch the system keep its promises
under conditions the happy path never exercises.

A plan is a set of rules over four faults, targeted by actor type, by one
specific actor, or by message type. Three act at the enqueue path.
`Drop<TMessage>(every: n)` models delivery loss: the message never enters the
mailbox, is tallied in the plan's `Fires` count, and is deliberately *not*
dead-lettered, because dead letters are the runtime keeping its promise while
a drop models that promise being broken upstream. `Delay(actor, by)` holds a
message and re-enqueues it when the system clock reaches its due time.
`Duplicate<TMessage>()` enqueues a second copy, the standing test of handler
idempotency. The fourth fault acts at the dispatch path:
`CrashOnNth<TActor>(n)` throws a `ChaosInjectedException` at the target's nth writer
dispatch, before the handler runs, and the exception unwinds through the real
supervision machinery, so the recovery a chaos test certifies is the recovery
production runs, not a mock of it. The full rule catalog, with the targeting
overloads, lives in the [runtime reference](/reference/runtime/#chaos-plans).

<!-- spek-test: compile -->
```spek
message Job(int id);
message HandledSoFar();
message Handled(int n);

actor Processor
{
    int handled = 0;

    init() { become Live; }

    behavior Live
    {
        on Job j        => { handled = handled + 1; }
        on HandledSoFar => return new Handled(handled);
    }
}
```

```csharp
using Spek.Runtime;
using Spek.Testing;
using Xunit;

public sealed class ProcessorChaosTests
{
    [Fact]
    public void EveryThirdJobIsLost_TheOthersArrive()
    {
        var chaos = new ChaosPlan().Drop<Job>(every: 3);
        using var sim = new SimulatedActorSystem(seed: 11, chaos: chaos);
        var worker = sim.Spawn<Processor>();

        for (int i = 1; i <= 6; i++) worker.Tell(new Job(i));
        sim.Run();

        Assert.Equal(4, sim.Ask<Handled>(worker, new HandledSoFar()).n);
        Assert.Equal(2, chaos.Fires);                 // jobs 3 and 6
    }

    [Fact]
    public void CrashOnThirdDispatch_UnwindsThroughRealSupervision()
    {
        var chaos = new ChaosPlan().CrashOnNth<Processor>(n: 3);
        using var sim = new SimulatedActorSystem(seed: 41, chaos: chaos);
        var worker = sim.Spawn<Processor>();

        for (int i = 1; i <= 5; i++) worker.Tell(new Job(i));
        sim.Run();

        Assert.True(worker.IsStopped);                // default supervision: Stop
        Assert.Contains(sim.DeadLetters.Records,
            r => r.Cause is ChaosInjectedException);
    }
}
```

Under the simulator, chaos stops being statistical. A rule's counters advance
with arrival and dispatch order, the seed fixes both, and so the same fault
lands on the same message in every run: jobs 3 and 6 are the dropped ones
every time, and the third dispatch is the crashing one on any machine. In the
crash test the default supervision directive (`Stop`) takes the worker down,
the undelivered mail dead-letters with the injected exception as its recorded
cause, and both facts are assertable deterministically. Delay needs a special
note: because it re-enqueues on the *system clock*, a delayed message under
virtual time or simulation is held until the test advances past its due time,
and a thirty-second delay costs the suite nothing. Chaos tests end up as fast
and as reproducible as ordinary ones, which removes the usual excuse for not
writing them.

### Attachment: construction-only, and loud

A plan attaches in exactly one place, the system constructor:
`new ActorSystem(name, chaos: plan)`; `TestActorSystem` and
`SimulatedActorSystem` accept the same parameter. There is no ambient static
to set and no configuration file to forget, and that absence is the design:
fault injection must be impossible to leave enabled by accident. A
chaos-enabled system also announces itself on standard error the moment it
starts,

```text
[spek] CHAOS ENABLED on system 'sim-41' — fault injection is active. This configuration must never reach production.
```

and exposes `ChaosEnabled` so a deployment guard can assert the negative. One
asymmetry is deliberate: rules may still be *added* to an attached plan while
the system runs, which is how a soak harness flips faults on and off live, but
the plan object itself can never arrive after construction.

## Running tests

A Spek test project compiles its `.spek` files and runs the emitted tests with
the standard xUnit tooling, so IDE test explorers and CI find them like any other
xUnit test. The compiler detects a test project (it sees
`Microsoft.NET.Test.Sdk`) and compiles `…Tests` types as tests; in a normal
project the same type is an ordinary module or class.

The quickest start is the template:

```bash
dotnet new install Spek.Templates
dotnet new spek-test -n MyApp.Tests
cd MyApp.Tests
dotnet tool restore   # fetch the pinned spekc compiler
dotnet test
```

It scaffolds a project that references **Spek.Build** (which compiles `.spek`
during `dotnet build`) and pins `spekc` as a local tool. The project is ordinary
otherwise:

```xml
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
  </PropertyGroup>

  <ItemGroup>
    <PackageReference Include="Microsoft.NET.Test.Sdk" Version="17.8.0" />
    <PackageReference Include="xunit.runner.visualstudio" Version="2.5.4" />
  </ItemGroup>

  <ItemGroup>
    <PackageReference Include="Spek.Runtime" Version="0.1.*" />
    <PackageReference Include="Spek.Testing.Xunit" Version="0.1.*" />
    <PackageReference Include="Spek.Build" Version="0.1.*" PrivateAssets="all" />
  </ItemGroup>
</Project>
```

`Spek.Testing.Xunit` is the xUnit adapter: each public method on
a `…Tests` type lowers to a method marked with the framework-neutral
`[Spek.Testing.SpekTest]` attribute, and the adapter binds that attribute to an
xUnit fact so the standard runner discovers it. The test's name is the method
name.

### Setup, output, and the test thread

A `class …Tests` keeps per-test state in fields set in `init` (run before each
test), and its `TestActorSystem` field is disposed automatically afterward, so
there's nothing to clean up. For per-test output, take an
`Xunit.Abstractions.ITestOutputHelper` parameter in `init` (xUnit injects it)
and write to it. `Console.WriteLine` isn't attributed to a specific test because
actor work is async and runs on shared threads.

A runnable example lives in
[`samples/NativeTesting`](https://github.com/spek-lang/spek/tree/main/samples/NativeTesting).

## Next

You can now write actors, drive them under test, and prove they behave,
including the moments when they fail, when they're restarted, when nothing
happens but time, and when a hostile schedule or an injected fault does its
worst. The last chapter,
[Common pitfalls](/language/footguns/), gathers the sharp edges of the language
in one place: the blocking calls that starve siblings, the handler shapes that
fight the actor model, and how the compiler catches each one before it reaches
production.

## See also

- [Sending messages: Tell and Ask](/language/messaging/): `Tell`, `ask`, and
  the return-to-reply idiom your tests drive.
- [Supervision and failure](/language/supervision/): the strategies and
  directives `ExpectStop` / `ExpectRestart` assert on.
- [Persistence and passivation](/language/persistence/): `persist`,
  snapshots, and the auto-restore the two-lifetime test exercises.
- [Runtime reference](/reference/runtime/): `ActorSystem` and `ActorRef`
  underneath `TestActorSystem` and `SimulatedActorSystem`.
- [Errors reference: CE0134](/reference/errors/#ce0134): the warning that
  keeps actor time reads on `self.Clock`.
- [Errors reference: CE0119](/reference/errors/#ce0119): the no-raw-concurrency
  rule the simulator's determinism guarantee stands on.
