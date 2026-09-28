---
title: Supervision and failure
layout: default
parent: Language
nav_order: 6
permalink: /language/supervision/
description: "Let-it-crash failure handling: supervise strategies (OneForOne / AllForOne), directives (Restart / Stop / Escalate / Resume), per-child overrides, and typed exception arms."
---

# Supervision and failure

So far every actor in this book has been on its own. It receives a message,
runs a handler, and updates its [private state](isolation.md). But
handlers throw. A download times out, a parse fails, a downstream service is
down. What happens to an actor whose handler threw halfway through?

Most code treats a thrown exception as an emergency, something to catch, log,
and patch over inline, hoping you anticipated every case. The actor model takes
the opposite view: **failure is normal, expected, and someone else's job to
handle.** An actor that gets into a bad state should not limp on with corrupted
fields; it should die cleanly, and a *supervisor* (its parent) should decide
what happens next. This is the "let it crash" discipline, and it produces
systems that recover more reliably than ones that try to recover at every call
site, because the recovery logic lives in one place, separate from the business
logic, and a restarted actor always starts from a known-good state.

This chapter is about that supervisor. You will see how to declare a failure
policy with `supervise`, the four directives it can apply (`Restart`, `Stop`,
`Escalate`, `Resume`), the two strategies for handling a failure across a group
of children (`OneForOne` and `AllForOne`), how to override the policy for one
specific child, and how to branch on the exception type.

{: .note }
> **Where this comes from.** "Let it crash" is Erlang's idea from the 1980s,
> brought to the JVM by Akka and to .NET by Akka.NET and Proto.Actor. Spek's
> four directives (`Resume`, `Restart`, `Stop`, `Escalate`) are the same set
> Akka uses, and a parent's supervisor role plays exactly Akka's
> `supervisorStrategy`. If you have used Akka, the model here will feel
> familiar; the syntax is just tighter.

## The parent decides

The key rule, and the one thing to internalize before anything else: **a failing
actor never decides its own fate. Its parent does.**

In [the first-actor tutorial](first-actor.md) you spawned a child with
`spawn<Account>()`. That call did two things: it created the child, and it made
the spawning actor its *parent*. When a child's handler throws, the runtime
catches the exception and asks the parent what to do. The child has no say. This
is what lets the recovery policy live in one place, the parent, instead of
being scattered through every child's handlers.

Here is the whole shape: a `Coordinator` spawns a `Worker`, the worker's handler
throws, and the coordinator's `supervise` declaration says "restart it."

<!-- spek-test: compile -->
```spek
message ProcessJob(string url);

actor Worker
{
    behavior Running
    {
        on ProcessJob job =>
        {
            // A transient failure mid-job — just let it crash.
            throw new System.TimeoutException("download timed out");
        }
    }
}

actor Coordinator
{
    ActorRef worker;

    init() { worker = spawn<Worker>(); }

    behavior Running
    {
        on ProcessJob job => { worker.Tell(job); }
    }

    supervise OneForOne(on Failure: Restart);
}
```

Notice what the `Worker` does *not* contain: no `try`/`catch`, no retry counter,
no error-recovery branch. It just throws. All the recovery lives in the
`Coordinator`'s one-line `supervise` declaration. That separation is the entire
point.

## The four directives

When a handler throws, the supervisor applies one of four directives. They are
the values of the `Spek.FailureDirective` enum, and you will name them
directly in `supervise` arms.

| Directive  | What it does                                                                                   |
|------------|------------------------------------------------------------------------------------------------|
| `Restart`  | Discard the broken instance, build a fresh one, and let it process the next queued message. State resets (persistent actors reload from their latest snapshot; see [persistence](persistence.md)). |
| `Stop`     | Shut the actor down for good: drain its mailbox to the dead-letter sink, run `on PostStop`, and reject further sends. |
| `Escalate` | Pass the decision up to *this* supervisor's own parent. The failure climbs the tree until someone handles it. |
| `Resume`   | Skip the message that threw and carry on. State is left exactly as it was. Use this for a failure that does not corrupt anything. |

The default, when you write no policy at all, is `Stop`, in keeping with Spek's
fail-loud philosophy. An actor whose failures you never thought about will halt
rather than silently soldier on in an unknown state.

`Restart` is the one to picture carefully. The caller's `ActorRef` keeps
working: the runtime swaps a fresh instance in behind the same reference, so
nobody holding the ref needs to re-fetch it. And **messages already in the
mailbox survive**: they are processed by the fresh instance after it starts up.
A restart is a clean slate for *state*, not a dropped queue.

## `OneForOne` vs `AllForOne`

A `supervise` declaration names a *strategy* that decides how widely a directive
applies when a child fails. There are two.

- **`OneForOne`** applies the directive only to the child that failed. Its
  siblings keep running, untouched. This is what you want most of the time: one
  worker's bad download has nothing to do with the others.

- **`AllForOne`** applies the directive to *every* sibling when *any* child
  fails. Reach for this when the children form one coordinated unit and a
  single failure makes the whole group's state suspect, like a multi-stage
  pipeline where stage 2 crashing means stage 3's half-built state is garbage.

Here a three-stage pipeline restarts as a unit: if any stage throws, all three
are rebuilt together.

<!-- spek-test: compile -->
```spek
message Step();

actor Stage
{
    behavior Running { on Step => { throw new System.InvalidOperationException("stage failed"); } }
}

actor Pipeline
{
    ActorRef parse;
    ActorRef transform;
    ActorRef load;

    init()
    {
        parse     = spawn<Stage>();
        transform = spawn<Stage>();
        load      = spawn<Stage>();
    }

    behavior Running { on Step => { } }

    // If any stage fails, restart the whole pipeline in lockstep.
    supervise AllForOne(on Failure: Restart);
}
```

## Retry budgets

A `Restart` directive on its own restarts forever. An actor that throws on every
message would restart in a tight loop. Two named options put a budget on it:

- **`maxRetries:`** is the maximum number of restarts allowed inside the window.
  Once exceeded, the directive degrades to `Stop`.
- **`withinTime:`** is the length of that window, written as any
  `System.TimeSpan`-valued expression. Spek has no special duration literal;
  you use the [BCL type](csharp-syntax.md) directly, e.g.
  `System.TimeSpan.FromMinutes(1)`.

<!-- spek-test: compile -->
```spek
message Step();

actor FlakyStep
{
    behavior Running { on Step => { throw new System.InvalidOperationException("flaked"); } }
}

actor Pipeline
{
    ActorRef stage;

    init() { stage = spawn<FlakyStep>(); }

    behavior Running { on Step => { stage.Tell(new Step()); } }

    // Restart up to 5 times per minute, then give up and Stop.
    supervise OneForOne(
        on Failure: Restart,
        maxRetries: 5,
        withinTime: System.TimeSpan.FromMinutes(1)
    );
}
```

{: .note }
> `maxRetries` and `withinTime` are ordinary named arguments, not reserved
> words, so you can still use those names as plain identifiers elsewhere. Misspell
> one (`maxRetres:`) and you get [CE0117](../reference/errors.md#ce0117) naming the
> expected options, rather than a cryptic parse error.

## Branching on the exception type

Not every failure deserves the same directive. A network blip should restart; a
programming bug should stop. A single strategy can list **multiple `on Failure`
arms** that narrow by exception type. They are matched **top-to-bottom, first
match wins**, exactly like C# `catch` clauses.

<!-- spek-test: compile -->
```spek
message Fetch(string url);

actor Fetcher
{
    behavior Running
    {
        on Fetch f => { throw new System.IO.IOException("network"); }
    }
}

actor Gateway
{
    ActorRef fetcher;

    init() { fetcher = spawn<Fetcher>(); }

    behavior Running
    {
        on Fetch f => { fetcher.Tell(f); }
    }

    supervise OneForOne(
        on Failure(System.IO.IOException): Restart,
        on Failure(System.InvalidOperationException): Stop,
        on Failure: Escalate   // anything else bubbles up
    );
}
```

The last arm, the untyped `on Failure: Action`, is the catch-all. It matches any
exception that no typed arm above it caught. Order matters: because matching
stops at the first hit, a catch-all has to come *last*. Put a typed arm after a
catch-all and the compiler flags it as unreachable with
[CE0081](../reference/errors.md#ce0081); write two catch-alls in one strategy and
you get [CE0082](../reference/errors.md#ce0082). These are the same dead-code checks
the C# compiler runs on `catch` order, surfaced at the Spek level.

{: .note }
> The typed arms lower to a plain `if (cause is IOException) return ...;` chain
> in the generated parent code, so there is zero runtime cost over a hand-written
> `switch`.

## Per-child overrides

The strategies so far apply one policy to *every* child a parent spawns.
Sometimes one child is special. The per-child form,
`supervise(field, strategy: ...)`, layers an override on top of the default: the
named child gets its own policy, and everyone else falls through to the default.

<!-- spek-test: compile -->
```spek
message Tick();

actor Cell
{
    behavior Running { on Tick => { throw new System.InvalidOperationException("bad cell"); } }
}

actor Grid
{
    ActorRef critical;
    ActorRef scratch;

    init()
    {
        critical = spawn<Cell>();
        scratch  = spawn<Cell>();
    }

    behavior Running { on Tick => { } }

    supervise OneForOne(on Failure: Restart);                  // default for every child
    supervise(critical, strategy: OneForOne(on Failure: Stop)); // ...except `critical`
}
```

`critical` is stopped on failure; `scratch` has no override, so it falls through
to the default and restarts. Overrides are resolved first, the default second.

Putting the pieces together, a realistic supervisor reads as a small policy
table: typed arms, a retry budget, and one special-cased child.

<!-- spek-test: compile -->
```spek
message Render(string template);

actor RenderWorker
{
    behavior Running
    {
        on Render r => { throw new System.IO.IOException("template not found"); }
    }
}

actor RenderFarm
{
    ActorRef fast;
    ActorRef bulk;

    init()
    {
        fast = spawn<RenderWorker>();
        bulk = spawn<RenderWorker>();
    }

    behavior Running
    {
        on Render r => { fast.Tell(r); }
    }

    supervise OneForOne(
        on Failure(System.IO.IOException): Restart,
        on Failure(System.InvalidOperationException): Stop,
        on Failure: Escalate,
        maxRetries: 3,
        withinTime: System.TimeSpan.FromMinutes(1)
    );

    // The bulk worker is disposable — never restart it, just stop it.
    supervise(bulk, strategy: OneForOne(on Failure: Stop));
}
```

## When you need code, not a table

`supervise` is declarative: each arm maps an exception type to a fixed
directive. When the right directive depends on *runtime* state, such as the
retry count so far, the time of day, or a circuit-breaker flag, drop down to the
imperative hook the declarative form generates for you. Override `OnChildFailure`
directly and return a `FailureDirective`:

<!-- spek-test: compile -->
```spek
message Job(int attempt);

actor Retrier
{
    behavior Running { on Job j => { throw new System.TimeoutException("slow"); } }
}

actor Manager
{
    ActorRef worker;

    init() { worker = spawn<Retrier>(); }

    behavior Running
    {
        on Job j => { worker.Tell(j); }
    }

    FailureDirective OnChildFailure(ActorRef child, System.Exception cause, object message)
    {
        if (cause is System.TimeoutException) { return FailureDirective.Restart; }
        return FailureDirective.Escalate;
    }
}
```

{: .warning }
> Pick **one** form per actor. A `supervise` declaration *generates* an
> `OnChildFailure` override, so writing both on the same actor would silently
> drop your hand-written one. The compiler refuses, with
> [CE0118](../reference/errors.md#ce0118). Use the declarative `supervise` for fixed
> policies, the explicit `OnChildFailure` when the decision needs to compute.

A **root** actor, one you spawned directly from the `ActorSystem` with no
parent to defer to, has no supervisor. It decides its own fate by overriding
`OnFailure` instead. The signature drops the `child` parameter, since the actor
*is* the failing one:

<!-- spek-test: compile -->
```spek
message Crash();

actor Root
{
    behavior Running { on Crash => { throw new System.InvalidOperationException("boom"); } }

    FailureDirective OnFailure(System.Exception ex, object message)
    {
        if (ex is System.TimeoutException) { return FailureDirective.Restart; }
        return FailureDirective.Stop;
    }
}
```

If a root actor escalates (or a non-root escalation reaches the top), there is
nobody above it to catch the failure, so `Escalate` degrades to `Stop` and the
cause is recorded in the dead-letter sink.

## Dead letters: where lost messages go

Supervision handles handlers that *throw*. A related question is what happens to
messages that have nowhere to go: sent to an actor that has already stopped, or
arriving when the active behavior has no matching `on` handler. Spek does not
drop these silently. Every such message is routed to the runtime's
**dead-letter sink**.

Three sinks ship with the runtime:

- `ConsoleDeadLetterSink`, the default, logs to stderr.
- `RecordingDeadLetterSink` captures everything in memory, ideal for test
  assertions.
- Your own `IDeadLetterSink` wires dead letters into application logging or
  metrics.

You choose the sink when you construct the `ActorSystem`, in the host's C# entry
point:

```csharp
var sink   = new RecordingDeadLetterSink();
var system = new ActorSystem("orders", deadLetterSink: sink);
// ... run the system ...
Assert.Contains(sink.Records, e => e.Reason == "target actor is stopped");
```

You will lean on `RecordingDeadLetterSink` again when we get to
[testing actors](testing.md), where asserting on dead letters is how you
prove a `Stop` directive actually fired.

## Where restart leads

You now have the failure half of the runtime: a child throws, its parent applies
a directive, and the system keeps running from a known-good state. The directive
that does the most interesting work is `Restart`, and its "fresh instance from a
known-good state" promise only fully pays off when that state can *survive* the
restart. The next chapter,
[Persistence and passivation](persistence.md), shows how a persistent
actor snapshots its state and auto-restores it after a restart, so a crashed
account comes back with its balance intact, no `on Restore` boilerplate
required.

## Related reading

- [Runtime reference: `FailureDirective`](../reference/runtime.md#failuredirective):
  the enum and the `ActorSlot`/`OnChildFailure` machinery behind these arms.
- [Persistence](persistence.md): how `Restart` and snapshot restore
  interact.
- [Error codes](../reference/errors.md): `CE0081`, `CE0082`, `CE0117`, `CE0118`,
  the compile-time checks this chapter relies on.
