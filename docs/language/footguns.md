---
title: Common pitfalls
layout: default
parent: Language
nav_order: 20
permalink: /language/footguns/
description: "The sharp edges of writing actors on .NET: and the compiler responses (rewrite / suggest / warn / error) that catch each one before it ships."
---

# Common pitfalls

You've now seen the whole language: actors and behaviors, immutable
messages, `Tell` and `ask`, the isolation guarantees, supervision,
persistence, invisible async, and the C# you can write inside a body.
This last chapter is the field guide to the places people still trip, and,
more importantly, to the compiler that catches them.

Most of these pitfalls are not Spek's invention. Spek compiles to C# and
runs on .NET, so it inherits .NET's hazards: the call that parks a
dispatcher thread, the one that kills the whole process, the mutable
payload that races across a mailbox boundary. The difference is that Spek
**owns the transpilation and the diagnostics**. It doesn't just lint these
patterns from the outside. For many of them it can emit the correct code
instead, and for the rest it gives you a precise compile-time error with a
caret pointing at the exact span.

So this chapter has two halves. First, the **response ladder**: the
principle behind every diagnostic, from the ones you never notice to the
ones that stop the build. Then a **tour of the specific pitfalls** you're
most likely to hit, each tied to the chapter that introduced the concept
and the `CE` code that guards it.

> **The rule of thumb:** every known pitfall gets the *least intrusive
> effective* response. If Spek can fix it without changing what your
> program means, it just fixes it. If it can't fix it safely, it tells you,
> as loudly as the danger warrants, and no louder.

{: .note }
> **How this maps to C#.** This chapter is the mirror image of the rest: the C#
> foot-guns Spek removes by construction. The central case is blocking I/O: write
> `File.ReadAllText(path)` and the compiler rewrites it to
> `await File.ReadAllTextAsync(path)` on the backend (CE0115), so you get the async
> form of whatever API you reached for without parking a thread. Where C# trusts
> you to remember, Spek rewrites or rejects.

## The response ladder

There are four rungs, from invisible to fatal:

| Rung | What you do | What Spek does | Example |
|------|-------------|----------------|---------|
| **Rewrite** | nothing | emits the correct, value-preserving form | `task.Result` → `(await task)`; `File.ReadAllText` → `await File.ReadAllTextAsync` |
| **Suggest** | optionally accept | a faint editor hint + one-click fix; the build is unaffected | [CE0116](/reference/errors/#ce0116): sequential `await` in a loop |
| **Warn** | decide | a compiler **Warning** (+ a quick-fix where one exists) | [CE0115](/reference/errors/#ce0115): sync `File.ReadAllText` (also rewritten) |
| **Error** | must change it | refuses to compile | [CE0083](/reference/errors/#ce0083) blocking calls, [CE0084](/reference/errors/#ce0084) process escapes |

### Rewrite: the pitfall disappears

When there's an equivalent that preserves the observable result, Spek's
[invisible-async](/language/async/) pass emits it and the program behaves as if you had
written the await yourself. The canonical cases are the sync-over-async blockers: `.Result`,
`.Wait()`, and `.GetAwaiter().GetResult()`. You read a `Task<T>` as if it
were a value and Spek emits the `await`:

<!-- spek-test: ignore; interop call; illustrates the rewrite, not a self-contained program -->
```spek
on Lookup l =>
{
    // emitted as: string name = (await directory.FetchAsync(l.id));
    string name = directory.FetchAsync(l.id).Result;
}
```

You never have to think about them: reach for whichever reads naturally
and the compiler makes it correct. This is the machinery you met in
[Async without await](/language/async/), seen from the pitfall side.

A rewrite must preserve meaning *exactly*. That's why `Task.WaitAny`
(which returns the **index** of the first completed task) is **not**
rewritten to `Task.WhenAny` (which returns the **task**): they aren't the
same value, so that one stays an error whose message names `Task.WhenAny` as the replacement.

### Suggest: a faint nudge

Some patterns are safe but slower than they need to be, and only the
developer can know whether the faster form is correct. Those surface as an
editor *suggestion* (LSP severity `Information` or `Hint`), sometimes with
a one-click fix, sometimes just a note. They never fail the build.

The worked example is a sequential `await` in a loop:

<!-- spek-test: ignore; demonstrates the CE0116 trigger -->
```spek
foreach (var id in ids)
{
    directory.RefreshAsync(id);   // CE0116 (hint) — awaited once per iteration
}
```

Under invisible async each `*Async` call is awaited before the next
iteration, so the waits run back-to-back. If the iterations are
independent, overlapping them is faster, but [CE0116](/reference/errors/#ce0116)
is a *hint with no auto-fix*, because parallelizing changes side-effect
ordering and turns the first exception into an aggregate. The compiler
can't prove a loop is independent, so it nudges and leaves the call to you.
That's the line between Suggest and Rewrite: a rewrite must preserve
meaning; this one can't, so it only suggests.

### Warn: you decide

A real hazard that the dev should still see, even when Spek can fix it,
gets a **Warning**. The worked example is synchronous file I/O, which
straddles two rungs:

<!-- spek-test: ignore; demonstrates the CE0115 trigger -->
```spek
on Load l =>
{
    string text = File.ReadAllText(l.path);   // CE0115 (warning) — emitted as await File.ReadAllTextAsync(...)
}
```

`File.ReadAllText` blocks the dispatcher while the disk responds, but it
has a drop-in `File.ReadAllTextAsync` sibling. So in a handler or method
body Spek **rewrites it** (the Rewrite rung): the emitted code is
`await File.ReadAllTextAsync(l.path)`, no blocking. The warning *also*
fires, for two reasons: it nudges you to write the async form directly (the
editor offers a one-click fix), and it covers the one spot the rewrite
can't reach, an `init` block or constructor, which can't be `async`, so
there the sync call genuinely blocks and you must decide. Warn *and*
rewrite, by deliberate design.

### Error: never acceptable

Some calls have no async form, or no place in an actor at all. Those are
hard [compile errors](/reference/errors/):

- **Blocking the dispatcher with no value-preserving rewrite** (`Thread.Sleep`,
  `Console.ReadLine`, `Monitor.Wait`, a wait-handle `WaitOne`) is
  [CE0083](/reference/errors/#ce0083).
- **Escaping the process** (`Environment.Exit`, `Environment.FailFast`,
  `Process.Kill`) bypasses supervision and severs every other actor
  mid-message, so it's [CE0084](/reference/errors/#ce0084).

When you genuinely need to bring the node down on a critical, unrecoverable
error, there's a verb that does it *gracefully* instead:

<!-- spek-test: compile -->
```spek
message WriteFailed(string detail);

actor LedgerWriter
{
    on WriteFailed e =>
    {
        self.System.Shutdown();   // graceful: drain, on Shutdown, term {}, exit
    }
}
```

`self.System.Shutdown()` is non-blocking: the handler returns normally,
then every actor drains its mailbox, each `on Shutdown` and shared-region
`term {}` runs, and the host exits. It reaches the node through an ambient
accessor (`self.System`, a sibling of `self.Log` / `self.Metrics`); nothing
is injected into your actor. That's the difference from `Environment.Exit`,
which severs the handler, and every sibling, where it stands.

## How Spek chooses a rung

Triaging a pitfall follows the same four questions every time:

1. **Is there a value-preserving rewrite?** → **Rewrite** it. (Optionally
   pair it with a **Suggest** so the editor teaches the idiomatic source.)
2. **Is it a genuine hazard with no single safe rewrite, or one that's
   sometimes legitimate?** → **Warn**, with a quick-fix where one applies.
3. **Is it never acceptable inside an actor** (process escapes, terminal
   blocks)? → **Error**.
4. **Is it purely a matter of style or idiom?** → **Suggest** only.

{: .note }
> **The context caveat.** A rewrite needs somewhere to put the `await`, so it
> only fires where an await is legal: handler and method bodies, and
> Task-returning lambdas. Inside an `init` block or a constructor (which
> can't be `async`) the blocking form is left untouched. This is rare, and
> it's the reason sync I/O is a *warning* rather than an automatic rewrite.

## A tour of the pitfalls

The rest of this chapter walks the specific traps you're most likely to
hit, grouped by the part of the language they touch. Each one names the
chapter that introduced the concept and the `CE` code that guards it; the
full catalog lives in the [error-code reference](/reference/errors/).

### Mutable message payloads

[Messages](/language/messages/) must be immutable, so the receiver can read
a payload concurrently with the sender holding the same reference. A
`message` field whose type *isn't* on the immutability whitelist (a
`List<T>`, a mutable array, an interface that hides a mutable
implementation) is [CE0010](/reference/errors/#ce0010):

<!-- spek-test: ignore; demonstrates the CE0010 trigger -->
```spek
message AddItems(List<string> items);   // CE0010 — List<T> is mutable
```

The fix is to reach for an immutable collection. `ImmutableList<T>`,
`ImmutableArray<T>`, primitives, `string`, and Spek `enum`s all pass:

<!-- spek-test: compile -->
```spek
message AddItems(System.Collections.Immutable.ImmutableList<string> items);

actor Cart
{
    int count = 0;

    behavior Active
    {
        on AddItems a => { count = count + a.items.Count; }
    }
}
```

### Mutating a payload after you send it

A subtler version of the same hazard. The payload type is immutable, but
*through one of its mutable fields* you reach in and write after handing it
off. Once a value is in another actor's mailbox, mutating it from the
sender races against the receiver, so [CE0085](/reference/errors/#ce0085)
flags a field or index assignment that reaches a sent value, even through
an alias:

<!-- spek-test: ignore; demonstrates the CE0085 trigger -->
```spek
message Update(int v);

actor Sender
{
    behavior Idle
    {
        on Update u =>
        {
            self.Tell(u);   // u handed off — it now belongs to the mailbox
            u.v = 99;        // CE0085 — mutating a moved value
        }
    }
}
```

This is the [isolation](/language/isolation/) guarantee in action: the
share-XOR-mutate rule says once you've shared a value you may not mutate it.
The fix is to mutate *before* the send, so there's exactly one hand-off:

<!-- spek-test: compile -->
```spek
message Update(int v);

actor Sender
{
    int next = 0;

    behavior Idle
    {
        on Update u =>
        {
            next = 99;                      // mutate your own state first…
            self.Tell(new Update(next));    // …then a single hand-off, no later mutation
        }
    }
}
```

### Reaching into another actor

The whole point of the actor model is that state is private and the only
way in is a message. So any member access on an `ActorRef` other than
`Tell` or `ask` is [CE0012](/reference/errors/#ce0012); reading a peer's
field or calling its method directly would bypass the mailbox entirely:

<!-- spek-test: ignore; demonstrates the CE0012 trigger -->
```spek
message Poke();

actor Coordinator
{
    ActorRef peer = ActorRef.NoSender;

    behavior Idle
    {
        on Poke p => { System.Console.WriteLine(peer.name); }   // CE0012
    }
}
```

Send a message instead. If you need a value back, that's exactly what
[`ask`](/language/messaging/) is for.

### `ask`, `self`, `sender`, `persist` outside a handler

A cluster of identifiers and statements only make sense *during message
dispatch*. `ask` ([CE0042](/reference/errors/#ce0042)), `self` and
`sender` ([CE0043](/reference/errors/#ce0043)), and `persist`
([CE0050](/reference/errors/#ce0050)) all require an `on` handler; there's
no "current message" inside `init`, a lifecycle hook, or a plain helper
method. Likewise `become` is rejected inside a plain helper method
([CE0051](/reference/errors/#ce0051)), to keep behavior switches visible in
the handler that drives them.

The fix is almost always to move the line into the handler, or to pass
`self` / `sender` in as a parameter:

<!-- spek-test: compile -->
```spek
message GetBalance();
message Balance(decimal amount);
message Start(ActorRef account);

actor Client
{
    behavior Active
    {
        on Start s =>
        {
            Balance b = s.account.Ask<Balance>(new GetBalance());   // OK — ask lives in a handler
        }
    }
}
```

### Blocking the dispatcher

Every actor in a system shares a pool of dispatcher threads. A handler that
*parks* its thread (`Thread.Sleep`, `Console.ReadLine`, a wait-handle
`WaitOne`) starves every sibling assigned to that thread. There's no async
equivalent that preserves the meaning, so this is the hard error
[CE0083](/reference/errors/#ce0083):

<!-- spek-test: ignore; demonstrates the CE0083 trigger -->
```spek
using System.Threading;

message Tick();

actor Beeper
{
    behavior Running
    {
        on Tick t => { Thread.Sleep(100); }   // CE0083 — parks a dispatcher thread
    }
}
```

The fix is the async form. A timed wait is `Task.Delay`, and because
[invisible async](/language/async/) supplies the `await`, you write it
without one. Durations are ordinary `System.TimeSpan` expressions, not
bespoke literals:

<!-- spek-test: compile -->
```spek
using System;
using System.Threading.Tasks;

message Tick();

actor Beeper
{
    behavior Running
    {
        on Tick t =>
        {
            Task.Delay(TimeSpan.FromMilliseconds(100));   // auto-awaited; no thread parked
        }
    }
}
```

For a genuine wait on an external event, model it as a delayed message
rather than a block, so the dispatcher stays free to serve other actors in
the meantime.

### Synchronous file I/O

Sync `File.ReadAllText` and its siblings block the dispatcher too, but they
have drop-in `*Async` versions, so this is the *warn-and-rewrite* case,
[CE0115](/reference/errors/#ce0115). In a handler the emitted code is
already the async form, but writing the async call yourself silences the
warning and reads more honestly:

<!-- spek-test: compile -->
```spek
using System.IO;

message Load(string path);

actor FileReader
{
    behavior Idle
    {
        on Load l =>
        {
            var text = File.ReadAllTextAsync(l.path);   // awaited automatically
        }
    }
}
```

### Spawning raw concurrency

The C# reflex, when a handler has slow work, is to push it onto another thread:
`Task.Run`, `Parallel.For`, `ThreadPool.QueueUserWorkItem`, `new Thread`. Each of
those runs a delegate *outside* the actor's turn, where it can read and write
actor state concurrently with the actor's own handlers: the exact data race Spek
exists to prevent. So all of them are rejected in Spek source,
[CE0119](/reference/errors/#ce0119).

Concurrency in Spek comes from actors. To move work off the current turn, spawn a
child actor and `Tell` it the job; the child's mailbox serializes the work just
like any other actor, so there's nothing to race:

<!-- spek-test: compile -->
```spek
message Crunch(int input);

actor Worker
{
    behavior Idle
    {
        on Crunch c => { System.Console.WriteLine(c.input * 2); }
    }
}

actor Coordinator
{
    ActorRef worker;

    init() { worker = spawn<Worker>(); }

    behavior Running
    {
        on Crunch c => { worker.Tell(c); }   // off this turn, still serialised
    }
}
```

Awaited async I/O is *not* affected: `await File.ReadAllTextAsync(...)` and other
Task-returning BCL calls are resumed inside the turn by [invisible
async](/language/async/). Only thread-*spawning* is forbidden.

### Mutating from a reader handler

[Shared regions](/language/shared-regions/) let many actors read the same
data concurrently, and `reader on` handlers run under a shared read lock.
Mutating an actor field, a region field, or a confined
[class](/language/classes/) from a reader would race against every other
reader, so it's [CE0087](/reference/errors/#ce0087). Promote the handler
to `writer on` (which takes the exclusive lock) when it needs to write:

<!-- spek-test: compile -->
```spek
message Get();
message Reply(int n);

actor Counter
{
    int n = 0;

    behavior Active
    {
        writer on Get g => { n = 0; return new Reply(n); }   // writer can mutate
    }
}
```

### Copying region data into actor state

Still in shared-region territory: assigning a region read *directly* into
an actor field is [CE0100](/reference/errors/#ce0100). The actor would then
hold a live reference to data the region still owns, and a later writer
could mutate it mid-read. Route the value through a local; the local makes
the borrow a deliberate, visible decision:

<!-- spek-test: compile -->
```spek
message Refresh();

shared Cache { string current = ""; }

actor Worker
{
    use Cache cache;
    string mine = "";

    behavior Idle
    {
        on Refresh r =>
        {
            var snap = cache.current;
            mine = snap;
        }
    }
}
```

### Non-exhaustive enum switches

Spek [enums](/language/enums/) are sealed: the variant set is closed. A
`switch` over an enum value must cover every variant (or include a `_`
discard), or it's [CE0103](/reference/errors/#ce0103). The main
rationale is rolling deploys: when a new variant ships, the compiler flags
every stale `switch` *before* an unhandled value reaches it in production:

<!-- spek-test: compile -->
```spek
enum Status { Active, Inactive, Pending }
message Tick();

actor Monitor
{
    behavior Idle
    {
        on Tick t =>
        {
            Status s = Status.Active;
            var label = s switch {
                Status.Active   => "a",
                Status.Inactive => "i",
                Status.Pending  => "p",   // every variant covered
            };
        }
    }
}
```

### Supervision and persistence traps

Two more, from the chapters on [supervision](/language/supervision/) and
[persistence](/language/persistence/):

- **A mis-spelled `supervise` option.** `maxRetries` and `withinTime` are
  ordinary named arguments, not keywords, so a typo parses cleanly and is
  caught by [CE0117](/reference/errors/#ce0117) rather than surfacing as a
  raw syntax error.
- **Declaring `supervise` *and* an `OnChildFailure` override.** The
  `supervise` decl already generates `OnChildFailure`, so a hand-written
  override would be silently dropped. [CE0118](/reference/errors/#ce0118)
  makes you pick one.
- **Save-but-never-reload.** This *isn't* a pitfall: a
  persistent actor [auto-restores](/language/persistence/)
  every captured field, so `on Restore` is optional and you can't silently
  persist state you never read back.

## Performance pitfalls

Not every pitfall throws an exception or blocks a thread; some just quietly
cost you throughput. Spek watches for these too:

- The blocking-call lints above exist because a handler that parks its pool
  thread starves its siblings on the shared dispatcher. The single runtime
  characteristic worth keeping in mind: **keep handlers non-blocking**, and
  the cooperative scheduler stays fair.
- CPU, memory, and thread-contention benchmarks run against every release, so
  a regression in allocations-per-message or dispatcher contention is caught
  before it ships.

## What you experience in the editor

None of these diagnostics are batch-only. Spek's
[language server](/reference/cli/) surfaces them live, and the fixable ones
carry a **quick-fix** in the lightbulb / "fix this" menu, the same
one-click experience you'd expect from ReSharper. Today that includes:

- `Thread.Sleep(ms)` → `Task.Delay(ms)`
- `Task.WaitAll(…)` → `Task.WhenAll(…)`
- `File.ReadAllText(…)` → `File.ReadAllTextAsync(…)`

In each case the edit lands in your source and invisible async then awaits
the result, so the fix is complete, not just a renamed call.

## Where to go next

That's the language. You've built actors, sent messages, survived failures,
persisted state, and learned the compiler's whole repertoire of guardrails.
The pattern across every chapter has been the same: Spek pushes the classes
of bug that plague concurrent .NET code (data races, blocked dispatchers,
unhandled variants, process escapes) out of *runtime* and into a
compile-time error with a caret under the exact span.

From here, the [error-code reference](/reference/errors/) is the exhaustive
catalog of every `CE` rule, and the [CLI reference](/reference/cli/)
covers `spekc` and the language server. If you want to see how the pieces
fit together at scale, the sample programs put a full actor system,
persistence, supervision, and all, into one buildable project.

## Related

- [Error codes](/reference/errors/): the full `CE`-code catalog, including
  every code linked from this chapter.
- [C# syntax](/language/csharp-syntax/): the inverse view, the C#
  expression and statement syntax Spek passes straight through to Roslyn.
- [Async & concurrency](/language/async/): the invisible-async machinery
  that powers the rewrites.
- [Isolation and ownership](/language/isolation/): the share-XOR-mutate
  guarantee behind CE0085 and CE0087.
