---
title: Persistence and passivation
layout: default
parent: Language
nav_order: 7
permalink: /language/persistence/
description: "Make an actor's state durable with persist + snapshots, restore it automatically with no on Restore handler, unload idle actors with passivate, and tune what gets saved with transient/deprecated/retired field markers."
---

# Persistence and passivation

So far your actors have lived entirely in memory. That is fine while the
process is up, since [isolation](/language/isolation/) guarantees nobody else can
corrupt their state, and [supervision](/language/supervision/) restarts them
when a handler throws. But a restart under supervision starts the actor from
its `init` block again: a freshly-restarted bank account is back to a zero
balance. And when the *process* exits, every actor's state is gone.

This chapter is about making state outlive both events. Spek gives you two
independent tools, and the key is that they are independent:

- **Durable persistence**: *you* decide, inside a handler, when to write the
  actor's state to disk so it can survive a crash or a process restart. You
  do this with the `persist;` statement.
- **Passivation**: *the runtime* decides that an idle actor should be unloaded
  from memory, and transparently reloads it the next time a message arrives.
  You opt in with `passivate after <duration>;`.

Both build on the same snapshot mechanism, and in both cases the reload is
**automatic**: you almost never write restore code by hand. We'll start
there, because it is the thing that makes the rest feel effortless.

{: .note }
> **Where these come from.** The durable-snapshot model is Akka Persistence's
> snapshot API, simplified: Akka's `SaveSnapshot(state)` + `Recover<SnapshotOffer>`
> is our `persist;` + an auto-generated restore. Event sourcing (Akka's primary
> persistence flavour) isn't a Spek feature. **Passivation** maps to Orleans'
> grain deactivation: Orleans unloads idle grains and reactivates them on the
> next message, and Spek does the same; you just declare the idle window in
> source instead of in config.

## `persist;` snapshots the actor

Inside an `on` handler body, the `persist;` statement captures the actor's
current field values and writes them through to the configured snapshot store.
There is nothing to list: the snapshot is whole-actor and automatic.

<!-- spek-test: compile -->
```spek
message Deposit(decimal amount);
message Balance(decimal amount);

actor Account
{
    decimal balance = 0.00m;

    init() { become Open; }

    behavior Open
    {
        on Deposit d =>
        {
            balance += d.amount;
            persist;
        }
        on Balance => return new Balance(balance);
    }
}
```

`persist;` compiles to an `await` of the runtime's snapshot write, so it
participates in [invisible async](/language/async/) like any other awaited
call: the handler suspends until the write completes and the actor
processes no other message in the meantime. A snapshot taken mid-handler
always reflects the field values *at that point*, so put `persist;` after the
mutations you want to durably record, as in the `Deposit` handler above.

### Where `persist;` is allowed

`persist;` is a write. It commits the actor's state, so it belongs in code
that is allowed to mutate that state:

- It must appear inside an `on` handler **body**. Using it in `init`, a plain
  helper method, or a lifecycle hook is [CE0050](/reference/errors/#ce0050),
  because those run outside message dispatch, where "save the current state" has
  no well-defined meaning.
- It can't appear in a `reader` handler. Readers promise not to mutate state
  (that is what lets the runtime run them concurrently; see
  [isolation](/language/isolation/)), and persisting is a writer-class
  operation, so the compiler rejects it as [CE0087](/reference/errors/#ce0087).
  Move the `persist;` into the writer arm that made the change.

## Durable vs. session-scoped: it's decided at spawn

A subtle but important point: `persist;` only writes to the store when the
actor was spawned **with a persistence key**. That choice is made by the host
code that spawns the actor, not by the actor itself: the same `Account`
declaration can run either durably or in-memory depending on how you start it.

```csharp
// Session-scoped — persist; is a no-op, nothing survives the process.
ActorRef acc = system.Spawn<Account>();

// Persistent — persist; writes to the snapshot store, keyed by "account-alice".
ActorRef acc = system.SpawnPersistent<Account>("account-alice");
```

`SpawnPersistent` binds the actor to a stable key. If the store already holds a
snapshot for that key, because a previous process saved one, the actor is
restored from it *before the first message is dispatched*. So the same actor
type is free to run as a throwaway in a test and as a durable entity in production. Only the spawn call differs. See the
[runtime reference](/reference/runtime/#spawning) for the full spawning API and
[`ISnapshotStore`](/reference/runtime/#isnapshotstore) for the bundled stores
(in-memory, file, SQLite, and append-only log).

{: .note }
> Children spawned from a parent's `spawn<T>()` expression are always
> session-scoped. Persistent identity does not propagate through child
> spawns; each durable actor gets its key explicitly from host code.

## Auto-restore: no `on Restore` needed

Here is the part that keeps simple persistence simple. When you write a
persistent actor and *don't* provide a restore handler, the compiler generates
one for you. It emits an `OnRestore` that rehydrates every captured field from
the snapshot, the exact mirror image of the automatic capture. Notice the
`Account` above had no restore code at all, yet a process restart would bring
its `balance` back. Here it is again with a passivation window, still with no
hand-written restore:

<!-- spek-test: compile -->
```spek
message Touch();
message Status(int hits);

actor Session
{
    int hits = 0;

    init() { become Live; }

    behavior Live
    {
        on Touch  => { hits += 1; persist; }
        on Status => return new Status(hits);
    }

    passivate after System.TimeSpan.FromMinutes(10);
}
```

The symmetry is the point: an actor that captures its fields can't silently
forget to restore them. The old footgun (persist three increments, reload
zero) is gone, because *not writing restore code* gives you a correct restore
rather than no restore.

## `on Restore(Snapshot s)` for custom rehydration

You only write a restore handler when the automatic, field-for-field reload
isn't enough, typically when restoring also has to **re-establish a
behavior**. The runtime doesn't remember which behavior was active when the
snapshot was taken; that's part of what you're rebuilding. So an actor whose
behavior depends on its state writes an explicit `on Restore` and calls
`become` from inside it:

<!-- spek-test: compile -->
```spek
message Freeze();
message Withdraw(decimal amount);
message Receipt(bool ok);

actor Vault
{
    decimal balance = 0.00m;
    bool    frozen  = false;

    init() { become Normal; }

    behavior Normal
    {
        on Withdraw w =>
        {
            balance -= w.amount;
            persist;
            return new Receipt(true);
        }
        on Freeze => { frozen = true; persist; become Locked; }
    }

    behavior Locked
    {
        on Withdraw => return new Receipt(false);
    }

    on Restore(Snapshot s) =>
    {
        balance = s.Get<decimal>("balance");
        frozen  = s.Get<bool>("frozen");

        if (frozen) { become Locked; }
        else        { become Normal; }
    }
}
```

When you write `on Restore`, it takes over completely. The compiler does not
also generate one, so *you* are now responsible for reading every field you
care about. A few things to know:

- **`Snapshot.Get<T>(name)` is type-safe.** A wrong type throws, so a field
  rename or type change during a schema migration fails loudly instead of
  silently reading a default.
- **`become` is allowed here** even though `on Restore` isn't a message handler. The semantic analyzer permits `become` in lifecycle hooks. Without
  it, a multi-behavior actor would restore its data but resume in the wrong
  behavior.
- The same handler serves both restore paths: a `SpawnPersistent` against a key
  with existing state, and (when persistence is configured) recovery after a
  supervised `Restart`. Both deliver the same `Snapshot`.

## `passivate after <duration>;`

A `passivate` declaration at actor scope asks the runtime to save-and-unload
the actor after a stretch of message inactivity. This is a memory-management
tool: an entity actor for every user, device, or session can be cheap to keep
*addressable* without keeping every one *resident*. Idle ones drift out of
memory; the next message for one transparently wakes it.

The duration is any `System.TimeSpan`-valued expression. There is no bespoke
duration literal, so you reach for the familiar BCL factories
(`System.TimeSpan.FromMinutes(30)`, `System.TimeSpan.FromSeconds(5)`,
`System.TimeSpan.FromHours(1)`):

<!-- spek-test: compile -->
```spek
message Page(string url);
message Report();
message Visits(int count);

actor Visitor
{
    int    count = 0;
    string lastUrl = "";

    init() { become Browsing; }

    behavior Browsing
    {
        on Page p => { count += 1; lastUrl = p.url; persist; }
        on Report => return new Visits(count);
    }

    passivate after System.TimeSpan.FromMinutes(30);

    on Restore(Snapshot s) =>
    {
        count   = s.Get<int>("count");
        lastUrl = s.Get<string>("lastUrl");
        become Browsing;
    }
}
```

**Passivation is not termination.** When the idle window elapses, the runtime:

1. runs `OnPassivate` (so the actor can flush anything in-flight),
2. takes a snapshot if the actor is persistent, exactly as `persist;` would,
3. drops the in-memory instance and releases its memory.

The actor reference stays valid the whole time. The *next* message rematerializes
the actor from its snapshot, runs the restore (auto-generated or your
`on Restore`), and then delivers the queued message. A passivated persistent
actor therefore wakes with its state intact; a passivated session-scoped actor
(no key) wakes fresh from `init`, having only released memory. Either way the sender never sees the round trip.

{: .note }
> Passivation and persistence are orthogonal. You can `passivate` without ever
> calling `persist;` (release memory, reset on wake), and you can `persist;`
> without `passivate` (durable, but stays resident). Combine them, as `Visitor`
> does, when you want both durability and a bounded memory footprint.

## Field markers: tuning what gets saved

By default every field is part of the snapshot. Three modifiers let you opt
individual fields out or stage them through a schema change. They are written
in front of the field's type and are mutually exclusive: a field is *normal*,
`transient`, `deprecated`, or `retired`:

<!-- spek-test: compile -->
```spek
message Record(decimal amount, string memo);
message Snap();
message Ledger(decimal total);

actor Book
{
    decimal total          = 0.00m;
    transient  int    perLife = 0;
    deprecated string memo    = "";
    retired    int    legacy  = 0;

    init() { become Open; }

    behavior Open
    {
        on Record r =>
        {
            total += r.amount;
            perLife += 1;
            persist;
        }
        on Snap => return new Ledger(total);
    }
}
```

What each marker does to the snapshot:

| Marker        | In the snapshot? | On the field           |
|---------------|------------------|------------------------|
| *(none)*      | captured + restored | the normal case      |
| `transient`   | never             | resets to its initializer each lifetime; for caches, counters, anything that shouldn't survive a restart |
| `deprecated`  | still captured + restored | the field is being phased out, but existing snapshots keep round-tripping so no data is lost during the transition |
| `retired`     | dropped on the next save | the field is no longer reachable from new logic; its key is reserved so a future field can't accidentally reuse the name, and the store sheds the stale data |

The mental model is gRPC's `reserved` / `deprecated`: fields never just vanish
from a schema. `transient` excludes a field that should always be recomputed.
`deprecated` then `retired` is the safe two-step retirement: deprecate it
while readers migrate off (the data still survives), then retire it to evict it
from the store. Both `transient` and `retired` fields are excluded from
auto-restore, exactly as they are from capture.

{: .note }
> The reference-level policing of these markers, a compile *warning*
> ([CE0101](/reference/errors/#ce0101)) when you read a `deprecated` field and a
> hard *error* ([CE0102](/reference/errors/#ce0102)) when you read a `retired`
> one, applies to fields of [shared regions](/language/shared-regions/),
> accessed through a `use` local. On a plain actor field the markers govern
> only what the snapshot stores; you can still read your own actor's
> `deprecated` or `retired` fields freely.

## What compiles to what

| Spek                                            | C# output                                                            |
|-------------------------------------------------|----------------------------------------------------------------------|
| `persist;`                                      | `await PersistAsync();` (writes the captured fields to the store)     |
| *(no `on Restore`)*                             | a generated `protected override void OnRestore(Snapshot s)` that re-reads every captured field |
| `on Restore(Snapshot s) => ...`                 | `protected override void OnRestore(Snapshot s) { ... }`; yours wins  |
| `passivate after System.TimeSpan.FromMinutes(30)` | `protected override TimeSpan? PassivationTimeout => TimeSpan.FromMinutes(30);` |
| `transient` / `retired` field                   | omitted from the generated `CaptureFields` and the restore           |

See the [runtime reference](/reference/runtime/#isnapshotstore) for the
`ISnapshotStore` API and the bundled stores.

## Next

You've now seen `persist;` and the auto-generated restore lean on `await`
without you writing a single `async` or `await` keyword. That
"invisible async" is a feature in its own right, and it's where we go next:
[Async without await](/language/async/) shows how Task-returning calls are
auto-awaited and how async propagates through your handlers.
