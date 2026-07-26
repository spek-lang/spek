---
title: Shared regions
layout: default
parent: Language
nav_order: 16
permalink: /language/shared-regions/
description: "shared read-concurrent regions: per-ActorSystem state behind a reader/writer lock the compiler manages, with reader/writer handlers, lazy init, and optional persistence."
---

# Shared regions

[Isolation and ownership](/language/isolation/) drew a hard line: an
`actor` owns its fields privately, and the runtime serialises every handler
that touches them. That ownership is what lets Spek delete locks from your
vocabulary, but it also means two actors can never share a single piece of
mutable state directly. The classic answer is to make *one* actor the owner
and have everyone else round-trip messages through it. That works, but it
turns a field read into a mailbox hop, a copy, and a reply.

A **`shared` region** is the other answer. It is mutable state that several
actors can reach, kept race-free not by an owning actor but by a
reader/writer lock the compiler acquires and releases for you. Reads run
concurrently; writes run alone. You already met this on the
[ownership table](/language/isolation/#invisible-ownership): the `shared`
row, "yes, mutable + concurrent." This chapter is that row in full.

You also already know the two halves of the lock. Back in
[isolation](/language/isolation/#guarantee-4-concurrent-readers-cant-write-ce0087)
you wrote `reader on X` and `writer on X` handlers to let an *actor's own*
reads overlap. A region reuses exactly that discipline, applied to state
that lives outside any single actor.

{: .note }
> **Where this comes from.** Erlang's
> [ETS tables](https://www.erlang.org/doc/man/ets.html) are the conceptual
> ancestor: process-shared in-memory state with concurrent read access and
> serialised writes. Spek inherits the per-system ownership model and the
> reader/writer semantics; the lock contract is per-region, not
> per-table-row.

## Declaring a region

A region declaration looks like a field list with a `shared` keyword in
front. It lives at file top level, alongside actors, messages, channels, and
enums:

<!-- spek-test: compile -->
```spek
shared MarketCache
{
    long lastPrice   = 0;
    long lastUpdated = 0;
}
```

Fields are the only required content. The visibility modifier (if any)
applies to the emitted C# class and defaults to `internal`. There is **one
instance of each region type per `ActorSystem`**: the runtime hands out the
singleton; you never `new` one yourself.

## Attaching a region to an actor

Inside an actor body, `use <RegionType> <localName>;` attaches the region
under a local name. That name then behaves like any other member access in
your handler bodies, such as `cache.lastPrice` or `cache.lastUpdated`:

<!-- spek-test: compile -->
```spek
message Update(long price);
message GetLast();
message LastPrice(long price);

shared MarketCache
{
    long lastPrice = 0;
}

actor PriceWriter
{
    use MarketCache cache;

    writer on Update u => { cache.lastPrice = u.price; }
}

actor PriceReader
{
    use MarketCache cache;

    reader on GetLast g => { return new LastPrice(cache.lastPrice); }
}
```

Two different actor *types* attached the same region. So can many instances
of each: every `PriceReader` you spawn talks to the same `MarketCache`.
Notice the handler modes carry their isolation meaning across the boundary:
`PriceWriter` updates the region under a `writer` handler, `PriceReader`
queries it under a `reader` handler and replies with the
[return-to-reply idiom](/language/messaging/) you learned in
[Tell and Ask](/language/messaging/).

The local name is a plain identifier, but pick something that is *not* a soft
keyword used in expression position. `writer`, `reader`, `after`, and friends
are fine as region names and locals in most spots, but a local literally
named `writer` collides with the handler-mode keyword inside a body, so name it
`feed` or `w` instead.

If the type after `use` doesn't resolve to a declared `shared X { ... }`, the
compiler stops you with **CE0097**:

<!-- spek-test: ignore -->
```spek
actor Bad
{
    use NotARegion cache;   // CE0097 — no `shared NotARegion { ... }` exists
}
```

Putting it together into a runnable program, the region is the rendezvous
point and the actors never touch each other:

<!-- spek-test: compile -->
```spek
message Update(long price);
message GetLast();
message LastPrice(long price);

shared MarketCache
{
    long lastPrice = 0;
}

actor PriceWriter
{
    use MarketCache cache;
    writer on Update u => { cache.lastPrice = u.price; }
}

actor PriceReader
{
    use MarketCache cache;
    reader on GetLast g => { return new LastPrice(cache.lastPrice); }
}

program Main
{
    var system = new ActorSystem("quotes");
    var feed   = system.Spawn<PriceWriter>();
    feed.Tell(new Update(101));
    system.AwaitTermination();
}
```

## The reader/writer concurrency model

The region's lock has the same shape as the per-actor lock behind
[`reader` / `writer` handlers](/language/isolation/#guarantee-4-concurrent-readers-cant-write-ce0087):
fair, no reader cap, async-friendly. Concretely:

- **Multiple readers run concurrently.** Every `reader on ...` handler, across
  *every* actor that attaches the region, shares the reader lock. A hundred
  `PriceReader` instances can answer queries at the same time.
- **A writer runs alone.** A `writer on ...` handler waits for all in-flight
  readers to drain, then runs exclusively; new readers wait for it to finish.
- **The region lock is independent of any actor's own lock.** A reader handler
  holds *both* its actor's reader lock and the region's reader lock. The two
  compose without deadlock because they're always acquired in the same order
  (actor lock first, then region lock).

You never write any of that. The compiler wraps each handler body in a
try/finally that acquires the right side of the lock on entry and releases it
on exit: `reader` handlers take the reader lock, `writer` handlers take the
writer lock.

### Readers can't write: CE0087, again

Because a reader handler may overlap with other readers, it must never mutate
region state: a write inside a reader would race the readers running beside
it. This is the very same **CE0087** you saw guarding actor fields in
[isolation](/language/isolation/#guarantee-4-concurrent-readers-cant-write-ce0087),
now extended to region fields:

<!-- spek-test: ignore -->
```spek
actor BadReporter
{
    use MarketCache cache;

    reader on Tick t =>
    {
        cache.lastPrice = 0;   // CE0087 — readers can't mutate region state
        return new Reply(cache.lastPrice);
    }
}
```

The fix is always the same: promote the handler to `writer on ...`, or move
the mutation into a separate writer arm and keep the reader read-only.

## Lazy initialization with `init`

An optional `init { ... }` block runs **once**, on the first reader or writer
access, holding the writer lock so no other access can observe partial state.
Use it to compute starting values that an initializer expression can't:

<!-- spek-test: compile -->
```spek
message GetStarted();
message Started(long at);

shared MarketCache
{
    long lastPrice = 0;
    long startedAt = 0;

    init
    {
        startedAt = DateTimeOffset.UtcNow.ToUnixTimeMilliseconds();
    }
}

actor Clock
{
    use MarketCache cache;
    reader on GetStarted g => { return new Started(cache.startedAt); }
}
```

The init body is ordinary Spek code: assign to fields, call BCL methods, log.
It runs in the region's *own* scope: there is no `self` actor identity, no
`become`, no `persist`, because those are actor concerns, not region concerns.
(One exception: on a persisted region, `self.Name` is the snapshot key; see
[Persistence](#persistence) below.)

If two actors race to be the first to touch the region, both queue behind
init: one runs the body, the other waits until it completes. Either way, the
state is fully initialised before any handler body runs. If init throws, the
region stays failed and every later access re-throws the original exception.
There is no automatic recovery, so fix the init body and restart the host.

### Cleanup with `term`

The disposal counterpart is `term { ... }`. It runs once at `ActorSystem`
shutdown, after dispatch has stopped, in reverse construction order: the
natural place to flush a buffer or release a handle the region opened in
`init`. Same scope rules as `init`: no messaging, no `become`, no `persist`.

<!-- spek-test: compile -->
```spek
message Tick();

shared Telemetry
{
    long events = 0;

    term
    {
        Console.WriteLine($"telemetry saw {events} events");
    }
}

actor Counter
{
    use Telemetry tel;
    writer on Tick t => { tel.events = tel.events + 1; }
}
```

## Persistence

By default a region is **transient**: it lives in memory and dies with the
process. Add `: Persisted` to make its state survive a restart. As with
[actor persistence](/language/persistence/), the Spek source declares the
*capability*; the host wires up the actual store in a `program` block.

<!-- spek-test: compile -->
```spek
message Update(long price);

shared MarketCache : Persisted
{
    long lastPrice   = 0;
    long lastUpdated = 0;

    init
    {
        // Override the default snapshot key.
        self.Name = "market-v2";
    }
}

actor PriceWriter
{
    use MarketCache cache;
    writer on Update u =>
    {
        cache.lastPrice   = u.price;
        cache.lastUpdated = DateTimeOffset.UtcNow.ToUnixTimeMilliseconds();
    }
}

program Main
{
    var system = new ActorSystem("myapp");

    // Required for every `: Persisted` region (CE0098 if missing).
    // Swap InMemorySnapshotStore for a FileSnapshotStore, SqliteSnapshotStore,
    // or LogSnapshotStore to persist beyond the process — see below.
    system.RegisterPersistenceProvider<MarketCache>(new InMemorySnapshotStore());

    var feed = system.Spawn<PriceWriter>();
    feed.Tell(new Update(42));
    system.AwaitTermination();
}
```

What you get:

- **Restore on first access.** When any reader or writer first touches the
  region, the runtime loads the snapshot at `Name` and overlays whichever keys
  are present onto the fields. Missing keys keep their initializer values
  (additive schema rule). If a snapshot exists, **`init` is skipped**: the
  snapshot wins, exactly as it does for
  [auto-restored actors](/language/persistence/).
- **Auto-save on writer-exit.** After every `writer on ...` handler completes
  and releases the writer lock, the runtime captures the current field values
  and writes them through the store. Saves are serialised per region, so the
  latest captured state always wins.
- **CE0098 at compile time.** Every `: Persisted` region must have at least one
  matching `RegisterPersistenceProvider<T>(...)` call somewhere in the
  compilation. The compiler rejects the build if any are missing, with no
  runtime "forgot to register" surprises.

### Available stores

Region persistence reuses the same plumbing as actor persistence, so every
`ISnapshotStore` works:

<!-- spek-test: ignore -->
```spek
// In-memory, for tests
system.RegisterPersistenceProvider<X>(new InMemorySnapshotStore());

// JSON files (one file per region)
system.RegisterPersistenceProvider<X>(
    new FileSnapshotStore("/var/lib/myapp/regions"));

// SQLite: transactional, single file
system.RegisterPersistenceProvider<X>(
    new SqliteSnapshotStore("/var/lib/myapp/regions.db"));

// Append-only log; survives crashes mid-write
system.RegisterPersistenceProvider<X>(
    new LogSnapshotStore("/var/lib/myapp/regions-log"));
```

### Transient fields

A field tagged `transient` is skipped from both capture and restore. The field
still exists on the class; the keyword just opts it out of persistence:

<!-- spek-test: compile -->
```spek
shared MarketCache : Persisted
{
    long lastPrice          = 0;   // captured + restored
    transient int requests  = 0;   // not in the snapshot
}

program Main
{
    var system = new ActorSystem("myapp");
    system.RegisterPersistenceProvider<MarketCache>(new InMemorySnapshotStore());
}
```

Use it for counters, in-flight handles, and derived caches: anything cheaper
to recompute than to serialise. After a restart, a transient field holds its
initializer value. (The keyword also parses on actor fields and on
non-persisted regions, where it's a harmless no-op, since there's nothing to opt
out of.)

### Borrowing region values into actor fields

Reading a region field *directly into an actor field* is rejected by
**CE0100**, because the actor would keep a reference to data the region still
owns. Once the region's lock releases, a concurrent writer could mutate that
data while the actor still holds it:

<!-- spek-test: ignore -->
```spek
on Refresh =>
{
    mine = cache.current;          // CE0100 — the borrow escapes the lock
}
```

Make the borrow explicit. Route the value through a local (a deliberate
snapshot), or wrap it in a copy:

<!-- spek-test: compile -->
```spek
message Refresh();

shared MarketCache
{
    string lastSymbol = "";
}

actor Trader
{
    use MarketCache cache;
    string mySymbol = "";

    on Refresh =>
    {
        var snap = cache.lastSymbol;   // intentional snapshot
        mySymbol = snap;
    }
}
```

This isn't lock guidance; the runtime already serialises around the region's
RW lock. The check exists because reference-typed data still escapes when the
lock releases; forcing the borrow through a local (or a deep-copy call) makes
you decide, on purpose, whether a snapshot or a copy is what you actually want.

### Phasing fields out: `deprecated` and `retired`

Field schemas evolve. Spek doesn't bake migration into the language, but it
ships two markers, borrowed from gRPC's reserved/deprecated mechanism, so you
can retire a field without losing the compile-time guarantees:

<!-- spek-test: compile -->
```spek
shared MarketCache : Persisted
{
    long lastPrice = 0;
    deprecated string oldSymbol = "";   // still works; references warn (CE0101)
    retired   string legacyTag  = "";   // references error (CE0102); key dropped on save
}

program Main
{
    var system = new ActorSystem("myapp");
    system.RegisterPersistenceProvider<MarketCache>(new InMemorySnapshotStore());
}
```

**`deprecated`** marks a field on its way out but still working. It's captured
and restored normally so existing snapshots round-trip, and code that
references it compiles, but each reference emits a **CE0101** warning to nudge
callers to migrate off.

**`retired`** marks a field gone for practical purposes. Its name stays
reserved (a future field can't reuse it), references are a **CE0102** error,
and the emitter skips it from capture/restore so the store drops the key on the
next save.

The fields stay in the source forever, like gRPC's `reserved`, so a later
schema change can't accidentally recycle a name and collide with old data. The
typical lifecycle is: mark `deprecated`, migrate callers off (a one-shot
data-pump actor copies the value into its replacement), then flip to `retired`
once nobody references it.

## Schema changes

What happens when you change a persisted region's fields between deploys:

| Scenario | Snapshot has | Source has | Result |
|---|---|---|---|
| Added field | old keys only | new field with initializer | Restored fields overwrite; new field keeps its initializer |
| Removed field | `oldKey: value` | (no field) | Shed silently on next save; a single warning logged at first restore |
| Renamed field | `oldName: value` | `newName = 0;` | Treated as remove + add; lossy |
| Type changed | `field: int 5` | `long field = 0;` | Restore aborts (cast throws); field keeps its initializer |

There is no automatic migration. To preserve data across a rename or a type
change, write a one-time helper that reads the old snapshot, transforms it, and
writes the new one before the region is first accessed.

## Limits

- One region instance per type per `ActorSystem`.
- Single-process only; cross-node replication is not supported.
- All region fields emit as public fields on the generated C# class;
  field-level visibility is not exposed.
- Saves run on every writer-handler exit, serialised per region. Coalescing or
  throttling writes is not supported.

## Where to next

A shared region coordinates state. The next chapter,
[Channels](/language/channels/), coordinates *conversations*: typed message
protocols that say which messages an actor must handle, with the compiler
checking your coverage (CE0090) so a protocol can't drift out of sync with its
handlers.

## Related reading

- [Isolation and ownership](/language/isolation/): the `reader`/`writer`
  discipline this chapter builds on, and the ownership table the `shared` row
  comes from
- [Persistence and passivation](/language/persistence/): the snapshot plumbing
  region persistence reuses
- [CE0097](/reference/errors/#ce0097): `use X foo;` referencing an unknown region
- [CE0098](/reference/errors/#ce0098): a `: Persisted` region with no provider
- [CE0100](/reference/errors/#ce0100): borrowing a region value into an actor field
