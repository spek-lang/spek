---
title: Isolation and ownership
layout: default
parent: Language
nav_order: 5
permalink: /language/isolation/
description: "Share-XOR-mutate and invisible ownership: why Spek needs no locks, and the CE0010/CE0085/CE0087 guarantees that make data races a compile error."
---

# Isolation and ownership

You have just seen how actors talk: [`Tell`](/language/messaging/) drops a
message in a mailbox, [`ask`](/language/messaging/) waits for a reply, and
the runtime delivers each message to the receiver one at a time. That
serialization is why handlers don't race. This chapter is about
*why* it is safe, and why you don't write a `lock` in Spek.

If you have written concurrent C#, you know the failure this prevents.
Two threads share a `Dictionary`, one writes while the other reads, and
you get a torn read, a corrupted entry, or an `InvalidOperationException`
deep in a call stack at 3am. The usual fixes (a `lock`, a
`ConcurrentDictionary`, an `Interlocked`) are discipline you have to
remember to apply every time, forever. Spek refuses that situation at
compile time. This chapter is the rule, and the CE codes that enforce it.

{: .note }
> **Where this comes from.** Share-XOR-mutate is Rust's ownership rule, shared
> *or* mutable but never both, applied at actor granularity and checked by the
> compiler (Pony's reference capabilities chase the same guarantee). It's the
> reason Spek needs no locks.

## The one rule: share **XOR** mutate

Spek organises every value in your program around one invariant,
borrowed from Rust and Pony and scoped to the actor boundary:

> At any instant, a value is either **shared and immutable**, held by any
> number of actors, or **owned and mutable**, reachable by exactly one.
> **Never both.**

That is the whole model. A data race needs two things to be true at once:
the data is *reachable from two places*, and the data *can change*.
Forbid those two from coexisting and the race is impossible. There is
nothing left to lock, because there is nothing two threads can fight
over.

If a value can't be written, share it freely; if
it can be written, give it a single owner. The rest of this chapter
names the places each case lives.

## Mutation is fine; *shared* mutation is not

Here is the part that surprises people coming from Erlang or Elixir,
where "everything is immutable" is the rule. Inside an actor, Spek
lets you write ordinary, mutable, C#-shaped code:

<!-- spek-test: compile -->
```spek
message Inc();

actor Counter
{
    int n = 0;

    behavior Idle
    {
        on Inc => { n = n + 1; }
    }
}
```

This does *not* violate share-XOR-mutate. The field `n` is mutable, but
it is *owned*: it lives inside one `Counter` and no other actor can reach
it. Fields are private by construction. There is no syntax to expose
one, and reaching into another actor's state by name is a compile error
(below). The only way to influence another actor is to send it a
message. So `n` sits firmly on the "owned and mutable" side of the line:
exactly one actor can touch it, and the runtime hands that actor one
message at a time, so only one handler ever writes `n` at a time.

This is the deliberate choice that makes Spek feel like C# rather than
Erlang. You keep the familiar, comfortable inside-an-actor programming
model *precisely because* the boundary around the actor is airtight. You
do not pay an immutable-everywhere tax to get the isolation guarantee.

## Invisible ownership

Notice what you did *not* write in that example. There is no `lock`, no
`move`, no `&mut`, no lifetime annotation, no `: Owned` marker. Spek's
ownership is **invisible**: you never spell it out. The compiler infers
who owns a value from where it lives and where it flows, and it speaks up
only when a flow would break the rule.

Most of the time ownership is obvious from the kind of thing you wrote:

| Spek | C# muscle memory | Holds state? | How it stays race-free |
|------|------------------|--------------|------------------------|
| `module` | `static class` | none (stateless) | nothing to race on |
| `message` | `record` | yes, but immutable | immutable ⇒ no writes to race |
| `actor` | — | yes, mutable + owned | the runtime serialises access |
| `shared` | — | yes, mutable + concurrent | a reader/writer lock the compiler manages |

The first two are the *share-freely* kinds. A stateless
[`module`](/language/modules/) has no state to corrupt, and an immutable
[`message`](/language/messages/) can be aliased across any number of
actors with zero races because nobody can write to it. They flow through
your program with no ownership rules and no locks.

The last two are the *mutable* kinds, exactly where the danger would be
in plain C#. An `actor` owns its fields privately, and the runtime
serialises the handlers that touch them. A
[`shared`](/language/shared-regions/) region is mutable state several
actors coordinate on; it has its own reader/writer lock that the
compiler acquires and releases for you, so concurrent reads are free and
writes are exclusive. (Shared regions get their own chapter later; for
now, the point is that even the "several actors reach it" case is made safe
by a lock you never have to write.)

Notice what is missing from the table: there is no kind for "mutable data
that two actors can both reach and both write." That combination *is* the
data race, and Spek has no way to express it.

## Guarantee 1: shared values must be immutable (CE0010)

You met this rule in the [messages chapter](/language/messages/); here is
where it fits the bigger picture. A `message` is the only thing that
crosses an actor boundary. The instant you send one, it becomes reachable
from two actors: the sender and the receiver. By the rule, anything
reachable from two places must be immutable. So Spek requires every field
of every `message` to be *deeply, transitively* immutable, and checks it
at the declaration site:

<!-- spek-test: compile -->
```spek
message PriceUpdate(string symbol, decimal price);   // fine — both fields immutable
```

A mutable container is rejected, because it could be changed *after* the
message is sent: two actors sharing a value that can still change, the
exact thing the rule forbids:

<!-- spek-test: ignore -->
```spek
message Basket(List<string> items);                  // CE0010 — List<T> is mutable
```

An immutable container fixes it:

<!-- spek-test: compile -->
```spek
message Basket(System.Collections.Immutable.ImmutableList<string> items);   // fine
```

This is stricter than C#'s own records, which are only *shallow*-immutable:
a `record` property is read-only but can still point at a mutable
`List<T>`. Spek follows the type all the way down, which is what makes the
"shared ⇒ immutable" half of the rule actually hold. (`IReadOnlyList<T>`
and `IEnumerable<T>` are rejected too: the interface is read-only, but the
underlying object can still be mutated by whoever holds the original
reference.)

Once a value is a `message`, you never have to wonder whether sending
it is safe. The compiler already proved it is.

## Guarantee 2: owned values aren't reachable from outside (CE0012)

The flip side of "shared things are immutable" is "mutable things have one
owner." An [`ActorRef`](/language/messaging/) is a mailbox handle, not a
window into the actor's state, so Spek forbids reading a member off one.
The temptation looks reasonable: you hold a reference to a peer, you want
its balance, you reach for `peer.balance`. That would let two actors read
the same field while a third writes it.

<!-- spek-test: ignore -->
```spek
message Peek();

actor Snooper
{
    ActorRef peer;

    init(ActorRef p) { peer = p; }

    behavior Idle
    {
        on Peek => { var x = peer.balance; }   // CE0012 — can't read into another actor
    }
}
```

> `error[CE0012]: Cannot read member 'balance' on an actor reference;`
> `actors are only reachable via 'Tell' or 'ask'.`

The fix is the messaging you already know: `peer.Ask(...)` for the value and
let *its* handler read its own field under its own serialisation. The
field never leaves its owner.

## Guarantee 3: don't mutate a value after you send it (CE0085)

The first two guarantees fire at *declaration* sites. This one fires
inside a handler, and it is the most Rust-flavored rule in the language.

A message is immutable to its *type*, but ownership is also about *time*.
The moment you `Tell` a value, you have logically handed it to the
receiver, even though in the emitted C# it is just a reference both
actors now hold. If you then mutate that value, you are writing to
something another actor is about to read. Spek tracks the send and rejects
the later write:

<!-- spek-test: ignore -->
```spek
message Update(int v);

actor Forwarder
{
    ActorRef peer;

    init(ActorRef p) { peer = p; }

    behavior Idle
    {
        on Update u =>
        {
            peer.Tell(u);
            u.v = 99;          // CE0085 — 'u' already belongs to 'peer'
        }
    }
}
```

> `error[CE0085]: Cannot mutate 'u' after it was sent via 'Tell'/'ask':`
> `the receiving actor now logically owns this value, so mutating it`
> `(or any alias of it) races. Build a new value and send that instead.`

The diagnostic tells you the fix: a sent value belongs to its receiver, so
don't reach back for it: build a *new* value and send that.

<!-- spek-test: compile -->
```spek
message Update(int v);

actor Forwarder
{
    ActorRef peer;

    init(ActorRef p) { peer = p; }

    behavior Idle
    {
        on Update u =>
        {
            peer.Tell(u);
            peer.Tell(new Update(u.v + 1));
        }
    }
}
```

The analysis follows aliases, too. Stash the same value under another name
(`var w = u;`) and mutating `w` after sending `u` is caught just the same:
they name the same object, and the object has already moved. Reading a
sent value is fine; only *mutation* races.

{: .note }
> This is the half of ownership Rust calls *move*. In Spek you never write
> `move`; sending *is* the move, and the compiler infers the rest. The
> only sendable values are immutable messages, so the rule is narrow.

## Guarantee 4: concurrent readers can't write (CE0087)

By default every `on` handler runs as a *writer*: it is serialised against
every other handler on the same actor, so it may mutate fields freely (as
`Counter.n` did at the top of the chapter). But Spek lets you mark a
read-only handler `reader on X` to opt it into running *concurrently* with
other readers, so many readers can answer queries at once, since none of
them writes. (See [handler modes](/language/actors/#handler-modes) for the
full story.)

That concurrency is only sound if a reader truly never writes. So the
compiler enforces it: a `reader` handler that mutates actor state is
rejected.

<!-- spek-test: ignore -->
```spek
message Tick();

actor Counter
{
    int n = 0;

    behavior Idle
    {
        reader on Tick => n = n + 1;   // CE0087 — a reader can't mutate 'n'
    }
}
```

> `error[CE0087]: 'reader on ...' handler may not mutate actor field 'n'.`
> `Mark this handler 'writer on ...' or move the mutation into a writer arm.`

Put the read on a reader and the write on a writer. The runtime then runs
reads concurrently while keeping the write exclusive:

<!-- spek-test: compile -->
```spek
message Read();
message Reply(int v);
message Inc();

actor Counter
{
    int n = 0;

    behavior Idle
    {
        reader on Read => sender.Tell(new Reply(n));
        writer on Inc  => n = n + 1;   // the write stays exclusive
    }
}
```

This is share-XOR-mutate applied *within* a single actor's own state:
reads may overlap (shared), writes may not (mutate), and CE0087 keeps the
two from colliding.

## The compiler as teacher

Like Rust's borrow checker, Spek's isolation rules surface as specific,
explainable compile errors, not mysterious runtime corruption. Each one
catches a place where a value would have ended up both shared *and*
mutable, and tells you which half of the rule you crossed:

| You wrote | The compiler says | Because |
|---|---|---|
| a mutable field type on a `message` | [CE0010](/reference/errors/#ce0010) | shared values must be immutable |
| reading a member off an `ActorRef` | [CE0012](/reference/errors/#ce0012) | owned values aren't reachable from outside |
| mutating a value after you sent it | [CE0085](/reference/errors/#ce0085) | the value now belongs to its receiver |
| a `reader` handler mutating a field | [CE0087](/reference/errors/#ce0087) | concurrent readers can't write |

The fix is always the same shape: push the value back onto one side of the
line: make the data immutable, or confine it to a single owner.

## How this compares

The actor model is decades old; what is unusual is enforcing isolation in
the *type system* of a statically-typed, C#-family language.

- **Erlang / Elixir** get isolation for free because *everything* is
  immutable and processes share nothing, but you give up mutable,
  familiar in-process code to get it.
- **Akka.NET / Proto.Actor / Orleans** *recommend* immutable messages but
  cannot enforce it: nothing stops you sending a `List<T>` and sharing the
  reference. Isolation is documentation, not a guarantee.
- **Rust** enforces an analogous rule (`Send`/`Sync`, ownership and moves)
  for memory safety across threads in general.
- **Spek** enforces share-XOR-mutate at the actor boundary, in the type
  checker, while keeping mutable C#-shaped code *inside* the actor. You get
  Erlang's isolation guarantee without Erlang's immutable-everywhere tax.

The novel piece is that "send only immutable messages, and don't touch
them afterward" stops being advice you have to remember and becomes a rule
the compiler enforces, the same shift Rust made from "be careful with
pointers" to "the borrow checker won't let you."

## What's next

Isolation guarantees that no two actors corrupt each other's state. But an
actor can still fail *on its own*: a handler throws, an invariant breaks,
a downstream call times out. Because each actor is isolated, that failure
is contained to one actor, which is exactly what makes it safe to *manage*
rather than crash the process. The next chapter,
[supervision and failure](/language/supervision/), shows how a parent
watches its children and decides whether a failed actor should restart,
stop, or escalate.

## Related reading

- [Messages](/language/messages/): the immutability whitelist and the
  CE0010 check, in full.
- [Actors and behaviors](/language/actors/): private fields, the
  per-actor serialisation, and reader/writer handler modes.
- [Sending messages](/language/messaging/): `Tell`, `ask`, `sender`, and
  the `ActorRef` you send through.
- [Shared regions](/language/shared-regions/): the one place mutable state
  is reachable from several actors, behind a managed reader/writer lock.
