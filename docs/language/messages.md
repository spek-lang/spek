---
title: Messages
layout: default
parent: Language
nav_order: 3
permalink: /language/messages/
description: "Message records and the immutability whitelist: why every message field must be immutable, and the CE0010 check that enforces it."
---

# Messages

In [Actors and behaviors](/language/actors/) you saw that an actor processes
one message at a time and never exposes its fields. That isolation is the whole
point, but it only holds if the *messages* themselves carry no hidden door
back into shared mutable state. This chapter is about that door, and how Spek
nails it shut.

A **message** is the only thing that crosses an actor boundary. You declare one
with the `message` keyword:

<!-- spek-test: compile -->
```spek
message Deposit(decimal amount, string fromUser);
message GetBalance();
message Balance(decimal current);
```

Each `message` compiles to a C# `record` with a primary constructor whose
parameters become read-only properties. `message Deposit(decimal amount, string
fromUser);` emits `public record Deposit(decimal amount, string fromUser);`.
You've already used messages: every `on Deposit d => …` handler and every
`new Deposit(...)` in the tutorial was working with one. Here we look at what
makes a message *valid*, and why the compiler is so strict about it.

{: .note }
> **Where this comes from.** In Erlang, every message is a term, and terms are
> immutable by construction, so the language gets it for free. On .NET, Akka.NET,
> Proto.Actor, and Orleans all *recommend* immutable messages but can't enforce
> it: you can put a `List<string>` in a message and both actors end up sharing
> the same list. Spek's `message` keyword plus the [CE0010](/reference/errors/#ce0010)
> check is that discipline made mandatory by the compiler.

## Why messages must be immutable

Actor isolation frees you from locks only if the value handed from one
actor to another can't smuggle in a shared, mutable reference. If a message
could carry a `List<T>`, the sender and the receiver would both hold a pointer
to the *same* list, and now two actors can mutate the same memory at the same
time, which is exactly the data race the actor model exists to eliminate.

So Spek makes `message` a dedicated keyword and polices it. Two rules follow:

- Only a `message`-declared type may be sent with `Tell` or `ask` (the subject
  of the [next chapter](/language/messaging/)).
- Every field of a `message` must itself be immutable, checked transitively at
  the point of declaration.

This is stricter than "it's a `record`." C# records are only *shallow*
immutable: the record's own properties are read-only, but a property typed
`List<T>` still points at a mutable list. Spek rejects that at the message
declaration, before any actor ever receives it.

## The immutability whitelist

A message field's declared type must be one of:

- A primitive: `bool`, `byte`, `int`, `long`, `double`, `decimal`, `char`, and
  the rest of the numeric family.
- `string`.
- An immutable BCL type: `DateTime`, `DateTimeOffset`, `DateOnly`, `TimeOnly`,
  `TimeSpan`, `Guid`, `Uri`, or `Version`.
- `ActorRef`, an opaque capability handle; passing one lets the receiver reply
  or delegate without exposing any state.
- Another Spek `message` type (checked transitively, so its fields must pass too).
- A Spek [`enum`](/language/enums/) type, immutable by construction.
- A type parameter of a generic message (see [below](#generic-messages)).
- An immutable container from `System.Collections.Immutable`: `ImmutableArray<T>`,
  `ImmutableList<T>`, `ImmutableDictionary<K,V>`, `ImmutableHashSet<T>`, and the
  sorted/queue/stack variants, **provided every type argument is itself on the
  whitelist**.

Here is a message that exercises most of the list at once: an immutable list of
nested messages, an enum, a `Guid`, and a `DateTimeOffset`:

<!-- spek-test: compile -->
```spek
enum Priority { Low, Normal, High }

message LineItem(string sku, int quantity, decimal unitPrice);

message PlaceOrder(
    Guid orderId,
    System.Collections.Immutable.ImmutableList<LineItem> items,
    Priority priority,
    DateTimeOffset placedAt);
```

Anything not on the list is rejected with [CE0010](/reference/errors/#ce0010).
The most common offender is a mutable collection:

<!-- spek-test: ignore -->
```spek
// CE0010 — List<T> is mutable; sender and receiver would share it.
message Broken(System.Collections.Generic.List<string> tags);
```

```text
error[CE0010]: Message 'Broken' field 'tags' has type
'System.Collections.Generic.List<string>', which is not a known immutable type.
Use a primitive, string, DateTime/Guid/etc., a Spek 'message', or a
System.Collections.Immutable.* container.
```

The fix is the immutable counterpart:

<!-- spek-test: compile -->
```spek
message Tagged(System.Collections.Immutable.ImmutableList<string> tags);
```

### Why the read-only interfaces are rejected

`IReadOnlyList<T>`, `IReadOnlyDictionary<K,V>`, and `IEnumerable<T>` *look*
immutable, so it's worth being explicit: Spek rejects them too.

<!-- spek-test: ignore -->
```spek
// CE0010 — the interface is read-only, but the object behind it may not be.
message AlsoBad(System.Collections.Generic.IReadOnlyList<string> tags);
```

The interface only promises that *you* can't mutate through it; the concrete
object behind it can still be a `List<T>` that somebody else keeps mutating
(`someList.AsReadOnly()` is the classic trap). Spek requires the concrete
`Immutable*` type so the value is genuinely frozen, not just frozen from your
viewpoint.

{: .note }
> Arrays are mutable too (`int[]` elements can be reassigned in place), so a
> plain array is never a valid message field. That's the same
> [CE0010](/reference/errors/#ce0010) rule; reach for `ImmutableArray<T>`
> instead. Reflection-driven mutation is a separate concern, handled by the
> hostile-import scanner; see [CE0080](/reference/errors/#ce0080).

## Generic messages

A message can take type parameters, with the same syntax as C# generics:

<!-- spek-test: compile -->
```spek
message Response<T>(T value);
message Envelope<TPayload>(TPayload payload, DateTimeOffset stamped);
```

The immutability check treats a type parameter as "unknown but opaque" and
accepts it: the obligation to pass an immutable concrete type lands at the
construction site, where the actual type is known. (Spek does no generic
type-checking of its own; type parameters lower verbatim to C# and Roslyn checks
them, as [Generics](/language/generics/) covers.) One restriction follows: a
handler cannot be keyed on a generic message
([CE0139](/reference/errors/#ce0139)), so dispatch on a concrete wrapper and
carry the payload inside it.

## Default values

Any field may declare a default with `=`, and callers can then omit it:

<!-- spek-test: compile -->
```spek
message Shutdown(string reason = "normal");
message RetryPolicy(int maxAttempts = 3, decimal backoffSeconds = 0.5m, bool jitter = true);
```

These lower to record primary-constructor defaults verbatim. The default must be
a **literal** (a string, number, `bool`, `char`, or `null`) or a bare name
(such as an enum member). A computed expression like `TimeSpan.FromSeconds(1)`
is *not* a valid field default. Pass it explicitly at the construction site, or
default the field to `null` and compute the fallback in the handler.

## Naming reply messages

A reply message is just a message that a handler sends back in response to a
request. Spek doesn't enforce a naming style; two conventions show up in
practice:

| Convention                                      | Example                                                                  |
|-------------------------------------------------|--------------------------------------------------------------------------|
| **Data-only**: name it after the payload       | `message GetBalance();` replies with `message Balance(decimal current);` |
| **Suffixed**: `…Response`, `…Reply`, `…Result` | `message GetBalance();` replies with `message BalanceResponse(decimal current);` |

Both compile identically. The data-only style reads cleaner at the call site
(`acc.Ask(new GetBalance())` evaluates to a `Balance`); the suffixed style is easier
to grep and matches Akka.NET idioms. Pick one per project and stay consistent.

## Message families: `abstract message`

Sometimes several messages belong to one family and a handler wants to receive
the whole family at once: every cluster event, say, not each variant spelled
out. An `abstract message` declares that family base, and each variant names it:

<!-- spek-test: compile -->
```spek
abstract message ClusterEvent();

message NodeUp(string Node)   : ClusterEvent;
message NodeDown(string Node) : ClusterEvent;

actor Watcher
{
    on ClusterEvent e => { }   // receives NodeUp *and* NodeDown
}
```

A handler keyed on the base receives every variant. It works because the family
lowers to C# record inheritance (`NodeUp` and `NodeDown` are records deriving
from an abstract `ClusterEvent`), and the actor's dispatch is an ordinary C#
type switch, so a `ClusterEvent` arm matches any variant. If you also want to
handle one variant specially, put its handler *first*: the most-specific
matching arm wins, exactly as in a C# `switch`.

```spek
actor Watcher
{
    on NodeDown d     => { }   // NodeDown handled specially...
    on ClusterEvent e => { }   // ...everything else in the family lands here
}
```

An `abstract message` is a dispatch contract, not something you send: you can't
`new` it, only handle it. This is the message-side parallel to a
[`channel`](/language/channels/) and an [`interface`](/language/classes/#interfaces-the-class-contract):
where those are contracts a provider *implements*, a message family is the
contract a handler *receives*. See
[contracts per type](/language/classes/#interfaces-the-class-contract) for how
the three fit together.

The base must be empty: the shared fields live on each variant
([CE0125](/reference/errors/#ce0125)), and the base must be an `abstract message`
([CE0124](/reference/errors/#ce0124)).


## Messages in context

Messages don't live in a vacuum; they're the vocabulary an actor speaks.
Here's the whole loop from [Actors and behaviors](/language/actors/), now with
the message types it depends on declared alongside it:

<!-- spek-test: compile -->
```spek
message Deposit(decimal amount, string fromUser);
message GetBalance();
message Balance(decimal current);

actor Account
{
    decimal balance = 0m;

    on Deposit d  => { balance = balance + d.amount; }
    on GetBalance => return new Balance(balance);
}
```

`d.amount` reads a field off the incoming `Deposit`; `new Balance(balance)`
builds a fresh reply message. Because `Balance` is immutable, returning it can't
leak a handle to the actor's state. The caller gets a frozen snapshot of
`balance`, not a live view of it. That is the isolation guarantee from
[Actors and behaviors](/language/actors/), now extended across the boundary.

## Only a message may be sent

The flip side of the whitelist is the send check: the payload of `Tell` (and the
target of `ask`) must be a declared `message`. Inside a handler, sending a bare
primitive is caught with [CE0020](/reference/errors/#ce0020):

<!-- spek-test: ignore -->
```spek
message Ping();

actor Pinger
{
    ActorRef peer = null;
    on Ping => { peer.Tell(42); }   // CE0020 — 42 is a primitive, not a message
}
```

```text
error[CE0020]: 'Tell' payload must be a declared 'message' type, not a primitive value.
```

Wrap the value in a message instead, as in `peer.Tell(new Ping())`. The
mechanics of `Tell`, `ask`, `sender`, and the return-to-reply idiom are the
subject of the [next chapter](/language/messaging/).

## What compiles to what

| Spek                          | C# output                          |
|-------------------------------|------------------------------------|
| `message Foo(int x);`         | `public record Foo(int x);`        |
| `message Foo<T>(T value);`    | `public record Foo<T>(T value);`   |
| `message Foo(int x = 42);`    | `public record Foo(int x = 42);`   |
| `abstract message Base();`    | `public abstract record Base();`   |
| `message Foo(int x) : Base;`  | `public record Foo(int x) : Base();` |
| `new Foo(1)` at a call site   | `new Foo(1)` verbatim in `.g.cs`   |

## Next

You can now declare messages and trust that the compiler keeps them immutable.
The next step is moving them between actors: [Sending messages: Tell and
Ask](/language/messaging/) covers fire-and-forget `Tell`, request/reply `ask`,
the `sender` reference, and returning a value to reply.
