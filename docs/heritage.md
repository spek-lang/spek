---
title: Where Spek comes from
layout: default
nav_order: 2.5
permalink: /heritage/
description: "Which actor and language ideas Spek borrowed, from which systems, and where they show up in Spek source."
---

# Where Spek comes from

Spek did not invent the actor model. It is a C#-looking language that
takes rules other systems already proved, and turns the ones that can be
checked into compile errors. This page maps which idea, from
where, and where you meet it in Spek.

If you already write Akka.NET, Erlang, Orleans, or Proto.Actor, the
[migration guides](migration/index.md) translate concept for concept. Use this
page when you want the design ancestry rather than a rewrite of your
existing code.

Language-guide chapters that rest on a non-C# idea also open with a short
"Where this comes from" note. Those notes point at one ancestor. This
page puts them next to each other.

## C#: the surface you already know

Spek is aimed at C# developers. File-scoped namespaces,
`using`, `private`-by-default visibility, generics, LINQ, and NuGet all
work as they do in C#. Handler bodies are a [subset of C#](language/csharp-syntax.md),
and Roslyn type-checks the emitted code. A `message` lowers to a C#
`record`. A `module` lowers to a `static class`. A `program` block
lowers to `static async Task Main`.

Matching C# means you should not relearn `for` to get an actor model.
What Spek removes is shared mutable state, `lock`, the `async` / `await`
keywords, and `Task.Run`. The compiler rejects them because those are
the usual ways data races get into a C# program.

## Erlang / OTP: isolation and "let it crash"

An Erlang process owns its heap. Spek's [actors](language/actors.md) own
theirs. The compiler enforces the boundary ([isolation](language/isolation.md))
instead of relying on copy-everything immutability.

[Supervision](language/supervision.md) is Erlang's "let it crash": a
handler throws, the parent chooses Resume, Restart, Stop, or Escalate.
Retry budgets (`maxRetries`, `withinTime`) are the same idea as OTP
supervisor intensity.

Named `behavior` blocks are closer to `gen_statem` states (or to
`gen_server` plus explicit state) than to OTP *behaviours* (the callback
modules). [Modules](language/modules.md) are Erlang's namespacing habit
with C# static-class emission.

The supervisor is not a separate process. The parent actor *is* the
supervisor. That is the main OTP-to-Spek adjustment.

## Akka and Akka.NET: the vocabulary

`ActorRef`, `Tell`, `Ask`, `self`, `sender`, and `become` keep Akka's
names. [Sending messages](language/messaging.md) is that API with two
changes: `Ask` yields the reply value (invisible async supplies the
await), and a handler can reply with `return` as well as with
`sender.Tell(...)`. Both work; `return` is the one that reads like a
method result and is what `Ask` uses to infer the reply type.

`become` is borrowed name-for-name. Persistence's `persist;` is Akka
Persistence's snapshot save, without event sourcing: Spek writes a
whole-actor snapshot and restores fields automatically. Supervision
directives (`Restart`, `Resume`, `Stop`, `Escalate`) match Akka's.

[Clustering](language/clustering.md) follows the Akka Cluster / Erlang
distribution shape: named roots, location transparency for `Tell`, and
at-most-once delivery. Remote `Ask` is not supported.

## Orleans: idle unload

`passivate after <duration>` is grain deactivation: an idle actor drops
its in-memory instance and rematerializes on the next message. Orleans
puts that in config and grain lifetime. Spek puts the idle window in
source next to the actor. Spek does not auto-activate virtual actors.
You still `spawn`, including `SpawnPersistent<T>(key)` when you want a
keyed durable instance.

## Proto.Actor: the nearest library cousin

If you have used Proto.Actor on .NET, Spek feels like that model moved
into the language. `Spawn` keeps its name, `Request` becomes `Ask`,
`ReceiveTimeout` becomes `passivate after`. Proto.Actor is a library, so
the isolation rules are convention. In Spek they are `CE` diagnostics.

## Rust and Pony: share or mutate, not both

[Isolation](language/isolation.md) is Rust's ownership intuition, moved
from lexical borrows to the actor boundary. A value is shared-and-immutable
or owned-and-mutable, never both. `message` fields are a compiler
whitelist. After you `Tell` a value you may not mutate it (CE0085).
Pony's reference capabilities chase the same guarantee with a finer
capability lattice; Spek keeps one rule and one boundary.

Spek is not a borrow checker for locals inside a handler. Inside an
actor you write ordinary C#. The checker fires when something would
cross, or alias across, that boundary.

## Go and CSP: protocols, not pipes

A Spek [`channel`](language/channels.md) is a named protocol: the messages
an actor must handle, checked at compile time (CE0090). That is closer
to session types, Akka Typed `IReceive`, or an Orleans grain interface
than to a Go `chan`. There is no `select`, no channel close, and no
`context.Context` cancellation tree. Go teams who already treat a
goroutine plus an owned channel as an actor recognize the isolation
story. They do not get CSP multiplexing.

## Swift: conversions you write down

[`To` / `TryTo`](language/conversions.md) follow Swift's `as` / `as?`
split: a conversion is either known to succeed or it returns an optional.
C-style `(T)x` casts are rejected (CE0129). The spelling is Spek's. The
discipline is Swift's.

## ReactiveX: stream operators on handlers

[`debounce`, `throttle`, `distinct`](language/streams.md) are ReactiveX
operators attached to `on` handlers, not a separate observable type you
subscribe to by hand. They sit on the mailbox path so backpressure and
identity stay with the actor.

## Gleam, F#, Axum

Gleam is the existence proof that a small, typed actor language can
feel pleasant rather than academic. Spek did not take Gleam's syntax.

F#'s `MailboxProcessor<'Msg>` is an actor-shaped mailbox with DU cases
as messages. Spek splits those cases into separate `message` types and
uses `on` handlers instead of a nested `match` loop. See
[From F#](migration/from-fsharp.md).

Microsoft's experimental Axum explored actor isolation on .NET years
ago. Spek is not Axum, but the same question (can the language stop
shared mutable state on this runtime?) is the one Spek answers with
compile-time checks and a C# emission path.

## Testing and simulation

[`TestProbe`, `ExpectMsg`](language/testing.md) come from Akka's testkit.
Whole-system deterministic simulation (virtual time, single-step
dispatch) is the same idea as Akka's `TestKit` / deterministic dispatcher
and Erlang's Common Test plus `meck`-style control, aimed at making
timeouts and restarts reproducible.

## What Spek left behind

A short list of well-known ideas that are *not* in Spek, so you don't go
looking:

| Idea | Usual home | In Spek |
|------|------------|---------|
| Event sourcing as the persistence model | Akka Persistence | Snapshots only (`persist;`) |
| Everything immutable, always | Erlang terms | Mutable actor fields; immutable messages |
| Typed `ActorRef<T>` / session-typed refs | Akka Typed, Pony | `ActorRef` is untyped; channels check the actor declaration |
| `select` over several waits | Go | Not present |
| HOCON / XML actor config | Akka | Supervision, passivation, and persistence live in `.spek` source |
| Virtual activation | Orleans | Explicit `spawn` |
| Locks as the shared-state tool | C# | Shared regions use a reader/writer lock *inside* one process; they are not a cluster map |

## Related

- [Understanding Spek](understanding-spek.md): who the language is for, and
  the five C# habits that do not carry over.
- [Migration](migration/index.md): concept maps for Akka.NET, Erlang/OTP,
  Proto.Actor, Orleans, F#, and Go.
- [Language guide](language/index.md): the book; look for the "Where this comes
  from" notes at the top of individual chapters.
