---
title: Migration
layout: default
nav_order: 7
has_children: true
permalink: /migration/
description: "Spek for developers arriving from Akka.NET, Erlang/OTP, Proto.Actor, Orleans, F#, and Go."
---

# Migrating to Spek

Spek didn't invent the actor model. It borrowed what works from three decades
of prior art (Erlang/OTP, Akka, Proto.Actor, Orleans, Microsoft's experimental
Axum) and added compile-time guarantees that those ecosystems can't offer
retroactively.

If you're arriving from one of those systems, the concepts will feel familiar.
The syntax is a little different, the constraints are stronger (shared mutable
state is a compile error, not a style preference), and the runtime has less
ceremony than Akka's props-and-factories pattern.

Each of the pages below maps its source ecosystem onto Spek concept by concept.
Start with the one that matches your background.

The [Akka.NET guide](/migration/from-akka-net/) is for the highest-overlap
audience: most names survive unchanged (`ActorRef`, `Tell`, `become`,
`Restart`), and the main change is that configuration moves into the language.
[Erlang / OTP](/migration/from-erlang-otp/) developers will recognise almost
everything, with two adjustments worth knowing up front: the supervisor isn't a
separate process, and behaviors are closer to `gen_server` states than to
behaviours. [Proto.Actor](/migration/from-proto-actor/) is the closest cousin,
where `Request` becomes `ask`, `Spawn` keeps its name, and `ReceiveTimeout`
becomes `passivate after`.

The remaining three cross a wider gap. [Orleans](/migration/from-orleans/) is a
different model (virtual actors) with an overlapping crowd; Spek does explicit
supervision rather than auto-activation, with passivation as a comparable
idle-unload story. For [F#](/migration/from-fsharp/), `MailboxProcessor<'Msg>`
is an actor-ish primitive whose DU cases map to individual `message`
declarations and whose `AsyncReplyChannel<T>` maps to Spek's embedded-replyTo or
its inferred reply. [Go](/migration/from-go/) is the farthest reach: goroutines
and channels aren't actors, but many Go teams have already converged on the
pattern, so that page reads as much like a translation guide as a concept map,
and it's honest about what `select` can't translate.

One page cuts across all of these. [Request/reply patterns across actor
ecosystems](/migration/ask-patterns-compared/) shows how Erlang, Akka classic,
Akka Typed, Proto.Actor, Orleans, Pony, Go, F#, and Spek each model the
reply-type link between request and response. It's useful framing before you
decide how to model request-reply in your own Spek actors.

### Pages at a glance

| Coming from                    | Closest Spek analogue                                              | Biggest shift                                     |
|--------------------------------|--------------------------------------------------------------------|---------------------------------------------------|
| Akka.NET                       | 1:1 on most names (`ActorRef`, `Tell`, `become`, `Restart`)        | Config moves from HOCON into `.spek` source       |
| Erlang / OTP                   | `actor` = `gen_server` + `gen_statem`; behaviors = named states    | Supervisor is the parent actor, not a separate one |
| Proto.Actor                    | Drop-in cousin; `Request` → `ask`, `Spawn` keeps its name          | Language, not library; rules are compile errors   |
| Orleans                        | `SpawnPersistent<T>(key)` maps keyed grain activation              | Explicit actors, not virtual ones                 |
| F# (MailboxProcessor)          | `actor` + fields + `behavior`; DU cases → `message` declarations   | No DU-native pattern match at message level       |
| Go (goroutines + channels)     | `actor` = goroutine + owned channel, but addressable & supervised  | No `select`, no channel close, no `context.Context` |

The same ancestry, written as a design map rather than a rewrite guide, is
[Where Spek comes from](/heritage/).

## Design-heritage philosophy

Where Spek does something that looks identical to another framework, it's
deliberate: Spek defaults to matching established actor-framework
conventions (Akka, OTP, Proto.Actor) and reserves novelty for restrictions
that reinforce its compile-time guarantees.

Where Spek does something novel, that's its distinct value: `CE0010` (messages
must be immutable at compile time), `CE0080` (no reflection imports), the strict
`ask`-inside-on scoping. Each restriction is defensible, because it closes a bug
class that other frameworks can only catch in code review or runtime assertions.

Nothing in Spek surprises you for surprise's sake. If a name or syntax shape
looks different from Akka, OTP, Proto, or Orleans, there's a specific
compile-time reason behind it, and the relevant CE code explains what it is.
