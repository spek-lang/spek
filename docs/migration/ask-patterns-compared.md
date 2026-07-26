---
title: Request/reply patterns compared
layout: default
parent: Migration
nav_order: 7
permalink: /migration/ask-patterns-compared/
description: "How Erlang, Akka, Proto.Actor, Orleans, Pony, Go, F#, and Spek model the reply type of a request-reply message exchange."
---

# Request/reply patterns across actor ecosystems

Every actor framework has to answer the same question: **when an actor
replies to a request, how is the caller supposed to know the reply's
type?** The answers span a wide spectrum, from nothing-at-all
(Akka classic), through embedded reply references (Akka Typed, F#, Go),
through method-return-type (Orleans), to no-ask-at-all (Pony). Spek's
`return expr;` inference sits near Orleans' end of that spectrum
but is expressed in handler grammar rather than method signatures.

This page catalogues the options, then summarises how each framework's
choice shapes its ergonomics. Use it as design background reading
before deciding how to model request-reply in your own Spek actors.

## Erlang / Elixir

**Shape:** `handle_call/3` returns a `{reply, Value, State}` tuple.
The caller uses `gen_server:call/2`, which returns `Value`. Types are
runtime-dynamic; optional static checking via Dialyzer + `-spec`
annotations (`-spec get_balance(pid()) -> integer().`) catches
mismatches as warnings, not errors.

```erlang
handle_call(get_balance, _From, #state{balance = B} = S) ->
    {reply, B, S}.

% caller:
Balance = gen_server:call(Pid, get_balance).
```

**Static reply-type link:** Optional (Dialyzer). Compiler alone gives
you nothing.

## Akka classic / Akka.NET classic

**Shape:** `Receive<T>` handlers call `Sender.Tell(reply)` or
`Context.Sender.Tell(...)`. No static link between the request message
and the reply type: the caller specifies what it expects via
`Ask<TReply>(msg)`, and if the actor sends something else, it
dead-letters.

```csharp
Receive<GetBalance>(_ => Sender.Tell(balance));

// caller:
var balance = await actor.Ask<decimal>(new GetBalance());
```

**Static reply-type link:** None. Caller asserts; actor can violate
silently.

## Akka Typed

**Shape:** The request message carries a typed `ActorRef[Reply]`
embedded as a field. Type-checking is strict: the actor can only
respond via that exact `ActorRef`, and the compiler knows its message
type.

```scala
case class GetBalance(replyTo: ActorRef[Balance])

// handler:
case GetBalance(replyTo) => replyTo ! Balance(current)

// caller (Scala Akka Typed):
context.ask(target, GetBalance.apply) {
  case Success(Balance(n)) => ...
}
```

**Static reply-type link:** Yes, via the embedded `ActorRef[Reply]`
generic parameter.

## Proto.Actor

**Shape:** Inside a receiver, `context.Respond(msg)` sends the reply.
From the caller, `context.RequestAsync<TReply>(target, msg)` asserts
the reply type. No static link between the two: the actor can
`Respond` with anything, and a mismatch becomes a runtime cast error.

```csharp
if (ctx.Message is GetBalance) ctx.Respond(balance);

// caller:
var b = await ctx.RequestAsync<decimal>(target, new GetBalance());
```

**Static reply-type link:** None. Pattern is more Akka-classic than
Akka-Typed.

## Orleans

**Shape:** A grain method *is* the request-reply. The return type of
the method is the reply's type; the compiler knows both ends because
both ends type-check against the `IGrain` interface.

```csharp
public interface IBalance : IGrainWithStringKey
{
    Task<decimal> GetBalance();
}

// caller:
decimal b = await client.GetGrain<IBalance>("acct-1").GetBalance();
```

**Static reply-type link:** Yes, enforced by the grain interface.

This is the cleanest end of the spectrum: no separate request-message
type, no embedded reply-ref, no assertion at the call site. The price:
the grain's public surface and its implementation aren't separable.
What the grain exposes is exactly the set of methods on its interface.

## Pony

**Shape:** Actor behaviours are `be foo(x: U32)`: they can take
arguments but cannot return. Full stop. If you want a reply, you
either pass a `Promise[T]` explicitly, or embed a reply-ref in the
arguments and have the callee `tag`-send a message back.

```pony
actor Counter
  var n: U64 = 0
  be increment() => n = n + 1
  be get(p: Promise[U64]) => p(n)
```

**Static reply-type link:** Yes (the `Promise[T]` is typed), but
there is no `ask` primitive. Everything is fire-and-forget, including
replies. The whole language rejects the idea that request-reply
deserves its own construct.

## Go

**Shape:** No actor model, no reply primitive. The convention is to
embed a reply channel in the request struct. The goroutine doing the
work writes to `req.Reply`; the caller reads from that channel.

```go
type Request struct {
    Value int
    Reply chan int
}

// worker:
for r := range requests { r.Reply <- process(r.Value) }
```

**Static reply-type link:** Yes, via the channel's element type
(`chan int`). Not elegant, since each request struct must define its own
reply channel, but statically sound.

## F# (MailboxProcessor)

**Shape:** `AsyncReplyChannel<T>` is embedded in the relevant DU case.
The caller uses `PostAndReply(fun r -> Foo r)`, which internally does
the plumbing to wait on the reply.

```fsharp
type Msg = | GetBalance of AsyncReplyChannel<decimal>

let handle (msg: Msg) =
    match msg with
    | GetBalance reply -> reply.Reply(current)

// caller:
let b = agent.PostAndReply(fun r -> GetBalance r)
```

**Static reply-type link:** Yes, via the DU case's embedded
`AsyncReplyChannel<T>`. Ergonomically it's the same shape as Akka Typed
and Go (request-carries-reply-ref), just with F# syntax.

## Spek (explicit replyTo)

**Shape:** You carry the reply address in the message yourself,
same as Go / F# / Akka Typed. The handler `Tell`s the reply.

<!-- spek-test: ignore; fragment: a bare handler shown without its enclosing actor -->
```spek
message GetBalance(ActorRef replyTo);
message Balance(decimal amount);

on GetBalance g => g.replyTo.Tell(new Balance(current));
```

**Static reply-type link:** Yes; both `GetBalance` and `Balance` are
declared `message` types, and the compiler knows both. But the link
is *informal*: the compiler doesn't cross-check that `GetBalance` is
always replied to with `Balance`.

## Spek (inferred reply from `return`)

**Shape:** The handler body uses `return expr;`. The compiler
infers the reply type from that expression and registers it as
the handler's reply. The matching `ask` call-site picks up that
type automatically.

```spek
on GetBalance => return new Balance(current);

// caller, inside an on handler:
Balance b = wallet.Ask(new GetBalance());
```

**Static reply-type link:** Yes, compiler-inferred. Closest cousin:
Orleans' method-return-type approach. The difference: in Orleans the
public surface *is* the method signatures; in Spek the public surface
is the set of `message` declarations, and `on` handlers live on the
actor. This keeps the actor's public-facing message contract separate
from its internal helper methods and from the reply-type inference.

## Summary

| Framework          | Static reply-type link? | Mechanism                                         |
|--------------------|-------------------------|---------------------------------------------------|
| Erlang / Elixir    | Optional (Dialyzer)     | `{reply, V, S}` tuple; optional `-spec` annotations |
| Akka classic       | No                      | `Sender.Tell` + caller-asserted `Ask<T>`          |
| Akka.NET classic   | No                      | Same as Akka classic                              |
| Akka Typed         | Yes                     | Embedded `ActorRef[Reply]` in request             |
| Proto.Actor        | No                      | `context.Respond` + caller `RequestAsync<T>`      |
| Orleans            | Yes                     | Grain method return type                          |
| Pony               | Yes (no ask primitive)  | Explicit `Promise[T]` passed as argument          |
| Go                 | Yes                     | `Reply chan T` embedded in request struct         |
| F# MailboxProcessor | Yes                    | `AsyncReplyChannel<T>` embedded in DU case        |
| Spek (explicit)    | Yes (informal)          | `message Foo(ActorRef replyTo)` + caller Tell     |
| Spek (inferred)    | Yes (inferred)          | `return expr;` in `on` handler + `ask`            |

## Why Spek chose the inferred reply

Two forces pulled in the same direction:

1. **Orleans' method-return-type is the ergonomic gold standard.** No
   embedded reply-ref to plumb, no caller-side `Ask<T>` type assertion
   that the actor can silently violate: the type link is a single
   source of truth.
2. **Spek has `on` handler grammar, not method grammar.** Lifting
   Orleans' idea into an `on` handler meant keeping the actor's public
   message surface (its `message` declarations and `on` handler set)
   decoupled from its private C# helpers. An actor can have fifty
   private C# methods and expose exactly three `ask`-able handlers,
   and the compiler enforces that separation.

The result: the reply-type story is statically checked (like Akka Typed,
Orleans, Pony, Go, F#), without requiring callers to embed a reply-ref
(Akka Typed, Go, F#), without forcing the public surface to collapse
into a method interface (Orleans), and without losing the ability to
do explicit embedded-reply-ref when you want to model more exotic
patterns (Akka Typed–style "I'll reply *eventually*, maybe from a
different actor"). Pair that pattern with `Tell`, not `.Ask`: an ask
whose handler returns without replying faults the asker immediately.

See [messaging](/language/messaging/) for the runtime semantics of
`ask`, and the individual migration pages
([Akka.NET](/migration/from-akka-net/),
[Erlang/OTP](/migration/from-erlang-otp/),
[Proto.Actor](/migration/from-proto-actor/),
[Orleans](/migration/from-orleans/),
[F#](/migration/from-fsharp/),
[Go](/migration/from-go/)) for framework-specific translations.
