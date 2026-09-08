---
title: From F#
layout: default
parent: Migration
nav_order: 5
permalink: /migration/from-fsharp/
description: "From F# MailboxProcessor agents to Spek actors."
---

# Spek for F# developers

F# already gives you an actor-ish primitive in the standard library:
`MailboxProcessor<'Msg>`. If you've written `MailboxProcessor.Start (fun
inbox -> ...)` with a discriminated union for the message type, the jump
to Spek is mostly a syntactic one. The underlying idea (one mailbox,
one processing loop, state threaded forward) is the same.

The conceptual shifts are:

- **DU cases become individual `message` declarations.** F# leans on the
  compiler's exhaustiveness check over a closed DU; Spek separates each
  case into its own named message type and dispatches via `on` handlers.
- **Reply channels become either `sender.Tell` or inferred `return`.**
  F#'s `AsyncReplyChannel<T>` embedded in the DU case is the shape
  Akka Typed calls "embedded response reference". Spek supports the same
  pattern explicitly (`message Get(ActorRef replyTo)`), and also the
  Orleans-style inferred reply via `return expr;` inside `on`;
  see [inferred reply](#inferred-reply).
- **Tail-recursive loop with state becomes actor fields + `become`.**
  F# threads state through `loop newState` calls; Spek mutates fields
  on the actor and switches handler sets via `become`.
- **Supervision, persistence, passivation are framework features.**
  `MailboxProcessor` has none of these; you build them yourself, or
  reach for Akkling / FSharp.Akka. Spek bakes them into the language.

{: .note }
> **Already using Akkling or FSharp.Akka?** Those are F# DSLs over
> Akka.NET. The [Akka.NET migration page](from-akka-net.md) is
> the right starting point, and everything there applies to you.

## Concept mapping

| F# (MailboxProcessor)                                    | Spek                                                       |
|----------------------------------------------------------|------------------------------------------------------------|
| `type Msg = \| A \| B of int \| C of AsyncReplyChannel<T>` | one `message` declaration per case                       |
| `MailboxProcessor.Start(fun inbox -> loop init)`         | `actor Foo` + `init()` + starting `behavior`               |
| `let rec loop state = async { ... }`                     | actor fields + `behavior` block (no explicit loop)         |
| `let! msg = inbox.Receive()` + `match msg with ...`      | `on MsgType m => { ... }` handlers                         |
| Exhaustive DU match (`match msg with`)                   | `on` handlers per message + `on any msg =>` catch-all      |
| `return! loop (n + 1)`                                   | mutate a field: `count = count + 1;`                       |
| State switch (different `loop` function)                 | `become OtherBehavior;`                                    |
| `actor.Post(Increment)` (fire-and-forget)                | `actor.Tell(new Increment());`                             |
| `actor.PostAndReply(fun r -> Get r)`                     | `actor.Ask(new Get())` inside an `on` handler (inferred reply)  |
| `reply.Reply(value)` inside handler                      | `return value;` or `r.replyTo.Tell(new Reply(v));`         |
| `inbox.Scan(fun msg -> ...)`                             | not a Spek concept; no mailbox stash                |
| `async { ... }` computation expression                   | handler body is implicitly `async`; `ask` auto-awaits      |
| Unhandled exception crashes the `MailboxProcessor`       | `OnFailure(Exception, object)` + parent supervision        |
| No persistence                                           | `persist;` + `on Restore(Snapshot s)`                      |
| No idle-unload                                           | `passivate after System.TimeSpan.FromMinutes(30);`                              |
| Tests: call `Post` + `PostAndReply` directly             | `Spek.Testing` (`TestActorSystem`, `TestProbe`)            |

## Side-by-side

### A counter actor

**F# (MailboxProcessor):**

```fsharp
type Msg =
    | Increment
    | Get of AsyncReplyChannel<int>

let counter = MailboxProcessor.Start(fun inbox ->
    let rec loop n = async {
        let! msg = inbox.Receive()
        match msg with
        | Increment   -> return! loop (n + 1)
        | Get reply   -> reply.Reply(n); return! loop n
    }
    loop 0)

counter.Post(Increment)
counter.Post(Increment)
let current = counter.PostAndReply(fun r -> Get r)
```

**Spek:**

```spek
message Increment();
message Get();
message Count(int value);

actor Counter
{
    int count = 0;

    behavior Tracking
    {
        on Increment => { count = count + 1; }
        on Get       => return new Count(count);   // inferred reply
    }
}

program Main
{
    var system = new ActorSystem("counter");
    ActorRef counter = system.Spawn<Counter>();
    counter.Tell(new Increment());
    counter.Tell(new Increment());
    Count current = counter.Ask<Count>(new Get());
    Console.WriteLine(current.value);
    system.AwaitTermination();
}
```

### Become-based state: toggle switch

**F# (MailboxProcessor):**

```fsharp
type Msg = TurnOn | TurnOff

let switchAgent = MailboxProcessor.Start(fun inbox ->
    let rec off () = async {
        let! msg = inbox.Receive()
        match msg with
        | TurnOn  -> return! on ()
        | TurnOff -> return! off ()
    }
    and on () = async {
        let! msg = inbox.Receive()
        match msg with
        | TurnOff -> return! off ()
        | TurnOn  -> return! on ()
    }
    off ())
```

**Spek:**

```spek
message TurnOn();
message TurnOff();

actor Switch
{
    init() { become Off; }

    behavior Off { on TurnOn  => { become On; } }
    behavior On  { on TurnOff => { become Off; } }
}
```

### Request / reply

**F# (AsyncReplyChannel in a DU):**

```fsharp
type BalanceMsg =
    | Deposit of decimal
    | GetBalance of AsyncReplyChannel<decimal>

let wallet = MailboxProcessor.Start(fun inbox ->
    let rec loop balance = async {
        let! msg = inbox.Receive()
        match msg with
        | Deposit amt       -> return! loop (balance + amt)
        | GetBalance reply  -> reply.Reply(balance); return! loop balance
    }
    loop 0m)

wallet.Post(Deposit 100m)
let b = wallet.PostAndReply(fun r -> GetBalance r)
```

**Spek (inferred reply):**

```spek
message Deposit(decimal amount);
message GetBalance();
message Balance(decimal amount);

actor Wallet
{
    decimal balance = 0m;

    behavior Active
    {
        on Deposit d    => { balance += d.amount; }
        on GetBalance   => return new Balance(balance);
    }
}

// ... elsewhere, inside an on handler ...
Balance b = wallet.Ask(new GetBalance());
```

**Spek (explicit Akka Typed style):**

```spek
message Deposit(decimal amount);
message GetBalance(ActorRef replyTo);
message Balance(decimal amount);

actor Wallet
{
    decimal balance = 0m;

    behavior Active
    {
        on Deposit d  => { balance += d.amount; }
        on GetBalance g => g.replyTo.Tell(new Balance(balance));
    }
}
```

The explicit form is a drop-in for F#'s embedded `AsyncReplyChannel<T>`:
same pattern, different syntax for the embedded reply address.

## Inferred reply

`return expr;` inside an `on` handler makes the
handler's reply type inferred from the returned expression, and the
matching `ask` call-site picks up that type automatically. This lands
F#'s `AsyncReplyChannel<T>`-in-a-DU pattern as a grammar-native
construct:

```spek
on GetBalance => return new Balance(balance);
// caller:
Balance b = wallet.Ask(new GetBalance());
```

See the [messaging reference](../language/messaging.md) for the full
`ask` semantics, and the
[request/reply patterns comparison](ask-patterns-compared.md)
for how this stacks up against Erlang, Akka, Orleans, Proto.Actor, etc.

## Exhaustive match → catch-all + dead-letter routing

F# will warn (or error, depending on settings) if your `match msg with`
doesn't cover every DU case. Spek gives you the same safety via a
different mechanism: any message that doesn't match an `on` handler in
the current behavior goes to the runtime's dead-letter sink by default.
To handle everything-else-inside-one-behavior, use the catch-all pattern:

```spek
actor Logger
{
    behavior Listening
    {
        on LogInfo i  => Console.WriteLine(i.text);
        on LogError e => Console.Error.WriteLine(e.text);

        on any msg =>
            Console.WriteLine($"dropped: {msg.GetType().Name}");
    }
}
```

This is less strict than F#'s exhaustiveness check (a new `message`
type won't force you to add a handler), but the dead-letter default
gives you observability that the F# process-crash-on-unmatched doesn't.
If you want process-wide visibility across every actor's unmatched
messages, wire a custom `IDeadLetterSink` on the `ActorSystem`.

## What Spek adds over MailboxProcessor

- **Real actor hierarchies.** A `MailboxProcessor` has no parent / no
  children. Spek gives you `spawn<T>()` inside actors, and
  `OnChildFailure` runs up the tree.
- **Supervision.** `MailboxProcessor` crashes the way any unhandled
  `async` exception crashes: the loop exits, the agent dies, silently,
  unless you wrapped every handler in `try/with`. Spek's `supervise`
  declaration makes failure policy explicit and declarative.
- **Persistence.** `persist;` + `on Restore` is not something you roll
  yourself over MailboxProcessor; it's a runtime feature.
- **Passivation.** Idle-unload for rarely-used actors.
- **Compile-time message immutability** (CE0010). F# records are
  immutable by default, but a DU case holding a `ResizeArray<int>` or
  a mutable record isn't. Spek rejects that at compile time.
- **A dead-letter sink** that tests can record and assert against.

## What you lose leaving F#

- **DU-native pattern matching on message payloads.** In F# you can
  `match msg with | Deposit amt when amt > 100m -> ...`. Spek's message
  types are each their own type. Discriminating on payload happens
  inside the C# body of the handler, with C# pattern-matching syntax.
- **Computation-expression ergonomics.** F#'s `async { ... }` /
  `task { ... }` let you pipeline work inside a handler in a way that
  C# syntax (which is what Spek handler bodies use) can feel clumsier
  at. `ask` mitigates this for request-reply but doesn't give you the
  full CE story.
- **Currying / partial-application style.** Spek handler bodies are C# statements. There's no point-free chain-of-functions shape.
- **The DU itself as a type.** Spek has no sum type; each `message` is
  its own class. You can't pattern-match over "the whole message
  surface"; only over individual `on` handlers.
- **`MailboxProcessor.Scan`** for peeking without consuming. Spek has
  no mailbox stash.


See the [language reference](../language/index.md) for the full grammar and the
[runtime reference](../reference/runtime.md) for the `ActorRef` API these
lower onto.
