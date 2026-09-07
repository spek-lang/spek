---
title: "Sending messages: Tell and Ask"
layout: default
parent: Language
nav_order: 4
permalink: /language/messaging/
description: "The two ways to send a message: Tell (fire-and-forget) and Ask (request/reply); plus the sender reference and the return-to-reply idiom that ties a request to its answer."
---

# Sending messages: Tell and Ask

You now have an actor that owns some state ([Actors and
behaviors](/language/actors/)) and a `message` type that can safely cross
the boundary between two of them ([Messages](/language/messages/)). What
you don't have yet is a way to actually *move* a message from one actor to
another. That's this chapter.

There are exactly two send operations, and they map onto the only two
questions you ever ask about a send:

- **`Tell`**: *"deliver this and move on."* Fire-and-forget. The most
  common operation in any Spek program.
- **`Ask`**: *"deliver this and give me the reply back."* Request/reply,
  for the handful of places where the next line of code genuinely cannot
  run until the answer arrives.

Alongside them, two implicit references identify the parties to a send:
**`self`** (the actor doing the sending) and **`sender`** (the actor that
sent the message currently being handled). Together with the
**return-to-reply** idiom, those four pieces are everything there is to
inter-actor communication in Spek.

{: .note }
> **Where these come from.** The `Tell` / `Ask` / `sender` / `self`
> vocabulary is Akka's, and Spek keeps the names. Erlang draws the same
> line between `gen_server:cast` (fire-and-forget) and `gen_server:call`
> (request/reply). Spek adds two ergonomic twists: `Ask`'s result *is* the
> reply, not a `Task<T>` to await by hand, and a handler replies by
> `return`ing a value instead of calling `sender.Tell(...)`.

## `Tell`: fire and forget

`target.Tell(message)` drops a message into `target`'s mailbox and returns
immediately. It is `void`: there is no return value, nothing to await, and
no acknowledgement that the message was ever processed. The sender keeps
running; the recipient handles the message on its own turn, later.

<!-- spek-test: compile -->
```spek
namespace Demo;

message Deposit(decimal amount);
message Withdraw(decimal amount);
message Start();

actor Account
{
    decimal balance = 0.00m;

    init() { become Open; }

    behavior Open
    {
        on Deposit d  => { balance += d.amount; }
        on Withdraw w => { balance -= w.amount; }
    }
}

actor Root
{
    init() { become Idle; }

    behavior Idle
    {
        on Start =>
        {
            ActorRef acc = spawn<Account>();
            acc.Tell(new Deposit(500.00m));
            acc.Tell(new Deposit(250.00m));
            acc.Tell(new Withdraw(100.00m));
        }
    }
}

program Main
{
    var system = new ActorSystem("Demo");
    ActorRef root = system.Spawn<Root>();
    root.Tell(new Start());
    system.AwaitTermination();
}
```

The three `Tell`s above don't block and don't interleave: each one appends
to the account's mailbox, and the account processes them strictly in order,
one at a time, on its own turn. That single-message-at-a-time guarantee is
what makes `balance += d.amount` safe without a lock. The reason why is the subject of the next chapter, [Isolation and ownership](/language/isolation/).

`Tell` is the idiomatic way to communicate in Spek. Reach for it for
*everything* that doesn't strictly need a reply on the very next line.

### Sends are checked when the target is known

Although `ActorRef` itself is untyped, the compiler type-checks a send
whenever it can see where the ref came from. A local or field whose only
origin is a `spawn<T>(...)` has a known concrete actor type, and a `Tell` or
`.Ask` through it of a message that actor handles in **no behavior** is a
compile error ([CE0126](/reference/errors/#ce0126)): that mail could only
ever dead-letter. Messages handled in a *different* behavior than the current
one are fine (that's the [`become`](/language/actors/#become) state machine),
and refs whose origin the compiler can't see (`sender`, refs carried in
message fields) are left to the runtime's dead-letter sink. A target with an
`on any` catch-all accepts anything by construction.

## Replying with a separate message

Because `Tell` returns nothing, a reply can't come back as a return value.
It comes back the only way anything moves between actors: as **another
message**, delivered to a **different handler**.

This is what **`sender`** is for. Inside a handler, `sender` is an
`ActorRef` pointing at whoever sent the message you're currently processing.
To answer them, you `Tell` them back:

<!-- spek-test: compile -->
```spek
namespace Demo;

message GetBalance();
message BalanceResponse(decimal balance);
message Start();

actor Account
{
    decimal balance = 750.00m;

    init() { become Open; }

    behavior Open
    {
        on GetBalance => sender.Tell(new BalanceResponse(balance));
    }
}

actor Root
{
    init() { become Idle; }

    behavior Idle
    {
        on Start =>
        {
            ActorRef acc = spawn<Account>();
            acc.Tell(new GetBalance());
        }

        on BalanceResponse r =>
        {
            Console.WriteLine(r.balance);
        }
    }
}

program Main
{
    var system = new ActorSystem("Demo");
    ActorRef root = system.Spawn<Root>();
    root.Tell(new Start());
    system.AwaitTermination();
}
```

Follow the round trip: `Root` tells the account `GetBalance`; the account's
handler replies with `sender.Tell(new BalanceResponse(...))`; that response
lands in `Root`'s `on BalanceResponse` handler. The request and the reply
are **two separate deliveries**, and the actor's processing loop stays
strictly single-threaded and non-blocking the whole time.

### The sender is implicit

The example above works without anyone passing a "reply to" address around.
That's because every `Tell` written *inside an actor* (in a handler, an
`init`, or a method) automatically stamps the sending actor as the sender.
When `Root` calls `acc.Tell(new GetBalance())`, the account sees `Root` as
its `sender`, so `sender.Tell(...)` routes the answer straight back. This is
the same convention as Akka's `tell`.

The one case with no implicit sender is a send from *outside* any actor: a
`program` block, or C# interop calling `Tell(message)` directly. There, the
sender resolves to the runtime's `NoSender`. Telling `NoSender` is legal but
goes nowhere: the message is dead-lettered with a one-line diagnostic on
stderr, so a lost reply is at least visible rather than silent.

{: .note }
> `sender` is only meaningful while a message is being processed, so it is
> valid **only inside an `on` handler body**. Using it in an `init` or a
> plain method, where there is no "current message," is
> [CE0043](/reference/errors/#ce0043).

## `self`: a reference to the current actor

`self` is an `ActorRef` pointing at the actor that's currently running. You
reach for it whenever you need to hand your own address to someone else: to
register with a coordinator, to subscribe to a stream, or to schedule a
future message back to yourself.

<!-- spek-test: compile -->
```spek
namespace Demo;

message Register(ActorRef worker);
message Start();

actor Worker
{
    ActorRef coordinator;

    init(ActorRef c) { coordinator = c; become Idle; }

    behavior Idle
    {
        on Start => { coordinator.Tell(new Register(self)); }
    }
}

actor Coordinator
{
    init() { become Running; }

    behavior Running
    {
        on Register r => { /* r.worker is the Worker's ActorRef */ }
    }
}

program Main
{
    var system = new ActorSystem("Demo");
    ActorRef coordinator = system.Spawn<Coordinator>();
    ActorRef worker = system.Spawn<Worker>(coordinator);
    worker.Tell(new Start());
    system.AwaitTermination();
}
```

Like `sender`, `self` is valid **only inside an `on` handler body**; using
it elsewhere is also [CE0043](/reference/errors/#ce0043).

## `Ask`: request and reply as one expression

`Tell`-plus-a-handler is the honest default, but it has a cost: the logic
that asks a question and the logic that consumes the answer live in two
different handlers. Sometimes, at a program boundary, in a test, or in a
request-scoped workflow, you genuinely need the reply *before the next line
runs*. That's what `Ask` is for.

`Ask` is a method on the ref, and its value *is* the reply itself: with
invisible async there's no `Task<T>` to await or unwrap.

<!-- spek-test: compile -->
```spek
namespace Demo;

message Deposit(decimal amount);
message GetBalance();
message Balance(decimal amount);
message Start();
message Done(decimal finalBalance);

actor Account
{
    decimal balance = 0.00m;

    init() { become Active; }

    behavior Active
    {
        on Deposit d  => { balance += d.amount; }
        on GetBalance => return new Balance(balance);
    }
}

actor Client
{
    ActorRef account;

    init(ActorRef a) { account = a; become Ready; }

    behavior Ready
    {
        on Start =>
        {
            account.Tell(new Deposit(100.00m));
            account.Tell(new Deposit(50.00m));
            Balance b = account.Ask(new GetBalance());
            sender.Tell(new Done(b.amount));
        }
    }
}

program Main
{
    var system = new ActorSystem("Demo");
    ActorRef account = system.Spawn<Account>();
    ActorRef client = system.Spawn<Client>(account);
    client.Tell(new Start());
    system.AwaitTermination();
}
```

The shape is `target.Ask(new MessageType(args))`. Behind the scenes the compiler
rewrites that into `await target.AskAsync<Balance>(new GetBalance())` and
wraps the surrounding handler in `async` for you. You never write
`Task<T>`, you never write `await`, and you never unwrap a result; that
invisible-async machinery is its own chapter,
[Async without await](/language/async/). The `Balance b = …` line *has*
the reply once it runs.

### `return`: the reply idiom

Notice that the account above answers `GetBalance` with `return new
Balance(balance)`, not `sender.Tell(...)`. Inside an `on` handler, `return
expr;` **is** a reply: the expression is sent back to whoever sent the
message (the compiler lowers it to `sender.Tell(expr)`) and
the handler returns. A handler with no `return` is plain fire-and-forget.

`return` does one extra thing that `sender.Tell` can't: it tells the
compiler what the reply *type* is. At every `account.Ask(new GetBalance())` call
site, the compiler looks across the whole compilation for handlers of
`GetBalance`, finds the `return new Balance(...)`, and infers `Balance` as
the reply type, which is why `Balance b = account.Ask(new GetBalance());` type-
checks with no annotation at the call site. This works across files as long
as they're compiled together.

{: .note }
> Prefer `return new X(...)` over `sender.Tell(new X(...))` when the message
> is genuinely a *reply to the asker*. `return` makes the reply type visible
> to `ask` callers and reads as a direct answer. Reserve `sender.Tell(...)`
> for fan-out or event-style sends where no one is `ask`-ing.

If no handler for a message ever uses `return`, `Ask` can't infer a typed
reply and falls back to `AskAsync<object>`. That's legal, but the caller has to
pattern-match the result. Adding a `return` to the handler is the fix.

### When inference can't decide: explicit `.Ask<T>`

If two different actors both handle the same message but `return` different
reply types, inference can't pick one and again falls back to `object`. You
can settle it at the call site by spelling the reply type out:

<!-- spek-test: compile -->
```spek
namespace Demo;

message Register(ActorRef coordinator);
message Ping();
message FastPong();
message Start();

actor Worker
{
    init() { become Idle; }

    behavior Idle
    {
        on Register r => { }
        on Ping      => return new FastPong();
    }
}

actor Coordinator
{
    ActorRef worker;

    init(ActorRef w) { worker = w; become Running; }

    behavior Running
    {
        on Start =>
        {
            worker.Tell(new Register(self));
            FastPong p = worker.Ask<FastPong>(new Ping());
        }
    }
}

program Main
{
    var system = new ActorSystem("Demo");
    ActorRef w = system.Spawn<Worker>();
    ActorRef c = system.Spawn<Coordinator>(w);
    c.Tell(new Start());
    system.AwaitTermination();
}
```

The explicit `.Ask<FastPong>(…)` form overrides inference. It's also a fine
documentation choice even when inference *would* work: spelling the reply
type out at the call site is never wrong. Inference is the default;
`.Ask<T>` is the opt-in override.

### Rules for `Ask`

- Inside an actor, `Ask` is valid **only in an `on` handler body**. Using
  it from an `init` or a plain method is
  [CE0042](/reference/errors/#ce0042): there is no message turn to suspend
  and resume against. At a program boundary you call the
  runtime's `AskAsync<T>` directly; see
  [Asking from outside an actor](#asking-from-outside-an-actor).
- The target must be an `ActorRef`, and the argument is a `new` of a
  `message` type.
- The payload must be a `message`-declared type, like any send. Handing
  `Tell` or `Ask` a plain C# class is [CE0020](/reference/errors/#ce0020).

### Don't reach for `Ask` by reflex

`Ask` looks synchronous, and that's exactly its hazard: it introduces a
stop-and-wait dependency that gives up some of the pipelining that makes the
actor model fast. The honest default is `Tell` plus a response handler.
Keep `Ask` for the places that need it: test code, program boundaries, and
request-scoped workflows where the reply genuinely *must* land before the
next step.

### Asking from outside an actor

`Ask` lives inside handlers, but at a true program boundary
(`program Main`, an ASP.NET controller, a test) you're not in a handler at
all. There you call the underlying runtime method directly, with an
explicit reply type: the implicit-reply inference is handler-scoped, so a
bare `target.Ask(msg)` at the boundary has nothing to infer from. A
terminal failure (the target is stopped, crashes, or returns without
replying) faults the ask immediately with `AskException`; the
`System.TimeSpan` deadline overload covers the one remaining case, a
handler that hangs mid-turn:

<!-- spek-test: compile -->
```spek
namespace Demo;

message GetBalance();
message Balance(decimal amount);

actor Account
{
    decimal balance = 99.00m;

    init() { become Active; }

    behavior Active
    {
        on GetBalance => return new Balance(balance);
    }
}

program Main
{
    var system = new ActorSystem("Demo");
    ActorRef account = system.Spawn<Account>();
    Balance reply = account.AskAsync<Balance>(new GetBalance(), System.TimeSpan.FromSeconds(3));
    Console.WriteLine(reply.amount);
    system.AwaitTermination();
}
```

Inside the program block the `Task<Balance>` is auto-awaited just like a
handler's `ask`, so `reply` is the `Balance`, not a `Task`. See
[Async without await](/language/async/) for why no `await` appears here.

## What compiles to what

| Spek                                 | C# output                                              |
|--------------------------------------|--------------------------------------------------------|
| `target.Tell(msg)` (inside an actor) | `target.Tell(msg, _selfRef);`                          |
| `target.Ask(new Foo(args))`               | `await target.AskAsync<InferredReply>(new Foo(args))`  |
| `target.Ask<T>(new Foo(args))`            | `await target.AskAsync<T>(new Foo(args))`              |
| `return expr;` (inside an `on` body) | `_sender.Tell(expr, _selfRef); return;`                |
| `self`                               | `_selfRef`                                              |
| `sender`                             | `_sender`                                               |

A `Tell` written inside an actor carries `_selfRef` as the sender argument;
that's the mechanism behind the implicit sender. The
[runtime reference](/reference/runtime/) documents the `ActorRef` API these
lower onto.

## Next

You've now seen actors hold state, messages cross between them, and `Tell` /
`Ask` move those messages around. The promise underneath all of it has been
that `balance += d.amount` is safe with no lock in sight. The next chapter,
[Isolation and ownership](/language/isolation/), is where that promise gets
cashed: how Spek's share-XOR-mutate rule and invisible ownership make data
races a compile error rather than a 3am production incident.
