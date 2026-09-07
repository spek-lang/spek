---
title: Build your first actor
layout: default
parent: Language
nav_order: 1
permalink: /language/first-actor/
description: "Learn Spek by building one small program that grows from a counter into a bank, one concept per step."
---

# Build your first actor

In [Getting started](/getting-started/) you compiled and ran a `.spek`
file. Now we'll use that same loop to *learn the language*, by building
one small program and watching it grow. We start with a counter, turn it
into a bank account, and let the compiler stop us whenever we get ahead of
ourselves. By the end you'll have used every load-bearing idea in Spek:
actors, messages, `Tell`, `ask`, behaviors, `become`, and `spawn`.

This is the "learn by doing" tour. The chapters that follow it,
[Actors and behaviors](/language/actors/) and
[Messages](/language/messages/) and so on, are the precise, complete
treatment of each piece. Here we just build, and link out when you want
the full story.

> Every complete program below compiles. With the toolchain from
> [Getting started](/getting-started/), copy one into a `.spek` file and
> run it with `spekc compile` + `dotnet run`.

## A program that does nothing

Every runnable Spek program needs an entry point: a `program` block. Start
with the smallest thing that compiles and runs.

<!-- spek-test: compile -->
```spek
namespace Tutorial;

program Main
{
    var system = new ActorSystem("tutorial");
    system.AwaitTermination();
}
```

`ActorSystem` is the runtime that hosts your actors. `AwaitTermination()`
blocks the main thread until the system shuts down. Run this and… nothing
happens, which is correct: we haven't created any actors yet. Let's fix
that.

## Our first actor

An **actor** is an isolated unit of state and computation. It owns some
private fields, and it reacts to **messages**, and nothing else can touch it.
Here is a counter:

<!-- spek-test: parse -->
```spek
actor Counter
{
    int n = 0;

    on Inc => { n = n + 1; }
}
```

Read it top to bottom: `Counter` has one mutable field `n`, and one
handler that says "when an `Inc` message arrives, add one to `n`." The
`on Pattern => Body` form is how actors react to messages, and you'll write a
lot of these.

Notice what we *didn't* write. There's no `behavior` block and no `init`.
A single-behavior actor needs neither: bare `on` handlers fold into an
implicit behavior, and the actor starts ready to receive. We'll meet
behaviors and `init` later in this chapter, and the
[Actors and behaviors](/language/actors/) chapter covers them in full.

But `Inc` isn't defined yet. If we try to compile, the compiler tells us
so: `Inc` has to be a declared **message**.

## Messages: the only thing that crosses the boundary

You can't call a method on an actor or read its fields from outside. The
only way to interact with one is to send it a message, and not just any
type. Only types declared with the `message` keyword may be sent. Let's
declare `Inc`:

<!-- spek-test: parse -->
```spek
message Inc();
```

That's it: a message with no payload. A `message` compiles to an
immutable C# `record`. The immutability isn't decoration: it's what makes
sending safe. Once a value crosses into another actor, two actors can see
it, so [shared values must be immutable](/language/isolation/). The
compiler enforces that for you; more in the
[Messages](/language/messages/) chapter.

Now wire it up in `Main`. We **spawn** the actor to get an `ActorRef`, a
handle we can send to, and `Tell` it some messages:

<!-- spek-test: compile -->
```spek
namespace Tutorial;

message Inc();

actor Counter
{
    int n = 0;

    on Inc => { n = n + 1; }
}

program Main
{
    var system = new ActorSystem("tutorial");
    ActorRef counter = system.Spawn<Counter>();

    counter.Tell(new Inc());
    counter.Tell(new Inc());
    counter.Tell(new Inc());

    system.AwaitTermination();
}
```

`Tell` is fire-and-forget: it drops the message in the actor's mailbox and
returns immediately. The actor processes its mailbox one message at a
time, in order, so `n` ends up at `3`. There's no lock anywhere, and there
doesn't need to be: `n` is private to `Counter`, and only one message is
handled at a time. That single guarantee is what
[isolation](/language/isolation/) gives you.

## Getting a value back with `ask`

Our counter counts, but we can't see the result. `Tell` returns nothing.
It's one-way. When you genuinely need a reply, use `ask`.

First the handler has to *produce* a reply. Inside an `on` handler,
`return expr;` sends `expr` back to whoever asked, and the reply is itself
a message:

<!-- spek-test: compile -->
```spek
message Inc();
message GetCount();
message Count(int value);

actor Counter
{
    int n = 0;

    on Inc      => { n = n + 1; }
    on GetCount => return new Count(n);
}
```

The `on GetCount` handler uses the single-statement form (`=> statement;`
instead of `=> { ... }`) and returns a `Count` message carrying the current value. Now
we can `ask`:

<!-- spek-test: compile -->
```spek
namespace Tutorial;

message Inc();
message GetCount();
message Count(int value);

actor Counter
{
    int n = 0;

    on Inc      => { n = n + 1; }
    on GetCount => return new Count(n);
}

program Main
{
    var system = new ActorSystem("tutorial");
    ActorRef counter = system.Spawn<Counter>();

    counter.Tell(new Inc());
    counter.Tell(new Inc());

    Count c = counter.Ask<Count>(new GetCount());
    System.Console.WriteLine(c.value);      // prints 2

    system.AwaitTermination();
}
```

That one line rewards a closer look. `counter.Ask<Count>(new GetCount())` is an
expression, not a method call: it sends `GetCount` and evaluates to the reply.
You never wrote `await`, because in Spek the `await` is invisible, and
[Async without await](/language/async/) explains why there are no
`async`/`await` keywords at all.

The `<Count>` names the reply type. From inside an actor handler Spek can
usually infer that type from the handler's `return`, so you drop it; the
`program` block sits outside any actor, so here we name it explicitly. Either
way the reply is typed and the `await` stays out of sight (see
[Sending messages](/language/messaging/) for inference versus the explicit
override). Reach for `ask` when a reply *must* arrive before you continue. The
everyday idiom is still `Tell` plus a handler for the response, so `ask` belongs
mainly at program boundaries and in tests
([when to reach for it](/language/messaging/#dont-reach-for-ask-by-reflex)).

## Reacting to state: behaviors and `become`

Counters are fine, but real actors change *how* they respond over time.
Say our counter can be locked; while locked, deposits should be ignored.
We *could* thread an `if` through every handler, but Spek has something
better: named **behaviors**. An actor can declare several, exactly one is
active at a time, and `become` switches between them.

Let's grow the counter into something bank-like:

<!-- spek-test: compile -->
```spek
namespace Tutorial;

message Deposit(decimal amount);
message Lock();
message Unlock();
message GetBalance();
message Balance(decimal amount);

actor Account
{
    decimal balance = 0m;

    init() { become Open; }

    behavior Open
    {
        on Deposit d  => { balance = balance + d.amount; }
        on Lock       => { become Frozen; }
        on GetBalance => return new Balance(balance);
    }

    behavior Frozen
    {
        on Unlock     => { become Open; }
        on GetBalance => return new Balance(balance);
    }
}
```

Now there are two behaviors. In `Open`, deposits land and `Lock` flips us
to `Frozen`. In `Frozen` there's no `Deposit` handler at all. A deposit
that arrives while frozen isn't handled. It isn't silently lost
either; it goes to the
[dead-letter sink](/reference/runtime/#ideadlettersink). `Unlock` flips
back.

Two constructs are new here. `init()` runs once when the actor is spawned, the
constructor, and we use it to pick the starting behavior with `become Open;`.
`become Frozen;` then switches the active behavior atomically, *after* the
current handler finishes. Its target must be a behavior declared on this actor.

That last rule is a compile error, not a runtime surprise. Typo the target
and you get a build error pointing at the bad name:

<!-- spek-test: ignore -->
```spek
behavior Open
{
    on Lock => { become Frozn; }   // error[CE0011]: actor has no behavior named 'Frozn'
}
```

[CE0011](/reference/errors/#ce0011) catches it before the program ever
runs. A typo that would be a runtime surprise in other actor frameworks is
a build-time error here.

`init` and `become` get their full treatment, including
[lifecycle hooks](/language/actors/#lifecycle-hooks), in the next chapter.

## A handler that decides

Handlers aren't limited to one line. Inside `=> { ... }` you write ordinary
C# statements, so a handler can branch, reply to the `sender`, and bail out
early with a bare `return;`. Let's add withdrawals that refuse to overdraw:

<!-- spek-test: compile -->
```spek
namespace Tutorial;

message Deposit(decimal amount);
message Withdraw(decimal amount);
message GetBalance();
message Balance(decimal amount);
message Rejected(decimal requested, decimal available);

actor Account
{
    decimal balance = 0m;

    on Deposit d  => { balance = balance + d.amount; }
    on Withdraw w =>
    {
        if (w.amount > balance)
        {
            sender.Tell(new Rejected(w.amount, balance));
            return;
        }
        balance = balance - w.amount;
    }
    on GetBalance => return new Balance(balance);
}
```

Two things to note. `sender` is an implicit `ActorRef` to whoever sent the
current message, valid only inside an `on` handler. And the bare `return;`
ends the handler without producing an `ask` reply. Here it's how we say
"rejected, nothing more to do." The C# you can write inside a body is
covered in [C# syntax in bodies](/language/csharp-syntax/).

## Actors creating actors

The last core idea: actors can `spawn` other actors. An actor that spawns a
child becomes its *supervisor*: if the child crashes, the parent decides
what happens (see [Supervision and failure](/language/supervision/)). Let's
add a `Bank` that opens accounts and hands their references back:

<!-- spek-test: compile -->
```spek
namespace Tutorial;

message Deposit(decimal amount);
message GetBalance();
message Balance(decimal amount);
message OpenAccount();
message AccountOpened(ActorRef account);

actor Account
{
    decimal balance = 0m;

    on Deposit d  => { balance = balance + d.amount; }
    on GetBalance => return new Balance(balance);
}

actor Bank
{
    on OpenAccount =>
    {
        ActorRef child = spawn<Account>();
        return new AccountOpened(child);
    }
}

program Main
{
    var system = new ActorSystem("tutorial");
    ActorRef bank = system.Spawn<Bank>();

    AccountOpened opened = bank.Ask<AccountOpened>(new OpenAccount());
    ActorRef account = opened.account;

    account.Tell(new Deposit(100m));
    account.Tell(new Deposit(50m));

    Balance b = account.Ask<Balance>(new GetBalance());
    System.Console.WriteLine(b.amount);     // prints 150

    system.AwaitTermination();
}
```

`spawn<Account>()` creates a child `Account` under the current actor and
returns its `ActorRef`. We hand that ref back to the asker inside an
`AccountOpened` message. An `ActorRef` is allowed as a message field
because it's a handle, not mutable state, so passing actor references
around is safe.

## What you just learned

In one short program you used every load-bearing concept in Spek:

- **`actor`**: an isolated unit of state and computation.
- **`message`**: the immutable type that's the only thing allowed to
  cross an actor boundary.
- **`Tell`**: fire-and-forget send.
- **`.Ask`**: request a typed reply (inferred from the handler's `return`
  inside actors, or named with `.Ask<T>` at the program boundary), with the
  `await` made invisible.
- **`behavior` + `become`**: change how an actor responds over time, with
  typo-proof transitions.
- **`spawn`**: actors creating and supervising other actors.

That's the core. Next is the full treatment of actors and behaviors:

- [Actors and behaviors](/language/actors/): the full treatment of `init`,
  behaviors, `become`, lifecycle hooks, and visibility.

Then continue through:

- [Messages](/language/messages/): the immutability whitelist and why it
  matters.
- [Sending messages](/language/messaging/): `Tell`, `ask`, and `sender`
  in full, and when to reach for each.
- [Isolation and ownership](/language/isolation/): *why* all these rules
  exist, the one principle the whole language falls out of.
