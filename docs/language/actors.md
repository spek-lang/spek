---
title: Actors and behaviors
layout: default
parent: Language
nav_order: 2
permalink: /language/actors/
description: "The actor declaration in full: fields, init, named behaviors, become, and the PreStart/PostStop/Restore lifecycle hooks."
---

# Actors and behaviors

You met actors in [Build your first actor](/language/first-actor/): you
declared one, gave it a field and an `on` handler, switched it between two
behaviors with `become`, and never wrote a lock. This chapter is the precise
version of that tour. It covers everything that can appear inside an `actor`
declaration (fields, the `init` constructor, named behaviors, `become`, and
the three lifecycle hooks) and the rules the compiler enforces around each.

The *why* behind the no-locks guarantee is the subject of
[Isolation and ownership](/language/isolation/); the rules for the messages
that flow between actors are in [Messages](/language/messages/). This page is
the practical, complete guide to the actor itself.

{: .note }
> **Where this model comes from.** The actor model is [Hewitt, Bishop, and
> Steiger's 1973 work](https://en.wikipedia.org/wiki/Actor_model), made
> production-grade by Erlang/OTP and brought to the JVM by Akka. If you've
> written a `ReceiveActor` in Akka.NET, a `gen_server` in Erlang, or an
> `IActor` in Proto.Actor, this is the same concept. See the
> [migration guide](/migration/) for a concept-by-concept map.
>
> Actors and `become` are that same Erlang/Akka model: an Erlang process with a
> `receive` loop, or an Akka actor hot-swapping behavior. The difference is that
> you never inherit a base class or wire `Become` by hand. `actor`, `behavior`,
> and `become` are language constructs the compiler checks.

## Anatomy of an actor

An actor is an isolated unit of state and computation. It owns a mailbox,
processes one message at a time, and can only be reached through its
`ActorRef`, never by touching its state directly. Everything that makes up
an actor lives between the braces of its declaration:

<!-- spek-test: compile -->
```spek
namespace Banking;

message Deposit(decimal amount);
message Withdraw(decimal amount);
message GetBalance();
message Balance(decimal amount);

actor BankAccount
{
    decimal balance = 0m;          // field
    string  id      = "";          // field

    init(string accountId)         // constructor
    {
        id = accountId;
        become Open;
    }

    behavior Open                  // named behavior
    {
        on Deposit d  => { balance = balance + d.amount; }
        on Withdraw w => { balance = balance - w.amount; }
        on GetBalance => return new Balance(balance);
    }
}
```

The body of an actor may contain, in any order: field declarations, a single
`init` block, named `behavior` blocks (or bare `on` handlers, covered under the
[implicit `Default` behavior](#the-implicit-default-behavior)), the
lifecycle hooks `on PreStart` / `on PostStop` / `on Restore`, a `term`
disposal block (the teardown counterpart of `init`), and private
helper methods. The sections below cover each of these except `term`,
which works the same way as [its region counterpart](/language/shared-regions/#cleanup-with-term).

A few features visible in larger actors are covered in their own chapters and
only pointed to here: `supervise` declarations belong to
[Supervision and failure](/language/supervision/), `persist` and `passivate`
to [Persistence and passivation](/language/persistence/), and `use` (attaching
a [shared region](/language/shared-regions/)) to its own chapter.

## Visibility

Actors default to `private`, which is assembly-internal in the generated C#,
matching C# conventions. The explicit modifiers are `public`, `internal`,
`protected`, and `abstract`:

<!-- spek-test: compile -->
```spek
namespace Banking;

message Open();

public actor AccountManager   { on Open => { } }
internal actor AuditLogger    { on Open => { } }
```

Fields are *always* private. There is no syntax to expose a field to another
actor, and that is deliberate: the only legal thing to do with an `ActorRef`
is `Tell` it a message or use it with `ask`. State leaves an actor only by
being copied into an outgoing message, never by being read across the
boundary.

### Abstract actors

An `abstract` actor cannot be spawned directly; it exists to be inherited.
A subclass names its base after a colon. Behaviors marked `abstract` on the
base (with an empty body) declare a contract the subclass must fill in with
`override`:

<!-- spek-test: compile -->
```spek
namespace Workers;

message Run();

abstract actor WorkerBase
{
    abstract behavior Running { }
}

actor PrintWorker : WorkerBase
{
    int jobsDone = 0;

    init() { become Running; }

    override behavior Running
    {
        on Run => { jobsDone = jobsDone + 1; }
    }
}
```

The base supplies the shape, the `Running` behavior the runtime can rely on
existing, and the subclass supplies the state and the handlers. Each actor's
fields stay private to that actor, so `jobsDone` lives where it's used, on
`PrintWorker`.

## Fields

Fields hold an actor's mutable state. Each is a type, a name, and an optional
initializer:

<!-- spek-test: compile -->
```spek
namespace Banking;

message Ping();

actor Account
{
    decimal  balance  = 0m;
    bool     isFrozen = false;
    ActorRef? auditLog;

    on Ping => { }
}
```

Because each actor processes one message at a time, fields need no locks and
no `volatile`: there is only ever one thread inside the actor, so reads and
writes can never race. This is the whole reason the actor model exists, and
[Isolation and ownership](/language/isolation/) is where that claim is made
airtight.

A field name is a [soft keyword](/spek-v1-grammar/) position:
`message`, `actor`, `after`, and friends are usable as ordinary field names.

## The `init` block

`init` is the actor's constructor. It runs once, before any message is
processed, and is the place to stash constructor arguments into fields and
declare the starting `become`:

<!-- spek-test: compile -->
```spek
namespace Banking;

message Deposit(decimal amount);

actor BankAccount
{
    decimal balance = 0m;
    string  id      = "";

    init(string accountId, decimal opening)
    {
        id      = accountId;
        balance = opening;
        become Open;
    }

    behavior Open
    {
        on Deposit d => { balance = balance + d.amount; }
    }
}
```

The arguments you give `init` are the arguments you pass when you spawn the
actor, `system.Spawn<BankAccount>("alice", 100m)` here. Give parameters names
distinct from your fields; the convention is a descriptive parameter that
reads naturally at the assignment:

<!-- spek-test: compile -->
```spek
namespace Banking;

message Ping();

actor Account
{
    string id = "";

    init(string accountId)
    {
        id = accountId;
    }

    on Ping => System.Console.WriteLine(id);
}
```

`init` is optional. An actor with no `init` is legal, but then it needs
*some* way to reach a behavior. That brings us to behaviors.

## Behaviors

A **behavior** is a named set of message handlers. An actor can declare many
behaviors, but exactly one is *active* at any moment, and only the active
behavior's handlers can run. This is how an actor changes the way it responds
over its lifetime: a locked account ignores deposits not by checking a flag
in every handler, but by being in a behavior that has no deposit handler.

<!-- spek-test: compile -->
```spek
namespace Banking;

message Deposit(decimal amount);
message FreezeAccount();
message UnfreezeAccount();
message GetBalance();
message Balance(decimal amount);

actor BankAccount
{
    decimal balance = 0m;

    init() { become Open; }

    behavior Open
    {
        on Deposit d       => { balance = balance + d.amount; }
        on FreezeAccount   => { become Frozen; }
        on GetBalance      => return new Balance(balance);
    }

    behavior Frozen
    {
        on UnfreezeAccount => { become Open; }
        on GetBalance      => return new Balance(balance);
    }
}
```

While `Open`, deposits land; `FreezeAccount` switches to `Frozen`, which
declares no `Deposit` handler. A deposit arriving in that state goes
unhandled, and unhandled is observable rather than silent: the runtime
routes the message to the [dead-letter sink](/reference/runtime/#ideadlettersink)
for logging or auditing.

{: .note }
> **Where this comes from.** Named behaviors map closely to Erlang's
> `gen_statem` states and to Akka's FSM extension. Unlike Akka's
> `Become(handlerMethod)`, which takes a delegate, Spek's behaviors are
> first-class declarations: the compiler can enumerate them, name them in
> diagnostics, and reject a transition to one that doesn't exist. If you're
> used to Proto.Actor's `Become(otherReceive)`, this is a compile-time-checked
> version of the same idea.

### Handler bodies

A handler is `on Pattern => Body`. The body is either a block or a single
inline statement terminated with `;`. Both forms appear above. `on
GetBalance => return new Balance(balance);` is the inline form, `on Deposit d
=> { ... }` is the block form. Use whichever reads better; they compile
identically.

### Binding the message

`on Deposit d => ...` binds the incoming `Deposit` to a local named `d`, so
the body can read `d.amount`. For a message you only need to *match*, not
read, like the payload-free `FreezeAccount`, omit the bind and write `on
FreezeAccount => ...`. A handler that should run for *any* unmatched message
is written `on any msg =>`:

<!-- spek-test: compile -->
```spek
namespace Demo;

message Greet(string name);

actor Echo
{
    on Greet g => System.Console.WriteLine("hi " + g.name);
    on any msg => System.Console.WriteLine("unhandled: " + msg);
}
```

### Replying from a handler

Inside an `on` handler, `return expr;` sends `expr` back to whoever used
`ask`, the request/reply idiom you saw in the tutorial. When you need
to send to one specific actor, `sender` is the `ActorRef` of whoever sent the
message currently being handled, and `self` is this actor's own `ActorRef`.
Prefer `return` for a single reply to the asker; reach for `sender.Tell(...)`
or `self.Tell(...)` only for fan-out or for re-queueing work to yourself. The
mechanics of `Tell`, `ask`, `sender`, and `return` are the subject of
[Sending messages](/language/messaging/).

### The implicit `Default` behavior

A single-behavior actor doesn't need to write the `behavior X { ... }`
wrapper at all. Bare `on` handlers at actor scope fold into a synthesised
behavior named `Default`, and the actor starts in it automatically, so the
counter from the tutorial needs neither a `behavior` block nor an `init`:

<!-- spek-test: compile -->
```spek
namespace Demo;

message Inc();
message Get();
message Reply(int value);

actor Counter
{
    int n = 0;

    on Inc => { n = n + 1; }
    on Get => return new Reply(n);
}
```

is exactly equivalent to:

<!-- spek-test: compile -->
```spek
namespace Demo;

message Inc();
message Get();
message Reply(int value);

actor Counter
{
    int n = 0;

    behavior Default
    {
        on Inc => { n = n + 1; }
        on Get => return new Reply(n);
    }
}
```

The name `Default` is fixed, and chosen for readability: it's what appears in
stack traces, supervision messages, and dead-letter logs.

Mixing the two, some bare handlers *and* an explicit `behavior X { ... }`
block, is allowed, but every behavior an actor declares must be reachable.
If the bare handlers fold into `Default` but nothing ever does `become
Default;`, the compiler rejects it as
[CE0014](/reference/errors/#ce0014) ("behavior declared but never reached"):

<!-- spek-test: compile -->
```spek
namespace Demo;

message Inc();
message Reset();

actor Counter
{
    int n = 0;

    init() { become Active; }

    behavior Active
    {
        on Inc => { n = n + 1; become Default; }   // makes Default reachable
    }

    on Reset => { n = 0; become Active; }           // bare → folds into Default
}
```

This is the compiler-as-teacher pattern again: a dead behavior is almost
always a mistake, a `become` you forgot to write, so Spek surfaces it at
build time instead of letting messages quietly fall through to the
dead-letter sink at runtime.

## `become`

`become BehaviorName;` switches the active behavior. The switch is atomic and
takes effect *after* the current handler finishes; the rest of the handler
runs under the old behavior, and the next message is dispatched against the
new one. The target must be a behavior declared on this actor; naming one that
doesn't exist is [CE0011](/reference/errors/#ce0011):

<!-- spek-test: compile -->
```spek
namespace Banking;

message FreezeAccount();

actor Account
{
    bool isFrozen = false;

    init() { become Open; }

    behavior Open
    {
        on FreezeAccount =>
        {
            isFrozen = true;
            become Frozen;
        }
    }

    behavior Frozen { }
}
```

That CE0011 check is the everyday payoff: typo `become Frozn;` and you get a
build error pointing at the bad name, not a runtime surprise three weeks
later. Akka and Proto can't catch this without runtime reflection, because
their transition target is a method handle rather than a named, enumerable
declaration.

{: .note }
> **Where this comes from.** `become` is borrowed name-for-name from Akka. In
> Akka.NET it's `Context.Become(Running)`; in Proto.Actor, `context.Become(
> OtherReceive)`. Making the target a *named behavior* rather than a delegate
> is what lets the compiler validate it.

`become` is legal inside `on` handlers, inside `init`, and inside the
lifecycle hooks below. It is rejected inside plain helper methods, which are
meant to stay free of control-flow side effects
([CE0051](/reference/errors/#ce0051)).

## Lifecycle hooks

Three hooks let an actor run code at fixed points in its life. Each is written
like a handler, `on <Hook> => ...`, and each is optional:

| Hook                  | When it runs                                              |
|-----------------------|-----------------------------------------------------------|
| `PreStart`            | Once, before the first message is processed.              |
| `PostStop`            | Once, after the actor has stopped (graceful or supervised). |
| `Restore(Snapshot s)` | After crash recovery or passivation wake-up.              |

`PreStart` is the place for work that needs the actor alive but hasn't got a
message to hang off: announcing yourself, kicking off a first
`self.Tell(...)`, opening a resource. `PostStop` is the symmetric teardown
point. Both can read and write fields, `become`, and send messages:

<!-- spek-test: compile -->
```spek
namespace Audit;

message Opened(string id);
message Closed(string id);
message Touch();

actor Session
{
    ActorRef auditLog;
    string   id      = "";
    int      touches = 0;

    init(string sessionId, ActorRef log)
    {
        id       = sessionId;
        auditLog = log;
    }

    on PreStart => auditLog.Tell(new Opened(id));
    on Touch    => { touches = touches + 1; }
    on PostStop => auditLog.Tell(new Closed(id));
}
```

`on Restore(Snapshot s)` runs when the runtime brings an actor back, after a
crash-and-restart with persistence, or after a passivated actor is woken. It
hands you a `Snapshot` to read saved state out of:

<!-- spek-test: compile -->
```spek
namespace Banking;

message Deposit(decimal amount);

actor Account
{
    decimal balance  = 0m;
    bool    isFrozen = false;

    on Deposit d => { balance = balance + d.amount; persist; }

    on Restore(Snapshot s) =>
    {
        balance  = s.Get<decimal>("balance");
        isFrozen = s.Get<bool>("isFrozen");
    }
}
```

Most actors don't need to write `on Restore` at all;
persistent actors **auto-restore** their fields, and you only supply the hook
to do something custom. How `persist`, snapshots, passivation, and
auto-restore fit together is the whole of
[Persistence and passivation](/language/persistence/).

## Helper methods

An actor can declare private helper methods to factor logic out of handlers.
They look like C# methods and are emitted as private methods on the generated
class:

<!-- spek-test: compile -->
```spek
namespace Banking;

message Withdraw(decimal amount);
message Rejected();

actor Account
{
    decimal balance = 100m;

    on Withdraw w =>
    {
        if (CanCover(w.amount)) { balance = balance - w.amount; }
        else { sender.Tell(new Rejected()); }
    }

    bool CanCover(decimal amount)
    {
        return amount <= balance;
    }
}
```

Helpers may read and write fields freely, but they are *not* allowed to
`become`, `persist`, or otherwise drive the actor's control flow: those
belong in handlers and `init`. A `become` in a helper is
[CE0051](/reference/errors/#ce0051).

## Inheritance: abstract base actors

Actors follow the same inheritance model as [classes](/language/classes/#inheritance-abstract-base-classes):
**reuse plus abstract methods, and nothing more.** An `abstract actor` is a base
that shares `protected` fields and helper methods with the actors that extend it,
and can declare `abstract` methods each derived actor must implement. There is no
`virtual`/`override` keyword on methods: the emitter infers `override` when a
derived actor's method implements an inherited abstract one. Behaviors are
the exception; replacing an inherited one is spelled `override behavior`.

```spek
message Job(int value);
message Done(int result);

abstract actor Worker
{
    protected int handled = 0;

    public abstract int Transform(int x);
    protected void Bump() { handled = handled + 1; }
}

actor Doubler : Worker
{
    on Job j => { Bump(); return new Done(Transform(j.value)); }
    public int Transform(int x) { return x * 2; }
}
```

`Doubler` reuses the base's `handled` field and `Bump` helper and supplies the
abstract `Transform`. Only an `abstract actor` can be a base
([CE0123](/reference/errors/#ce0123)); a concrete actor is sealed. Abstract
methods are only allowed on an abstract actor ([CE0122](/reference/errors/#ce0122)).
Shared state must be `protected` to be reachable from a derived actor; a private
base field stays encapsulated.

This is for **implementation reuse**, not for sharing a message protocol; that's
what a [`channel`](/language/channels/) is for, and an actor can do both:
`actor Doubler : Worker, JobApi`. A derived actor still declares its own
behaviors and handlers; inheritance shares fields and methods, not the dispatch
table.

## What compiles to what

A concrete Spek actor becomes a **sealed** C# class deriving from the runtime's
actor base (or from its base actor); an `abstract actor` becomes an `abstract`
class, extendable but never instantiated. Fields become private fields on that
class: `protected` when shared with derived actors; each behavior becomes a
dispatch arm keyed on the currently-active behavior; `become` swaps that key;
`init` becomes the constructor body; and the lifecycle hooks become overrides the
runtime calls at the right moments. The mechanics of message delivery, what
`Tell` and `ask` actually do, are the next chapter.

## Next

- [Messages](/language/messages/): the message record and why it must be
  immutable, the rule that makes passing state between actors safe.
- [Sending messages: Tell and Ask](/language/messaging/): `Tell`, `ask`,
  `sender`, and the return-to-reply idiom in full.
- [Isolation and ownership](/language/isolation/): *why* an actor's fields
  need no locks, the one principle the whole language falls out of.
- [Supervision and failure](/language/supervision/): what happens when a
  handler throws, and how a parent decides a child's fate.
