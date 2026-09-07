---
title: Channels
layout: default
parent: Language
nav_order: 17
permalink: /language/channels/
description: "Channels: named message protocols an actor implements, with compile-time coverage checks (CE0090) so no declared input goes unhandled."
---

# Channels

Up to now, the set of messages an actor handles has lived entirely
inside the actor: you write an `on` handler, and that handler *is* the
contract. Nothing outside the actor states which messages it is supposed
to accept, and nothing checks that you covered them all. Forget a handler
and the message just becomes a [dead letter](/language/messaging/) at
runtime, with no compile error.

A **channel** lifts that contract out into a named declaration. It lists
the inputs an actor must handle and the events it may emit, and the
compiler verifies that any actor claiming the channel has a handler for
every declared input. Channels are how you say *"this actor speaks the
ConsoleHost protocol"* and have the compiler hold you to it.

This is the same idea as an interface in C#, a `Receive` type in Akka.NET,
or a grain interface in Orleans, a structural promise checked at compile
time. The difference is that a channel references the
[message records](/language/messages/) you already declared rather than
defining new types, and reply types stay inferred from your handlers
rather than being restated.

{: .note }
> **Where this comes from.** A `channel` is a *typed protocol*, closer to CSP /
> Go channels and session types than to a bare mailbox: it names the messages an
> actor must handle, and the compiler checks the actor covers them all (CE0090).

## Declaring a channel

A channel lists its inputs with `on MessageType;`, the same `on` keyword
that introduces a handler, but with no body. Each name must be a message
that already exists; a channel never declares a payload of its own.

An actor opts in with the colon syntax you already know from
[base actors](/language/actors/): `actor Foo : MyChannel`. The compiler
then checks that `Foo` handles every input the channel lists.

<!-- spek-test: compile -->
```spek
message Shutdown();
message Reboot();

channel ConsoleHost
{
    on Shutdown;
    on Reboot;
}

actor MyApp : ConsoleHost
{
    behavior Running
    {
        on Shutdown => { }
        on Reboot   => { }
    }
}
```

Delete the `on Reboot` handler and the build stops with **CE0090**:

```
error[CE0090]: Actor 'MyApp' implements channel 'ConsoleHost' but has
no 'on Reboot' handler. Every channel input (including inherited) must
be covered by at least one behavior handler.
```

The coverage check looks across *all* of the actor's behaviors: a
handler in any behavior satisfies the channel. The channel cares that the
message is handled somewhere, not which behavior is current.

{: .note }
> A channel is a **protocol contract, not a type**. It references existing
> messages by name and never defines a payload. To change what a `Shutdown`
> carries, edit the `message Shutdown` declaration, and the channel keeps
> referring to it by name, untouched.

## Multiple channels, shared handlers

An actor can implement several channels at once. List them after the
colon, comma-separated. When two channels declare the same input, one
handler covers both, exactly like a single C# method implementing two
interfaces that share a signature.

<!-- spek-test: compile -->
```spek
message Audit();
message Ping();

channel Audited  { on Audit; }
channel Pingable { on Ping;  }

actor Worker : Audited, Pingable
{
    behavior Idle
    {
        on Audit => { }
        on Ping  => { }
    }
}
```

If `Audited` and `Pingable` both declared `on Ping`, the single
`on Ping` handler above would satisfy both.

## Reply types stay on the handlers

A channel says what an actor *accepts*, not what it *replies*. There is
deliberately no `on Ping returns Pong;` in the grammar. The reply type is
whatever a handler returns, inferred the same way [Ask](/language/messaging/)
infers it from a `return` statement:

<!-- spek-test: compile -->
```spek
message Ping();
message Pong();

channel Pingable { on Ping; }

actor Echo : Pingable
{
    behavior Idle
    {
        on Ping => { return new Pong(); }
    }
}
```

`Echo` satisfies `Pingable` because it handles `Ping`. That the handler
*also* replies with a `Pong` is the handler's business; the channel
neither knows nor needs to. Keeping replies on the handler avoids
restating the reply type in two places and letting them drift apart.

There is also no typed `ActorRef<Pingable>`. An [`ActorRef`](/language/messaging/)
stays untyped. Channel verification is entirely actor-side, at the
declaration.

## Emitting events with `emits`

Inputs cover the request side. The other half of a protocol is the
*events* an actor produces unprompted: a price tick, a status change, a
disconnect. A channel declares those with `emits`:

<!-- spek-test: compile -->
```spek
message Subscribe();
message PriceChanged(decimal price);

channel Ticker
{
    on Subscribe;
    emits PriceChanged;
}

actor StockTicker : Ticker
{
    behavior Live
    {
        on Subscribe => { sender.Tell(new PriceChanged(101.5m)); }
    }
}
```

`emits PriceChanged;` is the contract for what the actor may push back
through `sender` without it being a reply. The compiler enforces that
contract with **CE0092**, which gates exactly one shape: a
`sender.Tell(...)` of a message that is *neither* the handler's inferred
reply type *nor* in any implemented channel's `emits` list:

| Call | Allowed? | Why |
|------|----------|-----|
| `self.Tell(X)` | yes | internal message pump, never gated |
| `someRef.Tell(X)` | yes | outbound to another actor |
| `sender.Tell(X)` where `X` matches `return new X();` | yes | it's the reply |
| `sender.Tell(X)` where `X` is in `emits` | yes | it's a declared event |
| `sender.Tell(X)` otherwise | **CE0092** | undeclared push to the caller |

So sending `PriceChanged` above is fine, since it's in `emits`. Send something
that *isn't*, and the build stops:

<!-- spek-test: ignore -->
```spek
message Subscribe();
message PriceChanged(decimal price);
message Unrelated();

channel Ticker
{
    on Subscribe;
    emits PriceChanged;
}

actor Leaky : Ticker
{
    behavior Idle
    {
        on Subscribe => { sender.Tell(new Unrelated()); }   // CE0092
    }
}
```

```
error[CE0092]: 'sender.Tell(new Unrelated())' in actor 'Leaky' is not
the handler's inferred reply type and is not declared in any implemented
channel's 'emits' list. Use 'return new Unrelated();' if it's a reply,
add 'emits Unrelated;' to a channel, or emit 'emits any;' to opt out of
strict enforcement.
```

The fix is in the message itself: either `Unrelated` *is* the reply (make
it a `return`), or it's a genuine event (add `emits Unrelated;` to the
channel). CE0092 only fires for actors that implement at least one
channel. An actor with no channel has no contract to violate, so its
`sender.Tell` calls are unrestricted.

### `emits any`: the escape hatch

Some ports are inherently open-ended: a diagnostics endpoint that may
push back arbitrary dumps, or a debug channel. For those, `emits any;` opts
the implementing actors out of strict `emits` checking entirely:

<!-- spek-test: compile -->
```spek
message EnableDiagnostics();
message HeapDump();

channel DiagnosticsPort
{
    on EnableDiagnostics;
    emits any;
}

actor Diagnostics : DiagnosticsPort
{
    behavior Idle
    {
        on EnableDiagnostics => { sender.Tell(new HeapDump()); }
    }
}
```

With `emits any;` present, CE0092 is suppressed for the whole actor.
Reach for it only when strict typing actively gets in the way. It's an advisory hatch, not the default.

## Composing channels through inheritance

Channels can build on other channels with the same colon syntax actors
use. A derived channel inherits every input and `emits` from its bases,
transitively, and adds whatever it declares directly.

<!-- spek-test: compile -->
```spek
message Shutdown();
message Reboot();
message StatusChanged(string status);

channel HostBase
{
    on Shutdown;
    emits StatusChanged;
}

channel ServerHost : HostBase
{
    on Reboot;                 // additive — Shutdown is inherited
}

actor WebServer : ServerHost
{
    behavior Running
    {
        on Shutdown => { sender.Tell(new StatusChanged("stopping")); }
        on Reboot   => { }
    }
}
```

An actor implementing `ServerHost` must cover **both** `Reboot` (declared
directly) and `Shutdown` (inherited); CE0090 walks the full inheritance
graph. The inherited `emits StatusChanged;` lifts into the derived
contract too, so the `sender.Tell(new StatusChanged(...))` above passes
CE0092 even though `StatusChanged` is declared one level up.

A channel may list several bases. Diamonds linearize cleanly: when more
than one ancestor contributes the same input, it appears once in the
flattened set, and the actor needs only one handler for it.

<!-- spek-test: compile -->
```spek
message Shutdown();

channel Top                  { on Shutdown; }
channel Left   : Top         { }
channel Right  : Top         { }
channel Bottom : Left, Right { }

actor Diamond : Bottom
{
    behavior Idle { on Shutdown => { } }
}
```

`Bottom` reaches `Shutdown` through both `Left` and `Right`, but `Diamond`
satisfies the whole graph with a single `on Shutdown` handler.

Inheritance is strictly **add-only**: a derived channel can extend its
bases but cannot override an inherited input, hide one, or refine a reply
type. It is also single-compilation-unit: there is no implicit root
channel, and inheritance resolves within one project, so shared bases are
copied in rather than referenced across assemblies.

## A worked example

A channel is worth writing when an actor's protocol spans several behaviors.
Here a connection accepts `Open` and `Close` and may emit a `Disconnected`
event, all stated once in the channel; the `Opened` reply stays on the
handler's `return`, as replies always do. The actor
[become](/language/actors/)s between states:

<!-- spek-test: compile -->
```spek
message Open();
message Close();
message Opened(string id);
message Disconnected(string reason);

channel Session
{
    on Open;
    on Close;
    emits Disconnected;
}

actor Connection : Session
{
    bool active = false;

    behavior Closed
    {
        on Open =>
        {
            active = true;
            become Active;
            return new Opened("conn-1");
        }
        on Close => { }
    }

    behavior Active
    {
        on Open  => { }
        on Close =>
        {
            active = false;
            sender.Tell(new Disconnected("client closed"));
            become Closed;
        }
    }
}
```

CE0090 sees `Open` and `Close` handled across the two behaviors and is
satisfied. The `Opened` reply rides through `return` (CE0092 treats it as
the reply type), and `Disconnected` is allowed because the channel
declares `emits Disconnected;`. Drop any one of those and the compiler
names the exact gap.

## The compile-time checks at a glance

Channel diagnostics all fire per-violation, so a single build surfaces
every gap at once:

- **[CE0090](/reference/errors/#ce0090)**: a channel input has no handler
  on the implementing actor (walks the inheritance graph).
- **[CE0091](/reference/errors/#ce0091)**: an unknown name in an actor's
  colon list, or a second *actor* where only one base actor is allowed.
- **[CE0092](/reference/errors/#ce0092)**: `sender.Tell(X)` where `X` is
  neither the handler's reply type nor in any channel's `emits` list.
- **[CE0093](/reference/errors/#ce0093)**: a channel inherits from an
  unknown name, or from a `message`/`actor`/`enum` that isn't a channel.
- **[CE0094](/reference/errors/#ce0094)**: circular channel inheritance
  (reported once per channel in the cycle).

## Where channels go next

A channel pins down *which* messages cross the boundary. The
[next chapter, Streams](/language/streams/), is about *how* a flood of
inbound events is shaped before a handler runs (debounce, throttle,
distinct), turning a firehose of channel inputs into a manageable rate.

## Related reading

- [Messages](/language/messages/): the immutable records a channel
  references by name.
- [Sending messages: Tell and Ask](/language/messaging/): where reply
  types are inferred from handler returns.
- [Actors and behaviors](/language/actors/): `become`, the colon-base
  syntax channels reuse, and multi-behavior coverage.
