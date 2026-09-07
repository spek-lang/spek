---
title: From Proto.Actor
layout: default
parent: Migration
nav_order: 3
permalink: /migration/from-proto-actor/
description: "Concept-by-concept map of Proto.Actor idioms into Spek."
---

# Spek for Proto.Actor developers

Proto.Actor is the closest architectural cousin to Spek. Both are
lightweight, .NET-native, and avoid Akka's props-and-factories ceremony.
The main differences are:

- **Spek is a language, Proto.Actor is a library.** Spek rules are
  compile-time errors; Proto.Actor relies on convention.
- **Virtual actors aren't a Spek concept.** Spek has clustering, but
  Proto's virtual-actor / "Cluster" grain model is closer to Orleans. The nearest Spek analogue is persist + passivate.

## Concept mapping

| Proto.Actor                                | Spek                                                 |
|--------------------------------------------|------------------------------------------------------|
| `IContext` (receive-context)               | Implicit: `sender` and `self` inside handlers |
| `Receive(context => ...)`                  | `on MsgType m => { ... }`                            |
| `Become(OtherReceive)`                     | `become OtherBehavior;`                              |
| `context.Sender`                           | `sender`                                             |
| `context.Self`                             | `self`                                               |
| `context.Send(pid, msg)`                   | `target.Tell(new Msg(...))`                          |
| `context.Request<TReply>(pid, msg)`        | `target.Ask(new MsgType(args))` (expression-valued; inside `on` handlers) |
| `context.Spawn(Props)`                     | `system.Spawn<T>(args)` (top-level) / `spawn<T>(args)` (inside an actor) |
| `Props.FromProducer(() => new Foo())`      | Constructor with args                                |
| `Props.WithSupervisor(strategy)`           | `supervise(child, strategy: OneForOne(...))`         |
| `OneForOneStrategy`                        | `OneForOne(...)`                                     |
| `AllForOneStrategy`                        | `AllForOne(...)`                                     |
| `SupervisorDirective.Resume / Restart / Stop / Escalate` | `FailureDirective.Resume / Restart / Stop / Escalate` |
| `Started` / `Stopping` / `Stopped` messages | `on PreStart` / `on PostStop`                       |
| `ReceiveTimeout` + `context.SetReceiveTimeout(...)` | `passivate after System.TimeSpan.FromMinutes(30);`                |
| `IRootContext` + `ActorSystem`             | `ActorSystem`                                        |
| `PID`                                      | `ActorRef`                                           |
| Tests: `Context.Spawn(...)` + message inspection | `Spek.Testing` (`TestActorSystem`, `TestProbe`, `ExpectMsg<T>`) |

## Side-by-side

### Echo actor

**Proto.Actor:**

```csharp
public class Echo : IActor
{
    public Task ReceiveAsync(IContext context)
    {
        if (context.Message is Ping)
            context.Respond(new Pong());
        return Task.CompletedTask;
    }
}

var system = new ActorSystem();
var pid = system.Root.Spawn(Props.FromProducer(() => new Echo()));
```

**Spek:**

```spek
message Ping();
message Pong();

actor Echo
{
    behavior Listening
    {
        on Ping => sender.Tell(new Pong());
    }
}

program Main
{
    var system = new ActorSystem("echo-demo");
    ActorRef echo = system.Spawn<Echo>();
    echo.Tell(new Ping());
    system.AwaitTermination();
}
```

### Become with state

**Proto.Actor:**

```csharp
public class Switch : IActor
{
    private Receive _current;

    public Switch() { _current = Off; }

    public Task ReceiveAsync(IContext context) => _current(context);

    private Task Off(IContext context)
    {
        if (context.Message is TurnOn) _current = On;
        return Task.CompletedTask;
    }

    private Task On(IContext context)
    {
        if (context.Message is TurnOff) _current = Off;
        return Task.CompletedTask;
    }
}
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

## What the language gives you over the library

- **Compile-time message-immutability check** (CE0010). Proto messages
  are conventional immutability; you can break it.
- **Compile-time `ask`-inside-handler scoping** (CE0042). Proto
  `context.RequestAsync(...)` works anywhere but only makes sense inside
  a receive.
- **Strict `ActorRef` opacity** (CE0012). Proto's `PID` is an opaque
  address; Spek's `ActorRef` goes further, so you can't even see its
  underlying type via compile-time inspection.
- **Dead-letter sink** you can observe and record in tests.

## Where Proto.Actor is ahead

- **Proto.Cluster's virtual-actor grains** with Consul/etcd-backed
  membership. Spek's clustering (`Spek.Cluster` + `Spek.Cluster.Tcp`)
  offers remote `Tell`, static-seed membership, consistent-hash
  placement, and located actors; membership backends and remote `Ask`
  are narrower than Proto's.
- **A gRPC wire transport between actors.** Spek's TCP transport
  speaks its own binary protocol; Spek's gRPC support
  ([hosting](/hosting/grpc/)) exposes channels to external clients
  rather than carrying actor-to-actor traffic.

## A note on mailbox semantics

Both runtimes use single-threaded per-actor dispatch with a FIFO mailbox.
Proto gives you more knobs (bounded mailboxes, custom dispatchers, stash
behavior). Spek's mailbox is unbounded, single-default-dispatcher,
no explicit stash: messages either match an `on` handler or go to
the dead-letter sink.
