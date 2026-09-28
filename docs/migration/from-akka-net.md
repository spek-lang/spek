---
title: From Akka.NET
layout: default
parent: Migration
nav_order: 1
permalink: /migration/from-akka-net/
description: "Akka.NET idioms mapped to their Spek equivalents, concept by concept."
---

# Spek for Akka.NET developers

If you've written a `ReceiveActor`, you already know ~80% of Spek. Most of
the concepts survive unchanged in name. What moves is:

- **Configuration becomes syntax.** Where Akka.NET configures via HOCON and
  `Props.Create<T>()`, Spek expresses intent directly in `.spek` source.
- **Rules move from runtime to compile time.** Akka.NET catches mutable
  message payloads during code review or when threading bugs surface;
  Spek makes them a compile error (CE0010).
- **Supervision lives in the Spek source**, declared per-child.
  This is more granular than Akka.NET's one-strategy-for-all-children
  model; see the [supervision notes below](#supervision).

## Concept mapping

| Akka.NET                                                    | Spek                                                |
|-------------------------------------------------------------|-----------------------------------------------------|
| `class Foo : ReceiveActor`                                  | `actor Foo`                                         |
| `Receive<Ping>(msg => ...)`                                 | `on Ping p => ...` inside a `behavior` block        |
| `Become(Running)` / `Become(OtherState)`                    | `become Running;` (same keyword)                    |
| `Context.Sender`                                            | `sender`                                            |
| `Self`                                                      | `self`                                              |
| `target.Tell(msg)`                                          | `target.Tell(new Msg(...))`                         |
| `target.Ask<Reply>(msg)`                                    | `target.Ask(new MsgType(args))` (expression-valued; inside `on` handlers) |
| `ActorRef`                                                  | `ActorRef` (same name)                              |
| `Props.Create<Foo>(args)` + `Context.ActorOf(props)`        | `spawn<Foo>(args)` inside an actor; `system.Spawn<Foo>(args)` at top level |
| `Context.ActorOf(Props.Create<Foo>(...), "name")`           | `system.SpawnNamed<Foo>("name")` for wire-addressable roots |
| `SupervisorStrategy` override + exception-type match arms   | `supervise OneForOne(on Failure(ExType): Action, ...)`; same top-to-bottom arms, plus optional per-child `supervise(child, strategy: ...)` overrides |
| `OneForOneStrategy(maxNrOfRetries, withinTimeRange)`        | `OneForOne(on Failure: Restart, maxRetries: N, withinTime: System.TimeSpan.FromSeconds(N))` |
| `PreStart()`                                                | `on PreStart => { ... }`                            |
| `PostStop()`                                                | `on PostStop => { ... }`                            |
| `PersistentActor` + `Recover<Snapshot>(s => ...)`           | `on Restore(Snapshot s) => { ... }`                 |
| `SaveSnapshot(state)`                                       | `persist;` (snapshots all fields)                   |
| `Context.SetReceiveTimeout(TimeSpan)`                       | `passivate after System.TimeSpan.FromMinutes(30);`                       |
| `Akka.TestKit` + `TestProbe` + `ExpectMsg<T>`               | `Spek.Testing` + `TestProbe` + `ExpectMsg<T>`       |
| `DeadLetterActorRef` / dead-letter observation              | `IDeadLetterSink` (with `RecordingDeadLetterSink` for tests) |
| `Context.Stop(actor)`                                       | Return `Stop` from `OnFailure` or `OnChildFailure`  |

## Side-by-side

### Declare an actor

**Akka.NET:**

```csharp
public class Greeter : ReceiveActor
{
    public Greeter()
    {
        Receive<Greet>(g => Console.WriteLine($"Hello, {g.Name}"));
    }
}

var greeter = system.ActorOf(Props.Create<Greeter>(), "greeter");
greeter.Tell(new Greet("world"));
```

**Spek:**

```spek
message Greet(string name);

actor Greeter
{
    behavior Listening
    {
        on Greet g => Console.WriteLine($"Hello, {g.name}");
    }
}

program Main
{
    var system = new ActorSystem("hello");
    ActorRef greeter = system.Spawn<Greeter>();
    greeter.Tell(new Greet("world"));
    system.AwaitTermination();
}
```

### Become-based state machines

**Akka.NET:**

```csharp
public class Switch : ReceiveActor
{
    public Switch() { Off(); }

    private void Off()
    {
        Receive<TurnOn>(_ => Become(On));
    }

    private void On()
    {
        Receive<TurnOff>(_ => Become(Off));
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

### Persistence

**Akka.NET:**

```csharp
public class Wallet : ReceivePersistentActor
{
    public override string PersistenceId => "wallet-1";
    private decimal balance = 0m;

    public Wallet()
    {
        Command<Deposit>(d =>
        {
            Persist(d, applied =>
            {
                balance += applied.Amount;
            });
        });
        Recover<SnapshotOffer>(o => balance = (decimal)o.Snapshot);
    }
}
```

**Spek:**

```spek
message Deposit(decimal amount);

actor Wallet
{
    decimal balance = 0m;

    behavior Active
    {
        on Deposit d =>
        {
            balance += d.amount;
            persist;              // snapshot all fields
        }
    }

    on Restore(Snapshot s) =>
        balance = s.Get<decimal>("balance");
}
```

## Supervision

Spek matches Akka.NET's supervision model feature-for-feature, with
near-identical semantics. Akka.NET uses exception-type matching on one
parent-wide strategy:

```csharp
protected override SupervisorStrategy SupervisorStrategy() =>
    new OneForOneStrategy(
        maxNrOfRetries: 10,
        withinTimeRange: TimeSpan.FromMinutes(1),
        decider: Decider.From(ex => ex switch
        {
            ArithmeticException _ => Directive.Resume,
            NullReferenceException _ => Directive.Restart,
            _ => Directive.Escalate,
        }));
```

The Spek equivalent is a `supervise` declaration on the parent actor:

```spek
supervise OneForOne(
    on Failure(System.ArithmeticException): Resume,
    on Failure(System.NullReferenceException): Restart,
    on Failure: Escalate,
    maxRetries: 10,
    withinTime: System.TimeSpan.FromMinutes(1));
```

Arms match top-to-bottom, first match wins, the same convention as Akka.
Spek adds a compile-time check (**CE0081** / **CE0082**) that catches
unreachable arms (typed arm after catch-all, duplicate catch-all,
duplicate typed arm) at compile time. Akka silently accepts these,
and the dead arms never fire at runtime.

Spek also offers a per-child override form Akka doesn't have directly:

```spek
supervise OneForOne(on Failure: Restart);               // default
supervise(hot, strategy: OneForOne(on Failure: Stop));  // narrow a specific child
```

Two children of the same class can have different policies. This
composes with exception-type matching: both the default and each
per-child override can declare its own arm list.

## What Spek adds that Akka.NET doesn't have

- **CE0010**: message fields must be immutable types at compile time.
  Akka.NET catches this in code review or at runtime when threading
  bugs surface.
- **CE0012**: you can't call a method on an `ActorRef` other than `Tell`
  or `ask`. No more accidentally reaching through to a child's field.
- **CE0020**: `Tell(someString)` is a compile error because `string`
  isn't declared with `message`. Messages must be explicit records.
- **CE0080**: `using System.Reflection;` is a compile error. The
  immutability guarantees can't be bypassed at runtime (`interop using` is the explicit, visible opt-out).

## What Akka.NET has that Spek doesn't

- **Akka.Cluster's depth.** Spek has opt-in clustering (remote `Tell`,
  consistent-hash placement, located actors; see
  [Clustering](../language/clustering.md)), but Akka.Cluster's membership
  protocols, cluster singletons, and distributed data are a far larger
  surface, and remote `Ask` is not supported in Spek.
- **Akka.Streams**: reactive streams on top of actors. Spek's
  [stream operators](../language/streams.md) shape a single handler's
  input; there is no graph-based streaming DSL.
- **Akka.FSM**: a dedicated finite-state-machine actor base class.
  Spek's `behavior` + `become` covers the core use-case; see
  [03_become.spek](https://github.com/spek-lang/spek/blob/develop/src/Spek.Tests/Fixtures/03_become.spek).
