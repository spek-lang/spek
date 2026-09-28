---
title: From Orleans
layout: default
parent: Migration
nav_order: 4
permalink: /migration/from-orleans/
description: "Orleans grains, persistence, and clustering compared with the Spek model."
---

# Spek for Orleans developers

Orleans is **virtual actors**; Spek is **explicit actors**. The biggest
mental shift:

- **Orleans activates grains on demand** when a message arrives at a
  `GrainReference`. The runtime picks a silo, deserializes state, runs
  the message. You don't spawn grains.
- **Spek spawns actors explicitly** and keeps them loaded until you
  `passivate`. The runtime doesn't auto-resurrect: you either have an
  `ActorRef` or you don't.

The stories converge at the edges:

- Orleans' **passivation** (idle deactivation) and Spek's **passivation**
  (idle unload + persist + wake-on-message) have the same basic shape.
- Orleans' **IPersistentState<T>** and Spek's **`persist;` + `on Restore`**
  do the same job.

Where the stories diverge most dramatically is addressing: Orleans routes
by key (`IMyGrain` + string/Guid), Spek routes by `ActorRef` capability.
If you need Orleans-style addressing, you'll build a naming layer on top
of Spek's `ActorSystem.SpawnPersistent(key, ...)`.

## Concept mapping

| Orleans                                        | Spek                                                  |
|------------------------------------------------|-------------------------------------------------------|
| `Grain` / `IGrain`                             | `actor Foo`                                           |
| `IMyGrain` + `IGrainWithStringKey` interface   | `actor Foo` + `system.SpawnPersistent<Foo>(key, ...)` |
| `GrainFactory.GetGrain<IMyGrain>(id)`          | `system.SpawnPersistent<Foo>(id, args)` (always spawns; restores the key's snapshot at spawn) |
| `GrainReference`                               | `ActorRef`                                            |
| `OnActivateAsync(CancellationToken)`           | `on PreStart => { ... }`                              |
| `OnDeactivateAsync(reason, CancellationToken)` | `on PostStop => { ... }`                              |
| Grain deactivation on idle (default ~2 hours)  | `passivate after System.TimeSpan.FromMinutes(30);`                         |
| Grain re-activation on next message            | Automatic: a passivated actor re-materializes on the next `Tell` |
| `[PersistentState]` + `IPersistentState<T>`    | Actor fields + `persist;` + `on Restore(Snapshot s)`  |
| `this.WriteStateAsync()`                       | `persist;`                                            |
| `IGrainObserver` / observables                 | Send a `Subscribe(subscriber)` message; actor tracks subs in a field |
| `StatelessWorker` attribute                    | Not a Spek concept; worker pools are explicit actors   |
| `IClusterClient.Connect()`                     | No dedicated client; a host binds `Cluster` directly |
| Timers / reminders                             | Passivation timer only; no reminder equivalent     |
| Streams (`IStreamProvider`)                    | Stream operators shape a handler's input (`debounce`/`throttle`/`distinct`); no pub/sub stream provider |

## Side-by-side

### A counter grain

**Orleans:**

```csharp
public interface ICounter : IGrainWithStringKey
{
    Task Increment();
    Task<int> Get();
}

public class Counter : Grain, ICounter
{
    private readonly IPersistentState<CounterState> _state;

    public Counter(
        [PersistentState("counter", "store")] IPersistentState<CounterState> state)
    {
        _state = state;
    }

    public async Task Increment()
    {
        _state.State.Count++;
        await _state.WriteStateAsync();
    }

    public Task<int> Get() => Task.FromResult(_state.State.Count);
}

public record CounterState { public int Count { get; set; } }
```

Called as:
```csharp
var counter = client.GetGrain<ICounter>("acct-1");
await counter.Increment();
var n = await counter.Get();
```

**Spek:**

```spek
message Increment();
message Get();
message Count(int value);

actor Counter
{
    int count = 0;

    behavior Active
    {
        on Increment => { count = count + 1; persist; }
        on Get       => sender.Tell(new Count(count));
    }

    on Restore(Snapshot s) => count = s.Get<int>("count");

    passivate after System.TimeSpan.FromMinutes(10);
}
```

Called from code that holds an `ActorRef`:

```csharp
var counter = system.SpawnPersistent<Counter>("acct-1");  // creates or restores
counter.Tell(new Increment());
```

Spek's `SpawnPersistent<T>(key, ...)` reattaches a key to its persisted
state; each spawn is a fresh instance restored from that key's snapshot.
That is key-to-state addressing, not Orleans-style key-to-activation
addressing: there is no `GetGrain` lookup returning the live instance for
a key, which is exactly what the naming layer above would add.

## What Spek adds on top of the Orleans model

- **Explicit behaviors with `become`.** Orleans grains are monolithic;
  Spek actors switch state handlers via `become`.
- **Compile-time immutability check** for message payloads (CE0010).
  Orleans grain calls pass arguments through Grain-Call-Filters with no
  guarantee that the arguments are immutable.
- **Lightweight.** An `ActorRef` in Spek is a reference to a slot,
  local by default. A single-process program carries no silo and no
  cluster plumbing, so small programs have no cold-start cost. Clustering is opt-in packages when you want it.

## Orleans features with no Spek equivalent

- **Distribution depth.** Orleans silos, virtual-actor grains, and
  placement/migration machinery are far deeper than Spek's opt-in
  clustering, which offers remote `Tell`, consistent-hash placement,
  and located actors. Remote `Ask` is not supported.
- **Reminders**: persistent scheduled callbacks. Spek has nothing like
  this; roll your own with a background timer actor.
- **Streams**: no Orleans-style pub/sub stream provider. Spek's
  [stream operators](../language/streams.md) shape a single handler's
  input; they are not a cross-actor streaming system.
- **Transactions across grains**: Orleans has distributed transactions;
  Spek doesn't.

## When to pick which

- **Spek**: single-process or small-cluster workloads, where you want
  compile-time concurrency safety and the language to push back on
  mistakes.
- **Orleans**: distributed workloads, when you need grains to move
  between silos and the framework to handle placement, reminders, and
  cross-cluster activation.

There's nothing wrong with using both. Orleans grains can call into
internal Spek-based ActorSystems for specific tight-loop workloads, or
vice versa.
