---
title: Streams
layout: default
parent: Language
nav_order: 18
permalink: /language/streams/
description: "Shape a flood of inbound messages before a handler runs: chain debounce, throttle, and distinct operators with => between the message pattern and the body."
---

# Streams

A [channel](/language/channels/) pins down *which* messages may cross an
actor's boundary. This chapter is about the message that arrives *too often*.
A mouse fires hundreds of move events a second; a search box emits a keystroke
per character; an at-least-once transport redelivers the same event twice. The
handler doesn't need every one of them. It needs the *latest*, or *one per
window*, or *only the ones that changed*.

You could write that rate-shaping by hand: a field for the last value, a
timer, a comparison at the top of the handler. But that's bookkeeping mixed
into business logic, and every handler that needs it reinvents it. Spek lets
you instead put a **stream operator chain** between the message pattern and the
body. The operators decide *if*, *when*, and *with what* the body runs; the
body stays focused on the one event that made it through.

```
on Search s =>            ← the message pattern (Chapter on Messages)
    debounce(250) =>      ← a stream operator: wait for a 250 ms lull
    {                     ← the body — runs only when the chain emits
        RunSearch(s.query);
    }
```

The shape is the handler you already know from
[Sending messages](/language/messaging/), with one or more `=>` operator steps
spliced in before the body. A handler with no operator steps is exactly the
plain handler from earlier chapters; the chain is opt-in, and adds nothing
when you don't use it.

{: .note }
> **Where this comes from.** `debounce` / `throttle` / `distinct` are ReactiveX
> (Rx) operators. Spek places them in front of a handler rather than over an
> `IObservable`, so the "stream" is just the actor's message flow with the
> operator applied before the body runs.

## A first chain: throttle

Operators live in the `Spek.Streams` namespace, so a stream-shaped handler
starts with `using Spek.Streams;`. That import does double duty: it brings the
operator types into scope *and* makes the lowercase factory functions
(`debounce`, `throttle`, …) callable by their bare names.

Here is a cursor actor that only cares about its position roughly 60 times a
second, no matter how fast the mouse moves:

<!-- spek-test: compile -->
```spek
using Spek.Streams;

message MouseMove(int x, int y);

actor Cursor
{
    int lastX = 0;
    int lastY = 0;

    on MouseMove m =>
        throttle(16) =>
        {
            lastX = m.x;
            lastY = m.y;
        }
}
```

`throttle(16)` emits at most one `MouseMove` per 16 ms window and drops the
rest. Without it, every single move would run the body, taking the actor's
writer lock each time (see [Isolation and ownership](/language/isolation/)).
With it, the body runs at a sane rate and the bound message `m` is whichever
move won the window.

The number is **milliseconds**. Both timer operators also accept a
[`System.TimeSpan`](/language/csharp-syntax/) if you'd rather be explicit:

<!-- spek-test: compile -->
```spek
using Spek.Streams;

message Search(string query);

module Searcher
{
    public void Run(string query) { }
}

actor SearchBox
{
    on Search s =>
        debounce(System.TimeSpan.FromMilliseconds(250)) =>
        {
            Searcher.Run(s.query);
        }
}
```

## The built-in operators

Three operators ship in `Spek.Streams`. Each is a small, self-contained data
filter: it sees messages and decides what to pass on.

| Operator              | Behaviour                                                                     |
|-----------------------|------------------------------------------------------------------------------|
| `debounce(ms)`        | Holds the latest message; emits once the source has been quiet for `ms`.     |
| `throttle(ms)`        | Emits at most one message per `ms` window, dropping the rest (leading edge). |
| `distinct(by: f)`     | Emits only when the key extracted by `f` differs from the previous emit.     |

`debounce` and `throttle` look similar but pull in opposite directions.
**Throttle** is for a steady firehose you want to *sample*: it lets one through
immediately, then ignores the rest of the window, which is good for live
position, progress, or telemetry. **Debounce** is for a burst you want to wait
*out*: it holds the newest value and only emits after the noise stops, good for
"the user finished typing" or "the config stopped changing." Use throttle when you
want regular updates during the storm; debounce when you only want the result
after it settles.

### debounce: wait for the lull

A search box should run the query once the user stops typing, not once per
keystroke. `debounce(250)` swallows every `Search` until 250 ms pass with no
new one, then emits the most recent:

<!-- spek-test: compile -->
```spek
using Spek.Streams;

message Search(string query);

module Searcher
{
    public void Run(string query) { }
}

actor SearchBox
{
    on Search s =>
        debounce(250) =>
        {
            Searcher.Run(s.query);
        }
}
```

### distinct: drop the unchanged

`distinct` deduplicates by a **key** you extract with a lambda (see
[Lambdas](/language/lambdas/)). It emits the first message it ever sees, then
emits again only when the key changes. It's the natural filter for an event
that gets redelivered: an at-least-once transport replaying the same
`ConfigChanged`, a watcher firing repeatedly with identical contents.

`distinct` is generic in *two* type parameters, the message type and the key
type, but you don't spell either one: the compiler types the selector's
parameter with the stream's message type, and C# infers the rest from the
lambda:

<!-- spek-test: compile -->
```spek
using Spek.Streams;

message ConfigChanged(string contents);

actor ConfigWatcher
{
    string current = "";

    on ConfigChanged c =>
        distinct(by: c => c.contents) =>
        {
            current = c.contents;
        }
}
```

The qualified `StreamOperators.distinct<ConfigChanged, string>(by: …)` form
stays legal when you prefer the types on the page.

## Chaining operators

Put more than one `=>` step in front of the body and the operators run **in
declaration order**: each step's output feeds the next, and the body fires only
when the *last* operator emits. Read the chain top to bottom as a pipeline.

This sensor throttles a noisy feed to one sample per 16 ms, then debounces so
the body only runs once the readings settle for 100 ms, then replies with the
value it landed on (the return-to-reply idiom from
[Sending messages](/language/messaging/)):

<!-- spek-test: compile -->
```spek
using Spek.Streams;

message Sample(double value);
message Reading(double value);

actor Sensor
{
    double latest = 0.0;

    on Sample s =>
        throttle(16) =>
        debounce(100) =>
        {
            latest = s.value;
            return new Reading(latest);
        }
}
```

You can mix the timer operators with `distinct`. A search box that waits for a
typing lull *and* skips a query identical to the last one it ran combines both:
`debounce` first to settle the burst, then `distinct` to drop a repeat.

<!-- spek-test: compile -->
```spek
using Spek.Streams;

message Search(string query);

module Searcher
{
    public void Run(string query) { }
}

actor SearchBox
{
    on Search s =>
        debounce(250) =>
        distinct(by: s => s.query) =>
        {
            Searcher.Run(s.query);
        }
}
```

The bound message survives the whole chain. Whatever the last operator chooses
to emit is the message the body sees as `s`, so `s.query` inside the body is
exactly the query that made it through, not some earlier one.

## How it runs

Understanding the runtime shape explains the one rule operators have to follow.

Operators run on the *inbound* path, **before** the message reaches the actor's
writer lock. When an operator decides to emit, it doesn't run your body
directly. It posts a synthetic self-message back into the actor's own mailbox.
That message re-enters dispatch like any other, takes the writer lock, restores
the binding, and runs the body. So the body still runs under the actor lock,
serialized against every other handler, exactly as if no chain were present.
Per-actor invariants from [Isolation and ownership](/language/isolation/) hold
unchanged: no two writer handlers run at once.

Two consequences follow from operators living *outside* the lock:

{: .warning }
> Operators must be **self-contained**. They run before the lock, so they must
> not read actor fields or call actor methods; there's no safe, serialized
> view of the actor's state at that point. Treat an operator as a pure data
> filter over the message: the `distinct` key selector should look only at the
> message (`s => s.query`), never at a field.

Each handler also gets its **own** operator-chain instance, created once when
the actor starts. Two different handlers (even two `on Tick` arms in different
[behaviors](/language/actors/)) never share operator state through the chain.
A debounce timer is private to the one handler it sits in front of.

## Custom operators

The chain accepts any expression that evaluates to a
`Spek.Streams.StreamOperator<T>`, so the built-ins aren't special: you can
write your own in a referenced C# project and call it the same way. Derive from
`StreamOperator<T>`, decide in `OfferAsync` whether to `Dispatch`, and expose a
lowercase factory:

```csharp
// In a referenced C# project.
namespace MyApp.Streams;

using Spek.Streams;

public sealed class StableForOperator<T> : StreamOperator<T>
{
    private readonly TimeSpan _window;

    public StableForOperator(TimeSpan window) => _window = window;

    public override async Task OfferAsync(T message)
    {
        // ...decide whether to emit, then:
        await Dispatch(message);
    }
}

public static class MyOperators
{
    public static StreamOperator<T> stableFor<T>(int ms)
        => new StableForOperator<T>(TimeSpan.FromMilliseconds(ms));
}
```

At the call site it looks just like a built-in. Import the namespace and chain
the factory; because it has a single type parameter the compiler supplies `<T>`
for you, the same as `debounce`:

<!-- spek-test: parse -->
```spek
using MyApp.Streams;

message Tick();

actor Monitor
{
    int n = 0;

    on Tick =>
        stableFor(500) =>
        {
            n = n + 1;
        }
}
```

(That snippet is shown as a fragment because it depends on the external
`MyApp.Streams` project; the built-in examples above are the fully compiled
ones.)

## Testing stream-shaped handlers

`debounce` and `throttle` arm their windows through the system clock, so
under a `TestActorSystem` with `virtualTime: true` they follow the virtual
clock: send the burst, `AdvanceClock` past the quiet window, and the emit
fires deterministically inside the advance, with no sleeps to tune and nothing timing-sensitive to flake. The
[next chapter](/language/testing/) covers the pattern. On a real-time
system, drive the assertion off an observable condition instead:
`TestActorSystem.WaitUntilAsync(() => …)` polls your predicate until it
holds (and throws a descriptive `TimeoutException` if it never does), so
you wait exactly as long as the debounce needs and no longer.

## Next

You've now seen the whole language surface: actors, messages, supervision,
persistence, regions, channels, and the stream operators that shape what
reaches a handler. The [next chapter, Testing actors](/language/testing/),
brings it all under test with `TestActorSystem` to spawn the system,
`TestProbe` with `ExpectMsg`/`ExpectStop`/`ExpectRestart` to assert on what
comes back, and the `…Tests` convention that runs under `dotnet test`.

## Related

- [Channels](/language/channels/): pins down *which* messages cross the
  boundary that streams then rate-shape.
- [Lambdas](/language/lambdas/): the key selector passed to `distinct(by: …)`.
- [Isolation and ownership](/language/isolation/): why the body still runs
  serialized under the actor lock even though operators run outside it.
- [Common pitfalls](/language/footguns/): more on the timer-and-scheduler
  edges to watch for.
