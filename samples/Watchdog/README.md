# Watchdog: supervision policies and passivation

A `Foreman` supervises two workers and a cache, and the run demonstrates how
a parent decides a failing child's fate — declaratively.

The Foreman's `supervise` declaration is the default policy: a typed arm
restarts children that throw `InvalidOperationException`, the untyped arm
stops anything else, and a `maxRetries`/`withinTime` budget degrades the
whole policy to Stop when a child keeps failing. One child gets a per-child
override — `supervise(fragile, strategy: OneForOne(on Failure: Stop))` —
so the same poison message restarts the steady worker but permanently stops
the fragile one. The output shows all of it: the restart resets the steady
worker's state (its count starts over), while messages to the stopped worker
route to the dead-letter sink rather than vanishing.

The cache demonstrates `passivate after`: two quick lookups share one
incarnation (hits go 1, 2), then after two idle seconds the runtime unloads
the actor. The next lookup transparently rematerializes it — session-scoped,
so it starts fresh at 1. Senders never notice the round trip.

```
dotnet run
```
