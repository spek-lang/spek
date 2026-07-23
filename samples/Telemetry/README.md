# Telemetry: stream operators and reader/writer state

A `Monitor` rate-shapes a noisy feed *before* its handlers run. Stream
operators sit between the message pattern and the body: `distinct(by:)`
drops consecutive readings with an unchanged value, so six incoming readings
become three handled ones; `throttle(200)` passes at most one heartbeat per
window, so a burst of ten becomes one. The operators run outside the actor
lock — the body still executes serialized, one message at a time.

Tallies live in a `shared` region. The stream-shaped handlers are writers
(the default) and take the region's writer lock; the query handler is
declared `reader on` and runs under the concurrent reader lock — and the
compiler holds it to that: a reader that tried to mutate the board would be
a CE0087 at build time, not a race at runtime. The region's `sinceBoot`
field carries the `transient` marker, opting it out of persistence capture
if the region ever becomes `: Persisted`.

```
dotnet run
```
