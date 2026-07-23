# Throughput — the runtime at full tilt

Every other demo in this directory is about something: elevators heal, the
fleet survives firmware faults. This one is about nothing but speed. It
spawns actors, pours messages through them, and reports how many dispatches
per second the runtime sustains across the machine's cores.

```bash
./demos/run.sh throughput                            # the headline: ping-pong sweep
./demos/run.sh throughput --pairs 2 --rounds 200000  # pin one pair count
./demos/run.sh throughput --mode tell --actors 8 --messages 1000000
./demos/run.sh throughput --mode ask  --actors 4 --messages 200000
```

The default mode serves a game of ping-pong. Each `Paddle` pair gets one
ball: a `Fire` message carrying a countdown, served with the peer paddle as
its sender. A paddle that receives a `Fire` counts the hit and returns
`new Fire(remaining - 1)`; a handler's return value routes to the sender of
the message it answers, so the pair rallies with no further help from the
harness. A single pair is a strictly ordered chain of dispatches and
therefore measures round-trip dispatch latency; the sweep over pair counts
(1, 2, 4, and so on up to the processor count) measures how well those
chains overlap across cores. The other two modes bracket the headline:
`tell` is fire-and-forget mailbox pressure from parallel host tasks, and
`ask` is request/reply round trips from outside the actor system. A warmup
pass runs before anything is measured, so the table excludes JIT and
tiering effects.

## The conservation contract

A throughput number is only as good as its delivery guarantee, so after
every measured pass the harness asks each actor for its counter and
requires the exact expected total: every message dispatched exactly once,
none lost, none duplicated. Any discrepancy exits non-zero, which makes the
demo double as an integration test of the public runtime surface, in the
same spirit as the other demos' contracts.

## Why there is no C# twin

The fleet demo owns the like-for-like comparison, and it earns it by making
both panes keep the same promises. Pure throughput has no honest twin: a
rival stripped down to raw channels would carry none of the actor model's
guarantees (supervision, per-actor ordering, introspection), it would win,
and the number would mean nothing. This demo instead tracks one number
against itself, release over release, and that number is the headline for
the dispatcher-era runtime work ahead.

## Watching it run

While a pass is in flight, `spekc observe <pid>` (or the VS Code actor
panel) attaches to the harness process and shows every paddle's and
counter's message count climbing live, with no instrumentation in the demo
code. Everything the runtime is being measured on is in
[`Throughput.Spek/Throughput.spek`](Throughput.Spek/Throughput.spek), which
fits on one screen; the load driver and the bookkeeping are
[`Throughput.Harness/Program.cs`](Throughput.Harness/Program.cs).
