---
title: Demos
layout: default
nav_order: 6
permalink: /demos/
description: "Three runnable demo systems and the benchmark suite that measures them: an IoT fleet paired with a raw-Channels C# twin, a self-healing elevator bank, and an actor-per-key rate limiter."
---

# Demos

The `demos/` directory of the repository holds three small systems and the
benchmark suite that measures two of them. Wherever a comparison is claimed,
it runs as a pair: a Spek implementation and a C# implementation of the same
system, under one load driver and one fault schedule, with both columns of
the results table computed by the same code path. You don't have to take our word for what supervision or passivation is
worth: the C# pane shows what the same behavior costs to build by hand.

One standing rule keeps the pairing honest: the C# side is written and
reviewed as production code, and it is open to pull requests like anything
else in the repository. "Your C# is a strawman" always has the same answer:
then improve it. The comparison means something only while the twin is the
code a careful C# developer would actually ship.

Each demo runs with one command from the repository root, and each accepts
its own flags after the name (the per-demo READMEs list them):

```bash
./demos/run.sh fleet
./demos/run.sh elevators
./demos/run.sh ratelimiter
./demos/run.sh benchmarks
```

## Fleet: supervision as the whole recovery story

Fleet is the primary demo, and the one to run first. It is an IoT device
fleet implemented twice. The Spek pane is a hub, a collector, and a device
actor whose entire recovery story is a single `supervise` clause. The C#
pane is the same fleet built the way a careful C# developer would build it
today, on `System.Threading.Channels` with a consumer task per device and
hand-rolled recovery. Both panes ingest an identical reading stream,
including a firmware fault (a NaN reading that crashes device processing)
injected every N readings, and the harness prints them side by side:
readings sent, faults injected, work lost, recoveries, throughput.

The demo's claim is specific. The Spek pane loses zero work under
the injected faults, and under steady-state measurement (the benchmark
suite below) it runs at CPU parity with the raw-Channels twin, a ratio of
about 1.02. The supervision, tracing, and introspection it carries cost
roughly nothing in wall-clock terms; what the twin still wins on is
allocation, which is the tracked remaining gap, measured release over
release. While a run drains, `spekc observe <pid>` attaches to the harness
process and shows the Spek pane's live actor table with no instrumentation
in the demo code.

## Elevators: watch it heal

Elevators is the demo you show rather than measure. Six cars, one
dispatcher, a building's worth of hall calls, and twice during the
sixty-second run a car's controller crashes mid-trip. Supervision restarts
the controller with clean state; the dispatcher notices the amnesia,
redistributes the car's stranded stops to healthy cars, and hands the
reborn controller its last known position. On screen the car turns red,
reads out of service, runs express down the shaft to the lobby, and rejoins
the rotation when its doors open at floor 1. The tally at the end shows every
hall call served. The demo source contains no try/catch.

There is no C# twin here. Elevators is a reliability demo at a scale a
person can read, not a benchmark. The measured numbers are in the fleet demo. This one is for watching the
recovery happen on screen.

## Rate limiter: actor-per-key infrastructure

The rate limiter is the infrastructure-shaped demo: a per-API-key token
bucket implemented twice and driven by traffic shaped the way API traffic
actually arrives, a hot working set of keys over a long cold tail. On the
Spek pane every key is an actor holding its own bucket, and eviction, the
demo's centerpiece: a two-second `passivate after` declaration. Idle
keys leave memory on their own. The C# twin fills the same table row with a
sweeper timer, a scan over every bucket, and an evict-versus-refill race
whose correctness argument takes a paragraph-long comment.

The throughput row favors the twin, and the framing is honest about it. A
lock-protected dictionary read costs about 22 nanoseconds; a Spek check is
a full request-reply through a mailbox, about 2 µs and roughly 420 bytes at
the current runtime. That is the price of per-key serialization,
passivation, and testable time, and it has already been halved twice by
runtime work. The rest of the table is the other side of that cost.

## The benchmark suite

The demo harnesses are the correctness gate. Their wall-clock throughput is
a cold single shot with JIT warmup inside the measurement. The numbers
worth quoting come from `demos/benchmarks`, which runs the same paired
panes under BenchmarkDotNet: JIT-warmed steady state, outlier-managed
iterations, and allocated bytes per operation. The C# twin is the
BenchmarkDotNet baseline in every pair, so the ratio column reads directly
as what the actor runtime costs for what it provides, and the allocated
column shows where that cost lives. Run it with `./demos/run.sh benchmarks`
(the runner builds Release, as BenchmarkDotNet requires) and pass
BenchmarkDotNet arguments through, such as `--filter '*Fleet*'`.

The demos also run in CI, where each exits non-zero when its contract
breaks (the fleet or the elevators lose work, the rate limiter fails its
bucket arithmetic or retains keys past the idle window), so every demo
doubles as an integration test of the public runtime surface.

## Where to go next

- [Supervision](language/supervision.md): the `supervise` clause the fleet
  and elevator demos are built on.
- [Persistence](language/persistence.md): where `passivate after` lives.
- [Samples](samples.md): smaller, single-feature programs to read before
  the demos.
- [Getting started](getting-started.md): install the toolchain if you have
  not run any Spek yet.
