# Spek.Benchmarks

Performance benchmarks for the Spek runtime, covering CPU (throughput and
latency), memory (allocations per operation), thread contention (the cost of
blocking handlers), and the runtime's attachable surfaces: diagnostics,
persistence, supervision, and ingress policies. Built on
[BenchmarkDotNet](https://benchmarkdotnet.org/).

This is the release performance gate. Run it before tagging a release, compare
against the last baseline, and investigate regressions. The findings and what
each number means are in
the performance chapter of the Spek documentation (published separately).

## Run

```bash
# All benchmarks (Release is mandatory — Debug numbers are meaningless):
dotnet run -c Release --project src/Spek.Benchmarks

# A subset:
dotnet run -c Release --project src/Spek.Benchmarks -- --filter '*Contention*'
dotnet run -c Release --project src/Spek.Benchmarks -- --filter '*Messaging*'
dotnet run -c Release --project src/Spek.Benchmarks -- --filter '*Diagnostics*'

# Faster, lower-fidelity pass while iterating:
dotnet run -c Release --project src/Spek.Benchmarks -- --job short
```

## What's measured

| Suite                  | Benchmark                                   | Captures                                                                                             |
|------------------------|---------------------------------------------|------------------------------------------------------------------------------------------------------|
| `MessagingBenchmarks`  | `TellThroughput`                            | Tell + dispatch cost, allocations/message                                                            |
|                        | `AskRoundTrip`                              | request/reply latency                                                                                |
|                        | `SpawnCost`                                 | actor creation cost                                                                                  |
| `ContentionBenchmarks` | `AsyncWait_FanOut` vs `BlockingWait_FanOut` | the thread-pool starvation a blocking handler causes. The **ratio** is the thread-contention signal |
| `DiagnosticsBenchmarks` | `Baseline` vs `ObserverAttached` / `FlightRecorderAttached` / `ChaosAttachedNoMatch` / `ObserverDetached` | the per-message tax of each debugging surface, and the residue after a tap detaches |
| `PersistenceBenchmarks` | `PersistPerMessage_InMemory` / `_File` / `_Sqlite` | persist-every-message cost against each snapshot store |
|                        | `PassivateRespawnCycle`                     | the unload, re-materialise, restore round trip |
| `SupervisionBenchmarks` | `NoCrash_Baseline` vs `CrashEvery100th_Restart` / `RestartStorm_EveryMessage` | what a crash-then-restart cycle costs on top of a healthy dispatch |
| `IngressPolicyBenchmarks` | `NoPolicy` vs `OneAllowPolicy` / `ThreeAllowPolicies` / `TokenBucket_NeverRejects` | per-message overhead of the resilience policy chain |
| `ClusterBenchmarks`    | `LocalAskBaseline` vs `MemoryTransportAsk` / `TcpLoopbackAsk` | what a request/reply pays to cross the node boundary |
|                        | `MemoryTransportTell`                       | remote fire-and-forget throughput (envelope + transport dispatch per message) |
|                        | `EnvelopeSerializeOnly_Small` / `_EightField` | the message → wire-bytes serialization step in isolation |
| `StreamOperatorBenchmarks` | `PassthroughBaseline` vs `DistinctAllPass` / `DistinctAllDrop` / `ThrottleZeroWindow` / `ThrottleAllDrop` / `DebounceOfferOnly` / `ComposeTwoDeep` | per-offer cost of each stream operator's hot path, no actors involved |
| `MailboxBenchmarks`    | `ColdWakeAsk` / `DeepDrain` / `PingPongLatency` | the mailbox's operating shapes: the park→wake ask, one continuous deep-backlog drain, and the wake-per-hop round trip |
| `AskBenchmarks`        | `SequentialAsk` vs `SequentialAskWithTimeout` / `SequentialTellWithReplyProbe`, plus `ConcurrentAskFanIn` | the timeout overload's wrapper tax, the hand-rolled Tell+probe idiom, and 64-way asker contention |
| `LifecycleBenchmarks`  | `SpawnStopCycle` / `SpawnPersistentCold` / `SystemDisposeWith1kActors` / `DeepChildTree` | whole actor lifetimes: spawn→message→stop, cold persistent spawn, system teardown, handler-side child chains |
| `TimerBenchmarks`      | `AdmitFirstTry` vs `DeferOnceThenAdmit`; `PassivationOff_` vs `PassivationArmed_TellThroughput` | the defer→timer→re-admission cycle, and what having `passivate after` armed costs per message |
| `ObservabilityCostBenchmarks` | `Baseline` vs `SelfLogDisabled` / `SelfLogEnabled` / `MetricSinkEnabled` / `ClockReadPerMessage` / `TickCount64PerMessage` | the per-message price of each observability seam once something real is plugged in |

`[MemoryDiagnoser]` adds the `Allocated` column (watch it for per-message
hot-path allocations). The contention ratio quantifies why CE0083 / CE0115 /
CE0116 flag blocking and sync calls in handlers.

`DiagnosticsBenchmarks` backs the documentation's claim that the debugging
surfaces cost roughly nothing until attached. Every benchmark repeats the
`TellThroughput` shape, so the ratio column reads directly as the price of
attaching an inbox observer, a flight recorder, or a chaos plan whose rules
never match, and `ObserverDetached` verifies that a disposed tap stops taxing
the actor.

`PersistenceBenchmarks` uses a deliberately smaller message count: the file
store flushes to disk on every save, so each message is real I/O rather than
a mailbox hop. Compare the stores against each other, not against the
messaging suite. `PassivateRespawnCycle` is paced by the passivation timer by
design; read its `Allocated` column, not its mean.

`SupervisionBenchmarks` prices the failure path. The delta between the
baseline and the every-100th arm, divided by the restart count, is the cost
of one crash: throw, supervision, dead-letter, and a fresh instance. The
storm arm, where every message crashes, bounds the worst case of an actor
crash-looping under an unlimited restart budget.

`IngressPolicyBenchmarks` measures admission control. One always-allow policy
is the fixed floor of having any policy attached, three separate the marginal
chain cost from that floor, and the token bucket (sized so nothing is ever
rejected) shows what the shipped rate limiter's permit check adds per message.

`ClusterBenchmarks` prices the node boundary in three rungs. The baseline is
a plain local ask; the in-memory transport arm sends the same request/reply
between two `ActorSystem`s through a shared fabric (envelope construction,
transport lookup, and a remote-sender ref rebuilt on the receiving side, but
no bytes); the TCP arm runs it over `Spek.Cluster.Tcp` on 127.0.0.1, adding
JSON serialization, binary framing, and a kernel loopback hop each way. Both
nodes are stood up once in `GlobalSetup`, so the rows show the steady-state
per-message tax rather than bootstrap cost. Remote asks aren't a runtime
primitive yet (`AskAsync` on a remote ref throws), so the transport arms use
the wire request/reply idiom — Tell with a named-root sender, reply routed
back across the transport — which carries one extra mailbox hop and a
`TaskCompletionSource` per round trip that the baseline's pooled reply cell
does not. Read the ratio as the price of the whole boundary. The
serialize-only rows call `JsonSpekSerializer` directly and isolate the
message-to-bytes step; the delta between the one-field and eight-field
records is how the cost scales with payload width.

`StreamOperatorBenchmarks` drives 100k offers straight into each operator
through its public `Configure`/`OfferAsync` contract, with a static no-op
sink and no actors, so the Allocated column is the operator's own. The
pass-through baseline is the floor every `=> operator` link pays (one virtual
call plus the dispatch delegate); distinct is measured on its pass and drop
paths, throttle on its all-pass (zero window) and all-drop (huge window)
bounds, and `ComposeTwoDeep` against the baseline gives the per-layer
indirection tax. `DebounceOfferOnly` measures only the record-and-re-arm
cost per offer — with a 10s quiet window and offers microseconds apart, no
emission ever fires during measurement, and that is by design (the emit path
is a timer callback, not part of the offer hot path).

`MailboxBenchmarks` separates the operating shapes that `TellThroughput`
blends together. `ColdWakeAsk` waits for the system to go verifiably idle
between asks, so every request pays the full park, work-item queue, drain
start, and reply path — the latency number for request-reply services. Its
delta against `AskRoundTrip`, whose back-to-back asks are caught by the drain
loop's second-chance spin, is what that spin saves. `DeepDrain` parks the
dispatch loop on a gate, pre-fills a 100k backlog, and releases it as one
continuous drain, and `PingPongLatency` bounces a message between two actors
whose mailboxes go empty on every hop.

`AskBenchmarks` prices the ask path beyond the plain round trip.
`SequentialAsk` is the reference row; the timeout overload shows what
`AsTask().WaitAsync` re-adds per call, and the Tell-with-probe row prices the
hand-rolled request/reply idiom (two mailbox hops plus a fresh
`TaskCompletionSource` against ask's single hop and reply cell).
`ConcurrentAskFanIn` drives 64 askers in 1000 awaited waves at one actor,
stressing reply-cell allocation and sender routing under contention; divide
by 64,000 for per-ask figures.

`LifecycleBenchmarks` extends `SpawnCost` to whole lifetimes: 10k
spawn→message→stop cycles (stop is voluntary, via a poison-pill message whose
handler calls `StopSelf` — the runtime's public stop surface), 10k cold
persistent spawns under unique keys (the snapshot-lookup wiring, no messages
sent), a 1k-actor system disposal measuring the teardown sweep, and
100-deep child chains grown handler-side through the child-spawn surface.

`TimerBenchmarks` prices the clock-driven machinery reachable from plain C#.
The defer pair pushes every message through one park cycle — policy verdict,
one-shot timer, defer-budget entry, re-admission — against the same policy
with deferral switched off; wall time includes the 1ms park, so read the
Allocated column. The passivation pair pins down the claim that `passivate
after` costs nothing per message: the timeout arms one periodic idle-check
timer at materialization and nothing is re-armed per send. There is no
arm/disarm row because no actor-facing timer surface exists; every runtime
timer is internal.

`ObservabilityCostBenchmarks` repeats the `TellThroughput` shape with each
observability seam engaged one at a time, installing no-op implementations so
the rows measure what the runtime and the handler pay to feed a sink rather
than any particular backend. The `self.Log` pair splits the disabled guard
from an enabled statement's property-list build, `MetricSinkEnabled` exposes
the runtime's own per-message metric emission (mailbox-depth gauge, dispatch
counter, duration histogram) once a registered sink reports itself enabled,
and the clock pair prices `self.Clock` against `Environment.TickCount64`.

## Note

Not part of `dotnet test` (benchmarks are long-running and run in their own
Release process). It is in `Spek.slnx` so it stays compiling.
