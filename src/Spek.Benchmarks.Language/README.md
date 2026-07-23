# Spek.Benchmarks.Language

Performance benchmarks for Spek-authored code — the shapes the compiler
actually emits, not hand-written C# equivalents. Where `Spek.Benchmarks`
prices the raw runtime (mailbox, persistence, supervision, driven by plain
C# actors), this project prices the language: behavior transitions, dispatch
arm counts, `reader on` accounting, shared-region locks, guard idioms, and
stream operator chains, all measured through assemblies produced by the real
pipeline. Built on [BenchmarkDotNet](https://benchmarkdotnet.org/).

Each suite carries its `.spek` source as a string. `[GlobalSetup]` parses it
with `SpekCompiler.Parse`, emits C# with `FileEmitter`, compiles it with
Roslyn at Release optimization, loads the assembly, and spawns the emitted
actors. Reflection ends there: ActorRefs and message instances are cached in
setup, so every measured operation is a plain `Tell` or `AskAsync` — the
same calls Spek-generated code makes in production. The one-time compile
cost lands in setup, never in the numbers.

## Run

```bash
# All benchmarks (Release is mandatory — Debug numbers are meaningless):
dotnet run -c Release --project src/Spek.Benchmarks.Language

# A subset:
dotnet run -c Release --project src/Spek.Benchmarks.Language -- --filter '*Behavior*'
dotnet run -c Release --project src/Spek.Benchmarks.Language -- --filter '*ReaderWriter*'
dotnet run -c Release --project src/Spek.Benchmarks.Language -- --filter '*StreamChain*'

# Faster, lower-fidelity pass while iterating:
dotnet run -c Release --project src/Spek.Benchmarks.Language -- --job short
```

## What's measured

| Suite                       | Benchmark                                            | Captures                                                                                      |
|-----------------------------|------------------------------------------------------|------------------------------------------------------------------------------------------------|
| `BehaviorBenchmarks`        | `SingleBehaviorDispatch` vs `BecomeTransition`       | the per-message price of `become` — the behavior-switch write plus its dispatch-table effect  |
|                             | `DispatchArmScaling(2/8/32)`                         | whether the emitted dispatch switch scales with arm count (always sends the LAST-declared type) |
| `ReaderWriterBenchmarks`    | `WriterAskRoundTrip` vs `ReaderAskRoundTrip`         | pure reader-arm accounting per ask (counter, queue-to-pool, idle pulse) — bodies are identical |
|                             | `ConcurrentWriterAsks` vs `ConcurrentReaderAsks`     | the scaling win: 16 workers, readers overlap by construction, writers serialize               |
|                             | `ReaderWriterInterleave`                             | the drain/idle handoff when the slot flips between writer and reader modes                    |
| `RegionBenchmarks`          | `OwnFieldIncrement` vs `RegionFieldIncrement`        | the region writer-lock tax per message over identical actor-local mutation                    |
|                             | `OwnFieldReaderAsk` vs `RegionReaderAsk`             | the region reader-lock tax on a `reader on` ask                                               |
| `GuardAndPatternBenchmarks` | `UnguardedDispatch` vs `GuardedDispatch`             | the in-body `if` guard idiom, always taken                                                    |
|                             | `NarrowMessageDispatch` vs `WideMessageDispatch`     | dispatch cost of an 8-field record vs a 1-field record (instances pre-created)                |
|                             | `NarrowReplyConstruct` vs `WideReplyConstruct`       | `return new Wide(...)` vs `return new Narrow(...)` on the ask reply path                      |
| `StreamChainBenchmarks`     | `BareHandler` vs `DistinctChain` / `ThrottleChainAllPass` | operator chain + re-admission per message, with nothing dropped                          |
|                             | `ComposeDepth`                                       | the per-extra-operator tax (distinct + throttle vs distinct alone)                            |

`[MemoryDiagnoser]` adds the `Allocated` column; with `OperationsPerInvoke`
set on every benchmark, both time and allocation columns read directly as
per-message (or per-round-trip) figures. Paired suites group by category, so
the `Ratio` column against each baseline is the language tax in question.

## Reading the numbers

The stream-chain ratios carry the suite's headline fact: a chain-passed
message never runs its body inline. The chain's dispatch posts a synthetic
body-trigger record back to self, so the body runs through the mailbox under
the actor lock — every passed message costs a second mailbox hop by design.
`DistinctChain` against `BareHandler` is that doubling plus the operator
itself; the suite arranges for nothing to be dropped (alternating distinct
keys, a zero throttle interval) so drops never masquerade as speed.

One deviation from the brief a reader might expect: Spek has no
dispatch-level guard. `on Msg m when m.value > 0` is not a language shape —
`when` exists only on `case` patterns and catch filters — so the Guard pair
measures the idiom users actually write, an `if` at the top of the handler
body. If dispatch guards ever land, that pair becomes their before/after.

The reader/writer suite reuses the shape of `demos/benchmarks/Readers.Spek`
with one refinement: the writer comparison arm (`WriteLookup`) has a body
byte-identical to the reader arm, so the sequential-ask delta is arm
accounting alone rather than a difference in work.

## Note

Not part of `dotnet test` (benchmarks are long-running and run in their own
Release process). It is in `Spek.slnx` so it stays compiling. Nothing here
replaces `Spek.Benchmarks` — run both when a change could move either the
runtime or the emitter.
