# Demo benchmarks — the steady-state numbers

The demo harnesses are the correctness gate: identical traffic, identical
faults, exit-nonzero on lost work. Their wall-clock throughput, though, is
a cold single shot — JIT warmup inside the measurement, one run, no
allocation data. This project runs the same paired panes under
BenchmarkDotNet for the numbers worth quoting: JIT-warmed steady state,
outlier-managed iterations, and allocated bytes per operation via
MemoryDiagnoser.

```bash
./demos/run.sh benchmarks                          # everything (Release)
./demos/run.sh benchmarks --filter '*Fleet*'
./demos/run.sh benchmarks --filter '*Check*' --job short
```

The C# twin is the BenchmarkDotNet **baseline** in every pair, so the
Ratio column reads directly as what the actor runtime costs for what it
provides — supervision, tracing, introspection, mailbox serialization —
and the `Allocated` column shows where that cost lives. For scale: at this
era's runtime the fleet pane runs at **CPU parity with raw Channels**
(ratio ~1.02) with a ~12x allocation ratio, and a full ask round-trip
costs ~2 µs / ~420 B against a ~22 ns locked dictionary read. Track the
ratios across releases; the dispatcher era targets what remains, and
these benchmarks are how its progress gets measured.

CI: the `benchmarks` workflow runs this suite alongside `Spek.Benchmarks`
on release tags and on manual dispatch (`--job short`), uploading the
BenchmarkDotNet artifacts for release-over-release comparison.
