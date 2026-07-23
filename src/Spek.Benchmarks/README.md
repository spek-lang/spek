# Spek.Benchmarks

Performance benchmarks for the Spek runtime, covering CPU (throughput and
latency), memory (allocations per operation), and thread contention (the cost of
blocking handlers). Built on [BenchmarkDotNet](https://benchmarkdotnet.org/).

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

`[MemoryDiagnoser]` adds the `Allocated` column (watch it for per-message
hot-path allocations). The contention ratio quantifies why CE0083 / CE0115 /
CE0116 flag blocking and sync calls in handlers.

## Note

Not part of `dotnet test` (benchmarks are long-running and run in their own
Release process). It is in `Spek.slnx` so it stays compiling.
