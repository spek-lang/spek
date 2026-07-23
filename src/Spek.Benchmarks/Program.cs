using BenchmarkDotNet.Running;
using Spek.Benchmarks;

// `dotnet run -c Release --project src/Spek.Benchmarks` runs all benchmarks;
// append `--filter *Contention*` (etc.) to run a subset. See README.md.
BenchmarkSwitcher.FromAssembly(typeof(MessagingBenchmarks).Assembly).Run(args);
