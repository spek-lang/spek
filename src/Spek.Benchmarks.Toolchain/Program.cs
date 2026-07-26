using BenchmarkDotNet.Running;
using Spek.Benchmarks.Toolchain;

// `dotnet run -c Release --project src/Spek.Benchmarks.Toolchain` runs all
// benchmarks; append `--filter '*Compiler*'` (etc.) to run a subset. See
// README.md.
BenchmarkSwitcher.FromAssembly(typeof(CompilerBenchmarks).Assembly).Run(args);
