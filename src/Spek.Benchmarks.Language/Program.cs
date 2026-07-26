using BenchmarkDotNet.Running;
using Spek.Benchmarks.Language;

// `dotnet run -c Release --project src/Spek.Benchmarks.Language` runs all
// benchmarks; append `--filter *Behavior*` (etc.) to run a subset. See
// README.md. Every suite compiles its .spek source in GlobalSetup, so the
// first iteration of each class pays a one-time parse + emit + Roslyn
// compile that never appears in the measured numbers.
BenchmarkSwitcher.FromAssembly(typeof(BehaviorBenchmarks).Assembly).Run(args);
