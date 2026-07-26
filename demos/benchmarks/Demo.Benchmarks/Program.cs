using BenchmarkDotNet.Configs;
using BenchmarkDotNet.Jobs;
using BenchmarkDotNet.Running;
using BenchmarkDotNet.Toolchains.InProcess.Emit;
using Demo.Benchmarks;

// `dotnet run -c Release --project demos/benchmarks/Demo.Benchmarks` runs the
// paired demo benchmarks (Spek pane vs C# twin, CPU + allocations); append
// `--filter *Fleet*` / `--filter *RateLimiter*` for a subset. The demo
// harnesses stay the correctness gate; these give the steady-state,
// JIT-warmed numbers the harness wall-clock can't.
//
// In-process toolchain on purpose: the Spek panes compile .spek sources via
// Spek.targets, a build step BenchmarkDotNet's generated child project can't
// reproduce (it rebuilds the project graph into a redirected output where the
// generated .g.cs path resolution breaks). The panes are already-built
// Release assemblies, so measuring in-process is both correct and simpler;
// ShortRun (3 warmup + 3 measured iterations) keeps CI time bounded while
// still measuring after JIT warmup.
var config = ManualConfig.Create(DefaultConfig.Instance)
    .AddJob(Job.ShortRun
        .WithToolchain(InProcessEmitToolchain.Instance)
        .AsDefault());

BenchmarkSwitcher.FromAssembly(typeof(FleetBenchmarks).Assembly).Run(args, config);
