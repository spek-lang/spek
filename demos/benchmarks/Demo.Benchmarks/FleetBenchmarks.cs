using BenchmarkDotNet.Attributes;
using Fleet.Contracts;

namespace Demo.Benchmarks;

/// <summary>
/// The fleet demo's panes under BenchmarkDotNet: one full ingest-and-drain
/// pass per operation, faults included, after proper JIT warmup. The C#
/// twin is the baseline, so the Ratio column reads directly as "what the
/// actor runtime costs per reading for supervision, tracing, and
/// introspection". MemoryDiagnoser turns the memory story into a number:
/// allocated bytes per pass, dominated on the Spek side by per-message
/// mailbox machinery.
/// </summary>
[MemoryDiagnoser]
public class FleetBenchmarks
{
    [Params(20_000)]
    public int Readings;

    private const int Devices = 50;
    private const int PoisonEvery = 1_000;

    [Benchmark(Baseline = true)]
    public async Task<long> CSharpChannels()
    {
        await using var fleet = new Fleet.CSharp.ChannelFleet();
        return await DriveAsync(fleet);
    }

    [Benchmark]
    public async Task<long> SpekActors()
    {
        await using var fleet = new Fleet.SpekFleet(quiet: true);
        return await DriveAsync(fleet);
    }

    private async Task<long> DriveAsync(IFleet fleet)
    {
        for (var i = 0; i < Readings; i++)
        {
            var poisoned = i % PoisonEvery == PoisonEvery - 1;
            fleet.Ingest(deviceId: i % Devices, value: poisoned ? double.NaN : i);
        }
        var stats = await fleet.DrainAndReportAsync();
        return stats.Processed;
    }
}
