using System.Diagnostics;
using Fleet.Contracts;

// The fleet demo harness: one driver, one fault schedule, two panes.
// Both implementations receive the identical reading stream — same device
// routing, same NaN positions — so any difference in the table below
// belongs to the systems under test, not to this file.

var readings = ArgInt("--readings", 100_000);
var devices = ArgInt("--devices", 50);
var poisonEvery = ArgInt("--poison-every", 1_000);

Console.WriteLine($"fleet demo — {readings:N0} readings, {devices} devices, " +
                  $"a firmware fault every {poisonEvery:N0} readings\n");

var results = new List<(string Name, FleetStats Stats, TimeSpan Elapsed)>();
foreach (var fleet in new IFleet[] { new Fleet.SpekFleet(), new Fleet.CSharp.ChannelFleet() })
{
    await using var _ = fleet;
    var sw = Stopwatch.StartNew();
    for (var i = 0; i < readings; i++)
    {
        var poisoned = poisonEvery > 0 && i % poisonEvery == poisonEvery - 1;
        fleet.Ingest(deviceId: i % devices, value: poisoned ? double.NaN : i);
    }
    var stats = await fleet.DrainAndReportAsync();
    sw.Stop();
    results.Add((fleet.Name, stats, sw.Elapsed));
}

var faults = poisonEvery > 0 ? readings / poisonEvery : 0;
var clean = readings - faults;

Console.WriteLine($"{"",-22}{results[0].Name,14}{results[1].Name,14}");
Row("readings sent", readings, readings);
Row("faults injected", faults, faults);
Row("processed", results[0].Stats.Processed, results[1].Stats.Processed);
Row("lost (should be 0)", clean - results[0].Stats.Processed, clean - results[1].Stats.Processed);
Row("recoveries", results[0].Stats.Recoveries, results[1].Stats.Recoveries);
Console.WriteLine($"{"drain time",-22}{results[0].Elapsed.TotalSeconds,13:0.00}s{results[1].Elapsed.TotalSeconds,13:0.00}s");
Console.WriteLine($"{"throughput",-22}{Rate(results[0]),11:N0}/s{Rate(results[1]),11:N0}/s");

Console.WriteLine();
Console.WriteLine("The recovery story on the Spek pane is one supervise clause");
Console.WriteLine("(Fleet.Spek/Fleet.spek); on the C# pane it is every try/catch");
Console.WriteLine("and counter in Fleet.CSharp/ChannelFleet.cs. Both panes are");
Console.WriteLine("open to review — if the C# can be written better, improve it.");

var anyLost = results.Any(r => clean - r.Stats.Processed != 0);
return anyLost ? 1 : 0;

void Row(string label, long a, long b) =>
    Console.WriteLine($"{label,-22}{a,14:N0}{b,14:N0}");

double Rate((string Name, FleetStats Stats, TimeSpan Elapsed) r) =>
    r.Stats.Processed / Math.Max(0.001, r.Elapsed.TotalSeconds);

int ArgInt(string name, int fallback)
{
    var i = Array.IndexOf(args, name);
    return i >= 0 && i + 1 < args.Length && int.TryParse(args[i + 1], out var v) ? v : fallback;
}
