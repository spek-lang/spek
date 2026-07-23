using Fleet.Contracts;
using Spek;
using Spek.Runtime;

namespace Fleet;

/// <summary>
/// Host adapter for the Spek pane: wires the actor fleet (Fleet.spek) to
/// the harness interface. Recovery counting needs no instrumentation in
/// the fleet itself — the runtime already tracks restarts, and
/// <see cref="ActorSystem.SnapshotActors"/> exposes them (the same surface
/// <c>spekc observe</c> renders).
/// </summary>
public sealed class SpekFleet : IFleet
{
    private readonly ActorSystem _system;
    private readonly ActorRef _collector;
    private readonly ActorRef _hub;

    /// <param name="quiet">Suppresses the console fault feed — for benchmark
    /// runs, where stdout noise would pollute the measurement logs.</param>
    public SpekFleet(bool quiet = false)
    {
        _system = new ActorSystem("fleet-spek",
            deadLetterSink: quiet ? new RecordingDeadLetterSink() : new FaultFeed());
        _collector = _system.Spawn<Collector>();
        _hub = _system.Spawn<FleetHub>(_collector);
    }

    /// <summary>The live fault feed: show the first few faults landing and
    /// recovering, then count the rest instead of scrolling the table away.</summary>
    private sealed class FaultFeed : IDeadLetterSink
    {
        private int _seen;
        public void DeadLetter(object message, string reason, Exception? cause)
        {
            var n = Interlocked.Increment(ref _seen);
            if (n <= 5)
                Console.WriteLine($"  [spek pane] fault #{n}: {cause?.Message ?? reason} — supervision restarting the device");
            else if (n == 6)
                Console.WriteLine("  [spek pane] … further faults recovering silently (counted below)");
        }
    }

    public string Name => "Spek";

    public void Ingest(int deviceId, double value)
        => _hub.Tell(new Reading(deviceId, value));

    public async Task<FleetStats> DrainAndReportAsync()
    {
        // Quiesce: idle twice in a row with a gap, so in-flight forwards
        // (device -> collector) are counted before we read the total.
        while (true)
        {
            if (_system.IsIdle)
            {
                await Task.Delay(50);
                if (_system.IsIdle) break;
            }
            await Task.Delay(10);
        }

        var total = await _collector.AskAsync<TotalReply>(new GetTotal());
        var restarts = _system.SnapshotActors().Sum(s => (long)s.Restarts);
        return new FleetStats(total.total, restarts);
    }

    public ValueTask DisposeAsync()
    {
        _system.Dispose();
        return ValueTask.CompletedTask;
    }
}
