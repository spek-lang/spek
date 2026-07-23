namespace Fleet.Contracts;

/// <summary>
/// What both panes implement. The harness drives the two implementations
/// through this one interface with an identical reading stream and an
/// identical fault schedule, so any asymmetry on screen belongs to the
/// systems under test — not to the harness.
/// </summary>
public interface IFleet : IAsyncDisposable
{
    /// <summary>Pane label ("Spek" / "C#").</summary>
    string Name { get; }

    /// <summary>One telemetry reading. NaN models a firmware fault: the
    /// device's processing crashes on it.</summary>
    void Ingest(int deviceId, double value);

    /// <summary>Waits until the fleet has drained, then reports.</summary>
    Task<FleetStats> DrainAndReportAsync();
}

/// <summary>The numbers the dashboard compares.</summary>
/// <param name="Processed">Readings that survived to the collector.</param>
/// <param name="Recoveries">Device recoveries (supervision restarts / worker rebuilds).</param>
public sealed record FleetStats(long Processed, long Recoveries);
