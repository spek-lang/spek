using BenchmarkDotNet.Attributes;
using Spek;
using Spek.Observability;
using Spek.Runtime;

namespace Spek.Benchmarks;

/// <summary>
/// The per-message cost of the observability seams a language user touches:
/// <c>self.Log</c> with and without a logger installed, the runtime's own
/// hot-path metric emission once a real (here: no-op) sink is registered,
/// and the <c>self.Clock</c> read against the cheapest BCL monotonic read.
/// Every benchmark is the 100k Tell-to-idle shape from
/// <see cref="MessagingBenchmarks.TellThroughput"/>, so the ratio column
/// reads directly as the price of each seam. The enabled arms install no-op
/// implementations on purpose: they measure what the RUNTIME and the handler
/// pay to feed a sink, not any particular backend.
/// </summary>
[MemoryDiagnoser]
public class ObservabilityCostBenchmarks
{
    [Params(100_000)]
    public int Messages;

    /// <summary>Plain counter, nothing installed — the reference every other
    /// row is compared against.</summary>
    [Benchmark(Baseline = true)]
    public int Baseline()
    {
        using var system = new ActorSystem("bench");
        var actor = system.Spawn<CounterActor>();
        for (int i = 0; i < Messages; i++)
            actor.Tell(new Ping());
        system.AwaitTermination();
        return Messages;
    }

    /// <summary>A <c>self.Log</c> statement per message with no logger
    /// installed: the default <see cref="NullStructuredLogger"/> answers
    /// <c>IsEnabled</c> false, so the guard — a property walk plus one
    /// interface call — is the whole cost and the property list is never
    /// built. This is the "log statements are free until a logger is
    /// registered" claim, measured.</summary>
    [Benchmark]
    public int SelfLogDisabled()
    {
        using var system = new ActorSystem("bench");
        var actor = system.Spawn<LoggingCounterActor>();
        for (int i = 0; i < Messages; i++)
            actor.Tell(new Ping());
        system.AwaitTermination();
        return Messages;
    }

    /// <summary>The same statement with an enabled no-op logger installed:
    /// every message now builds the property list (one KeyValuePair array,
    /// one boxed long) and crosses the Log interface. The delta over
    /// <see cref="SelfLogDisabled"/> is the floor any real logging backend
    /// starts from.</summary>
    [Benchmark]
    public int SelfLogEnabled()
    {
        using var system = new ActorSystem("bench");
        system.UseLogger(EnabledNoOpLogger.Instance);
        var actor = system.Spawn<LoggingCounterActor>();
        for (int i = 0; i < Messages; i++)
            actor.Tell(new Ping());
        system.AwaitTermination();
        return Messages;
    }

    /// <summary>
    /// Hot-path dispatch with an enabled no-op metric sink registered (the
    /// runtime wraps it in its guarded boundary, as it would a real adapter).
    /// Flipping <c>Enabled</c> on unlocks the runtime's per-message metric
    /// work that <see cref="NullMetricSink"/> elides: the mailbox-depth gauge
    /// at enqueue (a ConcurrentQueue.Count snapshot plus a tag array), the
    /// dispatch counter, and the handler-duration histogram with its
    /// Stopwatch reads — per message. This is what "wire up OTel metrics"
    /// costs before the backend does anything at all.
    /// </summary>
    [Benchmark]
    public int MetricSinkEnabled()
    {
        using var system = new ActorSystem("bench");
        system.UseMetricSink(new EnabledNoOpMetricSink());
        var actor = system.Spawn<CounterActor>();
        for (int i = 0; i < Messages; i++)
            actor.Tell(new Ping());
        system.AwaitTermination();
        return Messages;
    }

    /// <summary>A <c>self.Clock.GetTimestamp()</c> read per message: the
    /// null-coalesced walk to the system TimeProvider plus a virtual call
    /// into <c>Stopwatch.GetTimestamp</c>. The virtual-time-safe read every
    /// Spek handler should be using for measurements.</summary>
    [Benchmark]
    public int ClockReadPerMessage()
    {
        using var system = new ActorSystem("bench");
        var actor = system.Spawn<ClockReadCounterActor>();
        for (int i = 0; i < Messages; i++)
            actor.Tell(new Ping());
        system.AwaitTermination();
        return Messages;
    }

    /// <summary>Reference twin of <see cref="ClockReadPerMessage"/>:
    /// <see cref="Environment.TickCount64"/> per message, the cheapest
    /// monotonic read the BCL offers. The delta between the two rows prices
    /// the Clock abstraction itself.</summary>
    [Benchmark]
    public int TickCount64PerMessage()
    {
        using var system = new ActorSystem("bench");
        var actor = system.Spawn<TickCountCounterActor>();
        for (int i = 0; i < Messages; i++)
            actor.Tell(new Ping());
        system.AwaitTermination();
        return Messages;
    }
}

/// <summary>A logger that says yes to every level and then discards the
/// event — isolates what an ENABLED log statement costs the handler
/// (property-list build + the interface crossing) from any backend.</summary>
internal sealed class EnabledNoOpLogger : IStructuredLogger
{
    public static readonly EnabledNoOpLogger Instance = new();
    public bool IsEnabled(StructuredLogLevel level) => true;
    public void Log(StructuredLogLevel level, string eventName,
        IReadOnlyList<KeyValuePair<string, object?>>? properties = null,
        Exception? exception = null) { }
}

/// <summary>A metric sink that reports <c>Enabled</c> (unlocking the
/// runtime's per-message metric emission) and then discards every sample —
/// isolates the runtime's instrumentation cost from any backend.</summary>
internal sealed class EnabledNoOpMetricSink : IMetricSink
{
    public bool Enabled => true;
    public void Counter(string name, long delta = 1,
        IReadOnlyList<KeyValuePair<string, object?>>? tags = null) { }
    public void Gauge(string name, double value,
        IReadOnlyList<KeyValuePair<string, object?>>? tags = null) { }
    public void Histogram(string name, double value,
        IReadOnlyList<KeyValuePair<string, object?>>? tags = null) { }
}
