using Microsoft.Extensions.Logging;
using Spek.Runtime;

namespace Spek.Observability.OpenTelemetry;

/// <summary>
/// Convenience extensions for wiring an
/// <see cref="ActorSystem"/> to the OpenTelemetry-compatible
/// observability adapters. The runtime stays decoupled from any
/// specific telemetry pipeline; this file is the bridge
/// applications opt into.
/// </summary>
public static class SpekOpenTelemetryExtensions
{
    /// <summary>
    /// Replace the actor system's default no-op metric sink with
    /// a <see cref="MeterMetricSink"/> that publishes to the
    /// <c>"Spek.Runtime"</c> meter. Any OpenTelemetry SDK pipeline
    /// (or <c>dotnet-counters</c>) listening to that meter then
    /// receives every per-actor metric.
    ///
    /// <para>
    /// Returns the <paramref name="system"/> for chaining alongside
    /// other <c>Use*</c> calls. The created sink is owned by the
    /// caller; in an ASP.NET Core host the DI container's lifetime
    /// will dispose it.
    /// </para>
    /// </summary>
    public static ActorSystem UseOpenTelemetryMetrics(this ActorSystem system)
    {
        ArgumentNullException.ThrowIfNull(system);
        return system.UseMetricSink(new MeterMetricSink());
    }

    /// <summary>
    /// Replace the actor system's default no-op structured logger
    /// with one backed by a <see cref="ILoggerFactory"/>. Any
    /// pipeline registered on the logger factory (console, OTel
    /// logs exporter, Serilog, Seq) receives Spek's structured
    /// events.
    /// </summary>
    public static ActorSystem UseLoggerFactory(this ActorSystem system, ILoggerFactory factory)
    {
        ArgumentNullException.ThrowIfNull(system);
        ArgumentNullException.ThrowIfNull(factory);
        return system.UseLogger(new MicrosoftExtensionsLoggerStructuredLogger(factory));
    }
}
