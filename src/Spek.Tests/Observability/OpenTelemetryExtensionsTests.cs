using Spek.Observability;
using Spek.Observability.OpenTelemetry;
using Spek.Runtime;
using Xunit;

namespace Spek.Tests.Observability;

/// <summary>
/// Registration coverage for <see cref="SpekOpenTelemetryExtensions"/>:
/// the <c>Use*</c> overloads must install the right adapter on the
/// <see cref="ActorSystem"/>, return the system for chaining, and the
/// installed logger must actually carry an actor's <c>self.Log</c>
/// events into the wired <c>ILoggerFactory</c>.
/// </summary>
public sealed class OpenTelemetryExtensionsTests
{
    /// <summary>Logs one structured event per message, then acks the ask.</summary>
    private sealed class ChattyActor : ActorBase
    {
        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            Log.Log(StructuredLogLevel.Warning, "test.chatty.event",
                new[] { new KeyValuePair<string, object?>("payload", message) });
            sender.Tell("done", _selfRef!);
            return Task.CompletedTask;
        }
    }

    [Fact]
    public void UseLoggerFactory_InstallsTheAdapter_AndReturnsTheSystemForChaining()
    {
        using var system = new ActorSystem("otel-ext-logger");
        var factory = new RecordingLoggerFactory();

        var returned = system.UseLoggerFactory(factory);

        Assert.Same(system, returned);
        Assert.IsType<MicrosoftExtensionsLoggerStructuredLogger>(system.Logger);
        Assert.Contains("Spek.Runtime", factory.Categories);
    }

    [Fact]
    public async Task UseLoggerFactory_ActorSelfLog_FlowsToTheWiredILoggerAsync()
    {
        using var system = new ActorSystem("otel-ext-selflog");
        var factory = new RecordingLoggerFactory();
        system.UseLoggerFactory(factory);

        var actor = system.Spawn<ChattyActor>();
        var reply = await actor.AskAsync<object>("hello");
        Assert.Equal("done", reply);

        var entry = Assert.Single(factory.Logger.Entries,
            e => e.Message == "test.chatty.event");
        Assert.Equal(Microsoft.Extensions.Logging.LogLevel.Warning, entry.Level);
        var scope = Assert.Single(entry.Scopes);
        var fields = Assert.IsAssignableFrom<IReadOnlyDictionary<string, object?>>(scope);
        Assert.Equal("hello", fields["payload"]);
    }

    [Fact]
    public void UseLoggerFactory_NullArguments_Throw()
    {
        using var system = new ActorSystem("otel-ext-logger-nulls");
        var factory = new RecordingLoggerFactory();

        Assert.Throws<ArgumentNullException>(
            () => SpekOpenTelemetryExtensions.UseLoggerFactory(null!, factory));
        Assert.Throws<ArgumentNullException>(
            () => system.UseLoggerFactory(null!));
    }

    [Fact]
    public void UseOpenTelemetryMetrics_InstallsAMeterBackedSink_AndReturnsTheSystemForChaining()
    {
        // The runtime wraps user sinks in an internal guard, so assert the
        // wiring behaviorally: a metric emitted through system.Metrics must
        // surface on the "Spek.Runtime" meter the MeterMetricSink publishes to.
        long observed = 0;
        using var listener = new System.Diagnostics.Metrics.MeterListener
        {
            InstrumentPublished = (instrument, l) =>
            {
                if (instrument.Meter.Name == MeterMetricSink.MeterName &&
                    instrument.Name == "test.otel.ext.counter")
                {
                    l.EnableMeasurementEvents(instrument);
                }
            },
        };
        listener.SetMeasurementEventCallback<long>((_, value, _, _) =>
            Interlocked.Add(ref observed, value));
        listener.Start();

        using var system = new ActorSystem("otel-ext-metrics");

        var returned = system.UseOpenTelemetryMetrics();

        Assert.Same(system, returned);
        system.Metrics.Counter("test.otel.ext.counter", 3);
        Assert.Equal(3, Interlocked.Read(ref observed));
    }

    [Fact]
    public void UseOpenTelemetryMetrics_NullSystem_Throws()
        => Assert.Throws<ArgumentNullException>(
            () => SpekOpenTelemetryExtensions.UseOpenTelemetryMetrics(null!));
}
