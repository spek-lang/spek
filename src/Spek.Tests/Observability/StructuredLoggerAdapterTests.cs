using Microsoft.Extensions.Logging;
using Spek.Observability;
using Spek.Observability.OpenTelemetry;
using Xunit;

namespace Spek.Tests.Observability;

/// <summary>
/// Unit coverage for
/// <see cref="MicrosoftExtensionsLoggerStructuredLogger"/>, the adapter
/// that carries Spek's structured <c>self.Log</c> surface onto
/// <c>Microsoft.Extensions.Logging</c>. A recording logger asserts the
/// level mapping, the scope-borne structured fields, exception
/// passthrough, and the enabled-check short-circuit.
/// </summary>
public sealed class StructuredLoggerAdapterTests
{
    [Fact]
    public void Constructor_NullFactory_ThrowsArgumentNull()
        => Assert.Throws<ArgumentNullException>(
            () => new MicrosoftExtensionsLoggerStructuredLogger(null!));

    [Fact]
    public void Constructor_CreatesLoggerWithSpekRuntimeCategory()
    {
        var factory = new RecordingLoggerFactory();

        _ = new MicrosoftExtensionsLoggerStructuredLogger(factory);

        var category = Assert.Single(factory.Categories);
        Assert.Equal("Spek.Runtime", category);
    }

    [Theory]
    [InlineData(StructuredLogLevel.Trace,       LogLevel.Trace)]
    [InlineData(StructuredLogLevel.Debug,       LogLevel.Debug)]
    [InlineData(StructuredLogLevel.Information, LogLevel.Information)]
    [InlineData(StructuredLogLevel.Warning,     LogLevel.Warning)]
    [InlineData(StructuredLogLevel.Error,       LogLevel.Error)]
    [InlineData(StructuredLogLevel.Critical,    LogLevel.Critical)]
    public void Log_MapsEachSpekLevel_ToTheMatchingMicrosoftLevel(
        StructuredLogLevel spekLevel, LogLevel expected)
    {
        var factory = new RecordingLoggerFactory();
        var adapter = new MicrosoftExtensionsLoggerStructuredLogger(factory);

        adapter.Log(spekLevel, "level.mapping.probe");

        var entry = Assert.Single(factory.Logger.Entries);
        Assert.Equal(expected, entry.Level);
    }

    [Fact]
    public void Log_UnknownLevelValue_FallsBackToInformation()
    {
        var factory = new RecordingLoggerFactory();
        var adapter = new MicrosoftExtensionsLoggerStructuredLogger(factory);

        adapter.Log((StructuredLogLevel)999, "level.unknown.probe");

        var entry = Assert.Single(factory.Logger.Entries);
        Assert.Equal(LogLevel.Information, entry.Level);
    }

    [Fact]
    public void Log_FormatsTheEventNameAsTheMessage()
    {
        var factory = new RecordingLoggerFactory();
        var adapter = new MicrosoftExtensionsLoggerStructuredLogger(factory);

        adapter.Log(StructuredLogLevel.Information, "actor.spawned");

        var entry = Assert.Single(factory.Logger.Entries);
        // The adapter logs with the "{EventName}" template, so the
        // formatted message is exactly the stable event identifier.
        Assert.Equal("actor.spawned", entry.Message);
    }

    [Fact]
    public void Log_WithProperties_SurfacesThemAsAScopeDictionary()
    {
        var factory = new RecordingLoggerFactory();
        var adapter = new MicrosoftExtensionsLoggerStructuredLogger(factory);

        adapter.Log(StructuredLogLevel.Warning, "actor.restarted",
            new[]
            {
                new KeyValuePair<string, object?>("actor", "orders-17"),
                new KeyValuePair<string, object?>("attempt", 3),
            });

        var entry = Assert.Single(factory.Logger.Entries);
        var scope = Assert.Single(entry.Scopes);
        var fields = Assert.IsAssignableFrom<IReadOnlyDictionary<string, object?>>(scope);
        Assert.Equal("orders-17", fields["actor"]);
        Assert.Equal(3, fields["attempt"]);
    }

    [Fact]
    public void Log_ScopeIsClosedAfterTheCall()
    {
        var factory = new RecordingLoggerFactory();
        var adapter = new MicrosoftExtensionsLoggerStructuredLogger(factory);

        adapter.Log(StructuredLogLevel.Information, "with.fields",
            new[] { new KeyValuePair<string, object?>("k", "v") });
        adapter.Log(StructuredLogLevel.Information, "without.fields");

        // The second event must not inherit the first event's scope —
        // the adapter's `using var scope` has to unwind per call.
        Assert.Equal(2, factory.Logger.Entries.Count);
        Assert.Empty(factory.Logger.Entries[1].Scopes);
    }

    [Fact]
    public void Log_NullOrEmptyProperties_OpensNoScope()
    {
        var factory = new RecordingLoggerFactory();
        var adapter = new MicrosoftExtensionsLoggerStructuredLogger(factory);

        adapter.Log(StructuredLogLevel.Information, "no.props");
        adapter.Log(StructuredLogLevel.Information, "empty.props",
            Array.Empty<KeyValuePair<string, object?>>());

        Assert.Equal(2, factory.Logger.Entries.Count);
        Assert.All(factory.Logger.Entries, e => Assert.Empty(e.Scopes));
    }

    [Fact]
    public void Log_ExceptionPassesThroughUnwrapped()
    {
        var factory = new RecordingLoggerFactory();
        var adapter = new MicrosoftExtensionsLoggerStructuredLogger(factory);
        var boom = new InvalidOperationException("handler crashed");

        adapter.Log(StructuredLogLevel.Error, "actor.crashed", exception: boom);

        var entry = Assert.Single(factory.Logger.Entries);
        Assert.Same(boom, entry.Exception);
    }

    [Fact]
    public void IsEnabled_ReflectsTheUnderlyingLoggersLevel()
    {
        var factory = new RecordingLoggerFactory();
        factory.Logger.MinLevel = LogLevel.Warning;
        var adapter = new MicrosoftExtensionsLoggerStructuredLogger(factory);

        Assert.False(adapter.IsEnabled(StructuredLogLevel.Trace));
        Assert.False(adapter.IsEnabled(StructuredLogLevel.Information));
        Assert.True(adapter.IsEnabled(StructuredLogLevel.Warning));
        Assert.True(adapter.IsEnabled(StructuredLogLevel.Critical));
    }

    [Fact]
    public void Log_DisabledLevel_ShortCircuitsWithoutRecording()
    {
        var factory = new RecordingLoggerFactory();
        factory.Logger.MinLevel = LogLevel.Error;
        var adapter = new MicrosoftExtensionsLoggerStructuredLogger(factory);

        adapter.Log(StructuredLogLevel.Information, "suppressed.event",
            new[] { new KeyValuePair<string, object?>("wasted", "work") });

        Assert.Empty(factory.Logger.Entries);
    }
}
