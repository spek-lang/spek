using Microsoft.Extensions.Logging;

namespace Spek.Observability.OpenTelemetry;

/// <summary>
/// Implementation of <see cref="IStructuredLogger"/> on top
/// of <c>Microsoft.Extensions.Logging.ILogger</c>. Lets Spek's
/// runtime + user code structured-log calls flow through whatever
/// logging pipeline the host has registered (console, OTel logs
/// exporter, Serilog, Seq, etc.) without Spek's runtime depending
/// on any specific logging package.
///
/// <para>
/// The category is fixed at <c>"Spek.Runtime"</c> for runtime-emitted
/// events; user-code calls flow through the same logger but are
/// distinguished by event tags. A per-actor category would be
/// noisier than helpful and complicates the listener-side filter
/// configuration.
/// </para>
/// </summary>
public sealed class MicrosoftExtensionsLoggerStructuredLogger : IStructuredLogger
{
    private readonly ILogger _logger;

    public MicrosoftExtensionsLoggerStructuredLogger(ILoggerFactory factory)
    {
        ArgumentNullException.ThrowIfNull(factory);
        _logger = factory.CreateLogger("Spek.Runtime");
    }

    public bool IsEnabled(StructuredLogLevel level)
        => _logger.IsEnabled(MapLevel(level));

    public void Log(StructuredLogLevel level, string eventName,
        IReadOnlyList<KeyValuePair<string, object?>>? properties = null,
        Exception? exception = null)
    {
        var msLevel = MapLevel(level);
        if (!_logger.IsEnabled(msLevel)) return;

        // Build a structured log scope so the properties surface in
        // pipelines like Serilog / OTel Logs as proper fields, not
        // as a concatenated message string.
        if (properties is { Count: > 0 })
        {
            using var scope = _logger.BeginScope(ToDictionary(properties));
            _logger.Log(msLevel, exception, "{EventName}", eventName);
        }
        else
        {
            _logger.Log(msLevel, exception, "{EventName}", eventName);
        }
    }

    private static LogLevel MapLevel(StructuredLogLevel level) => level switch
    {
        StructuredLogLevel.Trace        => LogLevel.Trace,
        StructuredLogLevel.Debug        => LogLevel.Debug,
        StructuredLogLevel.Information  => LogLevel.Information,
        StructuredLogLevel.Warning      => LogLevel.Warning,
        StructuredLogLevel.Error        => LogLevel.Error,
        StructuredLogLevel.Critical     => LogLevel.Critical,
        _                               => LogLevel.Information,
    };

    private static IReadOnlyDictionary<string, object?> ToDictionary(
        IReadOnlyList<KeyValuePair<string, object?>> properties)
    {
        var d = new Dictionary<string, object?>(properties.Count);
        for (int i = 0; i < properties.Count; i++) d[properties[i].Key] = properties[i].Value;
        return d;
    }
}
