using Microsoft.Extensions.Logging;

namespace Spek.Tests.Observability;

/// <summary>
/// Test double for <see cref="ILoggerFactory"/>: hands every category
/// the same <see cref="RecordingLogger"/> and remembers which category
/// names were requested.
/// </summary>
internal sealed class RecordingLoggerFactory : ILoggerFactory
{
    public RecordingLogger Logger { get; } = new();
    public List<string> Categories { get; } = new();

    public ILogger CreateLogger(string categoryName)
    {
        Categories.Add(categoryName);
        return Logger;
    }

    public void AddProvider(ILoggerProvider provider) { }
    public void Dispose() { }
}

/// <summary>
/// Recording <see cref="ILogger"/>: captures each Log call together
/// with the scopes active at the moment of the call, and honors a
/// configurable minimum level so tests can exercise the
/// <c>IsEnabled</c> short-circuit.
/// </summary>
internal sealed class RecordingLogger : ILogger
{
    internal sealed record Entry(
        LogLevel Level,
        string Message,
        Exception? Exception,
        IReadOnlyList<object?> Scopes,
        object? State);

    private readonly object _gate = new();
    private readonly List<Entry> _entries = new();
    private readonly List<object?> _activeScopes = new();

    /// <summary>Minimum enabled level; default records everything.</summary>
    public LogLevel MinLevel { get; set; } = LogLevel.Trace;

    public IReadOnlyList<Entry> Entries
    {
        get { lock (_gate) return _entries.ToArray(); }
    }

    public IDisposable? BeginScope<TState>(TState state) where TState : notnull
    {
        lock (_gate) _activeScopes.Add(state);
        return new ScopePopper(this, state);
    }

    public bool IsEnabled(LogLevel logLevel) =>
        logLevel != LogLevel.None && logLevel >= MinLevel;

    public void Log<TState>(
        LogLevel logLevel, EventId eventId, TState state,
        Exception? exception, Func<TState, Exception?, string> formatter)
    {
        lock (_gate)
        {
            _entries.Add(new Entry(
                logLevel,
                formatter(state, exception),
                exception,
                _activeScopes.ToArray(),
                state));
        }
    }

    private sealed class ScopePopper : IDisposable
    {
        private readonly RecordingLogger _owner;
        private readonly object? _state;

        public ScopePopper(RecordingLogger owner, object? state)
        {
            _owner = owner;
            _state = state;
        }

        public void Dispose()
        {
            lock (_owner._gate) _owner._activeScopes.Remove(_state);
        }
    }
}
