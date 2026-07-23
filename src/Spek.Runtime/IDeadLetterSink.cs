namespace Spek.Runtime;

/// <summary>
/// Receives notifications about messages the runtime couldn't deliver —
/// unhandled message types, messages sent to a stopped actor, and messages
/// dropped because their handler threw. Tests use this to assert on what
/// went wrong; production deployments wire it to structured logging.
///
/// The default implementation (<see cref="ConsoleDeadLetterSink"/>) writes
/// a single line per event to <c>Console.Error</c>.
/// </summary>
public interface IDeadLetterSink
{
    /// <summary>
    /// Called when a message couldn't be delivered or processed successfully.
    /// <paramref name="cause"/> is non-null when the failure was an exception
    /// from the handler; null when the message was simply unhandled or the
    /// target actor was already stopped.
    /// </summary>
    void DeadLetter(object message, string reason, Exception? cause);
}

/// <summary>Default sink — one line per event to <see cref="Console.Error"/>.</summary>
public sealed class ConsoleDeadLetterSink : IDeadLetterSink
{
    /// <inheritdoc />
    public void DeadLetter(object message, string reason, Exception? cause)
    {
        Console.Error.WriteLine(
            $"[spek] dead-letter {message.GetType().Name}: {reason}" +
            (cause is null ? "" : $" — {cause.GetType().Name}: {cause.Message}"));
    }
}

/// <summary>In-memory sink for tests — records every event for later assertion.</summary>
public sealed class RecordingDeadLetterSink : IDeadLetterSink
{
    private readonly List<DeadLetterRecord> _records = new();
    private readonly object _gate = new();

    /// <summary>Snapshot of every recorded dead-letter event, in arrival order.</summary>
    public IReadOnlyList<DeadLetterRecord> Records
    {
        get { lock (_gate) return _records.ToArray(); }
    }

    /// <inheritdoc />
    public void DeadLetter(object message, string reason, Exception? cause)
    {
        lock (_gate) _records.Add(new DeadLetterRecord(message, reason, cause));
    }
}

/// <summary>
/// A single dead-letter event captured by <see cref="RecordingDeadLetterSink"/>.
/// </summary>
/// <param name="Message">The message that couldn't be delivered or processed.</param>
/// <param name="Reason">Human-readable description of why it failed.</param>
/// <param name="Cause">The handler exception, when the failure was a throw; otherwise null.</param>
public sealed record DeadLetterRecord(object Message, string Reason, Exception? Cause);
