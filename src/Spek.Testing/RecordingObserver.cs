using Spek.Runtime;

namespace Spek.Testing;

/// <summary>
/// In-memory inbox observer for tests — the tap-side mirror of
/// <see cref="RecordingDeadLetterSink"/>. Attach with
/// <c>system.Observe(actor, recorder.OnMessage)</c> and assert on the
/// traffic without instrumenting the actor under test.
/// </summary>
public sealed class RecordingObserver
{
    private readonly List<ObservedMessage> _messages = [];
    private readonly object _gate = new();

    /// <summary>Snapshot of every observed message, in arrival order.</summary>
    public IReadOnlyList<ObservedMessage> Messages
    {
        get { lock (_gate) return _messages.ToArray(); }
    }

    /// <summary>Number of messages observed so far.</summary>
    public int Count
    {
        get { lock (_gate) return _messages.Count; }
    }

    /// <summary>The observer callback; pass to <c>ActorSystem.Observe</c>.</summary>
    public void OnMessage(ObservedMessage observed)
    {
        lock (_gate) _messages.Add(observed);
    }
}
