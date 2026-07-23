using Spek.Runtime;
using Xunit.Sdk;

namespace Spek.Testing;

/// <summary>
/// Stand-in actor for use in tests. Captures every message it receives so tests
/// can assert on content, type, and ordering. Hand its <see cref="Ref"/> to code
/// under test wherever an <see cref="ActorRef"/> is expected.
/// </summary>
public sealed class TestProbe
{
    private static readonly TimeSpan DefaultTimeout = TimeSpan.FromSeconds(3);
    private static readonly TimeSpan DefaultSilenceWindow = TimeSpan.FromMilliseconds(300);

    private readonly TestProbeActor _actor;

    internal TestProbe(TestProbeActor actor, ActorRef @ref)
    {
        _actor = actor;
        Ref = @ref;
    }

    /// <summary>The probe's <see cref="ActorRef"/>. Inject it into the code
    /// under test wherever an <see cref="ActorRef"/> is expected — every
    /// message sent to it is captured for the <c>Expect…</c> assertions,
    /// and it identifies the probe as the sender in
    /// <see cref="Send"/>.</summary>
    public ActorRef Ref { get; }

    /// <summary>Sends a message to <paramref name="target"/> with this probe as the sender,
    /// so replies via <c>sender.Tell</c> land back in the probe's mailbox.</summary>
    public void Send(ActorRef target, object message)
        => target.Enqueue(message, Ref);

    /// <summary>Waits for the next message and asserts it is of type <typeparamref name="T"/>.</summary>
    public T ExpectMsg<T>(TimeSpan? timeout = null)
    {
        var window = timeout ?? DefaultTimeout;
        if (!_actor.TryTake(window, out var item))
            throw new XunitException(
                $"Expected message of type {typeof(T).Name} within {window}, but none arrived.");

        if (item.Message is T typed) return typed;

        throw new XunitException(
            $"Expected {typeof(T).Name} but received {item.Message.GetType().Name}: {item.Message}");
    }

    /// <summary>
    /// Non-generic <see cref="ExpectMsg{T}(TimeSpan?)"/>. Useful for dynamically-compiled
    /// message types whose <see cref="Type"/> is only known at runtime.
    /// </summary>
    public object ExpectMsg(Type expectedType, TimeSpan? timeout = null)
    {
        var window = timeout ?? DefaultTimeout;
        if (!_actor.TryTake(window, out var item))
            throw new XunitException(
                $"Expected message of type {expectedType.Name} within {window}, but none arrived.");

        if (expectedType.IsInstanceOfType(item.Message)) return item.Message;

        throw new XunitException(
            $"Expected {expectedType.Name} but received {item.Message.GetType().Name}: {item.Message}");
    }

    /// <summary>Waits for a message of type <typeparamref name="T"/> that matches the predicate.</summary>
    public T ExpectMsg<T>(Func<T, bool> predicate, TimeSpan? timeout = null)
    {
        var msg = ExpectMsg<T>(timeout);
        if (!predicate(msg))
            throw new XunitException(
                $"Message of type {typeof(T).Name} did not match predicate: {msg}");
        return msg;
    }

    /// <summary>Asserts no message arrives in the given window (default 300ms).</summary>
    public void ExpectNoMsg(TimeSpan? within = null)
    {
        var window = within ?? DefaultSilenceWindow;
        if (_actor.TryTake(window, out var item))
            throw new XunitException(
                $"Expected no message within {window}, but received {item.Message.GetType().Name}: {item.Message}");
    }
}
