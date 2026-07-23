using System.Threading.Channels;

namespace Spek.Runtime;

/// <summary>
/// One message as it entered an actor's mailbox, as seen by an inbox
/// observer (<see cref="ActorSystem.Observe"/>). Passive by construction:
/// Spek messages are immutable records, so holding the reference cannot
/// perturb the actor, reply on its behalf, or reorder delivery.
/// </summary>
/// <param name="Message">The actual message instance.</param>
/// <param name="Sender">The sending actor; null for system- or timer-originated sends.</param>
/// <param name="EnqueuedAt">Mailbox arrival time, read through the system clock (virtual-time aware).</param>
public sealed record ObservedMessage(
    object Message,
    ActorRef? Sender,
    DateTimeOffset EnqueuedAt);

/// <summary>
/// Detach handle for an inbox observer — tcpdump semantics: passive,
/// bounded, and lossy under pressure rather than ever stalling the actor.
/// Events queue into a bounded buffer consumed off the dispatch path; when
/// the observer falls behind, overflow is dropped and counted in
/// <see cref="Dropped"/> (never blocking the enqueue). An observer callback
/// that throws has its exception routed to the dead-letter sink; the actor
/// and its supervisor never see it. Dispose to detach — events already
/// buffered still drain to the callback before the pump exits.
/// </summary>
public sealed class InboxObserverHandle : IDisposable
{
    private readonly Channel<ObservedMessage> _buffer;
    private readonly ActorSlot _slot;
    private long _dropped;
    private int _disposed;

    internal InboxObserverHandle(
        ActorSlot slot, Action<ObservedMessage> onMessage,
        IDeadLetterSink? deadLetterSink, int bufferCapacity)
    {
        _slot = slot;
        _buffer = Channel.CreateBounded<ObservedMessage>(new BoundedChannelOptions(bufferCapacity)
        {
            SingleReader = true,
            SingleWriter = false,
        });
        _ = Task.Run(async () =>
        {
            try
            {
                await foreach (var observed in _buffer.Reader.ReadAllAsync().ConfigureAwait(false))
                {
                    try { onMessage(observed); }
                    catch (Exception ex)
                    {
                        deadLetterSink?.DeadLetter(observed.Message,
                            "inbox observer threw", cause: ex);
                    }
                }
            }
            catch (Exception ex)
            {
                // A dead pump must be loud: everything after this is dropped.
                Console.Error.WriteLine(
                    $"[spek] inbox observer pump crashed ({ex.GetType().Name}: {ex.Message}); tap is dead");
            }
        });
    }

    /// <summary>Events lost because the observer fell behind the bounded buffer.</summary>
    public long Dropped => Interlocked.Read(ref _dropped);

    internal void Publish(ObservedMessage observed)
    {
        if (!_buffer.Writer.TryWrite(observed))
            Interlocked.Increment(ref _dropped);
    }

    /// <summary>Stops accepting events; buffered ones still drain. Idempotent.</summary>
    internal void Complete() => _buffer.Writer.TryComplete();

    /// <inheritdoc />
    public void Dispose()
    {
        if (Interlocked.Exchange(ref _disposed, 1) != 0) return;
        _slot.DetachObserver(this);
        _buffer.Writer.TryComplete();
    }
}
