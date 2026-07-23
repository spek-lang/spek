namespace Spek.Streams;

/// <summary>
/// Base class for stream operators attached to actor handlers via
/// the <c>=&gt;</c> chain syntax. An operator receives messages of
/// type <typeparamref name="T"/> via <see cref="OfferAsync"/> and
/// decides if, when, and with what to invoke <see cref="Dispatch"/>
/// — its onward emit point.
///
/// The compiler wires each operator's <see cref="Dispatch"/> at
/// construction time so the chain delivers messages in source order
/// (each operator's dispatch invokes the next operator's
/// <see cref="OfferAsync"/>; the final operator's dispatch posts a
/// synthetic self-message that re-enters the actor's mailbox so the
/// handler body runs under the actor lock).
///
/// Operators are per-handler-per-actor-instance. They are
/// instantiated once during actor construction and live for the
/// lifetime of the actor.
/// </summary>
public abstract class StreamOperator<T>
{
    /// <summary>
    /// The next-step callback the chain invokes when the operator
    /// decides to emit. Wired by the generated actor code at
    /// construction time. Subclasses call <see cref="Dispatch"/>
    /// (the protected accessor) rather than this field directly.
    /// </summary>
    private Func<T, Task>? _dispatch;

    /// <summary>Backing store for <see cref="Clock"/>. System time until
    /// <see cref="Configure"/> installs the actor's clock.</summary>
    private TimeProvider _clock = TimeProvider.System;

    /// <summary>
    /// Invoked by the runtime once during construction to attach the
    /// operator's onward emit point. After this call, the operator
    /// may invoke <see cref="Dispatch"/> at any time.
    ///
    /// <paramref name="clock"/> is the time source for any window math
    /// or timer the operator keeps; the generated actor code passes the
    /// actor's own clock (<c>self.Clock</c>), so operators ride virtual
    /// time under the test kit's manual clock exactly like passivation
    /// and restart windows do. Omit it (hand-configured operators, unit
    /// tests, library users) to run on the system clock.
    /// </summary>
    public void Configure(Func<T, Task> dispatch, TimeProvider? clock = null)
    {
        ArgumentNullException.ThrowIfNull(dispatch);
        if (_dispatch is not null)
            throw new InvalidOperationException(
                $"StreamOperator<{typeof(T).Name}> already configured.");
        _dispatch = dispatch;
        if (clock is not null) _clock = clock;
    }

    /// <summary>
    /// The operator's time source — the owning actor's clock when the
    /// runtime configured the chain, <see cref="TimeProvider.System"/>
    /// otherwise. Time-based operators read timestamps and create
    /// timers through this so a virtual-time test can drive them with
    /// an explicit <c>Advance</c> instead of real waiting.
    /// </summary>
    protected TimeProvider Clock => _clock;

    /// <summary>
    /// Subclasses call this to emit a message onward. Throws if the
    /// operator has not yet been configured by the runtime.
    /// </summary>
    protected Task Dispatch(T message)
    {
        if (_dispatch is null)
            throw new InvalidOperationException(
                $"StreamOperator<{typeof(T).Name}> has not been configured.");
        return _dispatch(message);
    }

    /// <summary>
    /// Receive a message destined for the wrapped handler. The
    /// operator decides whether (and when) to invoke
    /// <see cref="Dispatch"/> with the same or a transformed value.
    /// </summary>
    public abstract Task OfferAsync(T message);

    /// <summary>
    /// Called when the actor stops, so the operator can flush
    /// pending state — emit a buffered final value, cancel a timer,
    /// etc. The base implementation is a no-op.
    /// </summary>
    public virtual Task StopAsync() => Task.CompletedTask;
}
