// ActorRef and IRemoteEndpoint live in the bare `Spek` namespace, alongside
// other fundamentals. Engine internals stay
// in `Spek.Runtime`. Same assembly, so internal types (ActorSlot,
// ActorBase) are still reachable via the `using` below.
using Spek.Resilience;
using Spek.Runtime;

namespace Spek;

/// <summary>
/// Stable reference to an actor. The underlying <see cref="ActorBase"/>
/// instance can be replaced by the runtime (e.g. during Restart) without
/// invalidating any refs callers hold.
///
/// <see cref="ActorRef"/> is polymorphic between two
/// dispatch shapes:
///
/// <list type="bullet">
///   <item><b>Local refs</b> — wrap an <see cref="ActorSlot"/> in this
///         process. <c>Tell</c> enqueues directly on the slot's
///         mailbox.</item>
///   <item><b>Remote refs</b> — wrap an <see cref="IRemoteEndpoint"/>
///         supplied by the cluster layer (<c>Spek.Cluster</c>).
///         <c>Tell</c> hands the message off to the endpoint, which
///         routes it across the wire to a peer
///         <see cref="ActorSystem"/>.</item>
/// </list>
///
/// Both shapes present the same surface — callers can't tell which
/// one they're holding (location transparency). Refs are untyped for now;
/// a typed <c>ActorRef&lt;IFoo&gt;</c> is a future addition.
/// </summary>
public sealed class ActorRef
{
    // Exactly one of these is non-null (other than NoSender, which has
    // all null — used as a sentinel for "no sender" in Tell). The reply
    // cell is the third, lightweight shape: the sender ref of an ask,
    // where a Tell completes the asker's task directly instead of going
    // through a mailbox.
    private readonly ActorSlot? _slot;
    private readonly IRemoteEndpoint? _remote;
    private readonly ReplyCell? _replyCell;
    private readonly short _replyToken;   // the cell generation this ref may complete

    internal ActorRef(ActorSlot slot)
    {
        _slot = slot;
        _remote = null;
    }

    internal ActorRef(ReplyCell cell, short token)
    {
        _slot = null;
        _remote = null;
        _replyCell = cell;
        _replyToken = token;
    }

    /// <summary>
    /// Constructs a remote ref backed by <paramref name="endpoint"/>.
    /// Public so the cluster layer (<c>Spek.Cluster</c>) can issue
    /// remote refs from outside <c>Spek.Runtime</c>.
    /// </summary>
    public ActorRef(IRemoteEndpoint endpoint)
    {
        _slot = null;
        _remote = endpoint ?? throw new ArgumentNullException(nameof(endpoint));
    }

    private ActorRef() => _slot = null;  // NoSender construction only

    /// <summary>Fire-and-forget send. Returns immediately.</summary>
    public void Tell(object message)
    {
        if (_slot is not null) _slot.Enqueue(this, message, NoSender);
        else if (_remote is not null) _remote.Dispatch(this, message, NoSender);
        else if (_replyCell is not null) _replyCell.Deliver(_replyToken, message);
        else DeadLetterToNoSender(message);
    }

    /// <summary>
    /// Fire-and-forget send with an explicit <paramref name="sender"/>. The
    /// recipient sees <paramref name="sender"/> in their <c>sender</c>
    /// keyword and any reply via <c>sender.Tell(reply)</c> routes back
    /// to it. Used by hosting adapters (and test probes) that bridge
    /// external lifecycle events into actor messages and need replies
    /// to flow back out.
    /// </summary>
    public void Tell(object message, ActorRef sender)
    {
        if (_slot is not null) _slot.Enqueue(this, message, sender);
        else if (_remote is not null) _remote.Dispatch(this, message, sender);
        else if (_replyCell is not null) _replyCell.Deliver(_replyToken, message);
        else DeadLetterToNoSender(message);
    }

    /// <summary>
    /// A Tell aimed at <see cref="NoSender"/> (a reply to a message that
    /// carried no sender). Silently dropping it makes lost replies
    /// undiagnosable, so write one dead-letter line to stderr — the
    /// documented behavior. NoSender belongs to no system, so the
    /// system-configured sink isn't reachable from here.
    /// </summary>
    private static void DeadLetterToNoSender(object message) =>
        Console.Error.WriteLine(
            $"[spek] dead-letter {message.GetType().Name}: " +
            "reply sent to NoSender — the original message carried no sender " +
            "(it was sent from outside an actor, e.g. a program block or a " +
            "raw Tell without a sender argument)");

    /// <summary>Request-reply. Awaitable inside on-handler bodies only.</summary>
    public ValueTask<TResponse> AskAsync<TResponse>(object message)
    {
        if (_remote is not null)
            throw new NotSupportedException(
                "AskAsync against a remote actor is not yet supported — " +
                "use Tell with an explicit reply message for now.");

        var scope = _slot?.SystemForChildren?.ReplyScope;
        var cell = PooledReplyCell<TResponse>.Rent(scope);
        var token = cell.Token;
        if (_slot is null)
        {
            // Wedge-class guard: an ask against a slotless ref (NoSender, a
            // reply ref) used to return a task that never completed. Fail
            // fast instead — a hang is the worst possible diagnostic.
            cell.Fail(token, new InvalidOperationException(
                "AskAsync on a ref with no local actor (NoSender or a reply " +
                "ref) — this ask could never complete."));
            return new ValueTask<TResponse>(cell, token);
        }
        // Issued before the enqueue so the scope never observes a completion
        // it hasn't seen the ask for (dispatch can complete the cell inline).
        scope?.OnIssued();
        _slot.Enqueue(this, message, new ActorRef(cell, token));
        return new ValueTask<TResponse>(cell, token);
    }

    /// <summary>
    /// Fails a pending ask when the target's handler threw before it could
    /// reply. On a cell-backed sender the asker's task faults directly; a
    /// regular actor sender routes to <see cref="ActorBase.FailReplyWith"/>,
    /// whose base implementation is a no-op — safe to call always.
    /// </summary>
    internal void FailReply(Exception ex)
    {
        if (_replyCell is not null) _replyCell.Fail(_replyToken, ex);
        else _slot?.Current?.FailReplyWith(ex);
    }

    /// <summary>
    /// True when this ref is an ask's reply-cell sender whose reply has not
    /// yet completed — the dispatch loop fails such a cell if the handler
    /// returns without replying, so a no-timeout <c>AskAsync</c> can't hang.
    /// False for regular sends and already-completed asks (making a follow-up
    /// <see cref="FailReply"/> a harmless no-op).
    /// </summary>
    internal bool IsPendingReply => _replyCell is not null && !_replyCell.IsCompleted(_replyToken);

    /// <summary>
    /// Request-reply with a deadline. Identical to
    /// <see cref="AskAsync{TResponse}(object)"/> but faults with
    /// <see cref="TimeoutException"/> if no reply arrives within
    /// <paramref name="timeout"/> — an ask against a stopped, wedged, or
    /// reply-less target should never hang its caller forever.
    ///
    /// Since perf r11 the deadline rides the reply cell itself, armed on the
    /// system's shared <see cref="AskDeadlineSweeper"/> — no bridging
    /// <c>Task&lt;T&gt;</c>, no per-ask WaitAsync timer, so a timeout ask
    /// allocates the same as a plain one. Deadlines fire up to one sweep
    /// period (~25&#160;ms) late, never early.
    /// </summary>
    public ValueTask<TResponse> AskAsync<TResponse>(object message, TimeSpan timeout)
    {
        if (_remote is not null)
            throw new NotSupportedException(
                "AskAsync against a remote actor is not yet supported — " +
                "use Tell with an explicit reply message for now.");

        var scope = _slot?.SystemForChildren?.ReplyScope;
        var cell = PooledReplyCell<TResponse>.Rent(scope);
        var token = cell.Token;
        if (_slot is null)
        {
            cell.Fail(token, new InvalidOperationException(
                "AskAsync on a ref with no local actor (NoSender or a reply " +
                "ref) — this ask could never complete."));
            return new ValueTask<TResponse>(cell, token);
        }
        // Issued before Arm: arming on a disposed system fails the cell
        // immediately, and that failure must land on an already-issued ask.
        scope?.OnIssued();
        // Arm before enqueueing so a reply can never race an unarmed window;
        // the cell's token gate makes reply-vs-deadline first-wins either way.
        _slot.SystemForChildren?.AskDeadlines.Arm(cell, token, timeout);
        _slot.Enqueue(this, message, new ActorRef(cell, token));
        return new ValueTask<TResponse>(cell, token);
    }

    /// <summary>True if the target actor has stopped (voluntary exit or a Stop
    /// directive). A reply-cell ref (an ask's sender) counts as stopped once
    /// its reply has landed.</summary>
    public bool IsStopped =>
        _slot?.IsStopped ?? _remote?.IsKnownDead ?? _replyCell?.IsCompleted(_replyToken) ?? true;

    /// <summary>
    /// True if the actor has a live instance loaded. False when the slot has
    /// been passivated (unloaded to free memory) — the next <see cref="Tell(object)"/>
    /// will re-materialise it. Always true for remote refs (the runtime can't
    /// inspect a remote actor's materialization state).
    /// </summary>
    public bool IsMaterialized => _remote is not null || _slot?.Current is not null;

    /// <summary>True if this ref points at an actor in another system.</summary>
    public bool IsRemote => _remote is not null;

    /// <summary>
    /// Per-actor configuration for the reader/writer concurrency
    /// model. Set in <c>init()</c> or any handler:
    /// <code>self.Readers.Strategy = ReaderStrategy.Fair;</code>
    /// <code>self.Readers.Max = 8;</code>
    /// Defaults: writer-preferring, uncapped. No-op for remote refs —
    /// the policy is enforced on the actor's home node.
    /// </summary>
    public ReaderPolicy Readers => _slot?.Readers ?? _remoteReadersStub;
    private static readonly ReaderPolicy _remoteReadersStub = new();

    /// <summary>The remote endpoint backing this ref, or null for local refs.</summary>
    public IRemoteEndpoint? RemoteEndpoint => _remote;

    /// <summary>
    /// The current live actor instance, or <c>null</c> if the slot has been
    /// stopped or passivated. Exposed to <c>Spek.Testing</c> only.
    /// </summary>
    internal ActorBase? Underlying => _slot?.Current;

    /// <summary>The backing slot — runtime-internal supervision plumbing only.</summary>
    internal ActorSlot? Slot => _slot;

    /// <summary>
    /// Attach an <see cref="IngressPolicy"/> that gates messages
    /// destined for this actor. Multiple policies may be attached;
    /// they are evaluated in attachment order and the first non-Allow
    /// decision wins. A rejected message dead-letters with the
    /// policy's reason; a deferred one is re-enqueued by the runtime
    /// after the policy's RetryAfter, dead-lettering only when its
    /// bounded re-admission budget runs out (or the actor has stopped).
    ///
    /// No-op for remote refs — apply ingress policies on the actor's
    /// home node, not on the caller side.
    /// </summary>
    public ActorRef AttachIngressPolicy(IngressPolicy policy)
    {
        ArgumentNullException.ThrowIfNull(policy);
        _slot?.AttachIngressPolicy(policy);
        return this;
    }

    /// <summary>
    /// Returns an <see cref="EventHandler{TEventArgs}"/> that forwards
    /// the event payload back to this actor via <see cref="Tell(object)"/>.
    /// Lets <c>init()</c> bridge C# events into the actor's mailbox
    /// without the lambda boilerplate:
    /// <code>
    /// _watcher.FileChanged += self.Forward();
    /// // T is inferred from the event delegate's TEventArgs.
    /// </code>
    /// The dispatched message can be handled by a <c>private on T</c>
    /// arm — the CE0020 relaxation lets private handlers bind to
    /// arbitrary CLR types (not just declared <c>message</c> types),
    /// so wrapping the event-args in a Spek message is unnecessary.
    /// </summary>
    public EventHandler<T> Forward<T>() => (_, e) => Tell(e!);

    /// <summary>Send with an explicit sender. Used by Spek.Testing's TestProbe.</summary>
    internal void Enqueue(object message, ActorRef sender) => _slot?.Enqueue(this, message, sender);

    internal static readonly ActorRef NoSender = new();
}

/// <summary>
/// Adapter contract for routing <see cref="ActorRef.Tell(object)"/> to a
/// remote actor across the wire. Implemented by the cluster layer
/// (<c>Spek.Cluster</c>) and supplied to <see cref="ActorRef(IRemoteEndpoint)"/>
/// when a system resolves a remote actor reference.
/// </summary>
public interface IRemoteEndpoint
{
    /// <summary>Forward a message to the remote actor identified by this endpoint.</summary>
    void Dispatch(ActorRef self, object message, ActorRef? sender);

    /// <summary>
    /// True if the underlying transport / membership layer has decided this
    /// endpoint's target node is unreachable. <see cref="ActorRef.IsStopped"/>
    /// surfaces this so user code observing <c>IsStopped</c> sees the same
    /// "actor's gone" signal whether the cause was local stop or remote
    /// node loss.
    /// </summary>
    bool IsKnownDead { get; }
}
