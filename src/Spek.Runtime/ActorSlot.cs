using System.Collections.Concurrent;
using Spek.Persistence;
using Spek.Resilience;

namespace Spek.Runtime;

/// <summary>
/// The mutable container behind an <see cref="ActorRef"/>. Owns the mailbox
/// and the processing loop; holds the current <see cref="ActorBase"/>
/// instance but can swap it on Restart, null it on Stop, or unload it on
/// passivation (re-materialising on the next message).
///
/// Separating "the identity callers hold" (<see cref="ActorRef"/>) from "the
/// currently-live instance" (this slot's <see cref="Current"/>) is the
/// plumbing that makes Restart and passivation possible — the instance can
/// change while the reference callers hold stays stable.
/// </summary>
internal sealed class ActorSlot : IDisposable, IThreadPoolWorkItem
{
    private readonly Func<ActorBase> _factory;
    private readonly ActorSystem? _system;
    private readonly string? _persistenceKey;
    private readonly ISnapshotStore? _snapshotStore;
    private readonly IDeadLetterSink? _deadLetterSink;
    private readonly ActorSlot? _parent;

    // Children tracked per slot so AllForOne strategies can broadcast a
    // restart to all siblings of a failing child. Children register
    // themselves via <see cref="RegisterChild"/> during spawn.
    private readonly List<ActorSlot> _children = new();
    private readonly object _childrenGate = new();

    // Two-ref mailbox tuple (perf r5). Trace context rides a TracedMessage
    // wrapper ONLY when a listener is active: a deep backlog's queue
    // segments survive Gen0/Gen1 while draining, so surviving bytes scale
    // with tuple width — an unconditional ActivityContext tripled it.
    private readonly ConcurrentQueue<(object Message, ActorRef Sender)> _mailbox = new();

    /// <summary>Trace-context envelope, used only while tracing listeners
    /// are attached; unwrapped at dequeue.</summary>
    private sealed class TracedMessage(object inner, System.Diagnostics.ActivityContext context)
    {
        public readonly object Inner = inner;
        public readonly System.Diagnostics.ActivityContext Context = context;
    }

    private static (object Message, ActorRef Sender, System.Diagnostics.ActivityContext ParentTrace)
        Unwrap((object Message, ActorRef Sender) raw)
        => raw.Message is TracedMessage t
            ? (t.Inner, raw.Sender, t.Context)
            : (raw.Message, raw.Sender, default);
    private readonly object _materializeGate = new();

    // Ingress policies applied before each message dispatch. Read inside
    // the dispatch loop so updates from AttachIngressPolicy are picked up
    // for the next message without locking on every send.
    private volatile IngressPolicy[] _ingressPolicies = Array.Empty<IngressPolicy>();
    private readonly object _policiesGate = new();

    // Reader/writer concurrency state.
    //
    // The lock has three states:
    //   _writerActive=true             — exclusive writer in flight
    //   _activeReaders > 0             — N readers in flight, no writer
    //   _writerActive=false && _activeReaders=0 — idle
    //
    // Coordination uses a single gate and two TCSs:
    //   _readersDrained — pulsed when _activeReaders drops to 0; a
    //                     pending writer awaits this.
    //   _writerReleased — pulsed when a writer exits; readers waiting
    //                     for the gate to clear (and other writers)
    //                     await this.
    //
    // Writer-preferring fairness is enforced by the entry order: when a
    // writer registers in `_writerWaiting`, new reader entries see this
    // and back off. Fair mode skips that check.
    public ReaderPolicy Readers { get; } = new();

    private readonly object _rwGate = new();
    private int _activeReaders;
    private bool _writerActive;
    private int _writerWaiting;
    // Lazy signaling (perf r4): these are null until a waiter actually
    // blocks — the uncontended fast paths must not allocate. A waiter
    // creates the TCS for the condition it awaits (under the gate); the
    // releaser consumes-and-nulls it, pulsing outside the gate. Every
    // pulse invalidates the instance, so a fresh waiter always waits on
    // a fresh (or shared in-flight) TCS — no stale-completed reuse.
    private TaskCompletionSource? _readersDrained;
    private TaskCompletionSource? _writerReleased;

    // In-flight reader accounting (perf r9): a counter plus a lazily
    // created idle pulse (the r4 lazy-signaling pattern) replaces the old
    // task list + ContinueWith + O(n) removal — Stop / Restart / Dispose
    // wait for the counter to hit zero.
    private int _inFlightReaders;
    private TaskCompletionSource? _readersIdle;
    private readonly object _readerIdleGate = new();

    // ─── Slow-handler watchdog stamps (detection only) ──────────────────────
    //
    // _dispatchStartMs: Environment.TickCount64, written unconditionally
    // right before the writer-arm handler invoke and cleared (0 = idle) in
    // the dispatch finally. One int64 store each way per message — the cost
    // class ObservabilityCostBenchmarks measured at noise.
    //
    // _readerPhaseStartMs tracks the reader PHASE, not each reader: stamped
    // when _inFlightReaders goes 0→1, cleared when it returns to 0 (both
    // under _readerIdleGate, count re-checked so a phase-edge race can't
    // strand a live phase stampless). Deliberately approximate — a wedged
    // reader keeps the count nonzero so the stamp goes stale and gets
    // flagged, which is the case that matters; the flip side is that a
    // continuously-busy stretch of overlapping fast readers older than the
    // threshold is flagged once too, even though no single reader is slow.
    //
    // The watchdog reads the stamps (Volatile) off its own timer thread; the
    // Watchdog* fields are its once-per-occurrence bookkeeping, written only
    // from inside its gated sweep.
    private long _dispatchStartMs;
    private long _readerPhaseStartMs;
    internal long WatchdogReportedDispatchStamp;
    internal long WatchdogReportedReaderStamp;

    internal long DispatchStartMs => Volatile.Read(ref _dispatchStartMs);
    internal long ReaderPhaseStartMs => Volatile.Read(ref _readerPhaseStartMs);
    internal string? LastMessageTypeName => _lastMessageType?.Name;

    private ActorBase? _current;
    private volatile bool _stopped;
    // Guards the stop sequence (OnPostStop → OnTerm) so it runs at most once per
    // slot, no matter which path triggers the stop — supervision, voluntary
    // StopSelf, or a system shutdown via Dispose/StopGracefully.
    private int _stopHooksRan;
    private int _processing;

    private ActorRef? _selfRef;
    private DateTime _lastActivityUtc;
    private TimeSpan? _passivationTimeout;
    private ITimer? _passivationTimer;

    // Semantic time flows through the system clock so tests can control it;
    // a slot created before Initialize (no system yet) falls back to real time.
    private TimeProvider SlotClock => _system?.Clock ?? TimeProvider.System;
    private volatile bool _disposed;
    private readonly List<DateTime> _restartLog = new();

    public ActorSlot(
        Func<ActorBase> factory,
        ActorSystem? system,
        string? persistenceKey,
        ISnapshotStore? snapshotStore,
        IDeadLetterSink? deadLetterSink,
        ActorSlot? parent = null)
    {
        _factory = factory;
        _system = system;
        _persistenceKey = persistenceKey;
        _snapshotStore = snapshotStore;
        _deadLetterSink = deadLetterSink;
        _parent = parent;
        _spawnedAt = SlotClock.GetUtcNow();
    }

    public ActorBase? Current => _current;
    public bool IsStopped => _stopped;
    public bool IsIdle => _mailbox.IsEmpty && _processing == 0;

    // ─── Introspection metadata (read by ActorSystem.SnapshotActors) ────────

    private readonly DateTimeOffset _spawnedAt;
    private volatile Type? _lastMessageType;

    /// <summary>Stable display identity for introspection: the persistence
    /// key when one exists, otherwise <c>TypeName</c> / <c>TypeName#N</c>
    /// assigned by <see cref="ActorSystem.TrackSlot"/>.</summary>
    internal string DisplayName { get; set; } = "(untracked)";

    internal string? PersistenceKey => _persistenceKey;

    /// <summary>Messages fully entering dispatch (post stopped-check), for
    /// test-kit invariants — "sent == dispatched + dead-lettered".</summary>
    internal long DispatchedCount => Interlocked.Read(ref _dispatchedCount);
    private long _dispatchedCount;

    /// <summary>Messages admitted to the mailbox (past the chaos and
    /// stopped gates; deferred/delayed re-admissions count again on
    /// re-entry). With <see cref="StoppedDropCount"/> this closes the
    /// audit-mode conservation identity: enqueued == dispatched +
    /// dead-lettered-on-dequeue + still-parked. Counter only — dispatch
    /// never reads it.</summary>
    internal long EnqueuedCount => Interlocked.Read(ref _enqueuedCount);
    private long _enqueuedCount;

    /// <summary>Dequeued messages dead-lettered at the dispatch-side stopped
    /// check — the one exit from the mailbox that never increments
    /// <see cref="DispatchedCount"/>. Counter only.</summary>
    internal long StoppedDropCount => Interlocked.Read(ref _stoppedDropCount);
    private long _stoppedDropCount;

    /// <summary>Current mailbox depth. O(depth) segment walk — audit and
    /// introspection only, never the dispatch path.</summary>
    internal int MailboxDepth => _mailbox.Count;

    /// <summary>The self ref captured at materialization — the simulator
    /// dispatches through it.</summary>
    internal ActorRef? Self => _selfRef;

    /// <summary>True when the mailbox holds at least one message.</summary>
    internal bool HasMail => !_mailbox.IsEmpty;

    /// <summary>Point-in-time introspection view. Reads counters and cheap
    /// snapshots only — never locks the mailbox, never touches actor state.</summary>
    internal ActorSnapshot Snapshot()
    {
        var instance = _current;
        string[] children;
        lock (_childrenGate)
            children = _children.Select(c => c.DisplayName).ToArray();
        return new ActorSnapshot(
            Path:            DisplayName,
            ActorType:       instance?.GetType().Name ?? "(unmaterialized)",
            Behavior:        instance?.CurrentBehaviorName,
            MailboxDepth:    _mailbox.Count,
            MailboxHead:     _mailbox.Take(8)
                                 .Select(i => ((i.Message as TracedMessage)?.Inner ?? i.Message).GetType().Name)
                                 .ToArray(),
            Restarts:        RestartCount,
            DispatchedCount: DispatchedCount,
            LastMessageType: _lastMessageType?.Name,
            SpawnedAt:       _spawnedAt,
            IsMaterialized:  instance is not null,
            IsStopped:       _stopped,
            Children:        children);
    }

    /// <summary>How many times this actor has been restarted by supervision —
    /// including sibling restarts under AllForOne, which do not consume the
    /// actor's own restart budget. Observability and budget accounting are
    /// deliberately separate: <see cref="_restartLog"/> answers "may it
    /// restart again?" (windowed, own-failure only); this counter answers
    /// "how many times did it restart?" Exposed for test observability
    /// (Spek.Testing, via InternalsVisibleTo).</summary>
    internal int RestartCount => Volatile.Read(ref _observedRestarts);

    private int _observedRestarts;

    /// <summary>
    /// Marks the slot stopped — the dispatch loop won't pick up another
    /// message after the current one completes. Called by
    /// <see cref="ActorBase.StopSelf"/> for voluntary self-shutdown.
    /// Pending mailbox messages are dead-lettered when the loop unwinds.
    /// </summary>
    internal void Stop()
    {
        _stopped = true;
        // Stop counter, tagged with the voluntary
        // cause (matches "supervision" / "budget-exceeded" tagging on
        // the supervised stop path so dashboards can sum or split).
        var typeName = _current?.GetType().Name ?? "unknown";
        _system?.Metrics.Counter(
            Spek.Observability.SpekMetricNames.ActorStop,
            tags: new[]
            {
                new KeyValuePair<string, object?>("actor.type", typeName),
                new KeyValuePair<string, object?>("cause", "voluntary"),
            });
        _system?.SignalSlotActivity();
        CompleteObservers();
    }

    /// <summary>
    /// Runs the stop sequence — <c>OnPostStop</c> then the <c>term { }</c> block
    /// (<c>OnTerm</c>) — exactly once per slot, regardless of which path triggers
    /// the stop. Failures in either hook are dead-lettered, never thrown, so one
    /// actor's bad cleanup can't block the rest of a shutdown.
    /// </summary>
    private void RunStopHooks(ActorBase instance)
    {
        if (Interlocked.Exchange(ref _stopHooksRan, 1) != 0) return;
        try { instance.InvokeOnPostStop(); }
        catch (Exception postEx)
        {
            _deadLetterSink?.DeadLetter(instance, "OnPostStop threw", cause: postEx);
        }
        try { instance.InvokeOnTerm(); }
        catch (Exception termEx)
        {
            _deadLetterSink?.DeadLetter(instance, "term block threw", cause: termEx);
        }
    }

    /// <summary>
    /// Graceful stop for the system-shutdown path. Marks the slot stopped
    /// and runs the stop hooks (<c>OnPostStop</c> → <c>OnTerm</c>) for the
    /// materialized actor. Called by <see cref="ActorSystem.Dispose"/> for every
    /// tracked slot *after* the drain, so a system shutdown — including the
    /// hostless <c>self.System.Shutdown()</c> path — stops actors gracefully
    /// instead of abandoning them. A never-materialized slot has no hooks to run.
    /// Idempotent: a slot already stopped (supervision/StopSelf) won't re-fire.
    /// </summary>
    internal void StopGracefully()
    {
        _stopped = true;
        var instance = _current;
        if (instance is not null) RunStopHooks(instance);
        CompleteObservers();
    }

    /// <summary>Registers a child slot for sibling-broadcast supervision.</summary>
    internal void RegisterChild(ActorSlot child)
    {
        lock (_childrenGate) _children.Add(child);
    }

    /// <summary>
    /// Append an ingress policy. Snapshot-replace under a small gate so
    /// the dispatch loop can read <see cref="_ingressPolicies"/>
    /// lock-free.
    /// </summary>
    internal void AttachIngressPolicy(IngressPolicy policy)
    {
        lock (_policiesGate)
        {
            var next = new IngressPolicy[_ingressPolicies.Length + 1];
            Array.Copy(_ingressPolicies, next, _ingressPolicies.Length);
            next[^1] = policy;
            _ingressPolicies = next;
        }
    }

    // ─── Inbox observers (passive tap at enqueue) ───────────────────────────

    // Snapshot-replace like the ingress policies: the enqueue hot path pays
    // one volatile read and a null check when nobody is watching.
    private readonly object _observersGate = new();
    private volatile InboxObserverHandle[]? _observers;

    internal InboxObserverHandle AttachObserver(
        Action<ObservedMessage> onMessage, int bufferCapacity)
    {
        var handle = new InboxObserverHandle(this, onMessage, _deadLetterSink, bufferCapacity);
        lock (_observersGate)
        {
            var current = _observers ?? [];
            var next = new InboxObserverHandle[current.Length + 1];
            current.CopyTo(next, 0);
            next[^1] = handle;
            _observers = next;
        }
        return handle;
    }

    internal void DetachObserver(InboxObserverHandle handle)
    {
        lock (_observersGate)
        {
            var current = _observers;
            if (current is null) return;
            var next = current.Where(o => !ReferenceEquals(o, handle)).ToArray();
            _observers = next.Length == 0 ? null : next;
        }
    }

    private void CompleteObservers()
    {
        var observers = _observers;
        if (observers is null) return;
        foreach (var o in observers) o.Complete();
    }

    // ─── Reader/writer concurrency primitives ───────────────────────────────

    /// <summary>
    /// Acquire a reader slot. Blocks (asynchronously) until: no writer
    /// is in flight, the reader-cap (<see cref="ReaderPolicy.Max"/>) is
    /// not exceeded, and (under writer-preferring) no writer is queued.
    /// </summary>
    internal async Task EnterReaderAsync()
    {
        while (true)
        {
            Task wait;
            lock (_rwGate)
            {
                bool blockedByActiveWriter   = _writerActive;
                bool blockedByQueuedWriter   = Readers.Strategy == ReaderStrategy.WriterPreferring
                                                && _writerWaiting > 0;
                bool blockedByCap            = _activeReaders >= Readers.Max;

                if (!blockedByActiveWriter && !blockedByQueuedWriter && !blockedByCap)
                {
                    _activeReaders++;
                    return;
                }
                // Compose a wait — re-check on whichever signal fires first.
                // The TCS is created here, by the blocked party, so the
                // uncontended paths never allocate one.
                wait = blockedByActiveWriter || blockedByQueuedWriter
                    ? (_writerReleased ??= new TaskCompletionSource(
                          TaskCreationOptions.RunContinuationsAsynchronously)).Task
                    : Task.Delay(1);   // cap-blocked: short backoff
            }
            await wait.ConfigureAwait(false);
        }
    }

    /// <summary>Release a reader slot. Pulses the readers-drained TCS
    /// when the count hits zero so a queued writer can proceed.</summary>
    internal void ExitReader()
    {
        TaskCompletionSource? toPulse = null;
        lock (_rwGate)
        {
            _activeReaders--;
            if (_activeReaders == 0)
            {
                toPulse = _readersDrained;
                _readersDrained = null;   // consumed: the next waiter makes a fresh one
            }
        }
        toPulse?.TrySetResult();
    }

    /// <summary>
    /// Acquire the writer slot. Blocks (asynchronously) until: no other
    /// writer is in flight AND no readers are in flight. Once acquired,
    /// the writer runs alone — readers cannot enter until
    /// <see cref="ExitWriter"/> is called.
    /// </summary>
    internal async Task EnterWriterAsync()
    {
        // Register intent so writer-preferring readers back off immediately.
        lock (_rwGate) _writerWaiting++;

        try
        {
            while (true)
            {
                Task wait;
                lock (_rwGate)
                {
                    if (!_writerActive && _activeReaders == 0)
                    {
                        _writerActive = true;   // fast path: no allocation
                        return;
                    }
                    // Wait on whichever lock state needs to clear; the
                    // blocked party creates the signal it needs.
                    wait = _writerActive
                        ? (_writerReleased ??= new TaskCompletionSource(
                              TaskCreationOptions.RunContinuationsAsynchronously)).Task
                        : (_readersDrained ??= new TaskCompletionSource(
                              TaskCreationOptions.RunContinuationsAsynchronously)).Task;
                }
                await wait.ConfigureAwait(false);
            }
        }
        finally
        {
            // Decrement waiting count once we either acquired or aborted.
            // Only decrement if we acquired — otherwise we'd leak a
            // waiting slot. The only way to exit the loop is via the
            // `return` above, so this finally always runs after that.
            lock (_rwGate) _writerWaiting--;
        }
    }

    /// <summary>Release the writer slot. Pulses the writer-released TCS
    /// so blocked readers and other writers can proceed.</summary>
    internal void ExitWriter()
    {
        TaskCompletionSource? toPulse;
        lock (_rwGate)
        {
            _writerActive = false;
            toPulse = _writerReleased;
            _writerReleased = null;   // consumed: nobody waits on it twice
        }
        toPulse?.TrySetResult();
    }

    /// <summary>Track an in-flight reader dispatch task so Stop /
    /// Restart can drain it before tearing the slot down.</summary>
    /// <summary>One reader dispatch, run concurrently with other readers.
    /// The caller has already entered the reader lock and incremented the
    /// in-flight counter; both release in the finally, whatever happens.</summary>
    private async Task RunReaderAsync(
        (object Message, ActorRef Sender, System.Diagnostics.ActivityContext ParentTrace) item,
        ActorBase instance)
    {
        // Child trace span under the sender's parent context so distributed
        // traces stitch across actor boundaries. StartActivity returns null
        // when no ActivityListener is registered (the common case), making
        // this effectively a no-op.
        using var activity = ActorSystem.ActivitySource.StartActivity(
            $"spek.actor.{instance.GetType().Name}.handler",
            System.Diagnostics.ActivityKind.Consumer,
            parentContext: item.ParentTrace);

        try
        {
            await instance.InvokeDispatch(item.Message, item.Sender)
                .ConfigureAwait(false);
            _lastActivityUtc = SlotClock.GetUtcNow().UtcDateTime;
        }
        catch (Exception readerEx)
        {
            // Reader exceptions don't trigger supervision — a reader can't
            // corrupt actor state by construction (CE0087 enforces this).
            // Dead-letter and keep the actor alive.
            _deadLetterSink?.DeadLetter(item.Message,
                "reader handler threw; message dead-lettered",
                cause: readerEx);

            // If the asker is awaiting a reply (its sender ref wraps a
            // reply cell), surface the failure as an AskException so the
            // caller's await throws instead of hanging. For regular
            // non-asking senders FailReply is a no-op — safe to call always.
            item.Sender?.FailReply(new AskException(
                targetActorPath:
                    instance.GetType().FullName ?? instance.GetType().Name,
                messageTypeName:
                    item.Message.GetType().Name,
                inner: readerEx));
        }
        finally
        {
            ExitReader();
            ReaderCompleted();
        }
    }

    /// <summary>The last reader out consumes-and-pulses the idle signal
    /// (created lazily by a drain waiter — uncontended paths allocate
    /// nothing).</summary>
    private void ReaderCompleted()
    {
        if (Interlocked.Decrement(ref _inFlightReaders) != 0) return;
        TaskCompletionSource? pulse;
        lock (_readerIdleGate)
        {
            // Clear the watchdog's reader-phase stamp only if the phase is
            // still over: a fresh reader may have entered (0→1) between our
            // decrement and this lock, and its stamp must survive us.
            if (Volatile.Read(ref _inFlightReaders) == 0) _readerPhaseStartMs = 0;
            pulse = _readersIdle;
            _readersIdle = null;
        }
        pulse?.TrySetResult();
    }

    /// <summary>Wait for all in-flight reader dispatches to complete.
    /// Used by Stop / Restart / Dispose paths.</summary>
    private Task DrainReadersAsync()
    {
        if (Volatile.Read(ref _inFlightReaders) == 0) return Task.CompletedTask;
        Task wait;
        lock (_readerIdleGate)
        {
            if (Volatile.Read(ref _inFlightReaders) == 0) return Task.CompletedTask;
            wait = (_readersIdle ??= new TaskCompletionSource(
                TaskCreationOptions.RunContinuationsAsynchronously)).Task;
        }
        return wait;
    }

    /// <summary>
    /// Snapshot of this slot's tracked children, for parent-side supervision
    /// logic (AllForOne broadcasts, future child-directory queries).
    /// </summary>
    internal IReadOnlyList<ActorSlot> GetChildren()
    {
        lock (_childrenGate) return _children.ToArray();
    }

    /// <summary>
    /// Externally-triggered restart — used by <c>AllForOne</c> supervision
    /// to restart a sibling that didn't itself throw. Drops the current
    /// instance; the next enqueue re-materialises fresh (and
    /// <c>OnRestore</c> reloads from the latest snapshot for persistent
    /// actors). In-flight dispatch on the old instance completes on that
    /// instance, then subsequent messages route to the new one.
    /// </summary>
    internal void ScheduleRestart()
    {
        if (_stopped) return;
        // A sibling restart is observable (counter, metric) but does not
        // consume this actor's restart budget — the failure wasn't its own.
        Interlocked.Increment(ref _observedRestarts);
        _system?.Metrics.Counter(
            Spek.Observability.SpekMetricNames.ActorRestart,
            tags: new[]
            {
                new KeyValuePair<string, object?>(
                    "actor.type", _current?.GetType().Name ?? "(unmaterialized)"),
                new KeyValuePair<string, object?>("restart.cause", "sibling"),
            });
        lock (_materializeGate) { _current = null; }
        _system?.SignalSlotActivity();
    }

    /// <summary>
    /// Force initial materialisation so <see cref="ActorBase.Initialize"/>
    /// (and thus <c>OnPreStart</c> / <c>OnRestore</c>) runs synchronously before
    /// the spawn call returns. Also captures the passivation timeout if the
    /// actor opted in.
    /// </summary>
    internal ActorBase Materialize(ActorRef self)
    {
        _selfRef = self;
        var instance = EnsureMaterialized(self);
        ConfigurePassivationIfEligible(instance);
        return instance;
    }

    /// <summary>
    /// Async-safe counterpart of <see cref="Materialize"/>. Used by
    /// <see cref="ActorSystem.SpawnAsync{TActor}"/> so database-backed
    /// snapshot stores don't block the caller's thread on Initialize.
    /// </summary>
    internal async Task<ActorBase> MaterializeAsync(ActorRef self)
    {
        _selfRef = self;

        ActorBase instance;
        if (_current is not null)
        {
            instance = _current;
        }
        else
        {
            // Factory call must happen under the same lock as the sync path
            // to preserve the "only one instance at a time" invariant.
            instance = _factory();
            await instance.InitializeAsync(self, _system, _persistenceKey, _snapshotStore, _deadLetterSink)
                .ConfigureAwait(false);
            lock (_materializeGate) { _current ??= instance; }
        }

        ConfigurePassivationIfEligible(instance);
        return instance;
    }

    private void ConfigurePassivationIfEligible(ActorBase instance)
    {
        // Wire the passivation timer if the actor declared
        // `passivate after Xs`. Non-persistent actors are
        // also eligible — they get memory release without state
        // preservation, and the next message materialises a fresh
        // instance via the factory + init() block. Persistent actors
        // get the same behaviour but with snapshot save/restore on
        // top, so subsequent rehydration sees their last state.
        if (_passivationTimer is not null) return;

        var timeout = instance.GetPassivationTimeout();
        if (timeout is null || timeout.Value <= TimeSpan.Zero) return;

        _passivationTimeout = timeout;
        // Check at roughly 1/4 the timeout — tight enough to be responsive,
        // loose enough not to burn cycles on idle actors.
        var period = TimeSpan.FromMilliseconds(Math.Max(50, timeout.Value.TotalMilliseconds / 4));
        _passivationTimer = SlotClock.CreateTimer(_ => _ = OnPassivationCheck(), null, period, period);
    }

    internal void Enqueue(ActorRef self, object message, ActorRef sender)
    {
        // Chaos enqueue-path faults (drop / delay / duplicate). Evaluated on
        // first admission only — a delayed message re-enters via
        // EnqueueDirect so a delay rule can't capture it forever.
        if (_system?.Chaos is { } chaos)
        {
            var decision = chaos.OnEnqueue(this, message);
            switch (decision.Kind)
            {
                case ChaosFaultKind.Drop:
                    // Modeled delivery loss: counted in ChaosPlan.Fires, not
                    // dead-lettered — a dead letter is the runtime keeping
                    // its promise; a drop is the fault being modeled.
                    return;
                case ChaosFaultKind.Delay:
                {
                    var timer = (ITimer?)null;
                    timer = SlotClock.CreateTimer(_ =>
                    {
                        // Timer callbacks run bare on the pool: an escaped
                        // exception is a process crash. Total by contract.
                        try
                        {
                            timer?.Dispose();
                            EnqueueDirect(self, message, sender);
                        }
                        catch (Exception ex)
                        {
                            _deadLetterSink?.DeadLetter(message,
                                "chaos delay re-admission failed", cause: ex);
                        }
                    }, null, decision.Delay, Timeout.InfiniteTimeSpan);
                    return;
                }
                case ChaosFaultKind.Duplicate:
                    EnqueueDirect(self, message, sender);
                    break;   // and fall through: the original enqueues below
            }
        }

        EnqueueDirect(self, message, sender);
    }

    private void EnqueueDirect(ActorRef self, object message, ActorRef sender)
    {
        if (_stopped)
        {
            _deadLetterSink?.DeadLetter(message, "target actor is stopped", cause: null);
            // Dead-letter metric. Same place the dead-letter
            // sink fires; users wanting to alert on stopped-actor
            // sends use this counter.
            _system?.Metrics.Counter(
                Spek.Observability.SpekMetricNames.DeadLetter,
                tags: new[] { new KeyValuePair<string, object?>("reason", "stopped") });
            // Fail an asker's reply cell so a no-timeout ask to a stopped
            // actor faults fast instead of hanging (red-team V1 — the most
            // dangerous case, since supervision routinely stops actors).
            sender.FailReply(new AskException(
                _current?.GetType().FullName ?? DisplayName,
                message.GetType().Name,
                "target actor is stopped"));
            return;
        }
        _lastActivityUtc = SlotClock.GetUtcNow().UtcDateTime;
        // Capture the sender's Activity context only when someone is
        // listening — the AsyncLocal read and the wrapper are tracing's
        // cost, not every message's.
        var queued = message;
        if (ActorSystem.ActivitySource.HasListeners()
            && System.Diagnostics.Activity.Current is { } activity)
            queued = new TracedMessage(message, activity.Context);
        _mailbox.Enqueue((queued, sender));
        Interlocked.Increment(ref _enqueuedCount);

        // Flight recorder: journal ingress only — enqueues carrying no
        // actor sender (host sends, channel adapters, timers). Internal
        // actor-to-actor traffic is re-derived by replay, not journaled.
        if (_system?.Trace is { } recorder && ReferenceEquals(sender, ActorRef.NoSender))
            recorder.RecordIngress(DisplayName, message);

        // Inbox observers: passive tap at the enqueue point. Per-sender
        // order is exact; cross-sender order is best-effort (concurrent
        // enqueues race here exactly as they race for the mailbox).
        if (_observers is { } observers)
        {
            var observed = new ObservedMessage(
                message,
                ReferenceEquals(sender, ActorRef.NoSender) ? null : sender,
                SlotClock.GetUtcNow());
            foreach (var o in observers) o.Publish(observed);
        }

        // Mailbox-depth gauge, rate-limited (perf r12). ConcurrentQueue.Count
        // walks the queue's segments — O(depth) exactly when mailboxes are
        // deep, which is exactly when someone is watching the metric — and
        // the gauge's consumers sample on second-scale cadences anyway. One
        // emit per slot per 100ms keeps dashboard fidelity while taking the
        // traversal (and the tag lookup) off the per-message path. The
        // last-emit race is benign: a lost update is at worst one extra emit.
        if (_system is { Metrics: { Enabled: true } metrics })
        {
            var nowMs = Environment.TickCount64;
            if (nowMs - _lastDepthGaugeMs >= DepthGaugePeriodMs)
            {
                _lastDepthGaugeMs = nowMs;
                metrics.Gauge(
                    Spek.Observability.SpekMetricNames.MailboxDepth,
                    _mailbox.Count,
                    tags: ActorTypeTags());
            }
        }

        TryProcess(self);
    }

    private const int DepthGaugePeriodMs = 100;
    private long _lastDepthGaugeMs;   // Environment.TickCount64 of the last gauge emit

    /// <summary>
    /// The per-actor metric tag set, cached per slot (perf r12 — this was a
    /// fresh single-entry array plus a GetType().Name per metric emit, up to
    /// three times per message with a sink enabled). Cached once the instance
    /// exists; passivation re-materializes the same type, so the cache stays
    /// true for the slot's lifetime.
    /// </summary>
    private KeyValuePair<string, object?>[]? _actorTypeTags;

    private static readonly KeyValuePair<string, object?>[] UnknownTypeTags =
        [new("actor.type", "unknown")];

    // Internal (not private) so the HandlerWatchdog's report counter reuses
    // the cached tag set instead of rebuilding it per sweep.
    internal IReadOnlyList<KeyValuePair<string, object?>> ActorTypeTags()
    {
        var cached = _actorTypeTags;
        if (cached is not null) return cached;
        var current = _current;
        if (current is null) return UnknownTypeTags;   // not materialized yet — don't cache
        return _actorTypeTags = [new("actor.type", current.GetType().Name)];
    }

    /// <summary>
    /// The slot whose actor instance is being constructed on this thread, if
    /// any. <see cref="ActorBase.Initialize"/> runs only AFTER the factory
    /// (the user's <c>init</c> body), so a <c>spawn</c> inside <c>init</c>
    /// finds <c>_selfRef</c>/<c>_system</c> still null — this ambient context
    /// is its fallback for the parent link and system plumbing. ThreadStatic
    /// is sufficient: the factory runs synchronously (init can't be async).
    /// </summary>
    [ThreadStatic]
    internal static ActorSlot? MaterializingSlot;

    /// <summary>Plumbing exposed for <see cref="ActorBase"/>'s spawn-in-init
    /// fallback (see <see cref="MaterializingSlot"/>).</summary>
    internal ActorSystem? SystemForChildren => _system;
    internal ISnapshotStore? SnapshotStoreForChildren => _snapshotStore;
    internal IDeadLetterSink? DeadLetterSinkForChildren => _deadLetterSink;

    private ActorBase EnsureMaterialized(ActorRef self)
    {
        if (_current is not null) return _current;
        lock (_materializeGate)
        {
            if (_current is not null) return _current;
            var prior = MaterializingSlot;
            MaterializingSlot = this;
            try
            {
                var instance = _factory();
                instance.Initialize(self, _system, _persistenceKey, _snapshotStore, _deadLetterSink);
                _current = instance;
                return instance;
            }
            finally
            {
                MaterializingSlot = prior;
            }
        }
    }

    private void TryProcess(ActorRef self)
    {
        // Under external dispatch (deterministic simulation) the slot never
        // self-schedules; the simulator single-steps DispatchItemAsync.
        if (_system?.ExternalDispatch == true) return;
        if (Interlocked.CompareExchange(ref _processing, 1, 0) != 0) return;

        // The slot IS the work item: no Task.Run closure/Task per wake.
        // Global queue on purpose — preferLocal parks the wake on the
        // ENQUEUING thread's local queue, which starves whenever that
        // thread keeps producing (a busy router) or blocks awaiting the
        // reply (an asker); measured as a fleet regression + ask jitter
        // in perf round r2.
        ThreadPool.UnsafeQueueUserWorkItem(this, preferLocal: false);
    }

    void IThreadPoolWorkItem.Execute() => _ = DrainGuardedAsync();

    private async Task DrainGuardedAsync()
    {
        try { await DrainAsync().ConfigureAwait(false); }
        catch (Exception ex)
        {
            // The drain runs fire-and-forget; an escaped exception here
            // (a runtime bug, not a handler failure — those are caught in
            // DispatchItemAsync) must never silently wedge the mailbox
            // with _processing stuck at 1.
            _deadLetterSink?.DeadLetter(this, "dispatch loop crashed; re-arming", cause: ex);
            Interlocked.Exchange(ref _processing, 0);
            if (!_mailbox.IsEmpty && !_stopped && _selfRef is { } self) TryProcess(self);
        }
    }

    private async Task DrainAsync()
    {
        // The canonical self ref, captured at materialization. A slot can
        // only have mail after its spawn materialized it, so this is set
        // by the time any wake runs.
        var self = _selfRef!;
        while (true)
        {
            while (_mailbox.TryDequeue(out var item))
                await DispatchItemAsync(self, Unwrap(item), inlineReaders: false).ConfigureAwait(false);

            // Second-chance window (perf r10): in request-reply traffic the
            // next message lands within a microsecond of the drain going
            // empty — the asker's continuation runs, then sends. A short
            // PURE spin (never yields the thread) catches it and skips a
            // full pool hop; an actor going idle for real wastes only the
            // sub-microsecond spin, once.
            if (!_stopped && SecondChance()) continue;

            Interlocked.Exchange(ref _processing, 0);
            // Pulse the system's idle-check signal so AwaitTermination
            // re-evaluates. Cheap call, coalesces with other signals.
            _system?.SignalSlotActivity();
            if (!_mailbox.IsEmpty && !_stopped) TryProcess(self);
            return;
        }
    }

    private bool SecondChance()
    {
        var spin = new SpinWait();
        while (!spin.NextSpinWillYield)   // pure spin: park before ever yielding
        {
            spin.SpinOnce();
            if (!_mailbox.IsEmpty) return true;
        }
        return false;
    }

    /// <summary>
    /// Dispatches ONE dequeued item through the full pipeline — stopped
    /// check, materialization, ingress policies, chaos, reader/writer arms,
    /// supervision. Shared verbatim between the production loop and the
    /// deterministic simulator so the two can't drift semantically: only
    /// the ordering choice and the clock differ. With
    /// <paramref name="inlineReaders"/> the reader arm is awaited inline
    /// (the simulator's single-threaded, still-legal schedule — readers are
    /// state-isolated by construction) instead of fired on the pool.
    /// </summary>
    internal async Task DispatchItemAsync(
        ActorRef self,
        (object Message, ActorRef Sender, System.Diagnostics.ActivityContext ParentTrace) item,
        bool inlineReaders)
    {
                if (_stopped)
                {
                    Interlocked.Increment(ref _stoppedDropCount);
                    _deadLetterSink?.DeadLetter(item.Message, "target actor is stopped", cause: null);
                    // Fail an asker's reply cell — see EnqueueDirect (red-team V1).
                    item.Sender.FailReply(new AskException(
                        _current?.GetType().FullName ?? DisplayName,
                        item.Message.GetType().Name,
                        "target actor is stopped"));
                    return;
                }

                ActorBase instance;
                try
                {
                    instance = EnsureMaterialized(self);
                }
                catch (Exception matEx)
                {
                    // RE-materialization threw — the factory, OnPreStart, or a
                    // persistent actor's OnRestore failed while the dispatch loop
                    // rebuilt a dropped instance (a Restart cleared _current, or a
                    // passivation-wake did). This is NOT spawn-time (that path,
                    // Materialize(), throws to the caller by design); here there is
                    // no live instance to ask OnFailure, and EnsureMaterialized left
                    // _current null — so a bare return would re-materialize and
                    // throw again on the very next mailbox item, spinning the pool
                    // and silently dead-lettering everything (red-team V2). Route it
                    // through supervision instead.
                    HandleMaterializationFailure(self, item, matEx);
                    return;
                }
                _lastMessageType = item.Message.GetType();
                Interlocked.Increment(ref _dispatchedCount);

                // Ingress policies (rate limit, bulkhead, admission gates)
                // run before the handler. First non-Allow decision wins;
                // the message is dead-lettered with the policy's reason.
                var policies = _ingressPolicies;
                if (policies.Length > 0
                    && !await AdmitAsync(policies, item.Message, item.Sender, item.ParentTrace, self))
                    return;

                // Split into reader vs writer dispatch.
                //   Reader → enter reader lock; fire-and-forget the
                //            handler so the loop keeps dequeueing
                //            other readers concurrently.
                //   Writer → enter writer lock (drains readers); await
                //            the handler; existing supervision flow.
                if (instance.ClassifyAsReader(item.Message))
                {
                    await EnterReaderAsync().ConfigureAwait(false);
                    // Reader accounting (perf r9): counter incremented while
                    // the loop still owns the item, so a Stop/Restart drain
                    // that starts now already sees this reader in flight.
                    if (Interlocked.Increment(ref _inFlightReaders) == 1)
                        // Watchdog reader-phase stamp (0→1 edge only — see
                        // the field comment for the phase approximation).
                        lock (_readerIdleGate) _readerPhaseStartMs = Environment.TickCount64;
                    if (inlineReaders)
                        await RunReaderAsync(item, instance).ConfigureAwait(false);
                    else
                        // Generic-state queue: no closure object, no wrapper
                        // Task — the loop keeps pulling while readers run.
                        ThreadPool.UnsafeQueueUserWorkItem(
                            static s => _ = s.slot.RunReaderAsync(s.item, s.instance),
                            (slot: this, item, instance), preferLocal: false);
                    return;
                }

                // Writer path: wait for any in-flight readers to drain,
                // then run alone with the writer lock held.
                await EnterWriterAsync().ConfigureAwait(false);
                // Slow-handler watchdog stamp: one unconditional int64 write
                // per message (0 = idle); the finally below clears it however
                // the handler exits. Stamped after lock acquisition so the
                // clock starts on the handler, not on a healthy lock queue.
                _dispatchStartMs = Environment.TickCount64;
                try
                {
                    // Child trace span under the sender's
                    // parent. See the reader-path comment for the
                    // null-listener semantics.
                    using var activity = ActorSystem.ActivitySource.StartActivity(
                        $"spek.actor.{instance.GetType().Name}.handler",
                        System.Diagnostics.ActivityKind.Consumer,
                        parentContext: item.ParentTrace);

                    // Handler duration histogram + dispatch
                    // counter. The histogram includes lock acquisition
                    // because that's what handler authors actually
                    // experience as "how long does my handler take";
                    // separating lock-wait vs body-time is a follow-up.
                    var metricsOn = _system is { Metrics.Enabled: true };
                    var swStart = System.Diagnostics.Stopwatch.GetTimestamp();
                    if (metricsOn)
                        _system!.Metrics.Counter(
                            Spek.Observability.SpekMetricNames.MailboxDispatch,
                            tags: ActorTypeTags());

                    // Chaos crash-on-nth: thrown here, inside the real
                    // dispatch try, so it unwinds through the real
                    // supervision machinery below.
                    if (_system?.Chaos is { } chaosPlan
                        && chaosPlan.ShouldCrash(this, item.Message, out var chaosCrash))
                        throw new ChaosInjectedException(chaosCrash);

                    await instance.InvokeDispatch(item.Message, item.Sender);
                    _lastActivityUtc = SlotClock.GetUtcNow().UtcDateTime;

                    // The handler returned normally. If it was an ask and the
                    // handler never replied (unhandled message, an early
                    // return, a StopSelf mid-ask), the reply cell is still
                    // pending — fail it so a no-timeout AskAsync faults with a
                    // diagnostic instead of hanging forever. A handler that
                    // replied via `return`/`sender.Tell` already completed the
                    // cell, so this is a no-op. (Red-team V1; Spek has no
                    // cross-turn deferred-reply pattern, so a pending cell here
                    // genuinely means no reply is coming.)
                    if (item.Sender.IsPendingReply)
                        item.Sender.FailReply(new AskException(
                            instance.GetType().FullName ?? instance.GetType().Name,
                            item.Message.GetType().Name,
                            "handler returned without replying to the ask"));

                    if (metricsOn)
                    {
                        var elapsedMs = System.Diagnostics.Stopwatch
                            .GetElapsedTime(swStart).TotalMilliseconds;
                        _system!.Metrics.Histogram(
                            Spek.Observability.SpekMetricNames.HandlerDurationMs,
                            elapsedMs,
                            tags: ActorTypeTags());
                    }
                }
                catch (Exception ex)
                {
                    // Akka-style supervision: when there's a parent, the
                    // parent decides the child's fate via its OnChildFailure.
                    // The child's own OnFailure only runs for root actors
                    // (no parent to delegate to). If the child explicitly
                    // wants to self-manage despite having a parent, it can
                    // return an explicit directive from OnFailure — we
                    // default to Escalate here so the parent is consulted,
                    // but only when the child returned the uninformative
                    // default.
                    FailureDirective directive;
                    if (_parent is not null)
                    {
                        directive = FailureDirective.Escalate;
                    }
                    else
                    {
                        directive = instance.InvokeOnFailure(ex, item.Message);
                    }
                    var resolved = ResolveEscalation(directive, self, ex, item.Message);
                    _deadLetterSink?.DeadLetter(item.Message, DescribeDirective(resolved), cause: ex);

                    // Fail the asker's reply cell, exactly as the reader path
                    // does (RunReaderAsync). The handler threw before replying,
                    // so this ask can never complete normally under any
                    // directive — a no-timeout AskAsync would otherwise hang
                    // forever. No-op for non-asking senders. (Red-team V1, the
                    // "fail fast on all" ask semantics: the writer path
                    // dead-lettered but never touched the sender, so identical
                    // caller code failed fast against a reader and hung against
                    // a writer — an arm the caller can't see. The inner carries
                    // the handler's real exception so the asker learns why.)
                    item.Sender.FailReply(new AskException(
                        instance.GetType().FullName ?? instance.GetType().Name,
                        item.Message.GetType().Name,
                        ex));

                    // A Restart that exceeds the actor's configured retry
                    // budget degrades to Stop. Primitives live on
                    // ActorBase; the emitter wires `supervise` decls to
                    // the overrides in a future push.
                    if (resolved == FailureDirective.Restart && IsRestartBudgetExceeded(instance))
                    {
                        _deadLetterSink?.DeadLetter(item.Message,
                            "restart budget exceeded; stopping actor", cause: ex);
                        resolved = FailureDirective.Stop;
                    }

                    switch (resolved)
                    {
                        case FailureDirective.Resume:
                            return;

                        case FailureDirective.Restart:
                            // Restart counter, tagged with
                            // actor type and exception type so dashboards
                            // can split "we keep restarting because of X".
                            _system?.Metrics.Counter(
                                Spek.Observability.SpekMetricNames.ActorRestart,
                                tags: new[]
                                {
                                    new KeyValuePair<string, object?>("actor.type", instance.GetType().Name),
                                    new KeyValuePair<string, object?>("exception.type", ex.GetType().Name),
                                });
                            RecordRestart();
                            await TryPersist(instance);
                            lock (_materializeGate) { _current = null; }
                            return;

                        case FailureDirective.Stop:
                        default:
                            // Stop counter, tagged with
                            // cause so post-mortem dashboards can split
                            // voluntary vs supervised vs budget-exceeded.
                            _system?.Metrics.Counter(
                                Spek.Observability.SpekMetricNames.ActorStop,
                                tags: new[]
                                {
                                    new KeyValuePair<string, object?>("actor.type", instance.GetType().Name),
                                    new KeyValuePair<string, object?>("cause", "supervision"),
                                });
                            _stopped = true;
                            // OnPostStop, then the `term { }` block (via
                            // OnTerm) — runs after PostStop and before the actor
                            // reference is invalidated. Centralized + guarded so a
                            // later system-shutdown Dispose can't double-fire them.
                            RunStopHooks(instance);
                            break;
                    }
                }
                finally
                {
                    _dispatchStartMs = 0;   // watchdog stamp: back to idle
                    // Always release the writer lock — keeps readers
                    // from being permanently blocked even if a writer
                    // throws and supervision restarts the actor.
                    ExitWriter();
                }
    }

    /// <summary>
    /// Simulator single-step: dequeue and fully dispatch one message
    /// through <see cref="DispatchItemAsync"/>. Returns false when the
    /// mailbox was empty. Only the deterministic simulator calls this
    /// (production slots self-schedule via TryProcess), one in-flight
    /// dispatch per slot.
    /// </summary>
    internal async Task<bool> SimDispatchOneAsync()
    {
        if (Self is not { } self || !_mailbox.TryDequeue(out var item)) return false;
        await DispatchItemAsync(self, Unwrap(item), inlineReaders: true).ConfigureAwait(false);
        _system?.SignalSlotActivity();
        return true;
    }

    /// <summary>
    /// Timer-driven: if the actor has been idle long enough, persist its
    /// state and drop the live instance. The next <see cref="Enqueue"/> will
    /// re-materialise it via the factory and <c>OnRestore</c> will reload
    /// from the snapshot.
    /// </summary>
    private async Task OnPassivationCheck()
    {
        if (_disposed || _stopped || _passivationTimeout is null) return;

        ActorBase? toPassivate;

        // First checkpoint: under lock, confirm we're idle and the idle
        // window has elapsed.
        lock (_materializeGate)
        {
            if (_current is null) return;
            if (!_mailbox.IsEmpty || _processing != 0) return;
            if (SlotClock.GetUtcNow().UtcDateTime - _lastActivityUtc < _passivationTimeout.Value) return;
            toPassivate = _current;
        }

        // OnPassivate fires before persist so the actor can flush
        // in-memory state into its fields first.
        try { toPassivate.InvokeOnPassivate(); }
        catch (Exception ex)
        {
            _deadLetterSink?.DeadLetter(toPassivate, "OnPassivate threw", cause: ex);
            return;
        }

        // Non-persistent actors skip the snapshot write entirely —
        // memory release is the whole point. Persistent actors save
        // outside the lock so concurrent Enqueues don't block on the
        // snapshot store.
        if (_persistenceKey is null || _snapshotStore is null)
        {
            lock (_materializeGate)
            {
                if (_stopped) return;
                if (!_mailbox.IsEmpty || _processing != 0) return;
                _current = null;
            }
            return;
        }

        try { await toPassivate.InvokePersistAsync(); }
        catch (Exception ex)
        {
            _deadLetterSink?.DeadLetter(toPassivate, "persist-before-passivate threw", cause: ex);
            return; // Bail — don't drop an instance whose state we couldn't save.
        }

        // Second checkpoint: if messages arrived during persist, abort.
        // Otherwise, drop the current instance; the next Enqueue rebuilds it.
        lock (_materializeGate)
        {
            if (_stopped) return;
            if (!_mailbox.IsEmpty || _processing != 0) return;
            _current = null;
        }
    }

    /// <summary>
    /// If the child asked to Escalate, walk up the parent chain and ask each
    /// supervisor what to do. Returns a concrete directive (Resume / Restart
    /// / Stop) to apply to the child. If escalation reaches a root actor with
    /// no parent, we degrade to Stop and log the reason.
    /// </summary>
    private FailureDirective ResolveEscalation(
        FailureDirective directive,
        ActorRef self,
        Exception cause,
        object message)
    {
        // Climb the supervisor chain: each ancestor decides about ITS failing
        // child. When a supervisor escalates, it's saying "I can't handle this —
        // treat it as MY failure to MY parent", so we re-root one level up and ask
        // the grandparent about the parent (not re-ask the same parent). The depth
        // cap stops a pathologically deep all-escalating chain from spinning.
        var supervisor = _parent;   // the immediate parent's slot (this == failing child)
        var failing    = self;      // whose failure the current level decides on
        for (int depth = 0; depth < 8 && directive == FailureDirective.Escalate; depth++)
        {
            if (supervisor?.Current is null)
            {
                _deadLetterSink?.DeadLetter(message,
                    "escalate at root — no parent supervisor; stopping actor", cause);
                return FailureDirective.Stop;
            }
            directive = supervisor.Current.InvokeOnChildFailure(failing, cause, message);
            if (directive == FailureDirective.Escalate)
            {
                // Re-root: the supervisor becomes the failing child at the next level.
                failing    = supervisor._selfRef ?? failing;
                supervisor = supervisor._parent;
            }
        }

        if (directive == FailureDirective.Escalate)
        {
            _deadLetterSink?.DeadLetter(message,
                "escalation chain exceeded max depth — stopping actor", cause);
            return FailureDirective.Stop;
        }
        return directive;
    }

    // Re-materialization retry bound. Unlike a handler-failure restart, a
    // materialization failure has no instance whose supervise decl could
    // configure a budget, so a conservative fixed cap governs: a transient
    // store blip on OnRestore gets a few retries; a permanent fault (bad
    // OnPreStart, unreachable snapshot store) stops the slot instead of
    // dead-lettering every message forever.
    private const int MaxMaterializationRetries = 3;
    private static readonly TimeSpan MaterializationRetryWindow = TimeSpan.FromSeconds(10);

    /// <summary>
    /// Supervises a crash during RE-materialization (see the call site in
    /// <see cref="DispatchItemAsync"/>). With no instance to consult, routes
    /// the failure through the same supervisor chain a handler failure uses:
    /// a parented actor escalates so the parent's <c>OnChildFailure</c>
    /// decides, a root actor stops. A supervisor-blessed Restart/Resume earns
    /// a bounded retry (the instance stays null so the next message rebuilds
    /// it); past <see cref="MaxMaterializationRetries"/> in the window, or on
    /// a Stop, the slot stops — subsequent mail then drains through the
    /// stopped path instead of re-throwing. Fails the asker's reply cell
    /// throughout, exactly as the handler-failure path does (red-team V1/V2).
    /// </summary>
    private void HandleMaterializationFailure(
        ActorRef self,
        (object Message, ActorRef Sender, System.Diagnostics.ActivityContext ParentTrace) item,
        Exception ex)
    {
        RecordRestart();

        var directive = _parent is not null ? FailureDirective.Escalate : FailureDirective.Stop;
        var resolved = ResolveEscalation(directive, self, ex, item.Message);

        if ((resolved == FailureDirective.Restart || resolved == FailureDirective.Resume)
            && !IsMaterializationRetryBudgetExceeded())
        {
            _deadLetterSink?.DeadLetter(item.Message,
                "re-materialization failed; retrying on the next message", cause: ex);
            item.Sender.FailReply(new AskException(DisplayName, item.Message.GetType().Name, ex));
            lock (_materializeGate) { _current = null; }   // guarantee a clean rebuild
            return;
        }

        // Stop: terminal for this slot. Later mail takes the _stopped path and
        // dead-letters cleanly rather than re-materializing and re-throwing.
        _system?.Metrics.Counter(
            Spek.Observability.SpekMetricNames.ActorStop,
            tags: new[]
            {
                new KeyValuePair<string, object?>("actor.type", DisplayName),
                new KeyValuePair<string, object?>("cause", "materialization"),
            });
        _deadLetterSink?.DeadLetter(item.Message,
            "re-materialization failed; stopping actor", cause: ex);
        item.Sender.FailReply(new AskException(DisplayName, item.Message.GetType().Name, ex));
        _stopped = true;
        // No instance ever came alive, so there are no OnPostStop/term hooks
        // to run — they only fire for a materialized actor.
    }

    private bool IsMaterializationRetryBudgetExceeded()
    {
        var now = SlotClock.GetUtcNow().UtcDateTime;
        lock (_restartLog)
        {
            _restartLog.RemoveAll(t => now - t > MaterializationRetryWindow);
            // RecordRestart already logged this attempt, so Count includes it.
            return _restartLog.Count > MaxMaterializationRetries;
        }
    }

    private bool IsRestartBudgetExceeded(ActorBase instance)
    {
        var max = instance.GetMaxRestartsWithinWindow();
        if (max == int.MaxValue) return false;

        var window = instance.GetRestartWindow();
        var now = SlotClock.GetUtcNow().UtcDateTime;

        lock (_restartLog)
        {
            if (window is { } w)
                _restartLog.RemoveAll(t => now - t > w);
            // Include the pending restart (this call) in the count.
            return _restartLog.Count + 1 > max;
        }
    }

    private void RecordRestart()
    {
        Interlocked.Increment(ref _observedRestarts);
        lock (_restartLog) _restartLog.Add(SlotClock.GetUtcNow().UtcDateTime);
    }

    // Defer bookkeeping: attempts are tracked per message INSTANCE so a
    // re-admitted reference carries its own budget without wrapping the
    // user's message (handlers must keep seeing the raw type).
    private const int MaxDeferAttempts = 3;
    private static readonly System.Runtime.CompilerServices.ConditionalWeakTable<object, System.Runtime.CompilerServices.StrongBox<int>>
        _deferAttempts = new();

    /// <summary>
    /// Evaluate the ingress policy chain in order. Returns true if every
    /// policy returned Allow. Reject dead-letters immediately. Defer
    /// re-enqueues the message after the policy's <c>RetryAfter</c> delay —
    /// runtime-managed, through the system clock (deterministic under
    /// virtual time) — up to <see cref="MaxDeferAttempts"/> times, then
    /// dead-letters with the exhausted budget as the reason. Deferred
    /// re-admission re-enters the mailbox tail: arrival order across a
    /// deferral is deliberately not preserved (the message yielded its slot).
    /// A re-admission that finds the actor already stopped dead-letters via
    /// <see cref="Enqueue"/>'s stopped path — parked messages still reach a
    /// terminal state.
    /// </summary>
    private async Task<bool> AdmitAsync(
        IngressPolicy[] policies, object message, ActorRef sender,
        System.Diagnostics.ActivityContext parentTrace, ActorRef self)
    {
        var actorPath = _persistenceKey ?? (_current?.GetType().FullName ?? "<anonymous>");
        var ctx = new ResilienceContext(
            ActorPath:   actorPath,
            ChannelName: message.GetType().Name,
            Message:     message,
            Timestamp:   SlotClock.GetUtcNow());

        foreach (var policy in policies)
        {
            var decision = await policy.EvaluateAsync(ctx).ConfigureAwait(false);
            if (decision.Kind == PolicyDecisionKind.Allow) continue;

            if (decision.Kind == PolicyDecisionKind.Defer && decision.RetryAfter is { } delay)
            {
                var box = _deferAttempts.GetValue(message, static _ => new System.Runtime.CompilerServices.StrongBox<int>(0));
                if (box.Value < MaxDeferAttempts)
                {
                    box.Value++;
                    var timer = (ITimer?)null;
                    timer = SlotClock.CreateTimer(_ =>
                    {
                        // Timer callbacks run bare on the pool: an escaped
                        // exception is a process crash. Total by contract.
                        try
                        {
                            timer?.Dispose();
                            // Unconditional: if the actor stopped while the
                            // message was parked, Enqueue's stopped path
                            // dead-letters it. Every message reaches a
                            // terminal state — never silently dropped.
                            Enqueue(self, message, sender);
                        }
                        catch (Exception ex)
                        {
                            _deadLetterSink?.DeadLetter(message,
                                "defer re-admission failed", cause: ex);
                        }
                    }, null, delay, Timeout.InfiniteTimeSpan);
                    return false;
                }

                _deadLetterSink?.DeadLetter(message,
                    $"defer budget exhausted after {MaxDeferAttempts} attempts: {decision.Reason}",
                    cause: null);
                return false;
            }

            _deadLetterSink?.DeadLetter(message,
                $"rejected by ingress policy: {decision.Reason}", cause: null);
            return false;
        }
        return true;
    }

    private async Task TryPersist(ActorBase instance)
    {
        try { await instance.InvokePersistAsync(); }
        catch (Exception ex)
        {
            _deadLetterSink?.DeadLetter(instance, "persist-before-restart threw", cause: ex);
        }
    }

    private static string DescribeDirective(FailureDirective d) => d switch
    {
        FailureDirective.Resume   => "handler threw; resuming",
        FailureDirective.Restart  => "handler threw; restarting",
        FailureDirective.Escalate => "handler threw; escalating to parent",
        FailureDirective.Stop     => "handler threw; stopping actor",
        _                         => "handler threw",
    };

    public void Dispose()
    {
        _disposed = true;
        _passivationTimer?.Dispose();
        _passivationTimer = null;
        CompleteObservers();
    }
}
