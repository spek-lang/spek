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
internal sealed class ActorSlot : IDisposable
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

    // Mailbox tuple carries the captured `Activity`
    // context from the sender's async-local stack so the receiving
    // handler can create a child span under the same trace ID.
    // `default` ActivityContext means "no trace was active at send";
    // dispatch checks `IsValid` before bothering with the listener.
    private readonly ConcurrentQueue<(object Message, ActorRef Sender, System.Diagnostics.ActivityContext ParentTrace)> _mailbox = new();
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
    private TaskCompletionSource _readersDrained =
        new(TaskCreationOptions.RunContinuationsAsynchronously);
    private TaskCompletionSource _writerReleased =
        new(TaskCreationOptions.RunContinuationsAsynchronously);

    // In-flight reader dispatch tasks. Tracked so that Stop / Restart /
    // Dispose can wait for outstanding readers to drain before tearing
    // the slot down.
    private readonly object _readerTasksGate = new();
    private readonly List<Task> _inFlightReaderTasks = new();

    private ActorBase? _current;
    private volatile bool _stopped;
    // Guards the stop sequence (OnPostStop → OnTerm) so it runs at most once per
    // slot, no matter which path triggers the stop — supervision, voluntary
    // StopSelf, or a system shutdown via Dispose/StopGracefully.
    private int _stopHooksRan;
    private int _processing;

    private ActorRef? _selfRef;
    private DateTime _lastActivityUtc = DateTime.UtcNow;
    private TimeSpan? _passivationTimeout;
    private Timer? _passivationTimer;
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
    }

    public ActorBase? Current => _current;
    public bool IsStopped => _stopped;
    public bool IsIdle => _mailbox.IsEmpty && _processing == 0;

    /// <summary>How many times this actor has been restarted by supervision.
    /// Exposed for test observability (Spek.Testing, via InternalsVisibleTo).</summary>
    internal int RestartCount { get { lock (_restartLog) return _restartLog.Count; } }

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
                    if (_activeReaders == 0)
                        _readersDrained = new TaskCompletionSource(
                            TaskCreationOptions.RunContinuationsAsynchronously);
                    _activeReaders++;
                    return;
                }
                // Compose a wait — re-check on whichever signal fires first.
                wait = blockedByActiveWriter || blockedByQueuedWriter
                    ? _writerReleased.Task
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
            if (_activeReaders == 0) toPulse = _readersDrained;
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
                        _writerActive = true;
                        _writerReleased = new TaskCompletionSource(
                            TaskCreationOptions.RunContinuationsAsynchronously);
                        return;
                    }
                    // Wait on whichever lock state needs to clear.
                    wait = _writerActive ? _writerReleased.Task : _readersDrained.Task;
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
        TaskCompletionSource toPulse;
        lock (_rwGate)
        {
            _writerActive = false;
            toPulse = _writerReleased;
        }
        toPulse.TrySetResult();
    }

    /// <summary>Track an in-flight reader dispatch task so Stop /
    /// Restart can drain it before tearing the slot down.</summary>
    private void TrackReaderTask(Task task)
    {
        lock (_readerTasksGate) _inFlightReaderTasks.Add(task);
        _ = task.ContinueWith(_ =>
        {
            lock (_readerTasksGate) _inFlightReaderTasks.Remove(task);
        }, TaskScheduler.Default);
    }

    /// <summary>Wait for all in-flight reader tasks to complete.
    /// Used by Stop / Restart / Dispose paths.</summary>
    private async Task DrainReadersAsync()
    {
        Task[] snapshot;
        lock (_readerTasksGate) snapshot = _inFlightReaderTasks.ToArray();
        if (snapshot.Length == 0) return;
        try { await Task.WhenAll(snapshot).ConfigureAwait(false); }
        catch { /* individual reader exceptions already dead-lettered */ }
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
        _passivationTimer = new Timer(_ => _ = OnPassivationCheck(), null, period, period);
    }

    internal void Enqueue(ActorRef self, object message, ActorRef sender)
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
            return;
        }
        _lastActivityUtc = DateTime.UtcNow;
        // Capture the sender's Activity context so the
        // receiver's dispatch can create a child span under the same
        // trace ID. `Activity.Current?.Context ?? default` keeps the
        // capture cheap when no trace is active.
        var parentTrace = System.Diagnostics.Activity.Current?.Context
                         ?? default(System.Diagnostics.ActivityContext);
        _mailbox.Enqueue((message, sender, parentTrace));

        // Mailbox-depth gauge. Tagged with the actor type so dashboards
        // can split by class. Guarded by Metrics.Enabled so the per-message tag
        // array + GetType().Name aren't built when no real sink is attached
        // (the default NullMetricSink) — this is a hot path.
        if (_system is { Metrics: { Enabled: true } metrics })
            metrics.Gauge(
                Spek.Observability.SpekMetricNames.MailboxDepth,
                _mailbox.Count,
                tags: ActorTypeTag(self));

        TryProcess(self);
    }

    /// <summary>
    /// Small helper to tag every per-actor metric with the
    /// actor's runtime type name. Allocates a single-entry array;
    /// fine because the surrounding dispatch path is far hotter.
    /// </summary>
    private static IReadOnlyList<KeyValuePair<string, object?>> ActorTypeTag(ActorRef self)
    {
        var typeName = self.Slot?.Current?.GetType().Name ?? "unknown";
        return new[] { new KeyValuePair<string, object?>("actor.type", typeName) };
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
        if (Interlocked.CompareExchange(ref _processing, 1, 0) != 0) return;

        _ = Task.Run(async () =>
        {
            while (_mailbox.TryDequeue(out var item))
            {
                if (_stopped)
                {
                    _deadLetterSink?.DeadLetter(item.Message, "target actor is stopped", cause: null);
                    continue;
                }

                var instance = EnsureMaterialized(self);

                // Ingress policies (rate limit, bulkhead, admission gates)
                // run before the handler. First non-Allow decision wins;
                // the message is dead-lettered with the policy's reason.
                var policies = _ingressPolicies;
                if (policies.Length > 0 && !await AdmitAsync(policies, item.Message))
                    continue;

                // Split into reader vs writer dispatch.
                //   Reader → enter reader lock; fire-and-forget the
                //            handler so the loop keeps dequeueing
                //            other readers concurrently.
                //   Writer → enter writer lock (drains readers); await
                //            the handler; existing supervision flow.
                if (instance.ClassifyAsReader(item.Message))
                {
                    await EnterReaderAsync().ConfigureAwait(false);
                    var capturedItem = item;
                    var capturedInstance = instance;
                    var readerTask = Task.Run(async () =>
                    {
                        // Start a child trace span under the
                        // sender's parent context so distributed traces
                        // stitch across actor boundaries. StartActivity
                        // returns null when no ActivityListener is
                        // registered (the common case in tests with no
                        // OTel SDK), making this effectively a no-op.
                        using var activity = ActorSystem.ActivitySource.StartActivity(
                            $"spek.actor.{capturedInstance.GetType().Name}.handler",
                            System.Diagnostics.ActivityKind.Consumer,
                            parentContext: capturedItem.ParentTrace);

                        try
                        {
                            await capturedInstance.InvokeDispatch(capturedItem.Message, capturedItem.Sender)
                                .ConfigureAwait(false);
                            _lastActivityUtc = DateTime.UtcNow;
                        }
                        catch (Exception readerEx)
                        {
                            // Reader exceptions don't trigger supervision —
                            // a reader can't corrupt actor state by
                            // construction (CE0087 enforces this). Dead-letter
                            // and keep the actor alive.
                            _deadLetterSink?.DeadLetter(capturedItem.Message,
                                "reader handler threw; message dead-lettered",
                                cause: readerEx);

                            // If the asker is awaiting a reply (its
                            // sender ref wraps a ReplyActor slot), surface
                            // the failure as an AskException so the
                            // caller's await throws instead of hanging.
                            // Regular non-asking senders' FailReplyWith is
                            // a no-op so this is safe to call always.
                            var senderInstance = capturedItem.Sender?.Slot?.Current;
                            if (senderInstance is not null)
                            {
                                var ask = new AskException(
                                    targetActorPath:
                                        capturedInstance.GetType().FullName ?? capturedInstance.GetType().Name,
                                    messageTypeName:
                                        capturedItem.Message.GetType().Name,
                                    inner: readerEx);
                                senderInstance.FailReplyWith(ask);
                            }
                        }
                        finally
                        {
                            ExitReader();
                        }
                    });
                    TrackReaderTask(readerTask);
                    continue;   // don't await; loop keeps pulling
                }

                // Writer path: wait for any in-flight readers to drain,
                // then run alone with the writer lock held.
                await EnterWriterAsync().ConfigureAwait(false);
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
                            tags: ActorTypeTag(self));

                    await instance.InvokeDispatch(item.Message, item.Sender);
                    _lastActivityUtc = DateTime.UtcNow;

                    if (metricsOn)
                    {
                        var elapsedMs = System.Diagnostics.Stopwatch
                            .GetElapsedTime(swStart).TotalMilliseconds;
                        _system!.Metrics.Histogram(
                            Spek.Observability.SpekMetricNames.HandlerDurationMs,
                            elapsedMs,
                            tags: ActorTypeTag(self));
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
                            continue;

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
                            continue;

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
                    // Always release the writer lock — keeps readers
                    // from being permanently blocked even if a writer
                    // throws and supervision restarts the actor.
                    ExitWriter();
                }
            }
            Interlocked.Exchange(ref _processing, 0);
            // Pulse the system's idle-check signal so AwaitTermination
            // re-evaluates. Cheap call, coalesces with other signals.
            _system?.SignalSlotActivity();
            if (!_mailbox.IsEmpty && !_stopped) TryProcess(self);
        });
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
            if (DateTime.UtcNow - _lastActivityUtc < _passivationTimeout.Value) return;
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

    private bool IsRestartBudgetExceeded(ActorBase instance)
    {
        var max = instance.GetMaxRestartsWithinWindow();
        if (max == int.MaxValue) return false;

        var window = instance.GetRestartWindow();
        var now = DateTime.UtcNow;

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
        lock (_restartLog) _restartLog.Add(DateTime.UtcNow);
    }

    /// <summary>
    /// Evaluate the ingress policy chain in order. Returns true if every
    /// policy returned Allow. On the first Reject/Defer, dead-letters the
    /// message with the policy's reason and returns false.
    /// </summary>
    private async Task<bool> AdmitAsync(IngressPolicy[] policies, object message)
    {
        var actorPath = _persistenceKey ?? (_current?.GetType().FullName ?? "<anonymous>");
        var ctx = new ResilienceContext(
            ActorPath:   actorPath,
            ChannelName: message.GetType().Name,
            Message:     message,
            Timestamp:   DateTimeOffset.UtcNow);

        foreach (var policy in policies)
        {
            var decision = await policy.EvaluateAsync(ctx).ConfigureAwait(false);
            if (decision.Kind == PolicyDecisionKind.Allow) continue;

            var reason = decision.Kind == PolicyDecisionKind.Defer && decision.RetryAfter is { } delay
                ? $"deferred by ingress policy: {decision.Reason} (retry after {delay})"
                : $"rejected by ingress policy: {decision.Reason}";
            _deadLetterSink?.DeadLetter(message, reason, cause: null);
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
    }
}
