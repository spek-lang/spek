// ActorBase lives in the bare `namespace Spek` alongside ActorRef and
// the other user-facing fundamentals. Engine internals (ActorSlot,
// ActorSystem, IDeadLetterSink) stay in Spek.Runtime — same assembly,
// reachable via the using below.
using Spek.Observability;
using Spek.Persistence;
using Spek.Runtime;

namespace Spek;

/// <summary>
/// Base class for all generated actor classes. The mailbox and the processing
/// loop live on <see cref="ActorSlot"/>; this class owns the per-instance
/// state and the hooks users override (<c>OnPreStart</c>, <c>OnPostStop</c>,
/// <c>OnRestore</c>, <c>OnFailure</c>, <c>Unhandled</c>).
/// </summary>
public abstract class ActorBase
{
    // ─── Per-instance state supplied by the runtime at Initialize time ───────

    /// <summary>
    /// Reference to this actor — what <c>self</c> in Spek source compiles to.
    /// Set by the runtime at <c>Initialize</c> time; generated handler code
    /// reads it (e.g. to pass as the sender on outbound <c>Tell</c>s).
    /// </summary>
    protected ActorRef _selfRef = null!;

    /// <summary>
    /// Reference to the sender of the message currently being processed — what
    /// <c>sender</c> in Spek source compiles to. Updated by the dispatch loop
    /// before each handler runs; generated reply code Tells back to it.
    /// </summary>
    protected ActorRef _currentSender = null!;
    private ActorSystem? _system;
    private string? _persistenceKey;
    private ISnapshotStore? _snapshotStore;
    private IDeadLetterSink? _deadLetterSink;

    internal void Initialize(
        ActorRef self,
        ActorSystem? system = null,
        string? persistenceKey = null,
        ISnapshotStore? snapshotStore = null,
        IDeadLetterSink? deadLetterSink = null)
    {
        // Synchronous entry point — kept so the sync Spawn path stays fast
        // for in-memory stores. Async-backed stores should use the async
        // spawn path (InitializeAsync, SpawnAsync / SpawnPersistentAsync)
        // to avoid the blocking wait inside the restore branch.
        InitializeCore(self, system, persistenceKey, snapshotStore, deadLetterSink);

        if (_snapshotStore is not null && _persistenceKey is not null)
        {
            var snapshot = _snapshotStore.LoadAsync(_persistenceKey).GetAwaiter().GetResult();
            if (snapshot is not null) OnRestore(snapshot);
        }
    }

    internal async Task InitializeAsync(
        ActorRef self,
        ActorSystem? system = null,
        string? persistenceKey = null,
        ISnapshotStore? snapshotStore = null,
        IDeadLetterSink? deadLetterSink = null)
    {
        InitializeCore(self, system, persistenceKey, snapshotStore, deadLetterSink);

        if (_snapshotStore is not null && _persistenceKey is not null)
        {
            var snapshot = await _snapshotStore.LoadAsync(_persistenceKey).ConfigureAwait(false);
            if (snapshot is not null) OnRestore(snapshot);
        }
    }

    private void InitializeCore(
        ActorRef self,
        ActorSystem? system,
        string? persistenceKey,
        ISnapshotStore? snapshotStore,
        IDeadLetterSink? deadLetterSink)
    {
        _selfRef = self;
        _system  = system;
        _persistenceKey = persistenceKey;
        _snapshotStore  = snapshotStore;
        _deadLetterSink = deadLetterSink;

        OnPreStart();
    }

    // ─── Abstract / virtual hooks for generated or user-written actors ──────

    /// <summary>Generated override dispatches to the active behavior handler.</summary>
    protected abstract Task DispatchAsync(object message, ActorRef sender);

    /// <summary>Generated override captures all actor fields for snapshot.</summary>
    protected virtual IReadOnlyDictionary<string, object?> CaptureFields() =>
        new Dictionary<string, object?>();

    /// <summary>
    /// Lifecycle hook fired once when the actor instance starts — after its
    /// fields are wired up but before the first message is dispatched. Also
    /// fires when a passivated actor is re-materialised. Default is a no-op;
    /// override to acquire resources whose lifetime tracks a live instance.
    /// </summary>
    protected virtual void OnPreStart()           { }

    /// <summary>
    /// Lifecycle hook fired once when the actor stops for good — after the
    /// mailbox is drained to dead-letters, before the reference is invalidated.
    /// Default is a no-op. See <see cref="OnPassivate"/> for the going-idle
    /// counterpart and <see cref="OnTerm"/> for the disposal hook that runs
    /// after this.
    /// </summary>
    protected virtual void OnPostStop()           { }

    /// <summary>
    /// Restore hook for persistent actors — called with the latest
    /// <paramref name="s"/> snapshot during start (and after a Restart) so the
    /// instance can rehydrate its fields. Default is a no-op; the compiler
    /// emits an override for actors with persisted state.
    /// </summary>
    protected virtual void OnRestore(Snapshot s)  { }

    /// <summary>
    /// Disposal hook, the resource-cleanup counterpart to the
    /// constructor / <c>init { }</c> block. Runs once at the end of
    /// the stop sequence — after <see cref="OnPostStop"/>, before the
    /// actor reference is invalidated. The compiler emits the body of
    /// the actor's <c>term { }</c> block as an override of this method
    /// (and also implements <see cref="IAsyncDisposable"/> on the
    /// generated class) when present. Default is a no-op.
    /// </summary>
    protected virtual void OnTerm() { }

    /// <summary>
    /// Voluntarily stop this actor. The current handler runs to
    /// completion; the dispatch loop then drains pending mailbox
    /// messages to dead-letters and fires <see cref="OnPostStop"/>.
    /// Subsequent <c>Tell</c>s land in dead-letters with a "stopped"
    /// reason. Mirrors Akka's <c>Context.Stop(Self)</c> and Erlang's
    /// <c>gen_server:stop/3</c>.
    /// </summary>
    protected void StopSelf() => _selfRef?.Slot?.Stop();

    // ─── Observability accessors ─────────────────────────────────────────────

    /// <summary>
    /// The metric sink registered on this actor's
    /// <see cref="ActorSystem"/>. <c>self.Metrics</c> in Spek
    /// source resolves to this property. Defaults to
    /// <see cref="NullMetricSink"/> when no observability adapter
    /// is registered, so calls are inlined-away no-ops.
    /// </summary>
    protected IMetricSink Metrics => _system?.Metrics ?? NullMetricSink.Instance;

    /// <summary>
    /// The structured logger registered on this actor's
    /// <see cref="ActorSystem"/>. <c>self.Log</c> in Spek source
    /// resolves to this property.
    /// </summary>
    protected IStructuredLogger Log => _system?.Logger ?? NullStructuredLogger.Instance;

    /// <summary>
    /// The node-lifecycle handle. <c>self.System</c> in Spek source
    /// resolves to this (the emitter maps it to <c>this.SpekSystem</c>; the
    /// C# name avoids shadowing the <c>System</c> namespace inside generated
    /// actors). Use <c>self.System.Shutdown()</c> to bring the node down
    /// gracefully from inside a handler (the supported replacement for
    /// <c>Environment.Exit</c>). Narrow by design — see
    /// <see cref="ActorSystemHandle"/>.
    /// </summary>
    protected ActorSystemHandle SpekSystem => _systemHandle ??= new ActorSystemHandle(_system);
    private ActorSystemHandle? _systemHandle;

    /// <summary>
    /// The system's cooperative-cancellation token, fired when shutdown turns
    /// forceful. The emitter threads this into auto-awaited, cancellation-accepting
    /// calls in handler bodies — it is never written in Spek source. Defaults to a
    /// non-cancellable token when the actor has no system (test doubles).
    /// </summary>
    protected CancellationToken ShutdownToken => _system?.ShutdownToken ?? default;

    /// <summary>
    /// The W3C trace context flowing through this handler.
    /// <c>self.TraceContext</c> in Spek source resolves to this
    /// property. Backed by <see cref="System.Diagnostics.Activity.Current"/>:
    /// when an Activity is on the async-local stack the context is
    /// active; otherwise <see cref="NullTraceContext.Instance"/> is
    /// returned. Trace context propagates automatically through
    /// <c>Tell</c> / <c>AskAsync</c> via standard .NET Activity
    /// flow.
    /// </summary>
    protected ITraceContext TraceContext
    {
        get
        {
            var activity = System.Diagnostics.Activity.Current;
            return activity is null
                ? NullTraceContext.Instance
                : new ActivityTraceContext(activity);
        }
    }

    private sealed class ActivityTraceContext : ITraceContext
    {
        private readonly System.Diagnostics.Activity _activity;
        public ActivityTraceContext(System.Diagnostics.Activity activity) => _activity = activity;

        public string TraceId => _activity.TraceId.ToString();
        public string SpanId  => _activity.SpanId.ToString();
        public bool IsActive  => true;

        public IReadOnlyDictionary<string, string> Baggage
        {
            get
            {
                var result = new Dictionary<string, string>(StringComparer.Ordinal);
                foreach (var (key, value) in _activity.Baggage)
                    if (value is not null) result[key] = value;
                return result;
            }
        }

        public ITraceContext WithBaggage(string key, string value)
        {
            _activity.SetBaggage(key, value);
            return this;
        }
    }

    /// <summary>
    /// Fetch the per-system singleton instance of a `shared`
    /// region attached to this actor with `use X foo;`. The compiler
    /// emits a lazy property accessor that calls this once per region
    /// per actor instance; users do not call it directly.
    /// </summary>
    protected T GetSharedRegion<T>() where T : Spek.SharedRegion, new()
        => _system?.GetSharedRegion<T>()
           ?? throw new InvalidOperationException(
               "Actor not initialised — GetSharedRegion called before Initialize.");

    /// <summary>
    /// Called right before the runtime unloads this actor during passivation.
    /// Symmetric with <see cref="OnPreStart"/> (which fires when the actor is
    /// re-materialised on the next message). Use this to release resources
    /// whose lifetime should track an active actor instance — file handles,
    /// open sockets, running timers. <see cref="PersistAsync"/> is called
    /// after this, so state captured by <see cref="CaptureFields"/> is
    /// snapshotted with whatever this hook leaves behind.
    /// <para>
    /// Distinct from <see cref="OnPostStop"/>: OnPostStop means the actor is
    /// gone for good. OnPassivate means "going idle; will wake on next message."
    /// </para>
    /// </summary>
    protected virtual void OnPassivate() { }

    internal void InvokeOnPassivate() => OnPassivate();

    /// <summary>
    /// Called by generated code when a message arrives that no <c>on</c>
    /// clause in the active behavior handled. Default routes it to the
    /// dead-letter sink. Override to customise per actor.
    /// </summary>
    protected virtual void Unhandled(object message)
    {
        _deadLetterSink?.DeadLetter(message,
            $"no handler matched in {GetType().Name}", cause: null);
    }

    /// <summary>
    /// Used by generated code (visibility checks, future
    /// per-handler diagnostics) to route a message to the dead-letter
    /// sink with a custom reason. Wrapper around the private sink
    /// reference so emitted code in derived classes can call it.
    /// </summary>
    protected void ToDeadLetter(object message, string reason) =>
        _deadLetterSink?.DeadLetter(message, reason, cause: null);

    /// <summary>
    /// Classifies <paramref name="message"/> as targeting a reader
    /// arm (true) or a writer arm (false) in the actor's currently
    /// active behavior. Used by <see cref="ActorSlot"/>'s
    /// dispatch loop to decide whether to fire-and-forget the
    /// handler (reader) or run it under exclusive lock (writer).
    /// <para>
    /// Default implementation classifies every message as a writer
    /// — preserves single-threaded semantics for actors
    /// that don't opt into the reader/writer model. Generated actors override
    /// this with a per-behavior switch driven by a delegate that
    /// swaps on <c>become</c>.
    /// </para>
    /// <para>
    /// <c>protected internal</c> rather than <c>internal</c> so user
    /// code emitted into a separate assembly (the typical Spek
    /// transpiler flow) can override it.
    /// </para>
    /// </summary>
    protected internal virtual bool ClassifyAsReader(object message) => false;

    /// <summary>
    /// Called by <see cref="ActorSlot"/> to surface an asker-side
    /// failure when a reader handler threw. Default no-op — only the
    /// internal <c>ReplyActor&lt;T&gt;</c> overrides it to fail the
    /// caller's <see cref="TaskCompletionSource{TResult}"/> with the
    /// supplied <see cref="AskException"/>. Regular actors don't
    /// expect a reply on their sender path so the call is harmless.
    /// </summary>
    internal virtual void FailReplyWith(Exception ex) { }

    /// <summary>
    /// Decides how to handle an exception from <see cref="DispatchAsync"/>.
    /// Override to customise per actor. Default is <see cref="FailureDirective.Stop"/>
    /// — fail loud, in line with Spek's compile-time-guarantees philosophy.
    /// </summary>
    protected virtual FailureDirective OnFailure(Exception exception, object message)
        => FailureDirective.Stop;

    /// <summary>
    /// Called on the parent actor when a child fails with
    /// <see cref="FailureDirective.Escalate"/>. The parent inspects the
    /// failure and returns a directive to apply to the child.
    /// <para>
    /// Default: <see cref="FailureDirective.Stop"/> — supervisors are
    /// opt-in; a parent that hasn't overridden this just stops the
    /// misbehaving child.
    /// </para>
    /// <para>
    /// Keep overrides stateless. The parent's own dispatch loop may be
    /// running concurrently on another thread; this hook is called without
    /// serialising against it.
    /// </para>
    /// </summary>
    protected virtual FailureDirective OnChildFailure(ActorRef child, Exception cause, object message)
        => FailureDirective.Stop;

    /// <summary>
    /// If non-null, the runtime will unload this actor after the given idle
    /// duration — persisting its state and re-materialising (restoring from
    /// snapshot) on the next message. Actors opt in by overriding; the
    /// emitter generates this override when the Spek source contains
    /// <c>passivate after N.unit</c>. Default is null (stay loaded).
    /// </summary>
    protected virtual TimeSpan? PassivationTimeout => null;

    internal TimeSpan? GetPassivationTimeout() => PassivationTimeout;

    /// <summary>
    /// Maximum number of <see cref="FailureDirective.Restart"/> events this
    /// actor may hit inside <see cref="RestartWindow"/> before the runtime
    /// degrades the next Restart to <see cref="FailureDirective.Stop"/>.
    /// Default: <see cref="int.MaxValue"/> (unlimited — matches older
    /// behavior).
    /// </summary>
    protected virtual int MaxRestartsWithinWindow => int.MaxValue;

    /// <summary>
    /// Sliding window over which <see cref="MaxRestartsWithinWindow"/> is
    /// counted. Null means "unlimited history" (only useful if
    /// MaxRestartsWithinWindow is also unbounded). Default: null.
    /// </summary>
    protected virtual TimeSpan? RestartWindow => null;

    internal int GetMaxRestartsWithinWindow() => MaxRestartsWithinWindow;
    internal TimeSpan? GetRestartWindow() => RestartWindow;

    // ─── Supervision helper for generated OnChildFailure overrides ──────────

    private readonly Dictionary<ActorRef, List<DateTime>> _childRestartLog = new();
    private readonly object _childRestartLogGate = new();

    /// <summary>
    /// Applies a supervision policy for a specific child reference. Used by
    /// emitter-generated <see cref="OnChildFailure"/> overrides. For
    /// <see cref="FailureDirective.Restart"/> with a <paramref name="maxRetries"/>
    /// budget, tracks recent restart timestamps per child and degrades the
    /// decision to <see cref="FailureDirective.Stop"/> when the budget is
    /// exhausted inside <paramref name="window"/>. For other directives
    /// (Resume / Stop / Escalate), returns them unchanged.
    /// </summary>
    protected FailureDirective ApplyRestartPolicy(
        ActorRef child,
        FailureDirective action,
        int? maxRetries = null,
        TimeSpan? window = null)
    {
        if (action != FailureDirective.Restart) return action;
        if (maxRetries is null) return FailureDirective.Restart;

        lock (_childRestartLogGate)
        {
            if (!_childRestartLog.TryGetValue(child, out var log))
            {
                log = new List<DateTime>();
                _childRestartLog[child] = log;
            }
            var now = DateTime.UtcNow;
            if (window is { } w) log.RemoveAll(t => now - t > w);
            if (log.Count >= maxRetries.Value) return FailureDirective.Stop;
            log.Add(now);
            return FailureDirective.Restart;
        }
    }

    // ─── Internal bridges used by ActorSlot to reach protected overrides ────

    internal Task InvokeDispatch(object message, ActorRef sender)
    {
        _currentSender = sender;
        return DispatchAsync(message, sender);
    }
    internal FailureDirective InvokeOnFailure(Exception ex, object msg) => OnFailure(ex, msg);
    internal FailureDirective InvokeOnChildFailure(ActorRef child, Exception ex, object msg)
        => OnChildFailure(child, ex, msg);
    internal void InvokeOnPostStop() => OnPostStop();
    internal void InvokeOnTerm()     => OnTerm();
    internal Task InvokePersistAsync() => PersistAsync();

    // ─── Persist / spawn services available to generated code ──────────────

    /// <summary>
    /// Captures the actor's current fields (via <see cref="CaptureFields"/>)
    /// and writes them to the snapshot store under this actor's persistence
    /// key. No-op when the actor isn't persistent (no store or key configured).
    /// Generated code calls this where the Spek source requests a save.
    /// </summary>
    protected async Task PersistAsync()
    {
        if (_snapshotStore is null || _persistenceKey is null) return;
        await _snapshotStore.SaveAsync(_persistenceKey, new Snapshot(CaptureFields()));
    }

    /// <summary>
    /// Spawns a child actor of type <typeparamref name="TActor"/> under this
    /// actor, links it as a child (so failures can <see cref="FailureDirective.Escalate"/>
    /// to this parent's <see cref="OnChildFailure"/>), and returns its
    /// reference. <paramref name="args"/> are passed to the child's constructor.
    /// Children currently spawn non-persistent. Emitter-facing helper backing
    /// <c>spawn</c> inside a handler.
    /// </summary>
    /// <typeparam name="TActor">The child actor class to instantiate.</typeparam>
    /// <param name="args">Constructor arguments for the child.</param>
    /// <returns>A reference to the newly spawned child actor.</returns>
    protected ActorRef SpawnChildAsync<TActor>(params object[] args)
        where TActor : ActorBase
    {
        // Children currently spawn non-persistent — persistent-identity for
        // hierarchies is an open design question.
        // Parent link comes from _selfRef so the child can Escalate up. When
        // spawning from inside `init`, Initialize hasn't run yet (_selfRef and
        // _system are still null) — the materialization context supplies the
        // parent slot and system plumbing instead.
        var materializing = Spek.Runtime.ActorSlot.MaterializingSlot;
        var parentSlot = _selfRef is not null ? _selfRef.Slot : materializing;
        var system = _system ?? materializing?.SystemForChildren;
        var snapshotStore = _snapshotStore ?? materializing?.SnapshotStoreForChildren;
        var deadLetterSink = _deadLetterSink ?? materializing?.DeadLetterSinkForChildren;

        var slot = new ActorSlot(
            factory: () => (TActor)Activator.CreateInstance(typeof(TActor), args)!,
            system: system,
            persistenceKey: null,
            snapshotStore: snapshotStore,
            deadLetterSink: deadLetterSink,
            parent: parentSlot);

        var selfRef = new ActorRef(slot);
        slot.Materialize(selfRef);
        system?.TrackSlot(slot);

        // Register with the parent so AllForOne broadcasts can reach siblings.
        parentSlot?.RegisterChild(slot);

        return selfRef;
    }

    /// <summary>
    /// Emitter-facing helper: restart every child the parent knows about
    /// except the one that actually failed. Called from an
    /// emitter-generated <see cref="OnChildFailure"/> override when the
    /// parent's <c>supervise</c> strategy is <c>AllForOne</c> — the
    /// failing child's own restart is handled by the normal supervision
    /// path so we skip it here to avoid double-restart.
    /// </summary>
    protected void RestartSiblingsOf(ActorRef failingChild)
    {
        if (_selfRef?.Slot is not ActorSlot parentSlot) return;
        var failingSlot = failingChild.Slot;
        foreach (var child in parentSlot.GetChildren())
        {
            if (ReferenceEquals(child, failingSlot)) continue;
            child.ScheduleRestart();
        }
    }

    // ─── Dead-letter sink (singleton used for the NoSender ref) ─────────────

    internal static readonly ActorBase DeadLetter = new DeadLetterActor();

    private sealed class DeadLetterActor : ActorBase
    {
        protected override Task DispatchAsync(object message, ActorRef sender) =>
            Task.CompletedTask;
    }

    // ─── Reply plumbing for ask ─────────────────────────────────────────────

    internal sealed class ReplyActor<T> : ActorBase
    {
        private readonly TaskCompletionSource<T> _tcs;
        public ReplyActor(TaskCompletionSource<T> tcs) => _tcs = tcs;

        /// <summary>
        /// When the recipient's reader handler throws and the
        /// asker is awaiting a reply, the slot routes the failure
        /// here so the caller's <c>await target.AskAsync(...)</c>
        /// throws (with <see cref="AskException"/>) rather than
        /// hanging forever.
        /// </summary>
        internal override void FailReplyWith(Exception ex) =>
            _tcs.TrySetException(ex);

        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            if (message is T result)
                _tcs.TrySetResult(result);
            else
                _tcs.TrySetException(new InvalidCastException(
                    $"Expected {typeof(T).Name} but got {message.GetType().Name}"));
            return Task.CompletedTask;
        }
    }
}
