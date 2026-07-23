using Spek.Observability;
using Spek.Persistence;

namespace Spek.Runtime;

/// <summary>Root of an actor hierarchy. Entry point for spawning top-level actors.</summary>
public sealed class ActorSystem : IDisposable
{
    private readonly string _name;
    private readonly List<ActorSlot> _slots = [];
    private readonly Dictionary<string, ActorRef> _namedRoots = new(StringComparer.Ordinal);
    // Per-system registry of `shared` regions. Lazily populated
    // on first access via GetSharedRegion<T>; one instance per type per
    // ActorSystem (matches the locked "per-ActorSystem scope, like ETS"
    // semantic). Generated region classes derive from Spek.SharedRegion
    // and have a parameterless constructor.
    private readonly Dictionary<Type, Spek.SharedRegion> _sharedRegions = new();
    // Construction order, used to dispose regions LIFO at
    // ActorSystem shutdown. Mirrors C# `using` block unwind semantics.
    private readonly List<Spek.SharedRegion> _regionConstructionOrder = new();
    private readonly Lock _lock = new();
    private readonly ISnapshotStore _snapshotStore;
    private readonly IDeadLetterSink _deadLetterSink;

    // Observability sinks. Always-on: the runtime always
    // calls into the registered sink, but the default implementations
    // are no-ops so no telemetry overhead is paid until a real
    // adapter is plugged in via Use*().
    private IMetricSink _metricSink = NullMetricSink.Instance;
    private IStructuredLogger _logger = NullStructuredLogger.Instance;

    // ActivitySource for the dispatch trace span.
    // StartActivity returns null unless an ActivityListener is
    // registered (the OpenTelemetry SDK and the .NET Diagnostic
    // tooling both register listeners), so the cost is "one virtual
    // call returning null" when no one is listening. Source name
    // is stable across releases for listener-side filtering.
    internal static readonly System.Diagnostics.ActivitySource ActivitySource
        = new("Spek.Runtime");

    // Cluster-layer hook. Set when a transport registers via
    // RegisterClusterAdapter; the cluster layer is what knows how to
    // resolve `(node, path)` into a remote endpoint, dispatch incoming
    // envelopes from the wire to local named roots, and so on.
    private IClusterAdapter? _clusterAdapter;

    // Pulsed whenever a slot transitions (spawned / dispatch loop ends /
    // stops). <see cref="AwaitTermination"/> waits on this instead of
    // busy-looping with Thread.Sleep. ManualResetEventSlim's level-triggered
    // semantics coalesce multiple signals inside a single wait-then-check
    // iteration, so fast-firing transitions don't starve the check.
    private readonly ManualResetEventSlim _slotActivityChanged = new(initialState: true);

    // Set once the system has been torn down (Dispose ran). AwaitTermination
    // treats this as a terminal state and returns true — distinguishing
    // "no slots yet" (still starting) from "no slots anymore" (disposed), which
    // the bare `_slots.Count > 0` idle check can't. Set *inside* Dispose, after
    // the drain, so GracefulShutdown's own internal AwaitTermination still drains
    // first (it keys off idle, not off this flag). Volatile: read lock-free at
    // the top of AwaitTermination's loop.
    private volatile bool _terminated;

    /// <summary>
    /// Creates an actor system with a default <see cref="InMemorySnapshotStore"/>
    /// and a <see cref="ConsoleDeadLetterSink"/>. Pass explicit implementations
    /// to use different backends.
    /// </summary>
    public ActorSystem(
        string name,
        ISnapshotStore? snapshotStore = null,
        IDeadLetterSink? deadLetterSink = null)
    {
        _name = name;
        _snapshotStore  = snapshotStore   ?? new InMemorySnapshotStore();
        _deadLetterSink = deadLetterSink ?? new ConsoleDeadLetterSink();
    }

    /// <summary>The snapshot store this system writes through on <c>persist</c>.</summary>
    public ISnapshotStore SnapshotStore => _snapshotStore;

    /// <summary>The sink that receives unhandled / dropped messages.</summary>
    public IDeadLetterSink DeadLetterSink => _deadLetterSink;

    /// <summary>The system's logical name — used by the cluster layer to
    /// label this node in observability tools.</summary>
    public string Name => _name;

    /// <summary>Registered metric sink. Defaults to
    /// <see cref="NullMetricSink"/>; replace via
    /// <see cref="UseMetricSink(IMetricSink)"/> when wiring an
    /// observability adapter.</summary>
    public IMetricSink Metrics => _metricSink;

    /// <summary>Registered structured logger. Defaults to
    /// <see cref="NullStructuredLogger"/>; replace via
    /// <see cref="UseLogger(IStructuredLogger)"/>.</summary>
    public IStructuredLogger Logger => _logger;

    /// <summary>Register a metric sink. Typically called once
    /// at startup before any actors are spawned. Replacing the sink
    /// after spawn is allowed but causes a brief gap where in-flight
    /// metrics may not be observed by the new sink.</summary>
    public ActorSystem UseMetricSink(IMetricSink sink)
    {
        _metricSink = sink ?? throw new ArgumentNullException(nameof(sink));
        return this;
    }

    /// <summary>Register a structured logger. See
    /// <see cref="UseMetricSink"/> for the lifecycle note.</summary>
    public ActorSystem UseLogger(IStructuredLogger logger)
    {
        _logger = logger ?? throw new ArgumentNullException(nameof(logger));
        return this;
    }

    /// <summary>
    /// Fetch (or lazily create) the singleton shared-region
    /// instance of type <typeparamref name="T"/> for this system. Each
    /// `shared X { ... }` declaration emits a class deriving from
    /// <see cref="Spek.SharedRegion"/> with a parameterless constructor;
    /// generated actor code calls this once per attached region.
    /// Per-`ActorSystem` scope means two systems running side-by-side
    /// have independent regions even when the type is the same.
    /// </summary>
    public T GetSharedRegion<T>() where T : Spek.SharedRegion, new()
    {
        lock (_lock)
        {
            if (!_sharedRegions.TryGetValue(typeof(T), out var region))
            {
                region = new T();
                // Wire the system reference so the region can resolve
                // its store, dead-letter sink, etc.
                region.System = this;
                _sharedRegions[typeof(T)] = region;
                // Record construction order for LIFO disposal.
                _regionConstructionOrder.Add(region);
            }
            return (T)region;
        }
    }

    // Per-region snapshot-store registrations. Looked
    // up in PersistedRegion.ResolveStore before falling back to the
    // system-default snapshot store. Keyed by region class type.
    private readonly Dictionary<Type, ISnapshotStore> _regionStores = new();

    /// <summary>
    /// Register a snapshot store for one specific
    /// shared-region type, overriding the system default for that
    /// region. Call from a Spek <c>program</c> block before any
    /// actor that uses the region is spawned. Every region declared
    /// <c>shared X : Persisted { ... }</c> must have a registered
    /// provider somewhere in the compilation (CE0098).
    /// </summary>
    public void RegisterPersistenceProvider<T>(ISnapshotStore store)
        where T : Spek.SharedRegion
    {
        ArgumentNullException.ThrowIfNull(store);
        lock (_lock) _regionStores[typeof(T)] = store;
    }

    /// <summary>
    /// Internal lookup for <see cref="Spek.PersistedRegion"/> —
    /// returns the per-region store override for <paramref name="regionType"/>
    /// if one was registered, else <c>null</c> (caller falls back to
    /// the system default).
    /// </summary>
    internal ISnapshotStore? GetRegionStore(Type regionType)
    {
        lock (_lock)
            return _regionStores.GetValueOrDefault(regionType);
    }

    // ─── Named roots — wire-addressable top-level actors ────────────────────

    /// <summary>
    /// Spawns a top-level actor and registers it under <paramref name="path"/>
    /// so remote callers can address it across the cluster wire. Locally
    /// equivalent to <see cref="Spawn{TActor}"/>; the difference is the
    /// system remembers the name → ref binding and routes inbound
    /// <c>RemoteEnvelope.TargetPath = path</c> traffic here.
    /// </summary>
    public ActorRef SpawnNamed<TActor>(string path, params object[] args)
        where TActor : ActorBase
    {
        ArgumentException.ThrowIfNullOrEmpty(path);
        var actor = Spawn<TActor>(args);
        lock (_lock) _namedRoots[path] = actor;
        return actor;
    }

    /// <summary>
    /// Non-generic <see cref="SpawnNamed{TActor}"/> — useful when the
    /// actor type is known at runtime (e.g. located-actor auto-activation
    /// in <c>Spek.Cluster</c>'s <c>Locate&lt;T&gt;</c> machinery).
    /// </summary>
    public ActorRef SpawnNamed(Type actorType, string path, params object[] args)
    {
        ArgumentException.ThrowIfNullOrEmpty(path);
        EnsureSpekActor(actorType);
        var actor = Spawn(actorType, args);
        lock (_lock) _namedRoots[path] = actor;
        return actor;
    }

    /// <summary>Resolves a previously-named local root, or null if not bound.</summary>
    public ActorRef? ResolveNamed(string path)
    {
        lock (_lock) return _namedRoots.GetValueOrDefault(path);
    }

    /// <summary>
    /// Reverse lookup — returns the path that <paramref name="actor"/> was
    /// registered under via <see cref="SpawnNamed{TActor}"/>, or
    /// <c>null</c> if it's anonymous. Used by the cluster layer to put
    /// the local actor's path into the wire envelope's sender field so
    /// remote replies route back.
    /// </summary>
    public string? PathOfNamedRoot(ActorRef actor)
    {
        if (actor is null) return null;
        lock (_lock)
        {
            foreach (var (path, r) in _namedRoots)
                if (ReferenceEquals(r, actor)) return path;
            return null;
        }
    }

    // ─── Cluster layer plug-in points ───────────────────────────────────────

    /// <summary>
    /// Wired by <c>Spek.Cluster</c> when a transport registers with this
    /// system. The runtime stays remoting-agnostic — it knows there's
    /// "something" that can resolve remote refs and dispatch outbound
    /// envelopes, but it doesn't know what protocol or even that there's
    /// a wire involved (in-memory transports plug into the same hook).
    /// </summary>
    public void RegisterClusterAdapter(IClusterAdapter adapter)
    {
        _clusterAdapter = adapter ?? throw new ArgumentNullException(nameof(adapter));
    }

    /// <summary>The cluster adapter currently wired to this system, or null
    /// if the system is single-node.</summary>
    public IClusterAdapter? ClusterAdapter => _clusterAdapter;

    /// <summary>
    /// Called by the cluster layer when an inbound envelope arrives on the
    /// wire. Looks up the named root for <paramref name="targetPath"/> and
    /// Tells it the message, optionally with a remote-sender ref so
    /// <c>sender.Tell(reply)</c> on the receiving actor routes back across
    /// the wire.
    /// </summary>
    public void DeliverIncoming(string targetPath, object message, ActorRef? sender)
    {
        var root = ResolveNamed(targetPath);
        if (root is null)
        {
            _deadLetterSink.DeadLetter(
                message,
                reason: $"no named root '{targetPath}' on system '{_name}'",
                cause: null);
            return;
        }
        if (sender is not null)
            root.Tell(message, sender);
        else
            root.Tell(message);
    }

    /// <summary>
    /// Spawns an actor without a stable persistence key — <c>persist</c> writes
    /// are scoped to this process and won't be recovered across restarts. Use
    /// <see cref="SpawnPersistent{TActor}"/> when you need cross-run durability.
    /// </summary>
    public ActorRef Spawn<TActor>(params object[] args)
        where TActor : ActorBase
        => SpawnInternal(typeof(TActor), persistenceKey: null, args);

    /// <summary>
    /// Non-generic spawn — useful when the actor type is only known at runtime
    /// (e.g. dynamically-compiled Spek code loaded via reflection).
    /// </summary>
    public ActorRef Spawn(Type actorType, params object[] args)
    {
        EnsureSpekActor(actorType);
        return SpawnInternal(actorType, persistenceKey: null, args);
    }

    /// <summary>
    /// Spawns an actor bound to <paramref name="persistenceKey"/>. If a snapshot
    /// already exists for that key, the actor's <c>on Restore</c> handler fires
    /// before it begins processing messages. If the actor later fails with a
    /// <see cref="FailureDirective.Restart"/> directive, the rebuilt instance
    /// also reloads from the latest snapshot.
    /// </summary>
    public ActorRef SpawnPersistent<TActor>(string persistenceKey, params object[] args)
        where TActor : ActorBase
        => SpawnInternal(typeof(TActor), persistenceKey, args);

    /// <summary>Non-generic <see cref="SpawnPersistent{TActor}"/>.</summary>
    public ActorRef SpawnPersistent(Type actorType, string persistenceKey, params object[] args)
    {
        EnsureSpekActor(actorType);
        return SpawnInternal(actorType, persistenceKey, args);
    }

    private ActorRef SpawnInternal(Type actorType, string? persistenceKey, object[] args)
    {
        var slot = BuildSlot(actorType, persistenceKey, args);
        var selfRef = new ActorRef(slot);
        slot.Materialize(selfRef);  // OnPreStart + OnRestore fire here (blocking)
        TrackSlot(slot);
        return selfRef;
    }

    /// <summary>Async spawn — use when your <see cref="ISnapshotStore"/> does real I/O.</summary>
    public async Task<ActorRef> SpawnAsync<TActor>(params object[] args)
        where TActor : ActorBase
        => await SpawnInternalAsync(typeof(TActor), persistenceKey: null, args).ConfigureAwait(false);

    /// <summary>Async spawn (non-generic variant).</summary>
    public async Task<ActorRef> SpawnAsync(Type actorType, params object[] args)
    {
        EnsureSpekActor(actorType);
        return await SpawnInternalAsync(actorType, persistenceKey: null, args).ConfigureAwait(false);
    }

    /// <summary>Async persistent spawn — <c>OnRestore</c> fires after the snapshot loads.</summary>
    public async Task<ActorRef> SpawnPersistentAsync<TActor>(string persistenceKey, params object[] args)
        where TActor : ActorBase
        => await SpawnInternalAsync(typeof(TActor), persistenceKey, args).ConfigureAwait(false);

    /// <summary>Async persistent spawn (non-generic variant).</summary>
    public async Task<ActorRef> SpawnPersistentAsync(Type actorType, string persistenceKey, params object[] args)
    {
        EnsureSpekActor(actorType);
        return await SpawnInternalAsync(actorType, persistenceKey, args).ConfigureAwait(false);
    }

    private async Task<ActorRef> SpawnInternalAsync(Type actorType, string? persistenceKey, object[] args)
    {
        var slot = BuildSlot(actorType, persistenceKey, args);
        var selfRef = new ActorRef(slot);
        await slot.MaterializeAsync(selfRef).ConfigureAwait(false);
        TrackSlot(slot);
        return selfRef;
    }

    private ActorSlot BuildSlot(Type actorType, string? persistenceKey, object[] args) =>
        new(
            factory: () => (ActorBase)Activator.CreateInstance(actorType, args)!,
            system: this,
            persistenceKey: persistenceKey,
            snapshotStore: _snapshotStore,
            deadLetterSink: _deadLetterSink);

    private static void EnsureSpekActor(Type actorType)
    {
        if (!typeof(ActorBase).IsAssignableFrom(actorType))
            throw new ArgumentException(
                $"{actorType} must derive from {nameof(ActorBase)}.", nameof(actorType));
    }

    internal void TrackSlot(ActorSlot slot)
    {
        lock (_lock) _slots.Add(slot);
        SignalSlotActivity();  // a new slot might make us not-idle
    }

    /// <summary>
    /// Called by <see cref="ActorSlot"/> whenever its processing state
    /// transitions (dispatch loop ends, stop fires, etc.). Pulses
    /// <see cref="_slotActivityChanged"/> so <see cref="AwaitTermination"/>
    /// re-runs its check.
    /// </summary>
    internal void SignalSlotActivity() => _slotActivityChanged.Set();

    /// <summary>
    /// Non-blocking snapshot: <c>true</c> when at least one actor is tracked
    /// and every one is idle (mailbox empty, nothing processing). Lets callers
    /// poll for quiescence asynchronously (e.g. <c>await Task.Delay</c> between
    /// checks) instead of parking a thread in <see cref="AwaitTermination"/>.
    /// </summary>
    public bool IsIdle
    {
        get { lock (_lock) return _slots.Count > 0 && _slots.All(s => s.IsIdle); }
    }

    /// <summary>
    /// Blocks until every tracked actor is idle (mailbox empty, not
    /// processing). Returns immediately if the condition is already
    /// satisfied at call time. Requires at least one slot to have been
    /// tracked — a system with no spawns blocks forever, matching the
    /// original semantics.
    /// <para>
    /// Pass <paramref name="timeout"/> to cap the wait; returns
    /// <c>false</c> if the timeout elapses before the system goes idle.
    /// </para>
    /// </summary>
    public bool AwaitTermination(TimeSpan? timeout = null)
    {
        var deadline = timeout is { } t ? DateTime.UtcNow + t : (DateTime?)null;

        while (true)
        {
            // Terminal: the system was torn down (e.g. a handler called
            // self.System.Shutdown(), whose background GracefulShutdown drained
            // then disposed). Without this, the `_slots.Count > 0` idle check
            // below would never be satisfied once Dispose cleared the slots, and
            // this call would hang forever.
            if (_terminated) return true;

            try
            {
                if (deadline is { } d)
                {
                    var remaining = d - DateTime.UtcNow;
                    if (remaining <= TimeSpan.Zero) return false;
                    if (!_slotActivityChanged.Wait(remaining)) return _terminated;
                }
                else
                {
                    _slotActivityChanged.Wait();
                }
                _slotActivityChanged.Reset();
            }
            catch (ObjectDisposedException)
            {
                // Dispose() disposed the activity event out from under us — the
                // system has terminated. Treat as a clean return rather than
                // surfacing the race as an exception.
                return true;
            }

            if (_terminated) return true;

            bool allIdle;
            lock (_lock) allIdle = _slots.Count > 0 && _slots.All(s => s.IsIdle);
            if (allIdle) return true;
        }
    }

    /// <summary>
    /// Drains and shuts down in one call: waits for every tracked actor to go
    /// idle (bounded by <paramref name="timeout"/> if given), then disposes
    /// the system — actors stop and shared regions run their <c>term { }</c>
    /// blocks in reverse construction order. Returns <c>true</c> when the
    /// system drained cleanly, <c>false</c> when the timeout elapsed first
    /// (shutdown still proceeds). A system that never spawned an actor skips
    /// the wait — there is nothing to drain. Replaces the
    /// <c>AwaitTermination(); Dispose();</c> two-step.
    /// </summary>
    public bool GracefulShutdown(TimeSpan? timeout = null)
    {
        bool anySlots;
        lock (_lock) anySlots = _slots.Count > 0;
        var drained = !anySlots || AwaitTermination(timeout);
        // Drain timed out → turn forceful: cancel the shutdown token so any
        // wedged handler's in-flight awaited work unwinds before teardown.
        if (!drained) TryCancelShutdown();
        Dispose();
        return drained;
    }

    private void TryCancelShutdown()
    {
        try { _shutdownCts.Cancel(); } catch (ObjectDisposedException) { /* already torn down */ }
    }

    // ─── Actor-initiated shutdown (self.System.Shutdown()) ──────────────────

    private Action? _onShutdownRequested;
    private int _shutdownRequested;

    // Invisible cooperative-cancellation root. Cancelled when shutdown turns
    // *forceful* — a graceful drain that timed out, or teardown — never on the
    // first RequestShutdown (a clean drain lets in-flight messages finish, which
    // is what "graceful" means). The emitter threads this token into auto-awaited,
    // cancellation-accepting calls inside actor handlers (never visible in Spek
    // source), so a wedged handler's in-flight async work unwinds instead of being
    // abandoned. Hosting adapters link their StopAsync token into it via
    // LinkShutdownToken so the host's ShutdownTimeout flows all the way through.
    private readonly CancellationTokenSource _shutdownCts = new();

    /// <summary>
    /// The system's cooperative-cancellation token — fires when shutdown turns
    /// forceful (drain timeout or teardown). Reached from emitted handler code via
    /// <c>ActorBase.ShutdownToken</c>; it is never written in Spek source.
    /// </summary>
    public CancellationToken ShutdownToken => _shutdownCts.Token;

    /// <summary>
    /// Links an external token (e.g. a host's <c>StopAsync</c> token, already
    /// bounded by <c>HostOptions.ShutdownTimeout</c>) into the system's shutdown
    /// token: when the host cancels, in-flight handler work cancels too.
    /// </summary>
    public void LinkShutdownToken(CancellationToken external) =>
        external.Register(static state => ((CancellationTokenSource)state!).Cancel(), _shutdownCts);

    /// <summary>
    /// Registered by the hosting adapter to define what "shut down" means for
    /// this process — e.g. the Console host Tells the entry actor its
    /// <c>Shutdown</c> message, the same path Ctrl+C / SIGTERM use. When set,
    /// <see cref="RequestShutdown"/> invokes it; when unset (a bare
    /// <see cref="ActorSystem"/>, tests), RequestShutdown falls back to a
    /// background <see cref="GracefulShutdown"/>.
    /// </summary>
    public void OnShutdownRequested(Action handler) => _onShutdownRequested = handler;

    /// <summary>
    /// Actor-reachable, <b>non-blocking</b> shutdown trigger — what
    /// <c>self.System.Shutdown()</c> calls. It must not drain inline: the
    /// calling handler is itself keeping the system busy, so calling
    /// <see cref="GracefulShutdown"/> here would deadlock waiting for idle.
    /// Idempotent — only the first request fires.
    /// </summary>
    public void RequestShutdown()
    {
        if (Interlocked.Exchange(ref _shutdownRequested, 1) != 0) return;
        var handler = _onShutdownRequested;
        if (handler is not null)
            handler();
        else
            // Hostless fallback: drain on a background thread so the calling
            // handler can return and the actors can go idle.
            _ = Task.Run(() =>
            {
                try { GracefulShutdown(TimeSpan.FromSeconds(30)); }
                catch { /* best-effort teardown */ }
            });
    }

    /// <summary>
    /// Tears the system down. Signals forceful cancellation to unstick in-flight
    /// handlers, stops every actor gracefully (running <c>OnPostStop</c> and any
    /// <c>term { }</c> block), disposes the slots, then exits shared regions in
    /// reverse construction order — flushing persisted regions so a graceful
    /// shutdown is durable. Idempotent.
    /// </summary>
    public void Dispose()
    {
        // Terminal forceful signal: unstick any in-flight handler await before we
        // tear the actors down. Idempotent with the drain-timeout cancel above.
        TryCancelShutdown();
        lock (_lock)
        {
            // Stop actors gracefully before tearing the slots down: run
            // each actor's OnPostStop and term { } block. Without this, a system
            // shutdown — including the hostless self.System.Shutdown() path —
            // disposed the slots (just flipping a flag) and silently skipped every
            // actor's cleanup. Runs before the region term blocks below, since an
            // actor's cleanup may still touch a shared region.
            foreach (var slot in _slots) slot.StopGracefully();
            foreach (var slot in _slots) slot.Dispose();
            _slots.Clear();

            // Dispose shared regions in reverse construction
            // order (LIFO), mirroring C# `using` block unwinding. The
            // OnTerm hook runs the user's `term { }` body if one was
            // declared; the base implementation is a no-op so regions
            // without a term block exit silently. Failures are
            // swallowed and dead-lettered so a buggy term block on
            // one region can't block disposal of the others.
            for (int i = _regionConstructionOrder.Count - 1; i >= 0; i--)
            {
                var region = _regionConstructionOrder[i];
                // Durable shutdown: flush a persisted region's latest state
                // before tearing it down. Writer-exit saves are fire-and-forget, so
                // without this the final save can race teardown and be lost. The
                // actors are already stopped (above), so the region state is final.
                if (region is PersistedRegion persisted)
                {
                    try { persisted.FlushSave(); }
                    catch (Exception saveEx)
                    {
                        _deadLetterSink?.DeadLetter(
                            region, $"region '{region.Name}' shutdown flush failed", cause: saveEx);
                    }
                }
                try { region.InvokeOnTerm(); }
                catch (Exception termEx)
                {
                    _deadLetterSink?.DeadLetter(
                        region, $"region '{region.Name}' term block threw", cause: termEx);
                }
            }
            _regionConstructionOrder.Clear();
            _sharedRegions.Clear();
        }

        // Mark terminal and wake any thread parked in AwaitTermination so it
        // observes _terminated and returns, *before* the wait event is disposed.
        // (Set inside-lock would be cleaner for ordering, but the read in
        // AwaitTermination is lock-free against the volatile, and Set() must run
        // after the slot/region teardown above so a waiter that wakes sees a
        // fully torn-down system.)
        _terminated = true;
        try { _slotActivityChanged.Set(); } catch (ObjectDisposedException) { }
        _slotActivityChanged.Dispose();
        _shutdownCts.Dispose();
    }
}
