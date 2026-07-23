namespace Spek;

/// <summary>
/// Base class for `shared` regions. A shared region is per-
/// <see cref="Spek.Runtime.ActorSystem"/> state with its own
/// reader/writer lock, separate from any actor's per-slot lock. The
/// compiler emits one concrete subclass per <c>shared X { ... }</c>
/// declaration; user code never instantiates one — the actor system's
/// registry hands out the singleton per type.
///
/// Direct subclasses (default = transient) get just the RW lock + lazy
/// init. Inheriting <see cref="PersistedRegion"/> instead adds restore-
/// on-first-access and save-on-writer-exit via the system's snapshot
/// store; future <c>EventSourcedRegion</c> / <c>ReplicatedRegion</c> /
/// <c>ConflictFreeReplicatedRegion</c> markers will plug into the same
/// extension points.
/// </summary>
public abstract class SharedRegion
{
    // Set by ActorSystem.GetSharedRegion<T> immediately after
    // construction so the region can resolve its store, system-scoped
    // dead-letter sink, etc. Stays internal — user-written init bodies
    // talk to `Name` (settable) and `System` indirectly via inherited
    // helpers, never to this property by name.
    internal Spek.Runtime.ActorSystem? System { get; set; }

    /// <summary>
    /// User-facing name for this region instance. Defaults to the
    /// region type's simple name; settable from inside the user's
    /// `init { ... }` block via <c>self.Name = "...";</c>. Persistence
    /// uses this as the snapshot key, so changing it lets users
    /// quarantine old snapshots after a schema break.
    /// </summary>
    public string Name
    {
        get => _name ?? GetType().Name;
        set => _name = value;
    }
    private string? _name;
    private readonly object _gate = new();
    private int _activeReaders;
    private bool _writerActive;
    private TaskCompletionSource _readersDrained =
        new(TaskCreationOptions.RunContinuationsAsynchronously);
    private TaskCompletionSource _writerReleased =
        new(TaskCreationOptions.RunContinuationsAsynchronously);

    // Lazy-init coordination. The first thread to call
    // either EnterReaderAsync or EnterWriterAsync runs Initialize();
    // concurrent first-callers wait on _initDone. Init runs *before*
    // the actual lock acquisition, so it executes alone — no other
    // reader/writer is holding the lock concurrently because all of
    // them queue behind EnsureInitializedAsync first. After init
    // completes, _initState becomes 2 and EnsureInitializedAsync
    // returns immediately on every subsequent call.
    private int _initState;                  // 0 = not started, 1 = running, 2 = done
    private TaskCompletionSource? _initDone;

    /// <summary>
    /// Subclasses (the generated region classes) override this to
    /// run the user's `init { ... }` body. Called once per
    /// <see cref="Spek.Runtime.ActorSystem"/> on first reader/writer
    /// access, before any handler holds the RW lock — but only if
    /// <see cref="TryRestore"/> returned <c>false</c> (i.e. there
    /// was no snapshot to restore from).
    /// </summary>
    protected virtual void Initialize() { }

    /// <summary>
    /// Disposal hook. Called once during
    /// <see cref="Spek.Runtime.ActorSystem"/> shutdown, after the
    /// system stops dispatching messages. Regions are disposed in
    /// reverse construction order — same model as C# `using` blocks
    /// unwinding. The compiler emits the user's `term { }` block as
    /// an override of this method; the default base implementation
    /// is a no-op so regions without a term block pay nothing.
    /// </summary>
    protected virtual void OnTerm() { }

    /// <summary>
    /// Internal accessor for <see cref="OnTerm"/>, used by
    /// <see cref="Spek.Runtime.ActorSystem.Dispose"/> to invoke the
    /// user's `term { }` body across the assembly boundary.
    /// </summary>
    internal void InvokeOnTerm() => OnTerm();

    /// <summary>
    /// Hook for capability subclasses (<see cref="PersistedRegion"/>;
    /// future event-sourced / replicated variants) to populate the
    /// region's fields before <see cref="Initialize"/> runs. Called
    /// inside the init lock, so the implementation runs alone — no
    /// reader or writer is concurrent. Return <c>true</c> if the
    /// region was populated from external state (init should be
    /// skipped); <c>false</c> if no state was found and init should
    /// run normally. The base implementation always returns false.
    /// </summary>
    protected virtual bool TryRestore() => false;

    private async Task EnsureInitializedAsync()
    {
        // Fast path: already initialised.
        if (Volatile.Read(ref _initState) == 2) return;

        TaskCompletionSource? localDone;
        bool weAreInitializer = false;
        lock (_gate)
        {
            if (_initState == 2) return;
            if (_initState == 0)
            {
                _initState = 1;
                _initDone = new(TaskCreationOptions.RunContinuationsAsynchronously);
                weAreInitializer = true;
            }
            localDone = _initDone;
        }

        if (weAreInitializer)
        {
            try
            {
                // Try restore first. If a snapshot exists,
                // it overrides the init body; matches the locked
                // "snapshot wins over init" decision (mirrors actor
                // persistence semantics).
                if (!TryRestore())
                {
                    Initialize();
                }
            }
            finally
            {
                lock (_gate) _initState = 2;
                localDone!.TrySetResult();
            }
        }
        else
        {
            await localDone!.Task.ConfigureAwait(false);
        }
    }

    /// <summary>
    /// Acquire a reader slot. Multiple readers run concurrently;
    /// blocks (asynchronously) only while a writer is in flight.
    /// Pair every call with exactly one <see cref="ExitReader"/>.
    /// </summary>
    public async Task EnterReaderAsync()
    {
        await EnsureInitializedAsync().ConfigureAwait(false);
        while (true)
        {
            Task wait;
            lock (_gate)
            {
                if (!_writerActive)
                {
                    if (_activeReaders == 0)
                        _readersDrained = new(
                            TaskCreationOptions.RunContinuationsAsynchronously);
                    _activeReaders++;
                    return;
                }
                wait = _writerReleased.Task;
            }
            await wait.ConfigureAwait(false);
        }
    }

    /// <summary>Release a reader slot. Pulses the readers-drained
    /// TCS when the count hits zero so a queued writer can proceed.</summary>
    public void ExitReader()
    {
        TaskCompletionSource? toPulse = null;
        lock (_gate)
        {
            _activeReaders--;
            if (_activeReaders == 0) toPulse = _readersDrained;
        }
        toPulse?.TrySetResult();
    }

    /// <summary>
    /// Acquire the writer slot. Blocks (asynchronously) until no
    /// other writer is in flight AND no readers remain. Once
    /// acquired, the writer runs alone — readers cannot enter until
    /// <see cref="ExitWriter"/> is called.
    /// </summary>
    public async Task EnterWriterAsync()
    {
        await EnsureInitializedAsync().ConfigureAwait(false);

        // Measure how long this writer waited for the
        // lock so dashboards can flag contention. The histogram only
        // emits when the wait was non-zero (writer actually blocked);
        // an immediate-acquire path emits zero, which OTel collectors
        // tend to render as a low-latency p50. Fully-qualify
        // `Stopwatch` and the global `System` namespace because the
        // inherited <see cref="System"/> property on SharedRegion
        // shadows the namespace within the class body.
        var swStart = global::System.Diagnostics.Stopwatch.GetTimestamp();
        try
        {
            while (true)
            {
                Task wait;
                lock (_gate)
                {
                    if (!_writerActive && _activeReaders == 0)
                    {
                        _writerActive = true;
                        _writerReleased = new(
                            TaskCreationOptions.RunContinuationsAsynchronously);
                        return;
                    }
                    wait = _writerActive ? _writerReleased.Task : _readersDrained.Task;
                }
                await wait.ConfigureAwait(false);
            }
        }
        finally
        {
            var elapsedMs = global::System.Diagnostics.Stopwatch
                .GetElapsedTime(swStart).TotalMilliseconds;
            this.System?.Metrics.Histogram(
                Spek.Observability.SpekMetricNames.RegionLockWaitMs,
                elapsedMs,
                tags: new[] { new KeyValuePair<string, object?>("region", Name) });
        }
    }

    /// <summary>Release the writer slot. Pulses the writer-released
    /// TCS so blocked readers and other writers can proceed.
    ///
    /// Virtual so capability subclasses can hook in
    /// (<see cref="PersistedRegion"/> kicks off a save here; future
    /// event-sourced regions append events here).</summary>
    public virtual void ExitWriter()
    {
        TaskCompletionSource toPulse;
        lock (_gate)
        {
            _writerActive = false;
            toPulse = _writerReleased;
        }
        toPulse.TrySetResult();
    }
}
