using Spek.Runtime;
using Spek.Testing;
using Xunit;

namespace Spek.Tests.Runtime;

/// <summary>
/// Coverage for runtime public-API surface and dead-letter paths not
/// exercised by the existing Runtime tests:
/// <list type="bullet">
///   <item>Named-root registration / lookup / reverse-lookup
///         (<see cref="ActorSystem.SpawnNamed{TActor}"/>,
///         <see cref="ActorSystem.ResolveNamed"/>,
///         <see cref="ActorSystem.PathOfNamedRoot"/>).</item>
///   <item><see cref="ActorSystem.DeliverIncoming"/> — the cluster-layer
///         inbound hook — routing to a known root and dead-lettering an
///         unknown one.</item>
///   <item>Type-guard on the non-generic spawn overloads
///         (<see cref="ActorSystem.Spawn(System.Type, object[])"/>).</item>
///   <item>The default <see cref="ActorBase"/> <c>Unhandled</c> dead-letter.</item>
///   <item>Ask edge cases: wrong reply type → <see cref="System.InvalidCastException"/>;
///         ask to a stopped target → timeout; ask to a throwing writer
///         handler → timeout (the asker is NOT failed eagerly, unlike the
///         reader path — documents the writer/reader asymmetry).</item>
///   <item>The depth-8 escalation cap with a chain deeper than 8 all-escalating
///         supervisors (the existing escalation tests only cover the
///         "ran out of supervisors at the root" path, explicitly NOT the cap).</item>
/// </list>
/// </summary>
public class RuntimeCoverageTests
{
    // ─── messages ────────────────────────────────────────────────────────────
    public record Ping();
    public record Pong();
    public record Boom();
    public record Crash();
    public record Echo(int N);

    // ─── helper actors ─────────────────────────────────────────────────────────

    /// <summary>Replies to Ping with Pong; otherwise routes to Unhandled.</summary>
    private sealed class PingPong : ActorBase
    {
        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            _currentSender = sender;
            switch (message)
            {
                case Ping: sender.Tell(new Pong()); break;
                default:   Unhandled(message); break;   // default → dead-letter
            }
            return Task.CompletedTask;
        }
    }

    /// <summary>Always throws — a root actor (no parent) defaults to Stop.</summary>
    private sealed class Exploder : ActorBase
    {
        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            _currentSender = sender;
            throw new InvalidOperationException("boom");
        }
    }

    /// <summary>
    /// Writer handler that throws while the asker awaits a reply. The writer
    /// dispatch path runs supervision but does NOT call FailReplyWith, so the
    /// asker's task is left pending — only the timeout rescues it. Contrast
    /// with the reader path (covered in ReaderConcurrencyTests) which faults
    /// the asker eagerly with AskException.
    /// </summary>
    private sealed class WriterThatThrows : ActorBase
    {
        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            _currentSender = sender;
            throw new InvalidOperationException("writer boom");
        }
    }

    /// <summary>Replies to Ping with the WRONG type (Echo, not Pong).</summary>
    private sealed class WrongTypeReplier : ActorBase
    {
        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            _currentSender = sender;
            sender.Tell(new Echo(1));   // asker awaits Pong → InvalidCastException
            return Task.CompletedTask;
        }
    }

    /// <summary>A plain C# class that is NOT an actor — used to test the type guard.</summary>
    private sealed class NotAnActor { }

    // ─── Named roots ───────────────────────────────────────────────────────────

    [Fact]
    public void SpawnNamed_RegistersForLookup_AndReverseLookup()
    {
        using var system = new ActorSystem("named");

        var actor = system.SpawnNamed<PingPong>("ingress/orders");

        // Forward lookup returns the same ref.
        Assert.Same(actor, system.ResolveNamed("ingress/orders"));
        // Reverse lookup returns the registered path.
        Assert.Equal("ingress/orders", system.PathOfNamedRoot(actor));
    }

    [Fact]
    public void ResolveNamed_UnknownPath_ReturnsNull()
    {
        using var system = new ActorSystem("named");
        system.SpawnNamed<PingPong>("known");

        Assert.Null(system.ResolveNamed("does/not/exist"));
    }

    [Fact]
    public void PathOfNamedRoot_AnonymousActor_ReturnsNull()
    {
        using var system = new ActorSystem("named");
        var anonymous = system.Spawn<PingPong>();   // not registered under a name

        Assert.Null(system.PathOfNamedRoot(anonymous));
    }

    [Fact]
    public void SpawnNamed_SamePathAgain_ReverseLookupTracksCurrentHolder()
    {
        // Re-registering a path rebinds it to the new actor, and the
        // DISPLACED actor stops reverse-resolving — it is no longer a named
        // root, so the cluster layer must not stamp its old path into wire
        // envelopes as a reply-to address that now routes to the usurper.
        // Pins that the ref → path map (perf r16) unbinds in lockstep with
        // the path → ref map it mirrors, preserving the reverse-scan-era
        // behavior.
        using var system = new ActorSystem("named");

        var first  = system.SpawnNamed<PingPong>("ingress/orders");
        var second = system.SpawnNamed<PingPong>("ingress/orders");

        Assert.Same(second, system.ResolveNamed("ingress/orders"));
        Assert.Equal("ingress/orders", system.PathOfNamedRoot(second));
        Assert.Null(system.PathOfNamedRoot(first));
    }

    [Fact]
    public void SpawnNamed_EmptyPath_Throws()
    {
        using var system = new ActorSystem("named");

        Assert.Throws<ArgumentException>(() => system.SpawnNamed<PingPong>(""));
    }

    // ─── DeliverIncoming (cluster inbound hook) ──────────────────────────────────

    // Observable across threads — Spek.Tests is NOT in Spek.Runtime's
    // InternalsVisibleTo list, so ActorRef.Underlying is off-limits; a static
    // counter is the established cross-thread signal for these tests.
    private static int _pongsCollected;

    /// <summary>Reply sink: counts the Pongs it receives via its sender path.</summary>
    private sealed class Collector : ActorBase
    {
        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            _currentSender = sender;
            if (message is Pong) Interlocked.Increment(ref _pongsCollected);
            return Task.CompletedTask;
        }
    }

    [Fact]
    public async Task DeliverIncoming_KnownRoot_RoutesMessageToActorAsync()
    {
        Interlocked.Exchange(ref _pongsCollected, 0);
        using var system = new ActorSystem("deliver");

        // Register a named root whose Ping reply routes back to the sender.
        system.SpawnNamed<PingPong>("orders");
        // A reply sink that records the Pong it receives as "sender".
        var replySink = system.Spawn<Collector>();

        // Simulate an inbound wire envelope: target the named path, with the
        // collector as the (would-be remote) reply target. The named root's
        // Ping handler replies to that sender, so the Pong lands on Collector.
        system.DeliverIncoming("orders", new Ping(), replySink);

        // Generous ceiling: the Ping→Pong→Collector round-trip is two thread-pool
        // dispatches, which can be starved for several seconds under the fully
        // parallel suite. It returns as soon as the Pong lands (ms in the common
        // case); the long timeout only matters under heavy contention.
        await WaitUntilAsync(() => Volatile.Read(ref _pongsCollected) == 1, timeoutMs: 30_000);
        Assert.Equal(1, Volatile.Read(ref _pongsCollected));
    }

    [Fact]
    public void DeliverIncoming_UnknownRoot_DeadLetters()
    {
        var sink = new RecordingDeadLetterSink();
        using var system = new ActorSystem("deliver", deadLetterSink: sink);

        system.DeliverIncoming("no/such/path", new Ping(), sender: null);

        var record = Assert.Single(sink.Records);
        Assert.IsType<Ping>(record.Message);
        Assert.Contains("no named root", record.Reason);
        Assert.Contains("no/such/path", record.Reason);
        Assert.Null(record.Cause);
    }

    // ─── Type guard on non-generic spawn ─────────────────────────────────────────

    [Fact]
    public void Spawn_NonActorType_ThrowsArgumentException()
    {
        using var system = new ActorSystem("guard");

        var ex = Assert.Throws<ArgumentException>(() => system.Spawn(typeof(NotAnActor)));
        Assert.Contains(nameof(ActorBase), ex.Message);
    }

    [Fact]
    public void SpawnNamed_NonActorType_ThrowsArgumentException()
    {
        using var system = new ActorSystem("guard");

        Assert.Throws<ArgumentException>(() => system.SpawnNamed(typeof(NotAnActor), "path"));
    }

    // ─── Default Unhandled → dead-letter ─────────────────────────────────────────

    [Fact]
    public async Task UnhandledMessage_RoutesToDeadLetter_WithReasonAsync()
    {
        var sink = new RecordingDeadLetterSink();
        using var system = new ActorSystem("unhandled", deadLetterSink: sink);
        var actor = system.Spawn<PingPong>();

        actor.Tell(new Boom());   // PingPong has no Boom arm → Unhandled

        await WaitUntilAsync(() => sink.Records.Any(r => r.Message is Boom));

        var record = sink.Records.Single(r => r.Message is Boom);
        Assert.Contains("no handler matched", record.Reason);
        Assert.Contains(nameof(PingPong), record.Reason);
        Assert.Null(record.Cause);   // unhandled, not a thrown exception
    }

    // ─── Tell to a stopped actor → dead-letter ───────────────────────────────────

    [Fact]
    public async Task TellToStoppedActor_DeadLetters_WithStoppedReasonAsync()
    {
        var sink = new RecordingDeadLetterSink();
        using var system = new ActorSystem("stopped", deadLetterSink: sink);
        var actor = system.Spawn<Exploder>();

        actor.Tell(new Ping());   // throws → root actor defaults to Stop
        await WaitUntilAsync(() => actor.IsStopped);

        // Subsequent sends hit the stopped-actor dead-letter path in Enqueue.
        actor.Tell(new Ping());

        await WaitUntilAsync(() => sink.Records.Any(r =>
            r.Reason.Contains("target actor is stopped", StringComparison.Ordinal)));

        var stoppedRecord = sink.Records.Last(r =>
            r.Reason.Contains("target actor is stopped", StringComparison.Ordinal));
        Assert.IsType<Ping>(stoppedRecord.Message);
        Assert.Null(stoppedRecord.Cause);
    }

    // ─── Ask edge cases ──────────────────────────────────────────────────────────

    [Fact]
    public async Task AskWithTimeout_WrongReplyType_FaultsWithInvalidCastAsync()
    {
        using var system = new ActorSystem("ask-wrong");
        var actor = system.Spawn<WrongTypeReplier>();

        // The reply (Echo) can't satisfy the awaited Pong; the internal
        // ReplyActor<Pong> sets the asker's task to an InvalidCastException
        // — the asker faults rather than waiting out the timeout.
        await Assert.ThrowsAsync<InvalidCastException>(() =>
            actor.AskAsync<Pong>(new Ping(), TimeSpan.FromSeconds(5)).AsTask());
    }

    [Fact]
    public async Task AskWithTimeout_StoppedTarget_FaultsFastWithAskExceptionAsync()
    {
        using var system = new ActorSystem("ask-stopped");
        var actor = system.Spawn<Exploder>();

        actor.Tell(new Ping());          // crash → root stops
        await WaitUntilAsync(() => actor.IsStopped);

        // Asking a stopped actor can never produce a reply, so under the
        // "fail fast on all" semantics the send fails the asker immediately
        // with an AskException ("target actor is stopped") — no dead-letter
        // wait, no deadline. A generous timeout proves the failure is fast.
        await Assert.ThrowsAsync<AskException>(() =>
            actor.AskAsync<Pong>(new Ping(), TimeSpan.FromSeconds(5)).AsTask());
    }

    [Fact]
    public async Task AskWithTimeout_WriterHandlerThrows_FailsFastWithAskExceptionAsync()
    {
        using var system = new ActorSystem("ask-writer-throws");
        var actor = system.Spawn<WriterThatThrows>();

        // A writer handler that throws now fails the asker fast with an
        // AskException, symmetric with the reader path — closing the old
        // writer/reader asymmetry that left the asker to time out (red-team
        // V1, "fail fast on all"). The handler's own throw rides along as
        // the inner exception. A generous timeout proves the failure is fast.
        var ex = await Assert.ThrowsAsync<AskException>(() =>
            actor.AskAsync<Pong>(new Ping(), TimeSpan.FromSeconds(5)).AsTask());

        Assert.IsType<InvalidOperationException>(ex.InnerException);
        Assert.Equal("writer boom", ex.InnerException!.Message);
    }

    // ─── ActorRef surface: NoSender reply + remote-ask guard ──────────────────────

    [Fact]
    public async Task ReplyToActorWithNoSender_DoesNotThrow_AndIsRecoverableAsync()
    {
        // A bare Tell (no explicit sender) gives the recipient ActorRef.NoSender
        // as its sender. A handler that replies to NoSender must not crash the
        // actor — the reply is dropped (dead-lettered to stderr). We prove the
        // actor stays alive and keeps serving by following up with an ask.
        using var system = new ActorSystem("nosender");
        var actor = system.Spawn<RepliesToSender>();

        actor.Tell(new Ping());   // sender is NoSender; the inner reply is dropped

        // Actor is still healthy: an explicit ask gets a real reply.
        var reply = await actor.AskAsync<Pong>(new Ping(), TimeSpan.FromSeconds(5));
        Assert.IsType<Pong>(reply);
        Assert.False(actor.IsStopped);
    }

    private sealed class RepliesToSender : ActorBase
    {
        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            _currentSender = sender;
            sender.Tell(new Pong());   // when sender == NoSender this is dropped, not thrown
            return Task.CompletedTask;
        }
    }

    [Fact]
    public void AskAsync_AgainstRemoteRef_ThrowsNotSupported()
    {
        var endpoint = new DeadEndpoint();
        var remote = new ActorRef(endpoint);

        // Remote ask is deferred — must fail fast (the throw happens
        // synchronously, before any Task is produced), not silently hang.
        Assert.Throws<NotSupportedException>(() =>
        {
            _ = remote.AskAsync<Pong>(new Ping());
        });
    }

    [Fact]
    public void RemoteRef_IsStopped_TracksEndpointDeath()
    {
        var endpoint = new DeadEndpoint { Dead = false };
        var remote = new ActorRef(endpoint);

        Assert.True(remote.IsRemote);
        Assert.False(remote.IsStopped);

        endpoint.Dead = true;
        Assert.True(remote.IsStopped);   // IsKnownDead surfaces through IsStopped
    }

    /// <summary>Minimal remote endpoint that records dispatches and reports death on demand.</summary>
    private sealed class DeadEndpoint : IRemoteEndpoint
    {
        public bool Dead { get; set; }
        public int Dispatches { get; private set; }
        public void Dispatch(ActorRef self, object message, ActorRef? sender) => Dispatches++;
        public bool IsKnownDead => Dead;
    }

    // ─── Escalation depth-8 cap (chain deeper than 8) ────────────────────────────

    public record SpawnDeeper(int Remaining);
    public record GetLeaf();
    public record LeafRef(ActorRef Ref);

    /// <summary>
    /// Self-replicating supervisor: on <see cref="SpawnDeeper"/> it spawns a
    /// child of its own type one level shallower, building a chain. Every level
    /// returns Escalate from OnChildFailure, so a crash at the leaf walks the
    /// whole chain. With a chain depth > 8, ResolveEscalation's depth-8 cap
    /// fires before it runs out of supervisors at the root.
    /// </summary>
    private sealed class ChainNode : ActorBase
    {
        private ActorRef? _child;

        protected override async Task DispatchAsync(object message, ActorRef sender)
        {
            _currentSender = sender;
            switch (message)
            {
                case SpawnDeeper d when d.Remaining > 0:
                    _child = SpawnChildAsync<ChainNode>();
                    _child.Tell(new SpawnDeeper(d.Remaining - 1));
                    break;
                case GetLeaf:
                    // Drill down: the deepest node (no child) is the leaf.
                    if (_child is null) { sender.Tell(new LeafRef(_selfRef)); break; }
                    var leaf = await _child.AskAsync<LeafRef>(new GetLeaf(), TimeSpan.FromSeconds(10));
                    sender.Tell(leaf);
                    break;
                case Crash:
                    throw new InvalidOperationException("chain leaf boom");
            }
        }

        protected override FailureDirective OnChildFailure(ActorRef child, Exception cause, object message)
            => FailureDirective.Escalate;
    }

    [Fact]
    public async Task Escalation_ChainDeeperThanCap_DegradesToStop_WithMaxDepthReasonAsync()
    {
        var sink = new RecordingDeadLetterSink();
        using var system = new TestActorSystem("escalate-cap", deadLetterSink: sink);
        var probe = system.CreateProbe();

        // Build a chain of 12 nodes (root + 11 descendants). The leaf has 11
        // escalating ancestors — more than the depth-8 cap, so the walk hits
        // the cap and degrades to Stop rather than reaching a null supervisor.
        var root = system.Spawn<ChainNode>();
        root.Tell(new SpawnDeeper(11));

        probe.Send(root, new GetLeaf());
        var leafRef = probe.ExpectMsg<LeafRef>(TimeSpan.FromSeconds(20)).Ref;

        leafRef.Tell(new Crash());

        await WaitUntilAsync(() => sink.Records.Any(r =>
            r.Reason.Contains("escalation chain exceeded max depth", StringComparison.Ordinal)));

        var capRecord = sink.Records.First(r =>
            r.Reason.Contains("escalation chain exceeded max depth", StringComparison.Ordinal));
        Assert.IsType<InvalidOperationException>(capRecord.Cause);

        // The cap degrades to Stop — the leaf ends up stopped.
        await WaitUntilAsync(() => leafRef.IsStopped);
        Assert.True(leafRef.IsStopped);

        // And the "ran out of supervisors" path was NOT taken — the cap fired first.
        Assert.DoesNotContain(sink.Records, r =>
            r.Reason.Contains("escalate at root", StringComparison.Ordinal));
    }

    // ─── helpers ─────────────────────────────────────────────────────────────────

    // 30s default ceiling: these tests poll for async runtime effects (message
    // delivery, dead-letter routing) that are two+ thread-pool dispatches and can
    // be starved for seconds under the fully parallel suite. The predicate returns
    // as soon as it's satisfied (ms in the common case); the ceiling only bites
    // under heavy contention, where a tighter bound produced load flakes.
    private static async Task WaitUntilAsync(Func<bool> predicate, int timeoutMs = 30_000)
    {
        var deadline = DateTime.UtcNow.AddMilliseconds(timeoutMs);
        while (DateTime.UtcNow < deadline)
        {
            if (predicate()) return;
            await Task.Delay(25);
        }
        throw new TimeoutException($"Condition not met within {timeoutMs}ms.");
    }
}
