using System.Collections.Concurrent;
using Spek.Runtime;
using Xunit;

namespace Spek.Tests.Runtime;

/// <summary>
/// Actor-initiated graceful shutdown via <c>self.System.Shutdown()</c>.
/// The verb reaches the already-present <c>_system</c> back-reference (no
/// injection — the slot wires it at spawn) and calls the non-blocking
/// <see cref="ActorSystem.RequestShutdown"/>, which funnels through the host's
/// registered callback (the same path Ctrl+C / SIGTERM use) or, when hostless,
/// falls back to a background <see cref="ActorSystem.GracefulShutdown"/>.
/// </summary>
public class ActorInitiatedShutdownTests
{
    public record Boom();

    /// Mirrors what <c>self.System.Shutdown()</c> lowers to: the protected
    /// <c>SpekSystem</c> accessor on <see cref="ActorBase"/>, reached without
    /// any injected <see cref="ActorSystem"/>.
    private sealed class Watchdog : ActorBase
    {
        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            SpekSystem.Shutdown();
            return Task.CompletedTask;
        }
    }

    [Fact]
    public void RequestShutdown_InvokesRegisteredHandler_Once()
    {
        using var system = new ActorSystem("t");
        int calls = 0;
        system.OnShutdownRequested(() => Interlocked.Increment(ref calls));

        system.RequestShutdown();
        system.RequestShutdown();   // idempotent — only the first request fires

        Assert.Equal(1, calls);
    }

    [Fact]
    public async Task SelfSystemShutdown_FromHandler_TriggersRegisteredShutdown()
    {
        using var system = new ActorSystem("t");
        var fired = new TaskCompletionSource();
        system.OnShutdownRequested(() => fired.TrySetResult());

        var actor = system.Spawn<Watchdog>();   // Spawn wires _system → SpekSystem is live
        actor.Tell(new Boom());

        await Task.WhenAny(fired.Task, Task.Delay(TimeSpan.FromSeconds(5)));
        Assert.True(fired.Task.IsCompletedSuccessfully,
            "self.System.Shutdown() should trigger the registered shutdown handler");
    }

    [Fact]
    public void AwaitTermination_AfterDispose_ReturnsPromptly()
    {
        // Regression: once the system is torn down — which is what the
        // hostless self.System.Shutdown() path ends in (background
        // GracefulShutdown → Dispose) — AwaitTermination must recognise the
        // terminal state and return true. Pre-fix it could spin forever: the
        // `_slots.Count > 0` idle guard can never be satisfied by an empty,
        // disposed slot set, so "all actors finished and were cleaned up" was
        // indistinguishable from "never returns".
        var system = new ActorSystem("t");
        system.Spawn<Watchdog>();   // slots existed → this is "disposed", not "never started"
        system.Dispose();           // clears _slots, marks terminal

        var sw = System.Diagnostics.Stopwatch.StartNew();
        bool terminated = system.AwaitTermination(TimeSpan.FromSeconds(5));
        sw.Stop();

        Assert.True(terminated, "AwaitTermination() must return true once the system is disposed, not hang");
        Assert.True(sw.ElapsedMilliseconds < 2000, "it should return promptly, not wait out the timeout");
    }

    [Fact]
    public async Task SelfSystemShutdown_Hostless_LetsAwaitTerminationReturn()
    {
        // End-to-end of the footgun: a handler calls self.System.Shutdown() with
        // no host registered, while the "main thread" is parked in the no-arg
        // AwaitTermination(). The background GracefulShutdown drains then disposes
        // the slots; the parked AwaitTermination must still return rather than
        // hang on the now-empty slot set.
        var system = new ActorSystem("t");                 // hostless → background GracefulShutdown fallback
        var actor  = system.Spawn<Watchdog>();
        actor.Tell(new Boom());                            // handler calls SpekSystem.Shutdown()

        var termination = Task.Run(() => system.AwaitTermination());   // no-arg: blocks until terminal
        var winner = await Task.WhenAny(termination, Task.Delay(TimeSpan.FromSeconds(5)));

        Assert.True(winner == termination,
            "no-arg AwaitTermination() must return after a handler-initiated shutdown, not hang");
        Assert.True(termination.Result, "AwaitTermination() should report clean termination");
    }

    /// One link in a 10-deep stack. A message propagates *down* the stack
    /// (each link forwards to the one below); the leaf (no child) triggers
    /// `self.System.Shutdown()`. Every link records its graceful stop hook.
    private sealed class StackNode : ActorBase
    {
        private readonly int _depth;
        private readonly ActorRef? _next;
        private readonly ConcurrentDictionary<int, byte> _stopped;

        public StackNode(int depth, ActorRef? next, ConcurrentDictionary<int, byte> stopped)
        {
            _depth = depth; _next = next; _stopped = stopped;
        }

        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            if (_next is not null) _next.Tell(message);   // forward down the stack
            else SpekSystem.Shutdown();                   // leaf triggers shutdown
            return Task.CompletedTask;
        }

        // The graceful-stop hook — must fire for every actor on a clean shutdown.
        protected override void OnPostStop() => _stopped[_depth] = 1;
    }

    [Fact]
    public async Task TenStackedActors_LeafInitiatesShutdown_AllStopGracefully()
    {
        // 10 actors stacked in a chain. A Boom propagates root → leaf; the leaf
        // calls self.System.Shutdown(). Every one of the 10 must (a) stop —
        // OnPostStop fires — and (b) the system must terminate (AwaitTermination
        // returns). This is the deep-topology stress of the shutdown fix.
        var stopped = new ConcurrentDictionary<int, byte>();
        var system  = new ActorSystem("stack");

        ActorRef? next = null;
        for (int depth = 9; depth >= 0; depth--)          // build leaf-first so each link gets its child ref
            next = system.Spawn<StackNode>(depth, next, stopped);
        var root = next!;                                  // depth 0

        root.Tell(new Boom());                             // propagates down → leaf shuts the system down

        // Wait on the graceful-stop completion itself — every actor's OnPostStop
        // adds its depth to `stopped`. (AwaitTermination can't be the wait signal
        // here: it returns on *transient* idle, which races the still-propagating
        // Boom before shutdown has even been triggered.)
        var deadline = DateTime.UtcNow + TimeSpan.FromSeconds(10);
        while (stopped.Count < 10 && DateTime.UtcNow < deadline)
            await Task.Delay(25);

        Assert.Equal(10, stopped.Count); // ← every actor stopped gracefully (OnPostStop ran)
        Assert.True(system.AwaitTermination(TimeSpan.FromSeconds(5)),
            "system must be terminated after the leaf-initiated shutdown");
    }

    [Fact]
    public void RequestShutdown_NoHandler_IsNonBlocking()
    {
        // With no host callback, RequestShutdown falls back to a background
        // GracefulShutdown — it must NOT drain on the caller's thread (the
        // calling handler is itself keeping the system busy, so an inline
        // GracefulShutdown would deadlock waiting for idle). The contract under
        // test is that the call returns promptly and doesn't throw.
        var system = new ActorSystem("t");   // no slots: the fallback drains immediately
        var sw = System.Diagnostics.Stopwatch.StartNew();

        system.RequestShutdown();

        sw.Stop();
        Assert.True(sw.ElapsedMilliseconds < 1000,
            "hostless RequestShutdown must be non-blocking (drain runs on a background thread)");
    }
}
