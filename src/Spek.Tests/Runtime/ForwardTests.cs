using Spek;
using Spek.Runtime;
using Xunit;

namespace Spek.Tests.Runtime;

/// <summary>
/// Coverage for <see cref="ActorRef.Forward{T}"/> — the helper
/// that bridges C# events into an actor's mailbox without lambda
/// boilerplate.
/// </summary>
public sealed class ForwardTests
{
    /// <summary>Toy event source the test attaches to.</summary>
    private sealed class TickSource
    {
        public event EventHandler<TickArgs>? Tick;
        public void Fire(int n) => Tick?.Invoke(this, new TickArgs(n));
    }

    public sealed record TickArgs(int N);

    /// <summary>
    /// Captures every TickArgs the actor receives. Held statically so
    /// tests can assert on it without internal access to ActorRef.
    /// </summary>
    private sealed class CapturingActor : ActorBase
    {
        public static readonly List<TickArgs> Received = new();
        private static readonly object Gate = new();

        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            if (message is TickArgs t)
                lock (Gate) Received.Add(t);
            return Task.CompletedTask;
        }
    }

    [Fact]
    public async Task Forward_routes_event_payload_to_actor_via_Tell()
    {
        CapturingActor.Received.Clear();
        using var system = new ActorSystem("forward-1");
        var actor = system.Spawn<CapturingActor>();

        var source = new TickSource();
        source.Tick += actor.Forward<TickArgs>();

        source.Fire(1);
        source.Fire(2);
        source.Fire(3);

        var deadline = DateTime.UtcNow + TimeSpan.FromSeconds(2);
        while (CapturingActor.Received.Count < 3 && DateTime.UtcNow < deadline)
            await Task.Delay(20);

        Assert.Equal(new[] { 1, 2, 3 }, CapturingActor.Received.Select(t => t.N).ToArray());
    }

    [Fact]
    public void Forward_returns_a_distinct_handler_each_call()
    {
        // Each invocation builds a fresh delegate so callers can wire
        // the same actor to multiple sources without one += clobbering
        // another.
        using var system = new ActorSystem("forward-2");
        var actor = system.Spawn<CapturingActor>();

        var h1 = actor.Forward<TickArgs>();
        var h2 = actor.Forward<TickArgs>();

        Assert.NotSame(h1, h2);
    }

    [Fact]
    public async Task Forward_unsubscribes_cleanly_on_minus_assign()
    {
        // Standard EventHandler<T> idiom — capturing the handler ref
        // on attach lets the user detach it cleanly. Verifies the
        // returned delegate is the real registered handler, not a
        // wrapper that breaks identity.
        CapturingActor.Received.Clear();
        using var system = new ActorSystem("forward-3");
        var actor = system.Spawn<CapturingActor>();

        var source = new TickSource();
        var handler = actor.Forward<TickArgs>();
        source.Tick += handler;

        source.Fire(10);

        var deadline = DateTime.UtcNow + TimeSpan.FromSeconds(2);
        while (CapturingActor.Received.Count < 1 && DateTime.UtcNow < deadline)
            await Task.Delay(20);

        source.Tick -= handler;
        source.Fire(99);   // should NOT be received

        await Task.Delay(100);

        Assert.Single(CapturingActor.Received);
        Assert.Equal(10, CapturingActor.Received[0].N);
    }
}
