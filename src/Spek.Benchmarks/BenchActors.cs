using Spek;
using Spek.Runtime;

namespace Spek.Benchmarks;

// Messages — plain records (a real Spek program would declare these as
// `message` types; the runtime doesn't care, it dispatches on object).
public sealed record Ping;
public sealed record Pong;

/// <summary>Counts messages; a trivial synchronous writer handler.</summary>
public sealed class CounterActor : ActorBase
{
    public long Count;
    protected override Task DispatchAsync(object message, ActorRef sender)
    {
        _currentSender = sender;
        Count++;
        return Task.CompletedTask;
    }
}

/// <summary>Replies <see cref="Pong"/> to the sender — the ask round-trip target.</summary>
public sealed class EchoActor : ActorBase
{
    protected override Task DispatchAsync(object message, ActorRef sender)
    {
        _currentSender = sender;
        sender.Tell(new Pong());
        return Task.CompletedTask;
    }
}

/// <summary>
/// Handler that performs a SYNCHRONOUS blocking wait — the anti-pattern.
/// Each message parks the actor's thread-pool pump thread for <see cref="BlockMs"/>,
/// so concurrent siblings can't make progress until the pool injects more
/// threads (~1–2/sec). CE0083 flags this in Spek source; the benchmark
/// quantifies the cost.
/// </summary>
public sealed class BlockingActor : ActorBase
{
    public static int BlockMs = 20;
    protected override Task DispatchAsync(object message, ActorRef sender)
    {
        _currentSender = sender;
        Thread.Sleep(BlockMs);          // blocks the pool thread
        sender.Tell(new Pong());
        return Task.CompletedTask;
    }
}

/// <summary>
/// The correct shape: the same wait modeled with <c>await Task.Delay</c>, which
/// releases the pool thread so siblings run. Directly comparable to
/// <see cref="BlockingActor"/>.
/// </summary>
public sealed class AsyncWaitActor : ActorBase
{
    public static int WaitMs = 20;
    protected override async Task DispatchAsync(object message, ActorRef sender)
    {
        _currentSender = sender;
        await Task.Delay(WaitMs).ConfigureAwait(false);   // yields the thread
        sender.Tell(new Pong());
    }
}
