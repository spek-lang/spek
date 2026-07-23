using System.Collections.Concurrent;
using Spek.Runtime;

namespace Spek.Testing;

internal sealed class TestProbeActor : ActorBase
{
    private readonly BlockingCollection<(object Message, ActorRef Sender)> _received = new();

    internal bool TryTake(TimeSpan timeout, out (object Message, ActorRef Sender) item)
        => _received.TryTake(out item, timeout);

    protected override Task DispatchAsync(object message, ActorRef sender)
    {
        _received.Add((message, sender));
        return Task.CompletedTask;
    }
}
