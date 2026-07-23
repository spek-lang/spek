using Spek.Cluster;

namespace Spek.Cluster.Memory;

/// <summary>
/// In-process implementation of <see cref="ISpekTransport"/>. Send-side
/// hands the envelope to the shared <see cref="InMemoryClusterFabric"/>;
/// receive-side stores the handler and lets the fabric invoke it
/// synchronously when a peer transport dispatches to this node.
///
/// No serialization, no sockets, no connection management — just a
/// dictionary lookup. Same code paths as a real wire transport (the
/// <see cref="ISpekTransport"/> contract is shared) so tests exercise
/// the same dispatching logic production hits.
/// </summary>
public sealed class MemoryTransport : ISpekTransport
{
    private readonly InMemoryClusterFabric _fabric;
    private Func<RemoteEnvelope, Task>? _handler;

    internal MemoryTransport(NodeIdentity localNode, InMemoryClusterFabric fabric)
    {
        LocalNode = localNode;
        _fabric   = fabric;
    }

    public NodeIdentity LocalNode { get; }

    public event Action<NodeIdentity, RemoteEnvelope, Exception>? DeliveryFailed;

    public Task SendAsync(NodeIdentity target, RemoteEnvelope envelope, CancellationToken cancellationToken = default)
    {
        try
        {
            return _fabric.DispatchAsync(target, envelope);
        }
        catch (Exception ex)
        {
            DeliveryFailed?.Invoke(target, envelope, ex);
            return Task.CompletedTask;
        }
    }

    public void SetReceiveHandler(Func<RemoteEnvelope, Task> handler) =>
        _handler = handler ?? throw new ArgumentNullException(nameof(handler));

    internal Task ReceiveAsync(RemoteEnvelope envelope)
    {
        if (_handler is null) return Task.CompletedTask;
        return _handler(envelope);
    }

    public ValueTask DisposeAsync()
    {
        _fabric.Unregister(LocalNode);
        return default;
    }
}
