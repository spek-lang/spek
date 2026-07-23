using System.Collections.Concurrent;
using Spek.Cluster;

namespace Spek.Cluster.Memory;

/// <summary>
/// Shared in-process registry that pairs <see cref="MemoryTransport"/>
/// instances. When a transport calls
/// <see cref="ISpekTransport.SendAsync"/> with a target node, the fabric
/// looks up that node's transport and hands the envelope to its receive
/// handler.
///
/// One fabric per "logical cluster" you're modelling in tests — different
/// fabrics are isolated.
/// </summary>
public sealed class InMemoryClusterFabric : IDisposable
{
    private readonly ConcurrentDictionary<Guid, MemoryTransport> _transports = new();
    private bool _disposed;

    /// <summary>
    /// Creates a transport instance bound to a fresh
    /// <see cref="NodeIdentity"/>. The label is optional and shows up in
    /// observability tools; the UUID is what nodes use on the wire.
    /// </summary>
    public ISpekTransport CreateTransport(string? label = null)
    {
        if (_disposed) throw new ObjectDisposedException(nameof(InMemoryClusterFabric));
        var node = new NodeIdentity(Guid.NewGuid(), label);
        var t = new MemoryTransport(node, this);
        _transports[node.Id] = t;
        return t;
    }

    internal Task DispatchAsync(NodeIdentity target, RemoteEnvelope envelope)
    {
        if (_disposed) return Task.CompletedTask;
        if (_transports.TryGetValue(target.Id, out var t))
            return t.ReceiveAsync(envelope);
        // Target not registered — silent dead-letter. A production
        // transport would surface this via DeliveryFailed; for the
        // in-memory transport (test target), letting it slide is the
        // simpler default. Tests that care about misrouted-message
        // detection can swap to a custom fabric subclass.
        return Task.CompletedTask;
    }

    internal void Unregister(NodeIdentity node) =>
        _transports.TryRemove(node.Id, out _);

    public void Dispose()
    {
        _disposed = true;
        _transports.Clear();
    }
}
