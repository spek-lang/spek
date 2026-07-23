using System.Collections.Concurrent;
using Spek.Cluster;
using Spek.Cluster.Memory;
using Xunit;

namespace Spek.Tests.ClusterIntegration;     // matches the sibling memory-transport tests

/// <summary>
/// Edge-path coverage for <see cref="MemoryTransport"/> +
/// <see cref="InMemoryClusterFabric"/> beyond the happy round-trips the
/// cluster integration tests exercise: unknown-node sends, disposed
/// transports and fabrics, missing receive handlers, and the
/// <c>DeliveryFailed</c> surface.
/// </summary>
public sealed class MemoryTransportEdgeCaseTests
{
    private sealed record Ping(string Tag);

    private static RemoteEnvelope Envelope(string tag = "x") =>
        new("target-actor", new Ping(tag));

    [Fact]
    public async Task SendAsync_ToUnknownNode_SilentlyDeadLetters_WithoutDeliveryFailedAsync()
    {
        using var fabric = new InMemoryClusterFabric();
        var a = fabric.CreateTransport("node-a");

        var failures = 0;
        a.DeliveryFailed += (_, _, _) => Interlocked.Increment(ref failures);

        var ghost = new NodeIdentity(Guid.NewGuid(), "never-registered");
        await a.SendAsync(ghost, Envelope());

        // Documented in-memory contract: unregistered targets dead-letter
        // silently rather than raising DeliveryFailed.
        Assert.Equal(0, failures);
    }

    [Fact]
    public async Task SendAsync_AfterTargetTransportDisposed_DropsSilentlyAsync()
    {
        using var fabric = new InMemoryClusterFabric();
        var a = fabric.CreateTransport("node-a");
        var b = fabric.CreateTransport("node-b");

        var delivered = new ConcurrentQueue<RemoteEnvelope>();
        b.SetReceiveHandler(env => { delivered.Enqueue(env); return Task.CompletedTask; });

        var failures = 0;
        a.DeliveryFailed += (_, _, _) => Interlocked.Increment(ref failures);

        var targetNode = b.LocalNode;
        await b.DisposeAsync();   // unregisters node-b from the fabric

        await a.SendAsync(targetNode, Envelope("after-dispose"));

        Assert.Empty(delivered);
        Assert.Equal(0, failures);
    }

    [Fact]
    public async Task SendAsync_TargetNeverRegisteredAHandler_CompletesQuietlyAsync()
    {
        using var fabric = new InMemoryClusterFabric();
        var a = fabric.CreateTransport("node-a");
        var b = fabric.CreateTransport("node-b");   // no SetReceiveHandler call

        // ReceiveAsync's null-handler branch: the envelope is dropped
        // without exception — a node that hasn't bound its cluster yet
        // must not blow up its peers.
        await a.SendAsync(b.LocalNode, Envelope("no-handler"));
    }

    [Fact]
    public void SetReceiveHandler_Null_ThrowsArgumentNull()
    {
        using var fabric = new InMemoryClusterFabric();
        var a = fabric.CreateTransport("node-a");

        Assert.Throws<ArgumentNullException>(() => a.SetReceiveHandler(null!));
    }

    [Fact]
    public async Task SendAsync_HandlerThrowsSynchronously_RaisesDeliveryFailedAndCompletesAsync()
    {
        using var fabric = new InMemoryClusterFabric();
        var a = fabric.CreateTransport("node-a");
        var b = fabric.CreateTransport("node-b");

        var boom = new InvalidOperationException("receive pipeline exploded");
        b.SetReceiveHandler(_ => throw boom);

        var failures = new ConcurrentQueue<(NodeIdentity Target, RemoteEnvelope Envelope, Exception Error)>();
        a.DeliveryFailed += (target, envelope, error) => failures.Enqueue((target, envelope, error));

        var envelope = Envelope("kaboom");
        await a.SendAsync(b.LocalNode, envelope);   // must not throw to the caller

        var failure = Assert.Single(failures);
        Assert.Equal(b.LocalNode, failure.Target);
        Assert.Same(envelope, failure.Envelope);
        Assert.Same(boom, failure.Error);
    }

    [Fact]
    public async Task SendAsync_HandlerFaultsAsynchronously_FaultRidesTheReturnedTaskAsync()
    {
        using var fabric = new InMemoryClusterFabric();
        var a = fabric.CreateTransport("node-a");
        var b = fabric.CreateTransport("node-b");

        b.SetReceiveHandler(_ => Task.FromException(new InvalidOperationException("async fault")));

        var failures = 0;
        a.DeliveryFailed += (_, _, _) => Interlocked.Increment(ref failures);

        // Current contract: only synchronous receive-side throws surface
        // via DeliveryFailed; an asynchronously faulted handler task
        // propagates to whoever awaits the send instead.
        await Assert.ThrowsAsync<InvalidOperationException>(
            () => a.SendAsync(b.LocalNode, Envelope("async-fault")));
        Assert.Equal(0, failures);
    }

    [Fact]
    public async Task ReceiveHandler_GetsTheExactEnvelopeInstance_NoCopyingAsync()
    {
        using var fabric = new InMemoryClusterFabric();
        var a = fabric.CreateTransport("node-a");
        var b = fabric.CreateTransport("node-b");

        var delivered = new ConcurrentQueue<RemoteEnvelope>();
        b.SetReceiveHandler(env => { delivered.Enqueue(env); return Task.CompletedTask; });

        var envelope = Envelope("identity");
        await a.SendAsync(b.LocalNode, envelope);

        var received = Assert.Single(delivered);
        Assert.Same(envelope, received);   // in-process pipe: no serialization
    }

    [Fact]
    public async Task DisposedFabric_SendsBecomeNoOps_AndCreateTransportThrowsAsync()
    {
        var fabric = new InMemoryClusterFabric();
        var a = fabric.CreateTransport("node-a");
        var b = fabric.CreateTransport("node-b");

        var delivered = new ConcurrentQueue<RemoteEnvelope>();
        b.SetReceiveHandler(env => { delivered.Enqueue(env); return Task.CompletedTask; });

        fabric.Dispose();

        await a.SendAsync(b.LocalNode, Envelope("post-dispose"));
        Assert.Empty(delivered);

        Assert.Throws<ObjectDisposedException>(() => fabric.CreateTransport("late"));
    }

    [Fact]
    public async Task DisposeAsync_IsIdempotentAsync()
    {
        using var fabric = new InMemoryClusterFabric();
        var a = fabric.CreateTransport("node-a");

        await a.DisposeAsync();
        await a.DisposeAsync();   // unregistering twice must be harmless
    }
}
