using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Spek.Hosting.AspNetCore;
using Spek.Runtime;
using Xunit;

namespace Spek.Tests.Hosting;

/// <summary>
/// Integration coverage for <see cref="SpekHostedService{TActor}"/>.
/// Stands up a Generic Host with a Spek actor wired via
/// <see cref="SpekHostingServiceCollectionExtensions.AddSpekHostedService{TActor}"/>,
/// runs through StartAsync/StopAsync, and verifies the actor saw its
/// shutdown message.
/// </summary>
public class AspNetCoreHostTests
{
    public sealed record Shutdown();

    private static readonly object Gate = new();
    private static volatile bool _shutdownReceived;

    private sealed class WorkerActor : ActorBase
    {
        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            if (message is Shutdown)
            {
                lock (Gate) _shutdownReceived = true;
                _currentSender.Tell(0);     // Option D reply
                StopSelf();
            }
            return Task.CompletedTask;
        }
    }

    [Fact]
    public async Task GenericHost_StartsAndStops_ActorReceivesShutdown()
    {
        lock (Gate) _shutdownReceived = false;

        var builder = Host.CreateApplicationBuilder();
        builder.Services.AddSpekHostedService<WorkerActor>(
            shutdownFactory: () => new Shutdown(),
            shutdownGrace:   TimeSpan.FromSeconds(2));

        using var host = builder.Build();
        await host.StartAsync();

        // Trigger graceful shutdown — the IHostedService.StopAsync
        // call is what the host invokes when ApplicationStopping fires.
        await host.StopAsync(CancellationToken.None);

        // Give the actor's dispatch loop a moment.
        await Task.Delay(100);

        Assert.True(_shutdownReceived,
            "Worker actor should have received its Shutdown message during StopAsync.");
    }

    [Fact]
    public async Task ShutdownGrace_BoundsTheStopWait()
    {
        // An actor that ignores Shutdown — the host should still exit
        // when the grace window expires, not hang forever.
        lock (Gate) _shutdownReceived = false;

        var builder = Host.CreateApplicationBuilder();
        builder.Services.AddSpekHostedService<UnresponsiveActor>(
            shutdownFactory: () => new Shutdown(),
            shutdownGrace:   TimeSpan.FromMilliseconds(200));

        using var host = builder.Build();
        await host.StartAsync();

        var sw = System.Diagnostics.Stopwatch.StartNew();
        await host.StopAsync(CancellationToken.None);
        sw.Stop();

        // StopAsync must RETURN (the 200ms grace loop bounds the wait on the
        // unresponsive actor) rather than deadlocking — if the grace were
        // broken the await would hang until xUnit kills the run. The bound is
        // deliberately huge (60s, longer than the whole suite) only so that
        // heavy parallel load can't trip it: under CPU saturation StopAsync's
        // `await Task.Delay(50)` continuation can be delayed by seconds, which
        // is fine — it still exits on the wall-clock grace deadline. We're
        // asserting "bounded, not hung", not a tight latency.
        Assert.True(sw.Elapsed < TimeSpan.FromSeconds(60),
            $"Expected StopAsync to return via the grace loop, but it took {sw.Elapsed}.");
    }

    private sealed class UnresponsiveActor : ActorBase
    {
        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            // Deliberately don't StopSelf or reply — exercises grace bound.
            return Task.CompletedTask;
        }
    }
}
