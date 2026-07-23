// The systemd host adapter is annotated [SupportedOSPlatform("linux")], but
// its only platform-specific piece — the sd_notify P/Invoke — self-guards at
// runtime (no-op off Linux). The lifecycle mapper under test (host StopAsync →
// user Shutdown message → wait-for-actor-stop) is platform-neutral, so these
// tests run on any OS without touching a real init system.
#pragma warning disable CA1416

using System.Collections.Concurrent;
using System.Diagnostics;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Spek.Hosting.Systemd;
using Spek.Runtime;
using Xunit;
using HostReload = Spek.Hosting.Reload;
using HostShutdown = Spek.Hosting.Shutdown;

namespace Spek.Tests.Hosting;

/// <summary>
/// In-process coverage for <see cref="SpekSystemdHostedService{TActor}"/>:
/// the shared lifecycle plumbing every service host reuses. The graceful
/// stop path must deliver the <c>Spek.Hosting.Shutdown</c> message (the
/// shared vocabulary from Spek.Hosting.Abstractions) to the entry actor
/// and wait for it to stop; the grace window and the host cancellation
/// token both bound that wait. sd_notify emission needs a real systemd
/// socket and stays untested here.
/// </summary>
public sealed class SystemdHostTests
{
    /// <summary>Records the lifecycle messages it receives; stops on Shutdown.</summary>
    private sealed class LifecycleEntryActor : ActorBase
    {
        public static readonly ConcurrentQueue<object> Received = new();

        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            Received.Enqueue(message);
            if (message is HostShutdown) StopSelf();
            return Task.CompletedTask;
        }
    }

    /// <summary>Never stops — forces the grace/cancellation paths.</summary>
    private sealed class StubbornEntryActor : ActorBase
    {
        protected override Task DispatchAsync(object message, ActorRef sender)
            => Task.CompletedTask;
    }

    [Fact]
    public async Task StopAsync_DeliversSharedShutdownMessage_AndReturnsOnceTheActorStopsAsync()
    {
        while (LifecycleEntryActor.Received.TryDequeue(out _)) { }

        var service = new SpekSystemdHostedService<LifecycleEntryActor>(
            shutdownFactory: () => new HostShutdown(),
            reloadFactory: () => new HostReload(),   // registers the SIGHUP → Reload seam
            shutdownGrace: TimeSpan.FromSeconds(10));

        await service.StartAsync(CancellationToken.None);

        var sw = Stopwatch.StartNew();
        await service.StopAsync(CancellationToken.None);
        sw.Stop();

        // Returned because the actor stopped, not because the grace ran out.
        Assert.True(sw.Elapsed < TimeSpan.FromSeconds(8),
            $"StopAsync should return promptly after the actor stops; took {sw.Elapsed}.");

        var message = Assert.Single(LifecycleEntryActor.Received);
        Assert.IsType<HostShutdown>(message);

        await service.DisposeAsync();
    }

    [Fact]
    public async Task StopAsync_GivesUp_WhenTheGraceWindowExpiresAsync()
    {
        var service = new SpekSystemdHostedService<StubbornEntryActor>(
            shutdownFactory: () => new HostShutdown(),
            shutdownGrace: TimeSpan.FromMilliseconds(250));

        await service.StartAsync(CancellationToken.None);

        var stop = service.StopAsync(CancellationToken.None);
        await stop.WaitAsync(TimeSpan.FromSeconds(10));   // bounded, despite the live actor

        await service.DisposeAsync();
    }

    [Fact]
    public async Task StopAsync_HostCancellationToken_CutsTheWaitShortAsync()
    {
        var service = new SpekSystemdHostedService<StubbornEntryActor>(
            shutdownFactory: () => new HostShutdown(),
            shutdownGrace: TimeSpan.FromSeconds(30));

        await service.StartAsync(CancellationToken.None);

        using var cts = new CancellationTokenSource(TimeSpan.FromMilliseconds(200));
        var sw = Stopwatch.StartNew();
        await service.StopAsync(cts.Token);
        sw.Stop();

        Assert.True(sw.Elapsed < TimeSpan.FromSeconds(10),
            $"Cancelled StopAsync must not wait out the 30s grace; took {sw.Elapsed}.");

        await service.DisposeAsync();
    }

    [Fact]
    public async Task StopAsync_BeforeStart_IsANoOpAsync()
    {
        var service = new SpekSystemdHostedService<StubbornEntryActor>(
            shutdownFactory: () => new HostShutdown());

        await service.StopAsync(CancellationToken.None);   // no entry actor yet
        await service.DisposeAsync();                       // nothing to tear down
    }

    [Fact]
    public async Task AddSpekSystemdService_RegistersTheHostedServiceAsync()
    {
        var services = new ServiceCollection();

        var returned = services.AddSpekSystemdService<StubbornEntryActor>(
            () => new HostShutdown());

        Assert.Same(services, returned);

        await using var provider = services.BuildServiceProvider();
        var hosted = provider.GetRequiredService<IHostedService>();
        Assert.IsType<SpekSystemdHostedService<StubbornEntryActor>>(hosted);
    }

    [Fact]
    public void AddSpekSystemdService_NullArguments_Throw()
    {
        var services = new ServiceCollection();

        Assert.Throws<ArgumentNullException>(
            () => SpekSystemdServiceCollectionExtensions
                .AddSpekSystemdService<StubbornEntryActor>(null!, () => new HostShutdown()));
        Assert.Throws<ArgumentNullException>(
            () => services.AddSpekSystemdService<StubbornEntryActor>(null!));
    }

    [Fact]
    public void Constructor_NullShutdownFactory_Throws()
        => Assert.Throws<ArgumentNullException>(
            () => new SpekSystemdHostedService<StubbornEntryActor>(null!));
}
