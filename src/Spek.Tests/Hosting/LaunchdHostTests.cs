// The launchd host adapter is annotated [SupportedOSPlatform("macos")], but
// its only platform-specific piece — the vproc transaction P/Invoke —
// self-guards at runtime (null transaction unless actually launched by
// launchd on macOS). The lifecycle mapper under test is platform-neutral.
#pragma warning disable CA1416

using System.Collections.Concurrent;
using System.Diagnostics;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Spek.Hosting.Launchd;
using Spek.Runtime;
using Xunit;
using HostShutdown = Spek.Hosting.Shutdown;

namespace Spek.Tests.Hosting;

/// <summary>
/// In-process coverage for <see cref="SpekLaunchdHostedService{TActor}"/>.
/// launchd's stop signal arrives as the Generic Host's StopAsync; the
/// adapter must translate it into the shared <c>Spek.Hosting.Shutdown</c>
/// message and wait (bounded by the grace window) for the entry actor to
/// stop. Real launchd interaction (vproc transactions, SIGTERM routing)
/// needs the process to be launchd-spawned and stays untested here.
/// </summary>
public sealed class LaunchdHostTests
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

    /// <summary>Never stops — forces the bounded-grace path.</summary>
    private sealed class StubbornEntryActor : ActorBase
    {
        protected override Task DispatchAsync(object message, ActorRef sender)
            => Task.CompletedTask;
    }

    [Fact]
    public async Task StopAsync_DeliversSharedShutdownMessage_AndReturnsOnceTheActorStopsAsync()
    {
        while (LifecycleEntryActor.Received.TryDequeue(out _)) { }

        var service = new SpekLaunchdHostedService<LifecycleEntryActor>(
            shutdownFactory: () => new HostShutdown(),
            shutdownGrace: TimeSpan.FromSeconds(10));

        await service.StartAsync(CancellationToken.None);

        var sw = Stopwatch.StartNew();
        await service.StopAsync(CancellationToken.None);
        sw.Stop();

        Assert.True(sw.Elapsed < TimeSpan.FromSeconds(8),
            $"StopAsync should return promptly after the actor stops; took {sw.Elapsed}.");

        var message = Assert.Single(LifecycleEntryActor.Received);
        Assert.IsType<HostShutdown>(message);

        await service.DisposeAsync();
    }

    [Fact]
    public async Task StopAsync_GivesUp_WhenTheGraceWindowExpiresAsync()
    {
        var service = new SpekLaunchdHostedService<StubbornEntryActor>(
            shutdownFactory: () => new HostShutdown(),
            shutdownGrace: TimeSpan.FromMilliseconds(250));

        await service.StartAsync(CancellationToken.None);

        var stop = service.StopAsync(CancellationToken.None);
        await stop.WaitAsync(TimeSpan.FromSeconds(10));   // bounded, despite the live actor

        await service.DisposeAsync();
    }

    [Fact]
    public async Task StopAsync_BeforeStart_IsANoOpAsync()
    {
        var service = new SpekLaunchdHostedService<StubbornEntryActor>(
            shutdownFactory: () => new HostShutdown());

        await service.StopAsync(CancellationToken.None);
        await service.DisposeAsync();
    }

    [Fact]
    public async Task AddSpekLaunchdService_RegistersTheHostedServiceAsync()
    {
        var services = new ServiceCollection();

        var returned = services.AddSpekLaunchdService<StubbornEntryActor>(
            () => new HostShutdown());

        Assert.Same(services, returned);

        await using var provider = services.BuildServiceProvider();
        var hosted = provider.GetRequiredService<IHostedService>();
        Assert.IsType<SpekLaunchdHostedService<StubbornEntryActor>>(hosted);
    }

    [Fact]
    public void AddSpekLaunchdService_NullArguments_Throw()
    {
        var services = new ServiceCollection();

        Assert.Throws<ArgumentNullException>(
            () => SpekLaunchdServiceCollectionExtensions
                .AddSpekLaunchdService<StubbornEntryActor>(null!, () => new HostShutdown()));
        Assert.Throws<ArgumentNullException>(
            () => services.AddSpekLaunchdService<StubbornEntryActor>(null!));
    }

    [Fact]
    public void Constructor_NullShutdownFactory_Throws()
        => Assert.Throws<ArgumentNullException>(
            () => new SpekLaunchdHostedService<StubbornEntryActor>(null!));
}
