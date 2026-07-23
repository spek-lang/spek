using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Spek.Runtime;

namespace Spek.Hosting.AspNetCore;

/// <summary>
/// Adapter that hosts a Spek actor inside the .NET Generic Host
/// (<see cref="IHostedService"/>) lifecycle. The actor is spawned in
/// <see cref="StartAsync"/> and Tell-ed a user-supplied
/// <c>Shutdown</c> message in <see cref="StopAsync"/>; the host's
/// <see cref="IHostApplicationLifetime.ApplicationStopping"/> token
/// drives the same shutdown when triggered externally.
///
/// Spek actor exit codes flow into the Generic Host the same way a
/// regular `IHostedService` propagates errors — we don't currently
/// surface Option-D return values into <c>Environment.ExitCode</c>
/// because the Generic Host owns that decision via
/// <c>HostOptions.BackgroundServiceExceptionBehavior</c>.
/// </summary>
public sealed class SpekHostedService<TActor> : IHostedService, IAsyncDisposable
    where TActor : ActorBase
{
    private readonly Func<object> _shutdownFactory;
    private readonly TimeSpan _shutdownGrace;
    private ActorSystem? _system;
    private ActorRef? _entry;
    private ActorRef? _receiver;

    public SpekHostedService(Func<object> shutdownFactory, TimeSpan? shutdownGrace = null)
    {
        _shutdownFactory = shutdownFactory ?? throw new ArgumentNullException(nameof(shutdownFactory));
        _shutdownGrace   = shutdownGrace  ?? TimeSpan.FromSeconds(30);
    }

    public Task StartAsync(CancellationToken cancellationToken)
    {
        _system   = new ActorSystem(typeof(TActor).FullName ?? typeof(TActor).Name);
        _entry    = _system.Spawn<TActor>();
        _receiver = _system.Spawn<NullReceiverActor>();
        return Task.CompletedTask;
    }

    public async Task StopAsync(CancellationToken cancellationToken)
    {
        if (_entry is null || _system is null) return;

        // Tell with the receiver as sender so the actor's typed reply
        // doesn't go to dead letters. We don't propagate the exit code
        // out (Generic Host pattern), but capturing it cleanly avoids
        // dead-letter noise in logs.
        if (!_entry.IsStopped)
            _entry.Tell(_shutdownFactory(), sender: _receiver!);

        // Wait for the entry actor to stop or the grace window to
        // expire. The cancellation token already encodes
        // HostOptions.ShutdownTimeout.
        var deadline = DateTime.UtcNow + _shutdownGrace;
        while (!_entry.IsStopped && DateTime.UtcNow < deadline
               && !cancellationToken.IsCancellationRequested)
        {
            await Task.Delay(50, CancellationToken.None).ConfigureAwait(false);
        }
    }

    public ValueTask DisposeAsync()
    {
        _system?.Dispose();
        _system = null;
        return ValueTask.CompletedTask;
    }

    /// <summary>
    /// Receiver for the entry actor's Option-D reply. ASP.NET Core
    /// hosting doesn't surface the int return as Environment.ExitCode
    /// (the Generic Host owns that decision), so we just absorb it
    /// to keep replies out of the dead-letter sink.
    /// </summary>
    internal sealed class NullReceiverActor : ActorBase
    {
        protected override Task DispatchAsync(object message, ActorRef sender)
            => Task.CompletedTask;
    }
}

/// <summary>
/// DI registration helpers — the idiomatic way for users to wire a
/// Spek actor into <c>builder.Services</c>.
/// </summary>
public static class SpekHostingServiceCollectionExtensions
{
    /// <summary>
    /// Register a Spek actor as a singleton <see cref="IHostedService"/>.
    /// The actor is spawned at host startup and sent
    /// <paramref name="shutdownFactory"/>'s message at shutdown.
    /// </summary>
    public static IServiceCollection AddSpekHostedService<TActor>(
        this IServiceCollection services,
        Func<object> shutdownFactory,
        TimeSpan? shutdownGrace = null)
        where TActor : ActorBase
    {
        ArgumentNullException.ThrowIfNull(services);
        ArgumentNullException.ThrowIfNull(shutdownFactory);

        services.AddSingleton<IHostedService>(_ =>
            new SpekHostedService<TActor>(shutdownFactory, shutdownGrace));
        return services;
    }
}
