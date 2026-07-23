using System.Runtime.InteropServices;
using System.Runtime.Versioning;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Microsoft.Extensions.Hosting.Systemd;
using Spek.Runtime;

namespace Spek.Hosting.Systemd;

/// <summary>
/// Hosts a Spek actor as a Linux systemd service. Translates SIGTERM
/// → user's Shutdown message and SIGHUP → user's Reload message;
/// emits sd_notify events back to systemd when the actor produces
/// the corresponding output messages on its <c>LinuxServiceHost</c>
/// channel.
///
/// Composes with <see cref="SystemdHostBuilderExtensions.UseSystemd(IHostBuilder)"/>
/// from <c>Microsoft.Extensions.Hosting.Systemd</c> for journald
/// formatting + activation detection.
/// </summary>
[SupportedOSPlatform("linux")]
public sealed class SpekSystemdHostedService<TActor> : IHostedService, IAsyncDisposable
    where TActor : ActorBase
{
    private readonly Func<object> _shutdownFactory;
    private readonly Func<object>? _reloadFactory;
    private readonly TimeSpan _shutdownGrace;
    private ActorSystem? _system;
    private ActorRef? _entry;
    private ActorRef? _receiver;
    private PosixSignalRegistration? _sighup;
    private readonly IHostApplicationLifetime? _lifetime;

    /// <summary>
    /// Creates the hosted service. <paramref name="shutdownFactory"/>
    /// creates the user's Shutdown message for the SIGTERM path;
    /// <paramref name="reloadFactory"/>, if supplied, creates the user's
    /// Reload message for SIGHUP (when null, SIGHUP keeps its default
    /// behavior). <paramref name="shutdownGrace"/> caps how long
    /// <see cref="StopAsync"/> waits for the entry actor to stop after
    /// the Shutdown message (default 30s).
    /// </summary>
    public SpekSystemdHostedService(
        Func<object> shutdownFactory,
        Func<object>? reloadFactory = null,
        TimeSpan? shutdownGrace = null,
        IHostApplicationLifetime? lifetime = null)
    {
        _shutdownFactory = shutdownFactory ?? throw new ArgumentNullException(nameof(shutdownFactory));
        _reloadFactory   = reloadFactory;
        _shutdownGrace   = shutdownGrace ?? TimeSpan.FromSeconds(30);
        _lifetime        = lifetime;
    }

    /// <summary>
    /// Creates the <see cref="ActorSystem"/>, spawns the entry actor
    /// (<typeparamref name="TActor"/>) plus an internal no-op receiver
    /// used as the sender for host-originated messages, registers the
    /// SIGHUP → Reload handler (only when a reload factory was supplied),
    /// and notifies systemd of readiness via sd_notify <c>READY=1</c>.
    /// </summary>
    public Task StartAsync(CancellationToken cancellationToken)
    {
        _system   = new ActorSystem(typeof(TActor).FullName ?? typeof(TActor).Name);
        _entry    = _system.Spawn<TActor>();
        _receiver = _system.Spawn<NullReceiverActor>();

        // SIGHUP → Reload. SIGTERM is handled by the Generic Host's
        // ConsoleLifetime / SystemdLifetime via the StopAsync path,
        // so we don't double-register it here.
        if (_reloadFactory is not null)
        {
            _sighup = PosixSignalRegistration.Create(PosixSignal.SIGHUP, ctx =>
            {
                ctx.Cancel = true;     // veto default kill
                if (_entry is { IsStopped: false })
                    _entry.Tell(_reloadFactory(), sender: _receiver!);
            });
        }

        // sd_notify READY=1 — but only when the WHOLE host is up.
        // StartAsync runs per hosted service in registration order, so
        // notifying here would mark the unit ready while sibling services
        // (Kestrel, background workers) are still starting; Type=notify
        // orchestration would route traffic too early. ApplicationStarted
        // fires after every hosted service's StartAsync completes. The
        // direct send remains as the fallback when no lifetime was
        // available (hand-constructed service, no DI).
        if (_lifetime is not null)
            _lifetime.ApplicationStarted.Register(static () => SdNotify.Send("READY=1"));
        else
            SdNotify.Send("READY=1");
        return Task.CompletedTask;
    }

    /// <summary>
    /// Graceful-shutdown path for SIGTERM (routed here by the Generic
    /// Host's SystemdLifetime): notifies systemd via sd_notify
    /// <c>STOPPING=1</c>, sends the user's Shutdown message to the entry
    /// actor, then waits for the actor to stop itself — giving up when
    /// the shutdown grace period elapses or
    /// <paramref name="cancellationToken"/> fires, whichever comes first.
    /// </summary>
    public async Task StopAsync(CancellationToken cancellationToken)
    {
        SdNotify.Send("STOPPING=1");

        if (_entry is null) return;
        if (!_entry.IsStopped)
            _entry.Tell(_shutdownFactory(), sender: _receiver!);

        var deadline = DateTime.UtcNow + _shutdownGrace;
        while (!_entry.IsStopped && DateTime.UtcNow < deadline
               && !cancellationToken.IsCancellationRequested)
        {
            await Task.Delay(50, CancellationToken.None).ConfigureAwait(false);
        }
    }

    /// <summary>Unregisters the SIGHUP handler and tears down the
    /// <see cref="ActorSystem"/> (and every actor in it). Runs after
    /// <see cref="StopAsync"/> — by then the entry actor has either
    /// stopped gracefully or exhausted its grace period.</summary>
    public ValueTask DisposeAsync()
    {
        _sighup?.Dispose();
        _system?.Dispose();
        _system = null;
        return ValueTask.CompletedTask;
    }

    internal sealed class NullReceiverActor : ActorBase
    {
        protected override Task DispatchAsync(object message, ActorRef sender)
            => Task.CompletedTask;
    }
}

/// <summary>
/// Minimal sd_notify P/Invoke wrapper. Sends a state string to the
/// socket addressed by the <c>NOTIFY_SOCKET</c> environment variable.
/// No-op when not running under systemd. No third-party dependency —
/// this is just a hand-rolled call into <c>libsystemd.so.0</c>.
/// </summary>
internal static class SdNotify
{
    public static void Send(string state)
    {
        if (!OperatingSystem.IsLinux()) return;
        if (!SystemdHelpers.IsSystemdService()) return;

        try
        {
            sd_notify(unset_environment: 0, state: state);
        }
        catch (DllNotFoundException)
        {
            // libsystemd isn't on the box — common in containers.
            // Treat as no-op rather than crashing the host.
        }
    }

    [DllImport("libsystemd.so.0", EntryPoint = "sd_notify")]
    private static extern int sd_notify(int unset_environment, string state);
}

/// <summary>DI registration helper for the systemd host adapter.</summary>
[SupportedOSPlatform("linux")]
public static class SpekSystemdServiceCollectionExtensions
{
    /// <summary>
    /// Register a Spek actor as a systemd-hosted
    /// <see cref="IHostedService"/>. Combine with
    /// <c>builder.Services.UseSystemd()</c> for journald-format
    /// logging and `Type=notify` activation detection.
    /// </summary>
    public static IServiceCollection AddSpekSystemdService<TActor>(
        this IServiceCollection services,
        Func<object> shutdownFactory,
        Func<object>? reloadFactory = null,
        TimeSpan? shutdownGrace = null)
        where TActor : ActorBase
    {
        ArgumentNullException.ThrowIfNull(services);
        ArgumentNullException.ThrowIfNull(shutdownFactory);

        services.AddSingleton<IHostedService>(sp =>
            new SpekSystemdHostedService<TActor>(
                shutdownFactory, reloadFactory, shutdownGrace,
                sp.GetService<IHostApplicationLifetime>()));
        return services;
    }
}
