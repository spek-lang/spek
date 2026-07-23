using System.Runtime.Versioning;
using System.ServiceProcess;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Spek.Runtime;

namespace Spek.Hosting.WindowsService;

/// <summary>
/// Factory bundle handed to <see cref="SpekWindowsHostedService{TActor}"/>
/// to translate SCM control commands into user-defined Spek messages.
/// All factories except <see cref="ShutdownFactory"/> are optional —
/// if null, the corresponding SCM command is ignored. The actor's
/// channel declaration determines which inputs it actually accepts;
/// callers should provide factories matching that channel.
/// </summary>
public sealed class SpekWindowsServiceFactories
{
    /// <summary>
    /// Creates the user's Shutdown message, sent to the entry actor when
    /// SCM issues <c>SERVICE_CONTROL_STOP</c> (the host's
    /// <see cref="IHostedService.StopAsync"/> path). Required — every
    /// service must handle graceful shutdown.
    /// </summary>
    public required Func<object> ShutdownFactory { get; init; }

    /// <summary>Creates the user's Pause message for
    /// <c>SERVICE_CONTROL_PAUSE</c>. If null, the SCM pause command is
    /// ignored.</summary>
    public Func<object>? PauseFactory { get; init; }

    /// <summary>Creates the user's Continue message for
    /// <c>SERVICE_CONTROL_CONTINUE</c>, resuming after a pause. If null,
    /// the SCM continue command is ignored.</summary>
    public Func<object>? ContinueFactory { get; init; }

    /// <summary>Creates the user's PowerEvent message from the SCM-side
    /// event tag (e.g. "Suspend", "Resume", "BatteryLow") when SCM issues
    /// <c>SERVICE_CONTROL_POWEREVENT</c>. If null, power events are
    /// ignored.</summary>
    public Func<string, object>? PowerEventFactory { get; init; }

    /// <summary>Creates the user's SessionChange message from the SCM-side
    /// event tag (logon/logoff/lock/unlock) when SCM issues
    /// <c>SERVICE_CONTROL_SESSIONCHANGE</c>. If null, session changes are
    /// ignored.</summary>
    public Func<string, object>? SessionChangeFactory { get; init; }

    /// <summary>Creates the user's CustomCommand message from an SCM
    /// custom control code (128–255). If null, custom commands are
    /// ignored.</summary>
    public Func<int, object>? CustomCommandFactory { get; init; }
}

/// <summary>
/// Hosts a Spek actor inside Windows Service Control Manager. SCM
/// control commands routed in via <see cref="ServiceBase"/> override
/// hooks are translated into actor messages using the supplied
/// factories.
///
/// Designed to compose with <c>UseWindowsService()</c> on
/// <see cref="IHostBuilder"/> so it picks up SCM context from the
/// .NET Generic Host.
/// </summary>
[SupportedOSPlatform("windows")]
public sealed class SpekWindowsHostedService<TActor> : IHostedService, IAsyncDisposable
    where TActor : ActorBase
{
    private readonly SpekWindowsServiceFactories _factories;
    private readonly TimeSpan _shutdownGrace;
    private ActorSystem? _system;
    private ActorRef? _entry;
    private ActorRef? _receiver;

    /// <summary>
    /// Creates the hosted service. <paramref name="shutdownGrace"/> caps
    /// how long <see cref="StopAsync"/> waits for the entry actor to stop
    /// after receiving the Shutdown message (default 30s) — after that
    /// the host proceeds with teardown regardless.
    /// </summary>
    public SpekWindowsHostedService(
        SpekWindowsServiceFactories factories,
        TimeSpan? shutdownGrace = null)
    {
        _factories = factories ?? throw new ArgumentNullException(nameof(factories));
        _shutdownGrace = shutdownGrace ?? TimeSpan.FromSeconds(30);
    }

    /// <summary>
    /// Public so users can hand the actor's ref to a custom
    /// <see cref="ServiceBase"/> subclass (e.g. for handling control
    /// codes Spek doesn't translate by default).
    /// </summary>
    public ActorRef? Entry => _entry;

    /// <summary>
    /// Creates the <see cref="ActorSystem"/> and spawns the entry actor
    /// (<typeparamref name="TActor"/>), plus an internal no-op receiver
    /// used as the sender for host-originated messages. Called by the
    /// Generic Host once SCM has issued <c>SERVICE_START</c>.
    /// </summary>
    public Task StartAsync(CancellationToken cancellationToken)
    {
        _system   = new ActorSystem(typeof(TActor).FullName ?? typeof(TActor).Name);
        _entry    = _system.Spawn<TActor>();
        _receiver = _system.Spawn<NullReceiverActor>();
        return Task.CompletedTask;
    }

    /// <summary>
    /// Graceful-shutdown path for <c>SERVICE_CONTROL_STOP</c>: sends the
    /// message from <see cref="SpekWindowsServiceFactories.ShutdownFactory"/>
    /// to the entry actor, then waits for the actor to stop itself —
    /// giving up when the shutdown grace period elapses or
    /// <paramref name="cancellationToken"/> fires, whichever comes first.
    /// </summary>
    public async Task StopAsync(CancellationToken cancellationToken)
    {
        if (_entry is null) return;

        if (!_entry.IsStopped)
            _entry.Tell(_factories.ShutdownFactory(), sender: _receiver!);

        var deadline = DateTime.UtcNow + _shutdownGrace;
        while (!_entry.IsStopped && DateTime.UtcNow < deadline
               && !cancellationToken.IsCancellationRequested)
        {
            await Task.Delay(50, CancellationToken.None).ConfigureAwait(false);
        }
    }

    /// <summary>
    /// Send a Pause message to the entry actor. Called from the
    /// <see cref="ServiceBase.OnPause"/> hook when SCM issues
    /// <c>SERVICE_CONTROL_PAUSE</c>. No-op if no PauseFactory was
    /// supplied.
    /// </summary>
    public void Pause()
    {
        if (_entry is null || _factories.PauseFactory is null) return;
        _entry.Tell(_factories.PauseFactory(), sender: _receiver!);
    }

    /// <summary>Send a Continue message; mirror of <see cref="Pause"/>.</summary>
    public void Continue()
    {
        if (_entry is null || _factories.ContinueFactory is null) return;
        _entry.Tell(_factories.ContinueFactory(), sender: _receiver!);
    }

    /// <summary>Send a PowerEvent. <paramref name="eventName"/> is
    /// the SCM-side event tag (e.g. "Suspend", "Resume", "BatteryLow").</summary>
    public void PowerEvent(string eventName)
    {
        if (_entry is null || _factories.PowerEventFactory is null) return;
        _entry.Tell(_factories.PowerEventFactory(eventName), sender: _receiver!);
    }

    /// <summary>Send a SessionChange (logon/logoff/lock/unlock).</summary>
    public void SessionChange(string eventName)
    {
        if (_entry is null || _factories.SessionChangeFactory is null) return;
        _entry.Tell(_factories.SessionChangeFactory(eventName), sender: _receiver!);
    }

    /// <summary>Send a SCM custom control code (128–255).</summary>
    public void CustomCommand(int code)
    {
        if (_entry is null || _factories.CustomCommandFactory is null) return;
        _entry.Tell(_factories.CustomCommandFactory(code), sender: _receiver!);
    }

    /// <summary>Tears down the <see cref="ActorSystem"/> (and every actor
    /// in it). Runs after <see cref="StopAsync"/> — by then the entry
    /// actor has either stopped gracefully or exhausted its grace
    /// period.</summary>
    public ValueTask DisposeAsync()
    {
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

/// <summary>DI registration for the Windows Service host adapter.</summary>
[SupportedOSPlatform("windows")]
public static class SpekWindowsServiceCollectionExtensions
{
    /// <summary>
    /// Register a Spek actor as a Windows-Service-hosted
    /// <see cref="IHostedService"/>. Combine with
    /// <c>builder.Services.UseWindowsService()</c> from
    /// <c>Microsoft.Extensions.Hosting.WindowsServices</c> so the host
    /// detects SCM context and integrates with `sc.exe`.
    /// </summary>
    public static IServiceCollection AddSpekWindowsService<TActor>(
        this IServiceCollection services,
        Func<object> shutdownFactory,
        Func<object>? pauseFactory = null,
        Func<object>? continueFactory = null,
        Func<string, object>? powerEventFactory = null,
        Func<string, object>? sessionChangeFactory = null,
        Func<int, object>? customCommandFactory = null,
        TimeSpan? shutdownGrace = null)
        where TActor : ActorBase
    {
        ArgumentNullException.ThrowIfNull(services);
        ArgumentNullException.ThrowIfNull(shutdownFactory);

        var factories = new SpekWindowsServiceFactories
        {
            ShutdownFactory      = shutdownFactory,
            PauseFactory         = pauseFactory,
            ContinueFactory      = continueFactory,
            PowerEventFactory    = powerEventFactory,
            SessionChangeFactory = sessionChangeFactory,
            CustomCommandFactory = customCommandFactory,
        };

        var hostedService = new SpekWindowsHostedService<TActor>(factories, shutdownGrace);
        services.AddSingleton(hostedService);
        services.AddSingleton<IHostedService>(sp =>
            sp.GetRequiredService<SpekWindowsHostedService<TActor>>());
        return services;
    }
}
