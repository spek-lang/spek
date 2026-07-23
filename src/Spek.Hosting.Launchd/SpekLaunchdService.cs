using System.Runtime.InteropServices;
using System.Runtime.Versioning;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Spek.Runtime;

namespace Spek.Hosting.Launchd;

/// <summary>
/// Hosts a Spek actor under macOS launchd. Translates SIGTERM into
/// the user's Shutdown message and (when launchd's
/// <c>EnableTransactions</c> is set on the .plist) wraps message
/// processing in a vproc transaction so launchd doesn't idle-kill
/// the process.
///
/// launchd has no notify-back protocol equivalent to systemd's
/// sd_notify, so the channel surface is intentionally small —
/// <c>on Shutdown;</c> with no <c>emits</c>. Health is inferred from
/// the process exit code.
/// </summary>
[SupportedOSPlatform("macos")]
public sealed class SpekLaunchdHostedService<TActor> : IHostedService, IAsyncDisposable
    where TActor : ActorBase
{
    private readonly Func<object> _shutdownFactory;
    private readonly TimeSpan _shutdownGrace;
    private ActorSystem? _system;
    private ActorRef? _entry;
    private ActorRef? _receiver;
    private IDisposable? _vprocTransaction;

    /// <summary>
    /// Creates the hosted service. <paramref name="shutdownFactory"/>
    /// creates the user's Shutdown message for the SIGTERM path.
    /// <paramref name="shutdownGrace"/> caps how long
    /// <see cref="StopAsync"/> waits for the entry actor to stop after
    /// the Shutdown message; the default (20s) matches launchd's default
    /// <c>ExitTimeOut</c>, after which launchd escalates to SIGKILL.
    /// </summary>
    public SpekLaunchdHostedService(
        Func<object> shutdownFactory,
        TimeSpan? shutdownGrace = null)
    {
        _shutdownFactory = shutdownFactory ?? throw new ArgumentNullException(nameof(shutdownFactory));
        _shutdownGrace = shutdownGrace ?? TimeSpan.FromSeconds(20);   // launchd default ExitTimeOut
    }

    /// <summary>
    /// Creates the <see cref="ActorSystem"/>, spawns the entry actor
    /// (<typeparamref name="TActor"/>) plus an internal no-op receiver
    /// used as the sender for host-originated messages, and opens a
    /// vproc transaction so launchd treats the process as busy instead
    /// of idle-killing it (no-op outside launchd/EnableTransactions).
    /// </summary>
    public Task StartAsync(CancellationToken cancellationToken)
    {
        _system   = new ActorSystem(typeof(TActor).FullName ?? typeof(TActor).Name);
        _entry    = _system.Spawn<TActor>();
        _receiver = _system.Spawn<NullReceiverActor>();

        // Hold a vproc transaction open for as long as the host is
        // running. launchd treats us as "busy" and skips idle-kill.
        // No-op when the .plist doesn't have EnableTransactions, when
        // we're not running under launchd, or when libSystem doesn't
        // export vproc_transaction_begin.
        _vprocTransaction = VprocBridge.BeginTransaction();

        return Task.CompletedTask;
    }

    /// <summary>
    /// Graceful-shutdown path for SIGTERM (launchd's stop signal, routed
    /// here by the Generic Host): sends the user's Shutdown message to
    /// the entry actor, then waits for the actor to stop itself — giving
    /// up when the shutdown grace period elapses or
    /// <paramref name="cancellationToken"/> fires, whichever comes first.
    /// </summary>
    public async Task StopAsync(CancellationToken cancellationToken)
    {
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

    /// <summary>Closes the vproc transaction (telling launchd we're no
    /// longer busy) and tears down the <see cref="ActorSystem"/> (and
    /// every actor in it). Runs after <see cref="StopAsync"/> — by then
    /// the entry actor has either stopped gracefully or exhausted its
    /// grace period.</summary>
    public ValueTask DisposeAsync()
    {
        _vprocTransaction?.Dispose();
        _vprocTransaction = null;
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
/// Hand-rolled P/Invoke into <c>libSystem.dylib</c> for launchd's
/// vproc transaction API. No third-party dependency. Falls back to a
/// no-op when not running on macOS or when the symbol isn't exported
/// (sandboxed processes, older macOS).
/// </summary>
internal static class VprocBridge
{
    public static IDisposable? BeginTransaction()
    {
        if (!OperatingSystem.IsMacOS()) return null;
        // Only meaningful when launchd actually launched us — env var
        // XPC_SERVICE_NAME is set when running under launchd.
        if (Environment.GetEnvironmentVariable("XPC_SERVICE_NAME") is null) return null;

        try
        {
            var handle = vproc_transaction_begin(IntPtr.Zero);
            return handle == IntPtr.Zero ? null : new VprocTransaction(handle);
        }
        catch (DllNotFoundException) { return null; }
        catch (EntryPointNotFoundException) { return null; }
    }

    [DllImport("libSystem.dylib")]
    private static extern IntPtr vproc_transaction_begin(IntPtr vproc);

    [DllImport("libSystem.dylib")]
    private static extern void vproc_transaction_end(IntPtr vproc, IntPtr handle);

    private sealed class VprocTransaction : IDisposable
    {
        private IntPtr _handle;
        public VprocTransaction(IntPtr handle) => _handle = handle;

        public void Dispose()
        {
            if (_handle == IntPtr.Zero) return;
            try { vproc_transaction_end(IntPtr.Zero, _handle); }
            catch { /* swallow on shutdown */ }
            _handle = IntPtr.Zero;
        }
    }
}

/// <summary>DI registration helper for the launchd host adapter.</summary>
[SupportedOSPlatform("macos")]
public static class SpekLaunchdServiceCollectionExtensions
{
    /// <summary>
    /// Register a Spek actor as a launchd-hosted
    /// <see cref="IHostedService"/>.
    /// </summary>
    public static IServiceCollection AddSpekLaunchdService<TActor>(
        this IServiceCollection services,
        Func<object> shutdownFactory,
        TimeSpan? shutdownGrace = null)
        where TActor : ActorBase
    {
        ArgumentNullException.ThrowIfNull(services);
        ArgumentNullException.ThrowIfNull(shutdownFactory);

        services.AddSingleton<IHostedService>(_ =>
            new SpekLaunchdHostedService<TActor>(shutdownFactory, shutdownGrace));
        return services;
    }
}
