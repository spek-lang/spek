using System.Runtime.InteropServices;
using Spek.Runtime;

namespace Spek.Hosting.Console;

/// <summary>
/// Cross-platform console host adapter. Bootstraps an
/// <see cref="ActorSystem"/>, spawns a designated entry actor, and
/// translates terminal lifecycle events
/// (Ctrl+C / Ctrl+Break / SIGINT / SIGTERM / SIGHUP / SIGQUIT) into
/// <c>Shutdown</c> messages sent to that actor.
///
/// Process exit code comes from the actor's <c>on Shutdown =&gt;
/// return &lt;int&gt;;</c> handler (Spek's Option D reply convention).
/// If the handler has no return, the exit code defaults to <c>0</c>.
///
/// The adapter is intentionally minimal — it provides the signal
/// translation and process-lifetime plumbing; channel-coverage and
/// emits enforcement happen at compile time in the Spek source.
/// </summary>
public static class SpekConsoleHost
{
    /// <summary>
    /// Runs <typeparamref name="TActor"/> as a console host. Blocks until
    /// the actor stops (typically after handling its <c>Shutdown</c>
    /// message). Returns the exit code the handler produced, or
    /// <paramref name="defaultExitCode"/> if the handler had no return.
    /// </summary>
    /// <param name="shutdownFactory">
    /// Factory that produces the user-defined <c>Shutdown</c> message
    /// instance. The adapter doesn't know the user's type — it just
    /// asks for an instance whenever a terminal signal arrives.
    /// </param>
    /// <param name="systemName">
    /// Name passed to the underlying <see cref="ActorSystem"/>. Defaults
    /// to <c>"console-host"</c>.
    /// </param>
    /// <param name="shutdownGrace">
    /// Maximum time to wait for the actor to stop after the first
    /// shutdown signal. Hard-exits if the actor refuses to stop.
    /// Default 30 seconds (matches .NET Generic Host).
    /// </param>
    /// <param name="defaultExitCode">
    /// Returned when the actor stops without surfacing a typed exit
    /// code via Option D reply. Default <c>0</c>.
    /// </param>
    public static Task<int> RunAsync<TActor>(
        Func<object> shutdownFactory,
        string systemName = "console-host",
        TimeSpan? shutdownGrace = null,
        int defaultExitCode = 0)
        where TActor : ActorBase
    {
        ArgumentNullException.ThrowIfNull(shutdownFactory);

        var system = new ActorSystem(systemName);
        var entry  = system.Spawn<TActor>();
        return RunAsync(system, entry, shutdownFactory, shutdownGrace, defaultExitCode,
                        ownsSystem: true);
    }

    /// <summary>
    /// Overload for callers that want to bring their own
    /// <see cref="ActorSystem"/> + entry <see cref="ActorRef"/>
    /// (e.g. for spawning persistent actors via <c>SpawnPersistent</c>).
    /// The host does NOT take ownership of the system in this overload —
    /// disposal is the caller's responsibility.
    /// </summary>
    public static Task<int> RunAsync(
        ActorSystem system,
        ActorRef entryActor,
        Func<object> shutdownFactory,
        TimeSpan? shutdownGrace = null,
        int defaultExitCode = 0)
        => RunAsync(system, entryActor, shutdownFactory, shutdownGrace, defaultExitCode,
                    ownsSystem: false);

    private static async Task<int> RunAsync(
        ActorSystem system,
        ActorRef entryActor,
        Func<object> shutdownFactory,
        TimeSpan? shutdownGrace,
        int defaultExitCode,
        bool ownsSystem)
    {
        ArgumentNullException.ThrowIfNull(system);
        ArgumentNullException.ThrowIfNull(entryActor);
        ArgumentNullException.ThrowIfNull(shutdownFactory);

        // Track the actor's typed reply (Option D return value) so we
        // can surface it as the process exit code. We spawn a tiny
        // receiver actor whose ref we use as `sender` when Tell-ing
        // the Shutdown — the entry actor's `_currentSender.Tell(reply)`
        // routes back to it.
        var exitHolder    = new ExitCodeHolder(defaultExitCode);
        var receiverRef   = system.Spawn<ExitCodeReceiverActor>(exitHolder);

        var shutdownRequested = 0;
        void RequestShutdown()
        {
            // Ensure we only Tell once even if multiple signals fire
            // (Ctrl+C followed by SIGTERM, etc.).
            if (Interlocked.Exchange(ref shutdownRequested, 1) != 0) return;

            // Tell the entry actor with the receiver as the apparent
            // sender, so the entry actor's `_currentSender.Tell(reply)`
            // (Option D return-statement emit) routes back here.
            entryActor.Tell(shutdownFactory(), sender: receiverRef);
        }

        // Actor-initiated shutdown (`self.System.Shutdown()`) funnels through
        // the SAME path as Ctrl+C / SIGTERM: the entry actor gets its Shutdown
        // message, cleanup runs, the wait-loop below sees it stop, and we exit.
        system.OnShutdownRequested(RequestShutdown);

        // Cross-platform POSIX signals. PosixSignalRegistration is
        // .NET 6+ and translates each platform's native signal
        // delivery (Win32 console events on Windows, sigaction on
        // Unix) into a uniform callback.
        using var sigterm = PosixSignalRegistration.Create(PosixSignal.SIGTERM, ctx =>
        {
            ctx.Cancel = true;     // veto the default kill; we'll exit on our own terms
            RequestShutdown();
        });
        using var sigint  = PosixSignalRegistration.Create(PosixSignal.SIGINT,  ctx =>
        {
            ctx.Cancel = true;
            RequestShutdown();
        });
        using var sighup  = PosixSignalRegistration.Create(PosixSignal.SIGHUP,  ctx =>
        {
            ctx.Cancel = true;
            RequestShutdown();
        });
        using var sigquit = PosixSignalRegistration.Create(PosixSignal.SIGQUIT, ctx =>
        {
            ctx.Cancel = true;
            RequestShutdown();
        });

        // Belt-and-suspenders for environments where Console.CancelKeyPress
        // fires but the POSIX SIGINT registration didn't (older Windows
        // terminals, attached debuggers).
        ConsoleCancelEventHandler cancelHandler = (_, e) =>
        {
            e.Cancel = true;
            RequestShutdown();
        };
        System.Console.CancelKeyPress += cancelHandler;

        try
        {
            // Wait for the actor to stop. The actor stops itself via
            // `Stop()` after handling Shutdown, OR the supervision
            // tree decides to stop it.
            var grace = shutdownGrace ?? TimeSpan.FromSeconds(30);
            DateTime? hardDeadline = null;
            while (!entryActor.IsStopped)
            {
                await Task.Delay(50).ConfigureAwait(false);

                // Once Shutdown was requested, switch to a bounded grace
                // window. If the actor hasn't stopped within that, we
                // hard-exit so the process doesn't hang.
                if (shutdownRequested == 1 && hardDeadline is null)
                    hardDeadline = DateTime.UtcNow + grace;
                if (hardDeadline is { } deadline && DateTime.UtcNow > deadline)
                    break;
            }

            return exitHolder.Value;
        }
        finally
        {
            System.Console.CancelKeyPress -= cancelHandler;
            if (ownsSystem) system.Dispose();
        }
    }

    /// <summary>
    /// Tiny holder that lets the receiver actor and the host coordinate
    /// over a single integer (the eventual process exit code). Volatile
    /// reads/writes — only the receiver actor writes; only the host
    /// reads after the entry actor stops.
    /// </summary>
    internal sealed class ExitCodeHolder
    {
        private int _value;
        public ExitCodeHolder(int initial) => _value = initial;
        public int Value
        {
            get => Volatile.Read(ref _value);
            set => Volatile.Write(ref _value, value);
        }
    }

    /// <summary>
    /// Internal actor that captures the entry actor's Option-D reply
    /// (`return new T();` → `_currentSender.Tell(T)`) and pulls an
    /// integer out of it as the process exit code.
    /// </summary>
    internal sealed class ExitCodeReceiverActor : ActorBase
    {
        private readonly ExitCodeHolder _holder;

        public ExitCodeReceiverActor(ExitCodeHolder holder) => _holder = holder;

        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            if (message is int code) _holder.Value = code;
            return Task.CompletedTask;
        }
    }
}
