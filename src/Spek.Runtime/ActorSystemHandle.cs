using Spek.Runtime;

namespace Spek;

/// <summary>
/// The narrow, actor-reachable handle to the running node, returned by
/// the <c>self.System</c> accessor on every actor. Deliberately tiny: it does
/// <i>not</i> expose <c>Spawn</c>, <c>Dispose</c>, or the rest of
/// <see cref="ActorSystem"/> — a handler gets only the node-lifecycle verbs
/// that are safe to call from inside the actor loop.
/// </summary>
public sealed class ActorSystemHandle
{
    private readonly ActorSystem? _system;

    internal ActorSystemHandle(ActorSystem? system) => _system = system;

    /// <summary>
    /// Begins a graceful shutdown of this node: every actor drains its mailbox,
    /// each <c>on Shutdown</c> handler and shared-region <c>term {}</c> block
    /// runs, then the host exits. <b>Non-blocking</b> and safe to call from
    /// inside a handler — it signals the shutdown and returns, so the current
    /// handler completes first and the drain proceeds afterward.
    ///
    /// <para>This is the supported replacement for <c>Environment.Exit</c> /
    /// <c>Process.Kill</c> (which are <c>CE0084</c> errors): those sever every
    /// handler mid-message and bypass supervision; this winds the node down
    /// cleanly.</para>
    /// </summary>
    public void Shutdown() => _system?.RequestShutdown();
}
