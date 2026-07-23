namespace Spek.Runtime;

/// <summary>
/// Plug-in contract between <see cref="ActorSystem"/> and the cluster layer
/// (typically <c>Spek.Cluster</c> + a concrete transport package). The
/// runtime stays remoting-agnostic — it knows there's "something" that
/// resolves remote refs and routes outbound traffic across the wire, but
/// the runtime doesn't know what protocol or even that there's a wire
/// (in-memory transports plug into the same hook for tests).
///
/// Concrete implementations live in <c>Spek.Cluster</c>.
/// </summary>
public interface IClusterAdapter
{
    /// <summary>
    /// Builds a remote-backed <see cref="ActorRef"/> pointing at the actor
    /// addressed by <paramref name="nodeLabel"/> and <paramref name="path"/>
    /// on the remote system. The returned ref's <c>Tell</c> routes through
    /// the cluster's transport layer.
    /// </summary>
    ActorRef ResolveRemote(string nodeLabel, string path);
}
