namespace Spek.Runtime;

/// <summary>
/// A point-in-time, read-only view of one actor for live introspection
/// (<see cref="ActorSystem.SnapshotActors"/>). Metadata only: behavior
/// name, queue shape, and lifecycle facts the runtime already owns.
/// Actor field contents are deliberately not exposed — a state dump
/// leaks whatever the actor holds and needs a redaction story first.
/// </summary>
/// <param name="Path">Stable display identity — the persistence key when one exists, otherwise <c>TypeName</c> / <c>TypeName#N</c>.</param>
/// <param name="ActorType">The actor's type name; "(unmaterialized)" for a passivated or not-yet-materialized slot.</param>
/// <param name="Behavior">The active behavior's name; null when the actor is unmaterialized or hand-written without behaviors.</param>
/// <param name="MailboxDepth">Messages pending in the mailbox right now.</param>
/// <param name="MailboxHead">Type names of the first few pending messages (up to 8), in queue order.</param>
/// <param name="Restarts">Supervision restarts observed (own and sibling-caused).</param>
/// <param name="LastMessageType">Type name of the message most recently dispatched; null before the first dispatch.</param>
/// <param name="SpawnedAt">When the slot was created, on the system clock.</param>
/// <param name="IsMaterialized">False when passivated or not yet materialized.</param>
/// <param name="IsStopped">True when the actor has stopped (voluntary or supervised).</param>
/// <param name="Children">Display identities of child actors, for tree rendering.</param>
/// <param name="DispatchedCount">Total messages processed by this actor — the liveness counter.</param>
public sealed record ActorSnapshot(
    string Path,
    string ActorType,
    string? Behavior,
    int MailboxDepth,
    string[] MailboxHead,
    int Restarts,
    string? LastMessageType,
    DateTimeOffset SpawnedAt,
    bool IsMaterialized,
    bool IsStopped,
    string[] Children,
    long DispatchedCount = 0);   // total messages processed — the liveness counter
                                 // (defaulted so older positional constructions compile)
