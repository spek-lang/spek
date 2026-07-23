// FailureDirective is named in .spek source via
// `supervise OneForOne(on Failure: Restart)` (the keyword maps
// to enum members), so it lives in the bare `Spek` namespace.
namespace Spek;

/// <summary>
/// What the runtime should do when an actor's message handler throws.
///
/// <list type="bullet">
///   <item><see cref="Resume"/> — skip the current message and continue with
///     the next. State is unchanged, observers see no outward effect.</item>
///   <item><see cref="Restart"/> — persist the current state (if the actor is
///     persistent), discard the crashed instance, build a fresh one via the
///     slot's factory, and continue processing pending mailbox items. For
///     persistent actors, the new instance's <c>OnRestore</c> fires from the
///     latest snapshot before the next message is dispatched.</item>
///   <item><see cref="Escalate"/> — delegate the decision to the parent
///     actor. The parent's <c>OnChildFailure</c> receives the child's
///     <see cref="ActorRef"/>, the cause, and the message, and returns a
///     directive to apply to the child. Root actors have no parent;
///     escalating at root degrades to <see cref="Stop"/> with a dead-letter
///     entry explaining why.</item>
///   <item><see cref="Stop"/> — drain the mailbox into the dead-letter sink,
///     call <c>OnPostStop</c>, and reject further messages. Default.</item>
/// </list>
/// </summary>
public enum FailureDirective
{
    /// <summary>Skip the failed message and continue with the next; state is unchanged.</summary>
    Resume,

    /// <summary>Discard the crashed instance and rebuild a fresh one (restoring from snapshot if persistent), then continue processing.</summary>
    Restart,

    /// <summary>Delegate the decision to the parent actor's <see cref="ActorBase.OnChildFailure"/>; degrades to <see cref="Stop"/> at the root.</summary>
    Escalate,

    /// <summary>Drain the mailbox to dead-letters, run <c>OnPostStop</c>, and reject further messages. The default directive.</summary>
    Stop,
}
