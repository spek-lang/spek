namespace Spek.Resilience;

/// <summary>
/// Per-evaluation context handed to a resilience policy. Carries the
/// bare minimum a policy needs to decide — actor identity, channel
/// name, the message itself, and a wall-clock timestamp.
///
/// Read-only. Policies that need mutable per-call state should track
/// it internally (most policies are stateful and shared across many
/// evaluations).
/// </summary>
public readonly record struct ResilienceContext(
    string ActorPath,
    string ChannelName,
    object Message,
    DateTimeOffset Timestamp);
