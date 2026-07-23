namespace Spek.Resilience;

/// <summary>
/// What an <see cref="IngressPolicy"/> decided about a candidate
/// message.
/// </summary>
public enum PolicyDecisionKind
{
    /// <summary>Dispatch the message normally.</summary>
    Allow,
    /// <summary>
    /// Reject permanently. The runtime routes the message to the
    /// dead-letter sink and (for ask-pattern senders) replies with a
    /// rejection envelope using <see cref="PolicyDecision.Reason"/>.
    /// </summary>
    Reject,
    /// <summary>
    /// Reject for now; caller may retry after
    /// <see cref="PolicyDecision.RetryAfter"/>. Used by rate limiters
    /// that surface backoff hints.
    /// </summary>
    Defer
}

/// <summary>
/// Result of evaluating an <see cref="IngressPolicy"/>. Construct via
/// the static factories <see cref="Allow"/>, <see cref="Reject"/>,
/// <see cref="Defer"/> rather than the record constructor — the
/// factories enforce the invariants between <see cref="Kind"/>,
/// <see cref="Reason"/>, and <see cref="RetryAfter"/>.
/// </summary>
public readonly record struct PolicyDecision
{
    /// <summary>
    /// The verdict — allow, reject, or defer. Determines how the
    /// runtime routes the message and which of <see cref="Reason"/> /
    /// <see cref="RetryAfter"/> are meaningful; see
    /// <see cref="PolicyDecisionKind"/> for the routing semantics.
    /// </summary>
    public PolicyDecisionKind Kind { get; init; }

    /// <summary>
    /// Human-readable reason. Required for Reject/Defer; ignored for
    /// Allow.
    /// </summary>
    public string? Reason { get; init; }

    /// <summary>
    /// How long the caller should wait before re-sending. Set for
    /// Defer; null for Allow/Reject.
    /// </summary>
    public TimeSpan? RetryAfter { get; init; }

    /// <summary>The message may proceed to its handler.</summary>
    public static PolicyDecision Allow() =>
        new() { Kind = PolicyDecisionKind.Allow };

    /// <summary>
    /// Permanently reject the message. <paramref name="reason"/>
    /// travels with it to the dead-letter sink and into the rejection
    /// envelope for ask-pattern senders.
    /// </summary>
    public static PolicyDecision Reject(string reason) =>
        new() { Kind = PolicyDecisionKind.Reject, Reason = reason };

    /// <summary>
    /// Reject for now, with a backoff hint: the caller may retry after
    /// <paramref name="retryAfter"/>. Rate limiters use this to
    /// distinguish "over the limit right now" from a hard rejection.
    /// </summary>
    public static PolicyDecision Defer(TimeSpan retryAfter, string reason) =>
        new() { Kind = PolicyDecisionKind.Defer, RetryAfter = retryAfter, Reason = reason };
}
