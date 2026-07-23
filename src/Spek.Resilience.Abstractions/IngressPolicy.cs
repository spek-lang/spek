namespace Spek.Resilience;

/// <summary>
/// A policy consulted before a message reaches its channel handler.
/// Decides whether to admit the message, reject it, or defer it.
///
/// Implementations are typically stateful (token buckets, sliding
/// windows, semaphore-backed bulkheads). They must be thread-safe —
/// the runtime invokes <see cref="EvaluateAsync"/> from arbitrary
/// dispatcher threads.
///
/// Execution-time policies (retry, circuit breaker, timeout) wrap the
/// handler invocation itself and use a different contract — see the
/// <c>ExecutionPolicy</c> base in this assembly.
/// </summary>
public abstract class IngressPolicy
{
    /// <summary>
    /// Decide the fate of the message described by
    /// <paramref name="context"/> before it reaches the actor's
    /// handler. The policy only renders the verdict and updates its
    /// own admission state (tokens, windows, permits) — the runtime
    /// acts on the returned <see cref="PolicyDecision"/>: dispatch on
    /// Allow, dead-letter with the policy's reason on Reject, and
    /// delayed re-admission (bounded, clock-driven) on Defer.
    /// Runs on the dispatch path for every message, so implementations
    /// should complete synchronously whenever the decision doesn't
    /// require waiting.
    /// </summary>
    public abstract ValueTask<PolicyDecision> EvaluateAsync(
        ResilienceContext context,
        CancellationToken cancellationToken = default);
}
