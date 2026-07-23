namespace Spek.Resilience;

/// <summary>
/// Wraps the invocation of a channel handler with retry, circuit
/// breaker, timeout, hedging, etc. The policy decides when (or
/// whether) to invoke the supplied operation and is responsible for
/// surfacing its result, including any wrapped exceptions.
///
/// Implementations are typically stateful (failure counters, breaker
/// state machines) and must be thread-safe.
///
/// Compose multiple policies into a pipeline by chaining their
/// <see cref="ExecuteAsync{T}"/> calls — outer policies wrap the
/// inner ones, mirroring Polly v8's pipeline composition.
/// </summary>
public abstract class ExecutionPolicy
{
    /// <summary>
    /// Run <paramref name="operation"/> under this policy's control.
    /// The policy owns the invocation: it may call the operation once,
    /// several times (retry, hedging), or not at all (an open circuit
    /// breaker) and surfaces the final outcome — the successful result,
    /// or the failure translated per the policy's rules. Implementations
    /// must flow <paramref name="context"/> and a cancellation token
    /// (linked, when the policy imposes its own timeout) into every
    /// attempt, and must observe <paramref name="cancellationToken"/>
    /// between attempts.
    /// </summary>
    public abstract ValueTask<T> ExecuteAsync<T>(
        ResilienceContext context,
        Func<ResilienceContext, CancellationToken, ValueTask<T>> operation,
        CancellationToken cancellationToken = default);
}
