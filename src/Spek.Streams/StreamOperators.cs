namespace Spek.Streams;

/// <summary>
/// Lowercase factory functions for the built-in stream operators.
/// Spek source code calls these directly:
///
/// <code>
/// using Spek.Streams;
///
/// on event MouseMove(...)
///     => throttle(16)
///     => { /* body */ }
/// </code>
///
/// User-defined operators follow the same pattern — derive from
/// <see cref="StreamOperator{T}"/> and expose a lowercase factory
/// method in a static class. The grammar accepts any expression
/// that evaluates to a <see cref="StreamOperator{T}"/>.
/// </summary>
public static class StreamOperators
{
    /// <summary>
    /// Holds the latest message and emits it after the source has
    /// been quiet for <paramref name="milliseconds"/>.
    /// </summary>
    public static StreamOperator<T> debounce<T>(int milliseconds)
        => new DebounceOperator<T>(milliseconds);

    /// <inheritdoc cref="debounce{T}(int)"/>
    public static StreamOperator<T> debounce<T>(TimeSpan interval)
        => new DebounceOperator<T>(interval);

    /// <summary>
    /// Emits at most one message per
    /// <paramref name="milliseconds"/> window. Surplus messages
    /// are dropped (leading-edge throttle).
    /// </summary>
    public static StreamOperator<T> throttle<T>(int milliseconds)
        => new ThrottleOperator<T>(milliseconds);

    /// <inheritdoc cref="throttle{T}(int)"/>
    public static StreamOperator<T> throttle<T>(TimeSpan interval)
        => new ThrottleOperator<T>(interval);

    /// <summary>
    /// Emits only when the key extracted by
    /// <paramref name="by"/> differs from the previous emit's
    /// key. The first message always emits.
    /// </summary>
    public static StreamOperator<T> distinct<T, TKey>(Func<T, TKey> by)
        => new DistinctOperator<T, TKey>(by);

    /// <summary>
    /// Combines several operators into one logical step. Operators
    /// run in declaration order; the composed operator's dispatch
    /// fires after the last inner operator emits.
    /// </summary>
    public static StreamOperator<T> compose<T>(params StreamOperator<T>[] inner)
        => new ComposeOperator<T>(inner);

    /// <summary>
    /// Identity helper used by the compiler when emitting a single-
    /// step chain. Forces the C# type-inference engine to pick up
    /// <typeparamref name="T"/> from the explicit type argument so
    /// the inner factory call (which often has its own generics
    /// that depend on T) resolves correctly. User code does not call
    /// this directly.
    /// </summary>
    public static StreamOperator<T> typed<T>(StreamOperator<T> op) => op;
}
