namespace Spek;

/// <summary>
/// Typed success-or-failure payload for actor messages — the
/// idiomatic way to communicate "the operation might fail and the
/// caller wants the failure reason as a value, not as a thrown
/// exception."
///
/// <para>Spek doesn't propagate exceptions across actor boundaries
/// (supervision intercepts them), so the C# habit of "throw on
/// failure, return on success" doesn't fit reply messages. An actor
/// that might fail to satisfy a request returns an
/// <see cref="Outcome{T, E}"/> instead — the caller pattern-matches
/// on <see cref="Success"/> vs <see cref="Failure"/> to decide what
/// to do.</para>
///
/// <para>For the common case where the failure type doesn't matter,
/// use the type alias <see cref="Outcome{T}"/> which fixes
/// <typeparamref name="E"/> to <see cref="Reason"/>.</para>
/// </summary>
/// <example>
/// <code>
/// // Handler returns the typed outcome via Option D:
/// on GetUser g =>
/// {
///     var user = repo.Find(g.id);
///     return user is not null
///         ? Outcome&lt;User, Reason&gt;.Success(user)
///         : Outcome&lt;User, Reason&gt;.Failure(new Reason("not-found", "user does not exist"));
/// }
///
/// // Caller pattern-matches:
/// on UserResult r =>
/// {
///     switch (r.Result)
///     {
///         case Outcome&lt;User, Reason&gt;.SuccessCase s: render(s.Value); break;
///         case Outcome&lt;User, Reason&gt;.FailureCase f: handleError(f.Reason); break;
///     }
/// }
/// </code>
/// </example>
public abstract record Outcome<T, E>
{
    /// <summary>The operation succeeded; <see cref="Value"/> carries the result.</summary>
    public sealed record SuccessCase(T Value) : Outcome<T, E>;

    /// <summary>The operation failed; <see cref="Reason"/> carries the cause.</summary>
    public sealed record FailureCase(E Reason) : Outcome<T, E>;

    /// <summary>Construct a successful outcome.</summary>
    public static Outcome<T, E> Success(T value) => new SuccessCase(value);

    /// <summary>Construct a failure outcome with the supplied reason.</summary>
    public static Outcome<T, E> Failure(E reason) => new FailureCase(reason);

    /// <summary>True if this outcome is a success.</summary>
    public bool IsSuccess => this is SuccessCase;

    /// <summary>True if this outcome is a failure.</summary>
    public bool IsFailure => this is FailureCase;

    /// <summary>
    /// Try to extract the success value. Returns true and binds
    /// <paramref name="value"/> if this is a success; otherwise
    /// returns false and leaves <paramref name="value"/> at its
    /// default.
    /// </summary>
    public bool TryGetValue(out T value)
    {
        if (this is SuccessCase s) { value = s.Value; return true; }
        value = default!;
        return false;
    }

    /// <summary>
    /// Try to extract the failure reason. Returns true and binds
    /// <paramref name="reason"/> if this is a failure; otherwise
    /// returns false and leaves <paramref name="reason"/> at its
    /// default.
    /// </summary>
    public bool TryGetReason(out E reason)
    {
        if (this is FailureCase f) { reason = f.Reason; return true; }
        reason = default!;
        return false;
    }

    /// <summary>
    /// Map the success value through <paramref name="map"/>; pass
    /// failures through unchanged. Useful for chaining transformations
    /// without unboxing.
    /// </summary>
    public Outcome<TNew, E> Map<TNew>(Func<T, TNew> map) =>
        this is SuccessCase s
            ? Outcome<TNew, E>.Success(map(s.Value))
            : Outcome<TNew, E>.Failure(((FailureCase)this).Reason);

    /// <summary>
    /// Map the failure reason through <paramref name="map"/>; pass
    /// successes through unchanged.
    /// </summary>
    public Outcome<T, ENew> MapFailure<ENew>(Func<E, ENew> map) =>
        this is FailureCase f
            ? Outcome<T, ENew>.Failure(map(f.Reason))
            : Outcome<T, ENew>.Success(((SuccessCase)this).Value);
}

/// <summary>
/// Standardised failure-reason record for <see cref="Outcome{T}"/>.
/// Carries an opaque <see cref="Code"/> for programmatic dispatch
/// plus a human-readable <see cref="Message"/> for diagnostics.
///
/// Users with richer error needs can replace this with their own
/// type by using <see cref="Outcome{T, E}"/> directly with their
/// chosen <c>E</c>.
/// </summary>
public sealed record Reason(string Code, string Message);

/// <summary>
/// Convenience alias — <c>Outcome&lt;T&gt;</c> is
/// <c>Outcome&lt;T, Reason&gt;</c>. Use this when you want the
/// default <see cref="Reason"/> failure type and don't need a
/// custom error type per call.
/// </summary>
/// <remarks>
/// C# doesn't support partial type aliases at the language level,
/// so this is a thin record that forwards to the two-parameter
/// form rather than a true alias. Pattern-match on the
/// <c>Outcome&lt;T, Reason&gt;</c> cases through the
/// <see cref="Underlying"/> property.
/// </remarks>
public sealed record Outcome<T>(Outcome<T, Reason> Underlying)
{
    /// <summary>Construct a successful outcome carrying <paramref name="value"/>.</summary>
    public static Outcome<T> Success(T value)
        => new(Outcome<T, Reason>.Success(value));

    /// <summary>
    /// Construct a failure outcome from a <see cref="Reason"/> built out of the
    /// given <paramref name="code"/> and <paramref name="message"/>.
    /// </summary>
    public static Outcome<T> Failure(string code, string message)
        => new(Outcome<T, Reason>.Failure(new Reason(code, message)));

    /// <summary>True if this outcome is a success.</summary>
    public bool IsSuccess => Underlying.IsSuccess;

    /// <summary>True if this outcome is a failure.</summary>
    public bool IsFailure => Underlying.IsFailure;

    /// <summary>
    /// Try to extract the success value; returns true and binds
    /// <paramref name="value"/> on success, false otherwise.
    /// </summary>
    public bool TryGetValue(out T value) => Underlying.TryGetValue(out value);

    /// <summary>
    /// Try to extract the failure <see cref="Reason"/>; returns true and binds
    /// <paramref name="reason"/> on failure, false otherwise.
    /// </summary>
    public bool TryGetReason(out Reason reason) => Underlying.TryGetReason(out reason);
}
