namespace Spek.Hosting.AspNetCore.Rest;

/// <summary>
/// Host-side override surface for off-convention routes.
/// Passed to the <c>configure</c> lambda on
/// <c>app.MapChannel&lt;TChannel, TActor&gt;(basePath, configure)</c>.
///
/// Channels and messages stay transport-agnostic: the only place
/// REST-specific routing decisions live is here, in C# host code,
/// where they're reviewable alongside the rest of the application's
/// HTTP wiring.
/// </summary>
/// <example>
/// <code>
/// app.MapChannel&lt;UserApi, UserService&gt;("/users", routes =>
/// {
///     routes.Override&lt;ChangeEmail&gt;(HttpVerb.Patch, "/{id}/email");
/// });
/// </code>
/// </example>
public sealed class RouteConfigurator<TChannel>
{
    private readonly Dictionary<Type, RouteOverride> _overrides;

    internal RouteConfigurator(Dictionary<Type, RouteOverride> overrides)
        => _overrides = overrides;

    /// <summary>
    /// Replaces the convention-derived verb and path template for
    /// the given input message type. Path templates are relative
    /// to the channel's base path — pass <c>"/{id}/email"</c>
    /// (not <c>"/users/{id}/email"</c>) when the channel was
    /// mapped at <c>"/users"</c>.
    /// </summary>
    /// <typeparam name="TMessage">A message type listed in the
    /// channel's <c>on</c> inputs.</typeparam>
    public RouteConfigurator<TChannel> Override<TMessage>(HttpVerb verb, string template)
    {
        ArgumentNullException.ThrowIfNull(template);
        _overrides[typeof(TMessage)] = new RouteOverride(verb, template);
        return this;
    }
}
