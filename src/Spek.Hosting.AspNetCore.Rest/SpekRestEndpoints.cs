using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Routing;
using Microsoft.Extensions.DependencyInjection;
using Spek;
using Spek.Runtime;

namespace Spek.Hosting.AspNetCore.Rest;

/// <summary>
/// Extension methods that register a Spek channel as a set
/// of REST endpoints on an ASP.NET Core <c>IEndpointRouteBuilder</c>.
///
/// <para>
/// Most apps need only one call: <c>app.MapChannel&lt;TChannel,
/// TActor&gt;(basePath)</c>. The convention engine derives a route
/// for every input message in the channel; the bridge delegate
/// hands incoming requests to the actor system and translates the
/// reply to a JSON response with a status code chosen from the
/// reply type.
/// </para>
/// </summary>
public static class SpekRestEndpoints
{
    /// <summary>
    /// Registers REST endpoints for every input message in
    /// <typeparamref name="TChannel"/>. The actor type
    /// <typeparamref name="TActor"/> handles the requests via the
    /// actor system registered in DI as
    /// <see cref="ActorSystem"/>; the actor is spawned on the first
    /// request and reused for the lifetime of the host.
    /// </summary>
    /// <param name="app">The endpoint route builder, typically a
    /// <c>WebApplication</c>.</param>
    /// <param name="basePath">Path prefix shared by every route
    /// derived from this channel. Combined with each input's
    /// per-route template (e.g. <c>/users</c> + <c>/{id}</c> →
    /// <c>/users/{id}</c>).</param>
    /// <param name="configure">Optional callback for declaring
    /// off-convention routes via
    /// <see cref="RouteConfigurator{T}.Override{TMessage}"/>.</param>
    public static IEndpointConventionBuilder MapChannel<TChannel, TActor>(
        this IEndpointRouteBuilder app,
        string basePath,
        Action<RouteConfigurator<TChannel>>? configure = null)
        where TActor : ActorBase
    {
        ArgumentNullException.ThrowIfNull(app);
        ArgumentNullException.ThrowIfNull(basePath);

        var resolver = new ChannelRouteResolver<TChannel>();
        configure?.Invoke(resolver.Configurator);

        var group = app.MapGroup(basePath);

        // The actor reference is created lazily on first request and
        // reused for the host's lifetime. Spawning on Main → races
        // against startup; spawning on first request → simpler and
        // matches Spek's lazy-init philosophy.
        Lazy<ActorRef>? actorRef = null;

        foreach (var route in resolver.Routes())
        {
            var captured = route;
            group.MapMethods(captured.Template, new[] { captured.Verb.ToHttpMethodString() },
                async (HttpContext http) =>
                {
                    actorRef ??= new Lazy<ActorRef>(() =>
                    {
                        var system = http.RequestServices.GetRequiredService<ActorSystem>();
                        return system.Spawn<TActor>();
                    });

                    var message = await SpekMessageBinder.BindAsync(http, captured.MessageType);

                    object? reply;
                    try
                    {
                        reply = await actorRef.Value.AskAsync<object>(message);
                    }
                    catch (Exception ex)
                    {
                        // Hand back a generic 500 with the message; users wanting a
                        // sanitised error surface can register an exception handler
                        // via standard ASP.NET Core middleware.
                        http.Response.StatusCode = 500;
                        await http.Response.WriteAsJsonAsync(new
                        {
                            error = "internal error",
                            detail = ex.Message,
                        });
                        return;
                    }

                    var statusMap = http.RequestServices.GetService<SpekStatusCodeMap>()
                                  ?? SpekStatusCodeMap.WithBuiltInDefaults();
                    var status = reply is null ? 204 : statusMap.ResolveWithNameFallback(reply.GetType());

                    http.Response.StatusCode = status;
                    if (reply is not null)
                        await http.Response.WriteAsJsonAsync(reply, reply.GetType());
                });
        }

        return group;
    }
}
