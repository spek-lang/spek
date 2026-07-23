using Microsoft.Extensions.DependencyInjection;
using Spek.Runtime;

namespace Spek.Hosting.AspNetCore.Rest;

/// <summary>
/// DI registration helpers for Spek REST hosting.
/// Registers the actor system as a singleton and the status-code
/// map as a per-host singleton (with built-in defaults users can
/// extend via <see cref="SpekStatusCodeMap.Add{T}"/>).
/// </summary>
public static class SpekRestServiceCollectionExtensions
{
    /// <summary>
    /// Registers a Spek <see cref="ActorSystem"/> as a singleton
    /// service plus the default <see cref="SpekStatusCodeMap"/>.
    /// Call this once on the
    /// <see cref="IServiceCollection"/> when configuring the host.
    /// </summary>
    /// <param name="services">The service collection.</param>
    /// <param name="systemName">Name of the actor system; surfaces
    /// in dead-letter logs and supervision diagnostics.</param>
    /// <param name="configureStatusMap">Optional callback to
    /// register additional reply-type → status-code mappings on
    /// top of the built-in defaults
    /// (<c>NotFound</c> → 404, etc.).</param>
    public static IServiceCollection AddSpekActorSystem(
        this IServiceCollection services,
        string systemName,
        Action<SpekStatusCodeMap>? configureStatusMap = null)
    {
        ArgumentNullException.ThrowIfNull(services);
        ArgumentNullException.ThrowIfNull(systemName);

        services.AddSingleton(_ => new ActorSystem(systemName));

        services.AddSingleton(_ =>
        {
            var map = SpekStatusCodeMap.WithBuiltInDefaults();
            configureStatusMap?.Invoke(map);
            return map;
        });

        return services;
    }
}
