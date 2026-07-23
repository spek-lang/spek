using Microsoft.Extensions.DependencyInjection;

namespace Spek.Hosting.AspNetCore.Grpc;

/// <summary>
/// DI registration helpers for Spek gRPC hosting.
/// Mirror of <c>Spek.Hosting.AspNetCore.Rest</c>'s
/// <c>SpekRestServiceCollectionExtensions.AddSpekActorSystem</c>;
/// the gRPC variant additionally registers a default
/// <see cref="SpekGrpcStatusMap"/> so reply-type → gRPC status
/// translation works out of the box.
/// </summary>
public static class SpekGrpcServiceCollectionExtensions
{
    /// <summary>
    /// Registers the default <see cref="SpekGrpcStatusMap"/> on
    /// the service collection. Apps that want to extend or
    /// override the table pass a <paramref name="configure"/>
    /// callback; otherwise the convention defaults
    /// (`NotFound` → 5, `BadRequest` → 3, etc.) apply.
    ///
    /// <para>
    /// Call <c>builder.Services.AddSpekActorSystem(...)</c>
    /// (from <c>Spek.Hosting.AspNetCore.Rest</c>) separately to
    /// register the actor system itself — it's the same
    /// <see cref="Spek.Runtime.ActorSystem"/> singleton both
    /// hosting layers share.
    /// </para>
    /// </summary>
    public static IServiceCollection AddSpekGrpcStatusMap(
        this IServiceCollection services,
        Action<SpekGrpcStatusMap>? configure = null)
    {
        ArgumentNullException.ThrowIfNull(services);
        services.AddSingleton(_ =>
        {
            var map = SpekGrpcStatusMap.WithBuiltInDefaults();
            configure?.Invoke(map);
            return map;
        });
        return services;
    }
}
