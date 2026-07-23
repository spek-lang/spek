using Grpc.Core;
using Microsoft.Extensions.DependencyInjection;
using Spek.Hosting.AspNetCore.Grpc;
using Xunit;

namespace Spek.Tests.Hosting;

/// <summary>
/// DI wiring for <see cref="SpekGrpcServiceCollectionExtensions"/>:
/// the registered <see cref="SpekGrpcStatusMap"/> must be a singleton,
/// carry the convention defaults, and reflect the caller's
/// <c>configure</c> customizations.
/// </summary>
public sealed class GrpcServiceCollectionTests
{
    public sealed record NotFound(string Reason);
    public sealed record Custom(string What);

    [Fact]
    public async Task AddSpekGrpcStatusMap_RegistersResolvableSingletonWithDefaultsAsync()
    {
        var services = new ServiceCollection();

        var returned = services.AddSpekGrpcStatusMap();

        Assert.Same(services, returned);   // fluent chaining contract

        await using var provider = services.BuildServiceProvider();
        var map = provider.GetRequiredService<SpekGrpcStatusMap>();

        // Built-in convention table is present out of the box.
        Assert.Equal(StatusCode.NotFound, map.ResolveFor(typeof(NotFound)));
        Assert.Equal(StatusCode.OK, map.ResolveFor(typeof(Custom)));

        // Singleton: every consumer sees the same table.
        Assert.Same(map, provider.GetRequiredService<SpekGrpcStatusMap>());
    }

    [Fact]
    public async Task AddSpekGrpcStatusMap_ConfigureCallback_ExtendsTheDefaultTableAsync()
    {
        var services = new ServiceCollection();
        services.AddSpekGrpcStatusMap(map => map.Add<Custom>(StatusCode.DataLoss));

        await using var provider = services.BuildServiceProvider();
        var map = provider.GetRequiredService<SpekGrpcStatusMap>();

        // The custom entry lands on top of the defaults, not instead of them.
        Assert.Equal(StatusCode.DataLoss, map.ResolveFor(typeof(Custom)));
        Assert.Equal(StatusCode.NotFound, map.ResolveFor(typeof(NotFound)));
    }

    [Fact]
    public void AddSpekGrpcStatusMap_NullServices_ThrowsArgumentNull()
        => Assert.Throws<ArgumentNullException>(
            () => SpekGrpcServiceCollectionExtensions.AddSpekGrpcStatusMap(null!));
}
