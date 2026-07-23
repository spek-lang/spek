using Grpc.Core;
using Spek.Hosting.AspNetCore.Grpc;
using Xunit;

namespace Spek.Tests.Hosting;

/// <summary>
/// gRPC status code map. Confirms convention defaults
/// match the gRPC standard table and that user overrides take
/// precedence. The helper itself is small; these tests guard
/// against accidental drift in the convention names (the REST
/// status map is the source of truth — gRPC mirrors it onto
/// gRPC's standard codes).
/// </summary>
public sealed class GrpcStatusMapTests
{
    public sealed record User(string id, string name);
    public sealed record NotFound(string reason);
    public sealed record BadRequest(string reason);
    public sealed record Conflict(string reason);
    public sealed record Custom(string what);

    [Fact]
    public void DefaultMap_NotFound_ResolvesAsStatusCodeNotFound()
        => Assert.Equal(StatusCode.NotFound,
            SpekGrpcStatusMap.WithBuiltInDefaults().ResolveFor(typeof(NotFound)));

    [Fact]
    public void DefaultMap_BadRequest_ResolvesAsStatusCodeInvalidArgument()
        => Assert.Equal(StatusCode.InvalidArgument,
            SpekGrpcStatusMap.WithBuiltInDefaults().ResolveFor(typeof(BadRequest)));

    [Fact]
    public void DefaultMap_Conflict_ResolvesAsStatusCodeAlreadyExists()
        => Assert.Equal(StatusCode.AlreadyExists,
            SpekGrpcStatusMap.WithBuiltInDefaults().ResolveFor(typeof(Conflict)));

    [Fact]
    public void DefaultMap_UnknownReplyType_ResolvesAsOk()
        => Assert.Equal(StatusCode.OK,
            SpekGrpcStatusMap.WithBuiltInDefaults().ResolveFor(typeof(User)));

    [Fact]
    public void UserOverride_TakesPrecedenceOverConventionName()
    {
        var map = SpekGrpcStatusMap.WithBuiltInDefaults()
            .Add<NotFound>(StatusCode.Unavailable);
        Assert.Equal(StatusCode.Unavailable, map.ResolveFor(typeof(NotFound)));
    }

    [Fact]
    public void UserOverride_OnTypeNotInConvention_Resolves()
    {
        var map = SpekGrpcStatusMap.WithBuiltInDefaults()
            .Add<Custom>(StatusCode.ResourceExhausted);
        Assert.Equal(StatusCode.ResourceExhausted, map.ResolveFor(typeof(Custom)));
    }

    [Fact]
    public void EmptyMap_NoBuiltInDefaults_ResolvesEverythingAsOk()
    {
        var map = new SpekGrpcStatusMap();
        Assert.Equal(StatusCode.OK, map.ResolveFor(typeof(NotFound)));
        Assert.Equal(StatusCode.OK, map.ResolveFor(typeof(BadRequest)));
    }
}
