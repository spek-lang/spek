using Grpc.Core;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.DependencyInjection;
using Spek.Hosting.AspNetCore.Grpc;
using Spek.Runtime;
using Xunit;

namespace Spek.Tests.Hosting;

/// <summary>
/// Unit coverage for <see cref="SpekGrpcBridge"/> — the runtime helper
/// the compiler-generated gRPC bridges call. Rather than booting a
/// full Kestrel + gRPC endpoint, we drive <c>AskAsync</c> directly
/// against a real <see cref="ActorSystem"/> through a hand-rolled
/// <see cref="ServerCallContext"/> whose <c>UserState["__HttpContext"]</c>
/// carries a <see cref="DefaultHttpContext"/> — the documented seam
/// <c>ServerCallContext.GetHttpContext()</c> reads when gRPC services
/// are unit-tested outside ASP.NET Core hosting.
/// </summary>
public sealed class GrpcBridgeTests
{
    public sealed record GetUser(string Id);
    public sealed record UserReply(string Id, string Name);
    public sealed record NotFound(string Reason);

    /// <summary>
    /// Stand-in for a channel actor behind a generated bridge: replies
    /// to <see cref="GetUser"/> with a payload for known ids and a
    /// convention-named <see cref="NotFound"/> emit otherwise.
    /// </summary>
    private sealed class UserApiActor : ActorBase
    {
        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            if (message is GetUser g)
            {
                object reply = g.Id == "missing"
                    ? new NotFound($"no user {g.Id}")
                    : new UserReply(g.Id, "Ada");
                sender.Tell(reply, _selfRef!);
            }
            return Task.CompletedTask;
        }
    }

    private static ServerCallContext CreateCallContext(IServiceProvider services)
    {
        var context = new FakeServerCallContext();
        context.UserState["__HttpContext"] = new DefaultHttpContext
        {
            RequestServices = services,
        };
        return context;
    }

    [Fact]
    public async Task AskAsync_MapsRequestToActorAsk_AndReturnsOkWithReplyPayloadAsync()
    {
        using var system = new ActorSystem("grpc-bridge-ok");
        await using var provider = new ServiceCollection()
            .AddSingleton(system)
            .BuildServiceProvider();
        var context = CreateCallContext(provider);

        var (status, payload) = await SpekGrpcBridge.AskAsync<UserApiActor>(
            context, new GetUser("42"));

        Assert.Equal(StatusCode.OK, status);
        var user = Assert.IsType<UserReply>(payload);
        Assert.Equal("42", user.Id);
        Assert.Equal("Ada", user.Name);
    }

    [Fact]
    public async Task AskAsync_ConventionNamedReply_ResolvesToMatchingGrpcStatusAsync()
    {
        using var system = new ActorSystem("grpc-bridge-notfound");
        await using var provider = new ServiceCollection()
            .AddSingleton(system)
            .BuildServiceProvider();
        var context = CreateCallContext(provider);

        var (status, payload) = await SpekGrpcBridge.AskAsync<UserApiActor>(
            context, new GetUser("missing"));

        // The reply itself still comes back — the generated bridge decides
        // (via ThrowIfErrorStatus) whether to surface it or raise RpcException.
        Assert.Equal(StatusCode.NotFound, status);
        var notFound = Assert.IsType<NotFound>(payload);
        Assert.Equal("no user missing", notFound.Reason);
    }

    [Fact]
    public async Task AskAsync_ExplicitStatusMap_OverridesConventionDefaultsAsync()
    {
        using var system = new ActorSystem("grpc-bridge-custom-map");
        await using var provider = new ServiceCollection()
            .AddSingleton(system)
            .BuildServiceProvider();
        var context = CreateCallContext(provider);

        var map = new SpekGrpcStatusMap().Add<UserReply>(StatusCode.PermissionDenied);

        var (status, payload) = await SpekGrpcBridge.AskAsync<UserApiActor>(
            context, new GetUser("7"), map);

        Assert.Equal(StatusCode.PermissionDenied, status);
        Assert.IsType<UserReply>(payload);
    }

    [Fact]
    public async Task AskAsync_NullContextOrMessage_ThrowsArgumentNullAsync()
    {
        using var system = new ActorSystem("grpc-bridge-nulls");
        await using var provider = new ServiceCollection()
            .AddSingleton(system)
            .BuildServiceProvider();
        var context = CreateCallContext(provider);

        await Assert.ThrowsAsync<ArgumentNullException>(
            () => SpekGrpcBridge.AskAsync<UserApiActor>(null!, new GetUser("1")));
        await Assert.ThrowsAsync<ArgumentNullException>(
            () => SpekGrpcBridge.AskAsync<UserApiActor>(context, null!));
    }

    [Fact]
    public void ThrowIfErrorStatus_Ok_DoesNotThrow()
    {
        SpekGrpcBridge.ThrowIfErrorStatus(StatusCode.OK);
        SpekGrpcBridge.ThrowIfErrorStatus(StatusCode.OK, "detail is ignored for OK");
    }

    [Fact]
    public void ThrowIfErrorStatus_NonOk_ThrowsRpcExceptionWithStatusAndDetail()
    {
        var ex = Assert.Throws<RpcException>(
            () => SpekGrpcBridge.ThrowIfErrorStatus(StatusCode.NotFound, "no user 9"));

        Assert.Equal(StatusCode.NotFound, ex.StatusCode);
        Assert.Equal("no user 9", ex.Status.Detail);
    }

    [Fact]
    public void ThrowIfErrorStatus_NonOkWithoutDetail_UsesEmptyDetail()
    {
        var ex = Assert.Throws<RpcException>(
            () => SpekGrpcBridge.ThrowIfErrorStatus(StatusCode.InvalidArgument));

        Assert.Equal(StatusCode.InvalidArgument, ex.StatusCode);
        Assert.Equal(string.Empty, ex.Status.Detail);
    }

    /// <summary>
    /// Minimal in-process <see cref="ServerCallContext"/>. Only
    /// <c>UserState</c> matters for the bridge (it hosts the HttpContext
    /// used to resolve the actor system); the rest are inert stand-ins.
    /// </summary>
    private sealed class FakeServerCallContext : ServerCallContext
    {
        private readonly Metadata _requestHeaders = new();
        private readonly Metadata _responseTrailers = new();
        private readonly Dictionary<object, object> _userState = new();
        private Status _status;
        private WriteOptions? _writeOptions;

        protected override string MethodCore => "/spek.tests.UserApi/GetUser";
        protected override string HostCore => "localhost";
        protected override string PeerCore => "ipv4:127.0.0.1:50051";
        protected override DateTime DeadlineCore => DateTime.UtcNow.AddMinutes(1);
        protected override Metadata RequestHeadersCore => _requestHeaders;
        protected override CancellationToken CancellationTokenCore => CancellationToken.None;
        protected override Metadata ResponseTrailersCore => _responseTrailers;
        protected override Status StatusCore
        {
            get => _status;
            set => _status = value;
        }
        protected override WriteOptions? WriteOptionsCore
        {
            get => _writeOptions;
            set => _writeOptions = value;
        }
        protected override AuthContext AuthContextCore =>
            new(string.Empty, new Dictionary<string, List<AuthProperty>>());
        protected override IDictionary<object, object> UserStateCore => _userState;

        protected override ContextPropagationToken CreatePropagationTokenCore(
            ContextPropagationOptions? options) => throw new NotSupportedException();

        protected override Task WriteResponseHeadersAsyncCore(Metadata responseHeaders)
            => Task.CompletedTask;
    }
}
