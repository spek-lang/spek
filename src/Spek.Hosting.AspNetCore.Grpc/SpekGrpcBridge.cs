using Grpc.Core;
using Spek.Runtime;

namespace Spek.Hosting.AspNetCore.Grpc;

/// <summary>
/// Runtime helpers for compiler-generated gRPC bridge
/// classes. The bridge subclasses a protoc-generated server base
/// (e.g. <c>UserApi.UserApiBase</c>), overrides each RPC method,
/// and routes the request to the actor system via these helpers.
///
/// <para>
/// The compiler emits the bridge per channel as part of its
/// gRPC-aware codegen. User code never instantiates
/// these helpers directly — they're compiler-internal utilities
/// exposed publicly so the generated bridges can call them.
/// </para>
/// </summary>
public static class SpekGrpcBridge
{
    /// <summary>
    /// Resolves the actor instance from the gRPC call's DI scope
    /// and asks it with <paramref name="message"/>. Translates the
    /// reply into a tuple of (status, payload) for the bridge to
    /// map into a gRPC response. Replies that map to non-OK
    /// statuses are returned with <c>null</c> payload — the bridge
    /// throws an <see cref="RpcException"/> to surface the status
    /// to the gRPC client.
    /// </summary>
    public static async Task<(StatusCode Status, object? Payload)>
        AskAsync<TActor>(
            ServerCallContext context,
            object message,
            SpekGrpcStatusMap? statusMap = null)
        where TActor : ActorBase
    {
        ArgumentNullException.ThrowIfNull(context);
        ArgumentNullException.ThrowIfNull(message);

        var services = context.GetHttpContext().RequestServices;
        var system   = (ActorSystem)services.GetService(typeof(ActorSystem))!;
        var actor    = system.Spawn<TActor>();

        statusMap ??= SpekGrpcStatusMap.WithBuiltInDefaults();

        var reply = await actor.AskAsync<object>(message);
        var status = reply is null
            ? StatusCode.NotFound
            : statusMap.ResolveFor(reply.GetType());
        return (status, reply);
    }

    /// <summary>
    /// Throws an <see cref="RpcException"/> when the resolved
    /// status is non-OK. The bridge calls this after
    /// <see cref="AskAsync{TActor}"/> to translate Spek-side
    /// "error" emit types into proper gRPC status codes — gRPC
    /// clients see the error via the standard status surface
    /// rather than a successful response with an error body.
    /// </summary>
    public static void ThrowIfErrorStatus(StatusCode status, string? detail = null)
    {
        if (status == StatusCode.OK) return;
        throw new RpcException(new Status(status, detail ?? string.Empty));
    }
}
