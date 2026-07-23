// UserServiceGrpcBridge.cs — hand-written bridge between protoc's
// gRPC service base and Spek's actor.
//
// For now the bridge is hand-written. Each RPC override does
// three things:
//   1. translate the protoc request → Spek message
//   2. ask the actor system via SpekGrpcBridge.AskAsync
//   3. translate the Spek reply → protoc response, mapping the
//      reply *type* to a gRPC status code via SpekGrpcStatusMap
//
// In a follow-up minor the Spek compiler will auto-generate this
// class from the channel decl + the proto. For now it serves as
// the canonical example of "what the codegen should produce."
//
// Type aliases disambiguate between Spek-side and protoc-side
// message types — both have a `User` named type but they live
// in different namespaces and have different C# shapes (Spek
// records vs protoc Google.Protobuf classes).

using Grpc.Core;
using Spek.Hosting.AspNetCore.Grpc;

using ProtoUser           = GrpcUserApi.Generated.User;
using ProtoGetUserRequest = GrpcUserApi.Generated.GetUserRequest;
using ProtoCreateUserRequest = GrpcUserApi.Generated.CreateUserRequest;
using ProtoUserApiBase    = GrpcUserApi.Generated.UserApi.UserApiBase;

namespace GrpcUserApi;

public sealed class UserServiceGrpcBridge : ProtoUserApiBase
{
    public override async Task<ProtoUser> GetUser(
        ProtoGetUserRequest request, ServerCallContext context)
    {
        // protoc → Spek
        var spekRequest = new GetUser(request.Id);

        // Ask the actor system. AskAsync resolves the actor through
        // DI, dispatches the message, and waits for the reply.
        var (status, reply) = await SpekGrpcBridge.AskAsync<UserService>(
            context, spekRequest);

        // Reply type → gRPC status. NotFound on the Spek side maps
        // to StatusCode.NotFound on the gRPC wire (per
        // SpekGrpcStatusMap defaults).
        SpekGrpcBridge.ThrowIfErrorStatus(status,
            (reply as NotFound)?.reason);

        // Spek → protoc. We're guaranteed reply is a Spek `User`
        // here because non-User replies threw above.
        var u = (User)reply!;
        return new ProtoUser { Id = u.id, Name = u.name, Email = u.email };
    }

    public override async Task<ProtoUser> CreateUser(
        ProtoCreateUserRequest request, ServerCallContext context)
    {
        var spekRequest = new CreateUser(request.Name, request.Email);

        var (status, reply) = await SpekGrpcBridge.AskAsync<UserService>(
            context, spekRequest);
        SpekGrpcBridge.ThrowIfErrorStatus(status,
            (reply as NotFound)?.reason);

        var u = (User)reply!;
        return new ProtoUser { Id = u.id, Name = u.name, Email = u.email };
    }
}
