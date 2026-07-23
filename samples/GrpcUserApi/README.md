# GrpcUserApi: Spek hosting a gRPC service on ASP.NET Core

A self-contained sample that exposes a Spek channel and actor pair as a standard
gRPC service. The proto file is the canonical contract, the Spek actor handles
requests, and a small bridge class translates between the two.

```bash
# Build: Spek.targets compiles GrpcUserApi.spek into obj/, and
# Grpc.Tools auto-runs protoc on user_api.proto.
dotnet build

# Run:
dotnet run

# Try (using grpcurl or a generated gRPC client in any language):
grpcurl -plaintext -d '{"id":"u-1"}' localhost:5000 userapi.v1.UserApi/GetUser
grpcurl -plaintext -d '{"name":"Bob","email":"bob@x.com"}' localhost:5000 userapi.v1.UserApi/CreateUser
```

## Files

| File | Role |
|---|---|
| `user_api.proto` | **Canonical contract.** Defines the gRPC service, RPC methods, request/response messages. In a Google-style workflow this lives in a proto registry. |
| `GrpcUserApi.spek` | Spek-side channel + actor handlers **and the host**: a `program Main` block (`AddGrpc()` + `MapGrpcService<UserServiceGrpcBridge>()`), same shape as `RestUserApi`. Channel name (`UserApi`) matches the proto service name. |
| `UserServiceGrpcBridge.cs` | Hand-written bridge: subclasses `protoc`'s `UserApiBase`, overrides each RPC, calls `SpekGrpcBridge.AskAsync` to route to the actor. The **only** C# file here; the gRPC codegen will eventually generate it (see below). |
| `GrpcUserApi.csproj` | Project file. References `Grpc.Tools` for the protoc → C# generation; references the Spek packages. |

## How the pieces fit

```
                ┌─────────────────────┐
                │   user_api.proto    │  ← canonical contract
                └──────────┬──────────┘
                           │ Grpc.Tools (protoc)
                           ▼
        ┌──────────────────────────────────┐
        │  UserApi.UserApiBase, User, …    │  ← C# generated
        │  (in GrpcUserApi.Generated)      │
        └──────────────┬───────────────────┘
                       │
                       │ subclassed by
                       ▼
        ┌──────────────────────────────────┐
        │   UserServiceGrpcBridge.cs       │  ← hand-written bridge
        │   (subclass of UserApiBase)      │
        └──────────────┬───────────────────┘
                       │ SpekGrpcBridge.AskAsync<TActor>
                       ▼
        ┌──────────────────────────────────┐
        │   actor UserService : UserApi    │  ← Spek source
        │   (in GrpcUserApi.spek)          │
        └──────────────────────────────────┘
```

## Status code mapping

A handler returning `NotFound` (Spek message type) gets a gRPC
status of `NotFound` (`StatusCode.NotFound`, code 5) automatically.
The mapping table is in
[`SpekGrpcStatusMap`](../../src/Spek.Hosting.AspNetCore.Grpc/SpekGrpcStatusMap.cs):

| Spek reply type name | gRPC status |
|---|---|
| `NotFound` | `NotFound` (5) |
| `BadRequest` | `InvalidArgument` (3) |
| `Unauthorized` | `Unauthenticated` (16) |
| `Forbidden` | `PermissionDenied` (7) |
| `Conflict` | `AlreadyExists` (6) |
| `UnprocessableEntity` | `FailedPrecondition` (9) |
| `TooManyRequests` | `ResourceExhausted` (8) |
| _anything else_ | `OK` (0) |

To customize, register the override at DI configuration:

```csharp
builder.Services.AddSpekGrpcStatusMap(map =>
{
    map.Add<RateLimited>(StatusCode.ResourceExhausted);
});
```

## Future: auto-generated bridge

The hand-written bridge class is mechanical: every method does the same three
steps, translating the request, asking the actor, then translating the response.
A follow-up minor will auto-generate `UserServiceGrpcBridge` from the channel
decl plus the proto, the same way Spek already auto-generates the actor's
dispatch machinery. Until then, the manual bridge serves as the canonical
example of what the codegen will produce.
