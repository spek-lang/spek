---
title: gRPC hosting
layout: default
parent: Hosting
nav_order: 2
permalink: /hosting/grpc/
---

# gRPC hosting

`Spek.Hosting.AspNetCore.Grpc` puts a Spek channel behind a gRPC
service using `Grpc.AspNetCore.Server`. Nothing about Microsoft's
stack is hidden or wrapped: the package registers one service on
the standard `IEndpointRouteBuilder`, so reflection, interceptors,
health checks, mTLS, and streaming middleware keep working exactly
as they would on any other gRPC host.

## The 30-second tour

Three artifacts cooperate to expose one channel:

1. **A `.proto` file**: the wire contract.
2. **A Spek channel + actor**: handlers write Spek-shape records.
3. **A bridge class**: subclasses the protoc-generated server
   base, translates protoc requests ↔ Spek records, calls the
   actor via `SpekGrpcBridge.AskAsync`.

```spek
// GrpcUserApi.spek
namespace GrpcUserApi;

message GetUser(string id);
message User(string id, string name, string email);
message NotFound(string reason);

channel UserApi
{
    on GetUser;
    emits User; emits NotFound;
}

actor UserService : UserApi
{
    on GetUser g => g.id == "nope"
        ? new NotFound("user " + g.id + " not found")
        : new User(g.id, "Alice", "alice@example.com");
}
```

```protobuf
// user_api.proto
syntax = "proto3";
package userapi.v1;
option csharp_namespace = "GrpcUserApi.Generated";

service UserApi { rpc GetUser (GetUserRequest) returns (User); }

message GetUserRequest { string id = 1; }
message User { string id = 1; string name = 2; string email = 3; }
```

```csharp
// UserServiceGrpcBridge.cs (hand-written)
public sealed class UserServiceGrpcBridge : UserApi.UserApiBase
{
    public override async Task<User> GetUser(
        GetUserRequest request, ServerCallContext context)
    {
        var spekRequest = new GrpcUserApi.GetUser(request.Id);
        var (status, reply) = await SpekGrpcBridge.AskAsync<UserService>(
            context, spekRequest);
        SpekGrpcBridge.ThrowIfErrorStatus(status,
            (reply as GrpcUserApi.NotFound)?.reason);
        var u = (GrpcUserApi.User)reply!;
        return new User { Id = u.id, Name = u.name, Email = u.email };
    }
}
```

```csharp
// Program.cs
var builder = WebApplication.CreateBuilder();
builder.Services.AddSpekActorSystem("user-api");
builder.Services.AddSpekGrpcStatusMap();
builder.Services.AddGrpc();
var app = builder.Build();
app.MapGrpcService<UserServiceGrpcBridge>();
await app.RunAsync();
```

## Direction: proto-first vs code-first

Both directions run through `spekc`, and either side can be the
canonical contract.

Proto-first, where the `.proto` is the contract and Spek synthesizes
the channel, is a two-step pipeline: compile the proto to a descriptor
set, then import it.

```bash
protoc --descriptor_set_out=user_api.bin user_api.proto
spekc proto-import user_api.bin UserApi --out UserApi.g.spek
```

Code-first, where the Spek channel is the contract and the `.proto` is
generated from it, is one step:

```bash
spekc proto-export src/UserApi.spek UserApi --out protos/user_api.proto --package userapi
```

Neither verb is wired into MSBuild. When you want the conversion to run
as part of a build, invoke it from a target you write.

## What's manual, what's automated

| Piece | Status |
|---|---|
| Channel synthesis from `.proto` (proto-first) | automated (`spekc proto-import`) |
| `.proto` emission from channel (code-first) | automated (`spekc proto-export`) |
| C# stubs from `.proto` (UserApiBase, message classes) | automated (standard `Grpc.Tools`) |
| Bridge class subclassing UserApiBase | **hand-written** |
| Running `spekc proto-import`/`-export` in the build | manual (a target you write) |
| Status code mapping (Spek reply type → gRPC status) | automated (`SpekGrpcStatusMap` defaults) |

The sample at `samples/GrpcUserApi/` is the reference for both
hand-written pieces.

## Status code mapping

`SpekGrpcStatusMap` translates Spek reply types into gRPC
status codes. Built-in defaults match the REST hosting's
mapping, projected onto gRPC's standard table:

| Spek reply type name | gRPC `StatusCode` |
|---|---|
| `NotFound` | `NotFound` |
| `BadRequest` | `InvalidArgument` |
| `Unauthorized` | `Unauthenticated` |
| `Forbidden` | `PermissionDenied` |
| `Conflict` | `AlreadyExists` |
| `UnprocessableEntity` | `FailedPrecondition` |
| `TooManyRequests` | `ResourceExhausted` |
| _anything else_ | `OK` |

Customise via DI:

```csharp
builder.Services.AddSpekGrpcStatusMap(map =>
{
    map.Add<RateLimited>(StatusCode.ResourceExhausted);
    map.Add<UpstreamFailure>(StatusCode.Unavailable);
});
```

## Composing with Microsoft's gRPC stack

`MapGrpcService<T>` returns the standard ASP.NET Core endpoint
builder, so interceptors, reflection, auth, and OpenTelemetry
tracing all compose:

```csharp
app.MapGrpcService<UserServiceGrpcBridge>()
   .RequireAuthorization("admin");

builder.Services.AddGrpcReflection();
app.MapGrpcReflectionService();
```

## What's not supported

- **Streaming RPCs** (server-streaming, client-streaming, bidi).
  Channel synthesis emits a placeholder comment for streaming
  methods in the proto and surfaces a warning. Unary-only.
- **Multi-emit `oneof` response synthesis.** Channels with
  multiple `emits` declarations use the first emit type as the
  proto response and emit a placeholder comment.
- **MSBuild integration.** The `spekc proto-import`/`-export`
  verbs run from a target you write.
