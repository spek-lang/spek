---
title: REST hosting
layout: default
parent: Hosting
nav_order: 1
permalink: /hosting/rest/
---

# REST hosting

`Spek.Hosting.AspNetCore.Rest` exposes a Spek channel as a set
of REST endpoints on top of ASP.NET Core. The package is glue,
not a framework. Anything ASP.NET Core does (auth middleware,
OpenAPI, content negotiation, custom serialization) composes
naturally, because the package adds endpoints to a standard
`IEndpointRouteBuilder` rather than replacing the pipeline.

## The 30-second tour

```spek
namespace UserApi;

using Spek.Hosting.AspNetCore.Rest;
using Microsoft.AspNetCore.Builder;

message GetUser(string id);
message CreateUser(string name, string email);
message User(string id, string name, string email);
message NotFound(string reason);

channel UserApi
{
    on GetUser; on CreateUser;
    emits User; emits NotFound;
}

actor UserService : UserApi
{
    on GetUser g    => return g.id == "nope"
                          ? new NotFound("not found")
                          : new User(g.id, "Alice", "alice@example.com");
    on CreateUser c => return new User("u-" + c.name, c.name, c.email);
}

program Main
{
    var builder = WebApplication.CreateBuilder();
    builder.Services.AddSpekActorSystem("user-api");

    var app = builder.Build();
    app.MapChannel<UserApi, UserService>("/users");
    app.RunAsync();   // invisible async — no `await` keyword in Spek
}
```

What the convention engine derives from this:

| Handler | Verb | Route | Source of fields |
|---|---|---|---|
| `GetUser` | GET | `/users/{id}` | path: `id` |
| `CreateUser` | POST | `/users` | body: `{ name, email }` |

Hit `GET /users/u-1` and you get `200 { "id": "u-1", ... }`.
Hit `GET /users/nope` and you get `404 { "reason": "not found" }`.
Hit `POST /users -d '{"name":"Bob","email":"bob@x.com"}'` and you
get `200 { "id": "u-Bob", ... }`.

## Convention rules

The convention engine derives `(verb, path)` from each input
message in the channel.

### Verb from name prefix

| Prefix | Verb |
|---|---|
| `Get*`, `List*` | GET |
| `Create*`, `Add*` | POST |
| `Update*`, `Replace*` | PUT |
| `Patch*`, `Change*` | PATCH |
| `Delete*`, `Remove*` | DELETE |
| _no match_ | POST (default) |

The prefix must be followed by an uppercase letter, so `List`
matches `ListUsers` but not `ListenerStarted`.

### Path placeholders from `id` field

If the input message has a constructor parameter named `id`
(case-insensitive), the route appends `/{id}`. The exception
is the `List*` prefix: list-shaped GETs always collect from
the channel's base path even when the message has an `id`
field for some reason.

| Message | Verb | Route |
|---|---|---|
| `GetUser(string id)` | GET | `/{id}` |
| `CreateUser(string name)` | POST | `/` |
| `ListUsers(int page)` | GET | `/` |

### Field source: path / query / body

The remaining (non-`id`) fields bind from:

- **JSON request body** for body-bearing verbs (POST, PUT, PATCH).
- **Query string** for body-less verbs (GET, DELETE, HEAD, OPTIONS).
- **Default values** when the request doesn't supply the field.

```spek
message ListUsers(int page = 1, int pageSize = 20);
// → GET /users?page=2&pageSize=50
//   maps to ListUsers(page: 2, pageSize: 50)
```

## Off-convention routes: `RouteConfigurator.Override`

When the convention doesn't fit, declare the override at the
host call site. The channel and message decls stay
transport-agnostic.

```spek
program Main
{
    var builder = WebApplication.CreateBuilder();
    builder.Services.AddSpekActorSystem("user-api");

    var app = builder.Build();
    app.MapChannel<UserApi, UserService>("/users", routes =>
    {
        routes.Override<ChangeEmail>(HttpVerb.Patch, "/{id}/email");
    });
    app.RunAsync();
}
```

The path template is **relative to the channel's base path**:
pass `/{id}/email` (not `/users/{id}/email`) when the channel
was mapped at `/users`.

`HttpVerb` is a typed enum (`HttpVerb.Get`, `HttpVerb.Post`,
`HttpVerb.Put`, `HttpVerb.Patch`, `HttpVerb.Delete`,
`HttpVerb.Head`, `HttpVerb.Options`). The compiler catches
typos that string constants would let through.

## Status code mapping

The package looks at the *type* the handler returned and
chooses an HTTP status code. Built-in defaults:

| Reply type name | Status |
|---|---|
| `NotFound` | 404 |
| `BadRequest` | 400 |
| `Unauthorized` | 401 |
| `Forbidden` | 403 |
| `Conflict` | 409 |
| `UnprocessableEntity` | 422 |
| `TooManyRequests` | 429 |
| _anything else_ | 200 |

This matches by **simple type name**, so any `NotFound` message
the user defines (regardless of namespace) maps to 404.

To customize, register the mapping at DI configuration time:

```spek
program Main
{
    var builder = WebApplication.CreateBuilder();
    builder.Services.AddSpekActorSystem("user-api", map =>
    {
        map.Add<UserCreated>(201);     // CreateUser convention → 201
        map.Add<RateLimited>(429);
    });
    // …
}
```

A `null` reply gets a 204 (No Content).

## Exceptions

A handler that throws gets a 500 with a generic JSON body:

```json
{ "error": "internal error", "detail": "..." }
```

For sanitised error responses, register a standard ASP.NET Core
exception handler middleware before the endpoint mappings:

```spek
program Main
{
    var app = builder.Build();
    app.UseExceptionHandler("/error");   // standard ASP.NET Core
    app.MapChannel<UserApi, UserService>("/users");
    // …
}
```

## Composing with ASP.NET Core middleware

Because `MapChannel` returns an `IEndpointConventionBuilder`,
the standard ASP.NET Core fluent surface composes:

```spek
program Main
{
    var app = builder.Build();
    app
        .MapChannel<UserApi, UserService>("/users")
        .RequireAuthorization("admin")
        .WithName("UserApi")
        .WithOpenApi();
    // …
}
```

Auth, rate-limiting, output caching, OpenAPI documentation,
custom output formatters: all the standard ASP.NET Core
endpoint metadata works on top.

## What's not supported

- **Request context propagation.** Headers, auth claims, and
  correlation IDs aren't plumbed into the actor handler. Code that
  needs request context can drop into a custom delegate
  before / after `MapChannel`.
- **`[FromQuery]` / `[FromBody]` per-field overrides.** The
  convention is fully positional (id → path; rest → body
  or query depending on verb).
