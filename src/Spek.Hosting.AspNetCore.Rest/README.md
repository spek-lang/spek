# Spek.Hosting.AspNetCore.Rest

Expose Spek channels as REST endpoints.

```csharp
var builder = WebApplication.CreateBuilder();
builder.Services.AddSpekActorSystem("user-api");

var app = builder.Build();
app.MapChannel<UserApi, UserService>("/users", routes =>
{
    routes.Override<ChangeEmail>(HttpVerb.Patch, "/{id}/email");
});
await app.RunAsync();
```

That's it. Convention picks up:

- `on GetUser` → `GET /users/{id}` (binding `id` from path)
- `on CreateUser` → `POST /users` (body: `{ name, email }`)
- `on UpdateUser` → `PUT /users/{id}` (path: `id`; body: rest)
- `on DeleteUser` → `DELETE /users/{id}`
- `on ListUsers` → `GET /users` (query: `?page=…&pageSize=…`)

Off-convention routes go through the configurator. Channels and
messages stay transport-agnostic, so the same channel can be exposed
over REST in one app and over a different transport in another.

## How it works

It's built on ASP.NET Core's standard endpoint routing, so anything
ASP.NET Core does (auth middleware, OpenAPI, content negotiation,
custom serialization) composes naturally. The package is glue,
not a framework: ~500 lines of C# that wire Spek channels into
`IEndpointRouteBuilder`.

See `docs/hosting/rest.md` for the full reference.
