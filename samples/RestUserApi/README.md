# RestUserApi: Spek hosting a REST API on ASP.NET Core

A self-contained sample that exposes a Spek channel as a REST API.

```bash
# Run (dotnet build compiles RestUserApi.spek into obj/ first):
dotnet run

# Try:
curl http://localhost:5000/users/u-1
curl http://localhost:5000/users -d '{"name":"Bob","email":"bob@x.com"}' -H 'Content-Type: application/json'
curl -X PATCH http://localhost:5000/users/u-1/email -d '{"email":"new@x.com"}' -H 'Content-Type: application/json'
curl http://localhost:5000/users/nope     # → 404 NotFound
```

## What's interesting

The channel stays transport-agnostic. `channel UserApi` declares the service
contract, message types in and messages out, and nothing in `UserApi` knows
about REST. The same channel could be exposed over gRPC or queued messaging
without touching the channel decl.

Convention covers the common case. The handler-name prefixes (`Get*`, `Create*`,
`Update*`, `Delete*`, `List*`) drive HTTP verb selection automatically, and path
placeholders come from the `id` field by default. When a handler doesn't fit,
you reach for the configurator: `ChangeEmail` is a PATCH on a sub-path, and the
`routes.Override<ChangeEmail>(...)` call is the one place REST routing decisions
live.

Status codes come from the emitted reply type. A handler returning a `User` gets
a 200; one returning a `NotFound` gets a 404. The mapping is configurable via
`SpekStatusCodeMap`. All of this rides on standard ASP.NET Core, so anything
ASP.NET Core does, whether auth middleware, OpenAPI, custom serialization, or
custom formatters, composes naturally. Spek doesn't replace the HTTP pipeline; it
adds endpoints to it.
