# HelloWeb: a web server and middleware in Spek

A "Hello, World" HTTP server with four middleware components, written in Spek
as a `program` block that stands up an ASP.NET Core `WebApplication` through
interop. It started life as a probe into how far the existing surface gets us.
The boundary it found, after-next middleware needing an async lambda, has since
been closed, so the sample now exercises that case too.

## Run it

The middleware uses ASP.NET Core types, and the compiler needs those references
to know that `next(context)` returns a `Task` it can auto-await. The
`Spek.targets` build integration passes the project's references to the compiler
for you, so a plain `dotnet run` is enough:

```bash
ASPNETCORE_URLS=http://localhost:5080 dotnet run
curl -i http://localhost:5080/         # 200, "Hello, World! (served by Spek)", X-Powered-By: Spek
curl -i http://localhost:5080/health   # 200, "OK" (short-circuited)
```

The server console shows both a `[req]` line (before-next) and a `[time]` line
(after-next) per request.

## The four middleware

The server registers four inline `app.Use(...)` lambdas, run in order:

0. **Timing** records the time, awaits `next`, then logs how long the rest of
   the pipeline took. This is an after-next middleware.
1. **Request logging** writes `[req] GET /` to the console (before-next).
2. **Response header** stamps `X-Powered-By: Spek` on every response.
3. **Short-circuit** answers `/health` with `OK` without reaching the endpoint.

There are two shapes here, and the compiler's invisible-async pass handles both.
A before-next middleware (`return next(context);`) forwards `next`'s `Task` and
stays a synchronous `Func<…, Task>`. An after-next middleware (`next(context);`
followed by more work) needs to await the rest of the pipeline and then
continue. Spek has no `await` keyword, so the compiler makes the lambda `async`
and inserts the `await` for you. That second case used to fail with `CS1643`; it
now compiles and runs.

## How after-next works

The invisible-async pass descends into a lambda body only when the lambda's
converted delegate type returns `Task` or `ValueTask`, which is why a
value-returning `Func<int,int>` is never touched. It then awaits the call only
in statement position, so `return next(ctx)` keeps forwarding. This is why the
project's AspNetCore references matter: without them the compiler can't resolve
the `app.Use(...)` overload, can't see that the delegate returns `Task`, and
conservatively leaves the lambda synchronous. The `Spek.targets` build passes
those references to the compiler for you.
