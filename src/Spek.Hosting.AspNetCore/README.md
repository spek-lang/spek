# Spek.Hosting.AspNetCore

ASP.NET Core / Generic Host adapter for [Spek](https://github.com/spek-lang/spek).
Hosts a Spek actor as an `IHostedService` so it composes with the
.NET Generic Host's lifetime, DI, and configuration.

## Quick start

```spek
// MyService.spek — uses the canonical Shutdown / Started records
// from Spek.Hosting.Abstractions.
namespace MyService;

public actor Worker
{
    behavior Running
    {
        on Shutdown =>
        {
            // graceful drain
            return 0;
        }
    }
}
```

```csharp
// Program.cs
using Microsoft.Extensions.Hosting;
using Spek.Hosting;               // canonical Shutdown record
using Spek.Hosting.AspNetCore;

var builder = Host.CreateApplicationBuilder(args);

builder.Services.AddSpekHostedService<MyService.Worker>(
    shutdownFactory: () => new Shutdown());

var host = builder.Build();
await host.RunAsync();
```

The adapter:

- Spawns a single instance of the Spek actor at host startup.
- On `IHostedService.StopAsync`, sends the user's `Shutdown` message
  to the actor with `Tell(..., sender: receiver)`; the actor's
  return-value reply is captured by the adapter's receiver and is not
  surfaced as the process exit code (the Generic Host owns that
  decision).
- Disposes the underlying `ActorSystem` after the actor stops.

## Lifecycle messages

The adapter speaks the shared lifecycle records from
Spek.Hosting.Abstractions: `Shutdown` in, `Started` out.
