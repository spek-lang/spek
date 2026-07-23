# Spek.Hosting.AspNetCore

ASP.NET Core / Generic Host adapter for [Spek](https://github.com/spek-lang/spek).
Hosts a Spek actor as an `IHostedService` so it composes with the
.NET Generic Host's lifetime, DI, and configuration.

## Quick start

```spek
// MyService.spek — uses the canonical Shutdown / Started records
// from Spek.Hosting.Abstractions.
namespace MyService;

public actor Worker : AspNetCoreHost
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
  Option-D return-value reply propagates back as the host's exit
  code.
- Disposes the underlying `ActorSystem` after the actor stops.

## Channel coverage

This adapter satisfies the `AspNetCoreHost` channel from the
hosting guide in the Spek documentation.
The channel declares `on Shutdown;` and `emits Started;`.
