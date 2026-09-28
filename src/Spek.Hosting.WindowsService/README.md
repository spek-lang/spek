# Spek.Hosting.WindowsService

Windows Service Control Manager adapter for [Spek](https://github.com/spek-lang/spek).
Routes SCM lifecycle events into a Spek actor's lifecycle message
handlers.

## Quick start

```spek
// MyService.spek — uses the canonical Shutdown/Pause/Continue/HostState
// records exported by Spek.Hosting.Abstractions.
namespace MyService;

public actor Worker
{
    behavior Running
    {
        on Shutdown        => return 0;
        on Pause           => { sender.Tell(new StateChanged(HostState.Paused)); }
        on Continue        => { sender.Tell(new StateChanged(HostState.Running)); }
        on PowerEvent      => { /* ... */ }
        on SessionChange   => { /* ... */ }
        on CustomCommand   => { /* ... */ }
    }
}
```

```csharp
// Program.cs
using Microsoft.Extensions.Hosting;
using Spek.Hosting;               // canonical Shutdown / Pause / Continue / etc.
using Spek.Hosting.WindowsService;

var builder = Host.CreateApplicationBuilder(args);
builder.Services.AddSpekWindowsService<MyService.Worker>(
    shutdownFactory: () => new Shutdown(),
    pauseFactory:    () => new Pause(),
    continueFactory: () => new Continue());

var host = builder.Build();
await host.RunAsync();
```

Install with:

```cmd
sc create MyService binPath= "C:\Path\To\MyService.exe"
```

The adapter:

- Sends the Shutdown message on `IHostedService.StopAsync` (the SCM
  stop path when hosted with `UseWindowsService()`).
- Exposes public `Pause()`, `Continue()`, `PowerEvent()`,
  `SessionChange()`, and `CustomCommand()` methods for a user-written
  `ServiceBase` subclass to call.
- Tells the corresponding factory-produced message to the actor.

This package is `[SupportedOSPlatform("windows")]`, so referencing it in
a non-Windows project is a CA1416 warning. Use `Spek.Hosting.Console`
or your platform's adapter instead for cross-platform actors.

## Lifecycle messages

The adapter speaks the shared lifecycle records from
Spek.Hosting.Abstractions: `Shutdown`, `Pause`, `Continue`,
`PowerEvent`, `SessionChange`, and `CustomCommand` in.
