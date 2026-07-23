# Spek.Hosting.WindowsService

Windows Service Control Manager adapter for [Spek](https://github.com/spek-lang/spek).
Routes SCM lifecycle events into a Spek actor's `WindowsServiceHost`
channel handlers.

## Quick start

```spek
// MyService.spek — uses the canonical Shutdown/Pause/Continue/HostState
// records exported by Spek.Hosting.Abstractions.
namespace MyService;

public actor Worker : WindowsServiceHost
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

- Hooks `ServiceBase.OnStart`, `OnStop`, `OnPause`, `OnContinue`,
  `OnShutdown`, `OnPowerEvent`, `OnSessionChange`, and
  `OnCustomCommand`.
- Tells the corresponding factory-produced message to the actor.
- Reports SCM state transitions back via `SetServiceStatus` when the
  actor emits `StateChanged`.

This package is `[SupportedOSPlatform("windows")]`, so referencing it in
a non-Windows project is a CA1416 warning. Use `Spek.Hosting.Console`
or your platform's adapter instead for cross-platform actors.

## Channel coverage

This adapter satisfies the `WindowsServiceHost` channel from the
hosting guide in the Spek documentation.
