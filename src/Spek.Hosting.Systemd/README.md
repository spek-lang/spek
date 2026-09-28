# Spek.Hosting.Systemd

Linux systemd host adapter for [Spek](https://github.com/spek-lang/spek). Routes
`SIGTERM` and `SIGHUP` into Spek messages, and notifies systemd via
`sd_notify` (READY=1 once the host has started, STOPPING=1 on
shutdown).

## Quick start

```spek
// Worker.spek — uses the canonical Shutdown / Reload / Started /
// HostState / StateChanged / HealthReport records exported by
// Spek.Hosting.Abstractions.
namespace MyService;

public actor Worker
{
    behavior Running
    {
        on Shutdown =>
        {
            sender.Tell(new StateChanged(HostState.Stopping));
            return 0;
        }

        on Reload =>
        {
            sender.Tell(new StateChanged(HostState.Degraded));
            // re-read config...
            sender.Tell(new HealthReport(true, "config reloaded"));
        }
    }

    on PreStart =>
    {
        // startup work; the adapter itself sends sd_notify READY=1
        // once the whole host is up
    }
}
```

```csharp
// Program.cs
using Microsoft.Extensions.Hosting;
using Spek.Hosting;               // canonical Shutdown / Reload records
using Spek.Hosting.Systemd;

var builder = Host.CreateApplicationBuilder(args);
builder.Services.AddSpekSystemdService<MyService.Worker>(
    shutdownFactory: () => new Shutdown(),
    reloadFactory:   () => new Reload());

var host = builder.Build();
await host.RunAsync();
```

The adapter:

- Hooks `SIGTERM` (graceful stop) and `SIGHUP` (reload) via
  `PosixSignalRegistration`.
- Sends `sd_notify(READY=1)` once the whole host has started
  (`ApplicationStarted`).
- Sends `sd_notify(STOPPING=1)` on shutdown.

This package is `[SupportedOSPlatform("linux")]`, so referencing it from
non-Linux targets is a CA1416 warning. The native `sd_notify` calls
P/Invoke `libsystemd.so.0` on Linux only; the assembly loads fine on
other platforms but the systemd codepath short-circuits via
`SystemdHelpers.IsSystemdService()`.

## Lifecycle messages

The adapter speaks the shared lifecycle records from
Spek.Hosting.Abstractions: `Shutdown` and `Reload` in.
