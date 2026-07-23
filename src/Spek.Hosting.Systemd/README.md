# Spek.Hosting.Systemd

Linux systemd host adapter for [Spek](https://github.com/spek-lang/spek). Routes
`SIGTERM` and `SIGHUP` into Spek messages, and emits `sd_notify`
back-channel events (READY=1, STOPPING=1, RELOADING=1, WATCHDOG=1,
STATUS=...) when the actor produces them.

## Quick start

```spek
// Worker.spek — uses the canonical Shutdown / Reload / Started /
// HostState / StateChanged / HealthReport records exported by
// Spek.Hosting.Abstractions.
namespace MyService;

public actor Worker : LinuxServiceHost
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
        // emit Started — translates to sd_notify READY=1 once the host is wired
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
- Sends `sd_notify(READY=1)` once the actor has spawned and any
  `Started` emit fires.
- Sends `sd_notify(STOPPING=1)` on shutdown, `RELOADING=1` on reload,
  and `STATUS=<msg>` whenever a `HealthReport` is emitted.
- Periodically pings `WATCHDOG=1` if the unit file declares
  `WatchdogSec=`.

This package is `[SupportedOSPlatform("linux")]`, so referencing it from
non-Linux targets is a CA1416 warning. The native `sd_notify` calls
P/Invoke `libsystemd.so.0` on Linux only; the assembly loads fine on
other platforms but the systemd codepath short-circuits via
`SystemdHelpers.IsSystemdService()`.

## Channel coverage

This adapter satisfies the `LinuxServiceHost` channel from the
hosting guide in the Spek documentation.
