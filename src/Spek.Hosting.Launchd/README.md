# Spek.Hosting.Launchd

macOS launchd host adapter for [Spek](https://github.com/spek-lang/spek). Routes
`SIGTERM` (launchd's `bsd.signal` for `launchctl unload` and shutdown)
into a Spek actor's `Shutdown` message and wraps the process in a
`vproc_transaction` so launchd doesn't idle-kill busy actors.

## Quick start

```spek
// Worker.spek — uses the canonical Shutdown record from
// Spek.Hosting.Abstractions.
namespace MyDaemon;

public actor Worker : LaunchdHost
{
    behavior Running
    {
        on Shutdown =>
        {
            // graceful cleanup
            return 0;
        }
    }
}
```

```csharp
// Program.cs
using Microsoft.Extensions.Hosting;
using Spek.Hosting;               // canonical Shutdown record
using Spek.Hosting.Launchd;

var builder = Host.CreateApplicationBuilder(args);
builder.Services.AddSpekLaunchdService<MyDaemon.Worker>(
    shutdownFactory: () => new Shutdown());

var host = builder.Build();
await host.RunAsync();
```

Install with a `.plist` in `/Library/LaunchDaemons/`:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN"
  "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>Label</key>      <string>com.example.mydaemon</string>
    <key>ProgramArguments</key>
    <array>
        <string>/usr/local/bin/MyDaemon</string>
    </array>
    <key>RunAtLoad</key>  <true/>
    <key>KeepAlive</key>  <true/>
    <key>EnableTransactions</key>  <true/>
</dict>
</plist>
```

`EnableTransactions` is what enables the `vproc` integration the
adapter wraps. Without it the call is a no-op and launchd treats the
process as ordinarily killable.

## What's intentionally minimal

launchd has no `sd_notify` equivalent. There is no readiness signal,
no watchdog protocol, no freeform status string. Health is inferred
from process exit code and `ThrottleInterval`. So this adapter has
nothing to emit back beyond the transaction-busy hint, and the
`LaunchdHost` channel stays small (`on Shutdown;` only).

This package is `[SupportedOSPlatform("macos")]`.
