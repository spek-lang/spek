# Spek.Hosting.Console

Cross-platform console host adapter for [Spek](https://github.com/spek-lang/spek).
Routes terminal lifecycle events into a Spek actor's `on Shutdown`
handler.

## Quick start

```spek
// MyApp.spek — uses the canonical Shutdown from Spek.Hosting.Abstractions
namespace MyApp;

public actor MyApp : ConsoleHost
{
    behavior Running
    {
        on Shutdown =>
        {
            // graceful cleanup
            return 0;            // exit code, surfaced as Environment.ExitCode
        }
    }
}
```

```csharp
// Program.cs
using Spek;                       // ActorRef
using Spek.Hosting;               // canonical Shutdown record
using Spek.Hosting.Console;
using Spek.Runtime;               // ActorSystem

return await SpekConsoleHost.RunAsync<global::MyApp.MyApp>(
    shutdownFactory: () => new Shutdown());
```

The adapter wires:

| Source | Spek message |
|---|---|
| `Ctrl+C` (`SIGINT` on Unix) | `Shutdown` |
| `Ctrl+Break` (Windows) | `Shutdown` |
| `SIGTERM` (Unix) | `Shutdown` |
| `SIGHUP` (Unix) | `Shutdown` |

`Shutdown` ships in `Spek.Hosting.Abstractions` as the canonical
lifecycle record. The adapter takes a factory because some users
prefer to declare their own `message Shutdown(string reason);` (or
similar) inside their Spek source. Pass a factory that returns
*your* type and the adapter is happy. If you don't need extra fields,
use the canonical `new Shutdown()` and skip the redeclare.

## Exit code

The handler's `return <int>;` is collected via `_currentSender.Tell`
(Option D reply) and surfaced as the process exit code. If the
handler has no return, the adapter defaults to `0`.

## Channel coverage

This adapter satisfies the `ConsoleHost` channel from the
hosting guide in the Spek documentation.
Other host channels live in their own packages
(`Spek.Hosting.WindowsService`, `Spek.Hosting.Systemd`,
`Spek.Hosting.Launchd`, `Spek.Hosting.AspNetCore`).
