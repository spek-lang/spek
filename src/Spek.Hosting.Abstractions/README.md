# Spek.Hosting.Abstractions

Standard lifecycle messages and the `HostState` enum used across
every Spek hosting adapter.

## What's in here

- **Lifecycle messages** for actor → host signalling: `Shutdown`,
  `Pause`, `Continue`, `Reload`, `Started`, `PowerEvent`,
  `SessionChange`, `CustomCommand`, `StateChanged`, `HealthReport`.
- **`HostState`** enum: `Starting`, `Running`, `Paused`, `Stopping`,
  `Stopped`, `Degraded`.

## Why this is its own package

Following Microsoft's pattern for hosting abstractions
(`Microsoft.Extensions.Hosting.Abstractions`): if you want to
write a custom Spek hosting adapter (Cloud Foundry, Azure
Container Apps, custom orchestrator, etc.), you reference this
package for the standard message vocabulary without pulling any
specific adapter implementation as a dependency.

The shipped adapter packages (`Spek.Hosting.Console`,
`Spek.Hosting.WindowsService`, `Spek.Hosting.AspNetCore`,
`Spek.Hosting.Systemd`, `Spek.Hosting.Launchd`) all reference
this and re-export the same messages, so your `.spek` source
declares them once and they work across every hosting target.
