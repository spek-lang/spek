---
title: Hosting
layout: default
nav_order: 8
has_children: true
permalink: /hosting/
---

# Hosting

A Spek `program` block compiles to a `static async Task Main`, so
you can run it on any of the standard .NET host adapters. Spek
ships integrations for the common cases:

- **[REST](/hosting/rest/)**: expose a channel as HTTP endpoints
  via ASP.NET Core. Convention-derived routes for the common
  CRUD shapes; explicit overrides for everything else.
- **[gRPC](/hosting/grpc/)**: expose a channel as a gRPC service
  via `Grpc.AspNetCore.Server`. Proto-first and code-first
  workflows via `spekc proto-import` and `proto-export`.
- **[Observability](/hosting/observability/)**: metrics, traces,
  and structured logs via the `Spek.Observability.*` packages.
  Always-on, zero-overhead when no adapter is registered.
- **Console** (`Spek.Hosting.Console`): run an actor system
  as a long-lived console app. The default for getting started.
- **Generic Host** (`Spek.Hosting.AspNetCore`): run as an
  `IHostedService` so Spek composes with ASP.NET Core's
  lifetime, DI, and configuration. The base layer the REST
  hosting builds on top of.
- **Windows Service** (`Spek.Hosting.WindowsService`): SCM
  adapter, integrates with the Windows Service Control Manager.
- **systemd** (`Spek.Hosting.Systemd`): `sd_notify` integration
  for Linux service deployments.
- **launchd** (`Spek.Hosting.Launchd`): vproc transactions for
  macOS daemon deployments.

Hosting is intentionally separate from the language. A Spek
program describes *behavior*; the host decides *how it runs*.
The same actor can be hosted as a console app today and a
Windows service tomorrow without changing the `.spek` source.
