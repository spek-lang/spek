---
title: Observing
layout: default
parent: Reference
nav_order: 5
permalink: /reference/observe/
description: "Attach spekc observe to a running Spek process for a live, read-only actor table: behavior, mailbox depth, restarts, last message; over the same port-free diagnostics channel dotnet-counters uses."
---

# `spekc observe`: watching a live system

Every Spek process carries a live introspection surface. Attach `spekc
observe` to one by process id and it prints a once-per-second table of every
actor in the system: the active behavior, how deep the mailbox is, how many
times supervision has restarted it, and the type of the last message it
dispatched. Nothing has to be enabled in the program and no package added. If it runs on `Spek.Runtime`, it can be observed.

Run it the way the [CLI page](cli.md) runs the other verbs:

```bash
dotnet run --project src/Spek.Cli -- observe <pid>
```

The rest of this page uses the `spekc` form for brevity.

## Usage

```text
spekc observe <pid> [--actor <path>] [--once] [--json]
```

### `<pid>` (positional, required)

The process id of the running Spek program. `dotnet-counters ps` lists the
.NET processes on the machine, or use plain `ps`.

### `--actor <path>`

Drill into a single actor instead of rendering the whole table. The path is
the identity shown in the table's `ACTOR` column. A path that matches nothing
produces a one-line reply listing the paths that do exist.

### `--once`

Print one sample and exit. Without it, `observe` keeps printing a fresh
sample every second until you interrupt it with Ctrl-C.

### `--json`

Emit one JSON object per sample on stdout (NDJSON) instead of the rendered
table, for piping into `jq` or a dashboard collector. Errors become
`{"error": ...}` objects rather than prose. Combine with `--once` for a single machine-readable
snapshot.

## The actor table

Once per second the target process emits one sample per actor system, and
`observe` renders each sample as a table:

```text
$ spekc observe 48213 --once
[orders]
ACTOR                    BEHAVIOR      MAILBOX  RESTARTS  LAST MSG
Checkout                 Active              0         0  PlaceOrder
Payments                 Charging           12         2  Charge
ledger-eu-1              -                   0         0  Settle (passivated)
Mailer                   -                   0         1  SendReceipt (stopped)
```

The bracketed header is the actor system's name; a process hosting several
systems prints one table per system.

| Column     | Meaning |
|------------|---------|
| `ACTOR`    | Stable display identity: the persistence key for actors spawned with one (`ledger-eu-1`), otherwise the type name, suffixed `#N` from the second instance of a type on. Identities are stable across samples, so successive tables correlate. |
| `BEHAVIOR` | The active behavior's name, or `-` when the actor has no behaviors or is not currently in memory. |
| `MAILBOX`  | Messages pending in the mailbox at the instant of the sample. |
| `RESTARTS` | Supervision restarts observed, counting both the actor's own failures and sibling-caused restarts under all-for-one. |
| `LAST MSG` | The type name of the most recently dispatched message, or `-` before the first dispatch. |

Two lifecycle markers can follow the last column. `(stopped)` flags an actor
that has stopped, voluntarily or by supervision; `(passivated)` flags one
whose in-memory instance has been released. Both keep their rows for the
life of the system, so the restart count and last message type of an actor
that died remain visible, which is often what you attached to find out.

## Drilling into one actor

`--actor` switches to a detail view of a single actor:

```text
$ spekc observe 48213 --actor Payments --once
Payments  (Payments, up 02:14:09)
  behavior:  Charging
  mailbox:   12 pending; head: Charge x7, Refund x1
  restarts:  2
  last msg:  Charge
  state:     live
  children:  Retry, Retry#2
```

The header line adds the actor's type and its uptime. `mailbox` goes beyond
the depth: it groups the type names of the first few pending messages (up
to eight) with counts, so a backed-up actor shows what its backlog is made
of, not only how deep it is. `state` is `live`, `passivated`, or `stopped`,
and `children` lists child actors by the same display identities the table
uses, so a child's path feeds straight into another `--actor`.

## The transport: diagnostics IPC, not a port

`spekc observe` attaches the way `dotnet-counters` does. It opens an
EventPipe session over the .NET diagnostics IPC channel that every .NET
process exposes (a Unix domain socket, or a named pipe on Windows) and
subscribes to the runtime's `Spek-Introspection` event provider. While at
least one session is attached, the runtime samples each actor system once
per second and publishes the table as an event. Detach, and the sampling stops. When nobody is attached, the entire cost to the program is one weak
reference per actor system.

The alternatives were a web dashboard or a TCP command server, and both
were rejected for the same reason: they make the program listen on a port.
A listening socket is something to configure, firewall, authenticate, and
audit in every deployment, for a facility most processes never use. The
diagnostics channel already exists in every .NET process, opens nothing
new, and inherits the operating system's user boundary: attaching requires
being the OS user that owns the process (or root). It also matches muscle
memory: find the pid, attach, watch, Ctrl-C is the `dotnet-counters
monitor` workflow.

Observation is read-only and non-perturbing by construction. The sampler
reads counters and takes cheap queue snapshots; it never locks a mailbox,
never injects a message, and never touches actor state. The one-second
cadence runs on wall-clock time even when the program itself runs under a
virtual test clock, because the observer lives outside the program's notion
of time.

## Metadata only

Every field in a sample is metadata the runtime already owns: behavior
name, queue depth and head types, restart count, lifecycle facts. Actor
field contents never leave the process. A state dump would leak whatever
the actor holds (balances, credentials, personal data) and needs a
redaction story before it can exist; until it has one, the introspection
surface stays metadata-only.

## When the target isn't a Spek process

A pid that isn't a .NET process has no diagnostics channel to attach to, so
`observe` fails immediately with `cannot attach to pid <pid>` and exits 1.
A .NET process that doesn't host a Spek `ActorSystem` is quieter: the
session opens, because the channel is a .NET facility rather than a Spek
one, but no `Spek-Introspection` samples ever arrive and `observe` waits
silently. If the session ends without a sample (the target exits, say), it
prints `session ended before a sample arrived; is this a Spek process?`
and exits 1; otherwise, end the wait with Ctrl-C.

## Exit codes

| Exit code | Meaning                                                       |
|-----------|---------------------------------------------------------------|
| `0`       | At least one sample was rendered.                             |
| `1`       | Bad arguments, attach failure, or the session ended before a sample arrived. |

## Related reading

- [Observability](../hosting/observability.md): metrics, traces, and structured
  logs for dashboards and history. `observe` answers "what is this process
  doing right now"; the OpenTelemetry pipeline answers the same questions
  with a time axis.
- [Runtime](runtime.md): the same data is available in-process as
  `ActorSystem.SnapshotActors()`, which returns the read-only
  `ActorSnapshot` records the table is rendered from.
- [CLI](cli.md): the `compile` verb and the MSBuild integration.
