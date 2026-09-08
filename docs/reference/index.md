---
title: Reference
layout: default
nav_order: 9
has_children: true
permalink: /reference/
---

# Reference

Concrete, lookup-style documentation for the Spek toolchain and runtime.

- [CLI: `spekc compile`](cli.md): invocation surface, flags, typical workflow.
- [Error codes](errors.md): the full CE-code catalog with a triggering example
  for each.
- [Runtime](runtime.md): `ActorSystem`, `ActorRef`, `ISnapshotStore`,
  `FailureDirective`, and the TestKit surface.
- [Debugging](debugging.md): breakpoints, stepping, and call stacks
  in `.spek` source with the standard .NET debugger.
- [Observing: `spekc observe`](observe.md): attach a live, read-only
  actor table to a running Spek process by pid.

The [formal PEG grammar](../spek-v1-grammar.md) is the source of truth for
syntax; the pages in this section are the practical counterpart.
