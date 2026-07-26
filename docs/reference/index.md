---
title: Reference
layout: default
nav_order: 9
has_children: true
permalink: /reference/
---

# Reference

Concrete, lookup-style documentation for the Spek toolchain and runtime.

- [CLI: `spekc compile`](/reference/cli/): invocation surface, flags, typical workflow.
- [Error codes](/reference/errors/): the full CE-code catalog with a triggering example
  for each.
- [Runtime](/reference/runtime/): `ActorSystem`, `ActorRef`, `ISnapshotStore`,
  `FailureDirective`, and the TestKit surface.
- [Debugging](/reference/debugging/): breakpoints, stepping, and call stacks
  in `.spek` source with the standard .NET debugger.
- [Observing: `spekc observe`](/reference/observe/): attach a live, read-only
  actor table to a running Spek process by pid.

The [formal PEG grammar](https://github.com/spek-lang/spek/blob/main/docs/spek-v1-grammar.md) in the repository's
`docs/spek-v1-grammar.md` is the source of truth for syntax; the pages in
this section are the practical counterpart.
