---
title: Debugging
layout: default
parent: Reference
nav_order: 4
permalink: /reference/debugging/
description: "Debug .spek programs with the standard .NET debugger: Spek emits #line directives so breakpoints, stepping, and call stacks map back to your Spek source, no custom debug adapter required."
---

# Debugging Spek

Spek has no debugger of its own, and it doesn't need one. Because a `.spek`
file compiles to ordinary C# and then to a normal .NET assembly, the debugger
you already use for C#, the one in VS Code, Visual Studio, or Rider, can step
through your Spek source directly. The compiler emits `#line` directives into
the generated C#, so the PDB the C# compiler produces maps every instruction
back to the `.spek` line it came from. Set a breakpoint in a handler, and it
breaks there; look at the call stack, and it names your actors and behaviors.

There is no Spek debug adapter to install and no separate launch flow to learn.
This page is about pointing the standard .NET debugger at a Spek program.

## How the mapping works

The chain is short and entirely standard:

1. `spekc` (or the MSBuild integration) transpiles `Foo.spek` to `Foo.g.cs`,
   interleaving `#line N "Foo.spek"` directives that tell the C# compiler
   which `.spek` line each statement corresponds to.
2. The C# compiler honors those directives when it writes the **PDB** (the
   symbol file), so the PDB maps IL offsets to `.spek` lines, not `.g.cs` lines.
3. The .NET debugger reads the PDB. When execution reaches an instruction, it
   looks up the source line, which resolves to your `.spek`, and highlights
   it, breaks on it, and shows it in the stack.

The one requirement this places on the build is that the debugger has to be
able to *find* the `.spek` file from the path recorded in the PDB. Because the
generated `.g.cs` lives under `obj/`, a path relative to it would be brittle, so
**Debug builds emit absolute `#line` paths automatically** (see
[Source paths](#source-paths) below).

## Prerequisites

- **A Debug build.** Debug is what produces a PDB and disables the
  optimizations that make stepping jumpy. `dotnet build` and `dotnet run` default to it. (A Release build with `DebugType=portable` can be debugged too,
  but expect optimized-code stepping.)
- **A .NET debugger.** In VS Code that's the
  [C# extension](https://marketplace.visualstudio.com/items?itemName=ms-dotnettools.csharp)
  (or C# Dev Kit), which ships the `coreclr` debug type. Visual Studio and Rider
  have it built in.
- **The Spek build integration** (`Spek.targets`) or a `spekc compile` that
  emits `#line` directives; both do by default.

## VS Code launch configuration

Debugging a Spek program is a plain `coreclr` launch config pointing at the
built DLL. Put this in `.vscode/launch.json`:

```json
{
  "version": "0.2.0",
  "configurations": [
    {
      "name": "Debug Spek program",
      "type": "coreclr",
      "request": "launch",
      "preLaunchTask": "build",
      "program": "${workspaceFolder}/bin/Debug/net10.0/MyApp.dll",
      "cwd": "${workspaceFolder}",
      "console": "integratedTerminal",
      "stopAtEntry": false
    }
  ]
}
```

Replace `MyApp.dll` with your assembly name. Add a matching `build` task (the C#
extension offers to generate one, or run
`dotnet build` from a `tasks.json` entry). Press **F5**, and breakpoints set in
your `.spek` files are honored.

Visual Studio and Rider need no launch file: open the solution, set the Spek
project as the startup project, put a breakpoint in a `.spek` handler, and start
debugging.

## What you can and can't see

Stepping and breakpoints work on `.spek` lines, and the **call stack** shows
your actors and behaviors, because those are real C# classes and methods with
the names you wrote. A few things reflect that you are, underneath, debugging
generated C#:

- **Locals and watches** show the emitted shapes: an actor's `state` fields, the
  message record's properties, and occasionally a compiler-generated temporary.
  These read naturally (they carry your names) but they are the C# view.
- **Async handlers** (anything the [invisible async](/language/async/) pass
  rewrote) step through the compiler's async state machine. Breakpoints on the
  `.spek` lines still land; stepping *over* an `await` the compiler inserted
  behaves like any C# `await`.
- **Breakpoints bind to executable lines.** A line that maps only to a
  declaration with no emitted statement may bind to the nearest following
  statement, exactly as in C#.

If you ever want to see the C# a breakpoint sits on, the VS Code extension's
experimental **"Spek: Show Emitted C#"** command
(behind `spek.experimental.showEmittedCSharp`) prints it.

## Source paths

By default `spekc` writes **relative** `#line` paths, which keeps any
checked-in generated file machine-neutral. For debugging, the path has to
resolve from wherever the PDB is consulted, so:

- **Under `dotnet build`**, the [`Spek.targets`](/reference/cli/) integration
  passes `--abs-line-map` for you on **Debug** builds, emitting absolute paths.
  Release builds keep relative paths.
- **Invoking `spekc` directly**, add `--abs-line-map` to emit absolute paths:

  ```bash
  spekc compile MyActor.spek --out obj/spek --abs-line-map
  ```

Force either style with the `SpekcLineMapFlag` MSBuild property (set it to
`--abs-line-map` or empty), or drop `#line` entirely with `--no-line-map`; the debugger then steps
through the `.g.cs`.

## Related reading

- [`spekc` CLI](/reference/cli/): the `--abs-line-map`, `--no-line-map`, and
  `--out` options.
- [Async without await](/language/async/): why async handlers step through a
  state machine.
- [Testing actors](/language/testing/): the test kit, and running tests (which
  you can also debug with the same `coreclr` setup pointed at the test host).
