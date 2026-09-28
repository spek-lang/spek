---
title: CLI
layout: default
parent: Reference
nav_order: 1
permalink: /reference/cli/
---

# `spekc`: the Spek compiler CLI

`spekc` is the command-line front-end for the compiler. It reads `.spek`
source files, runs them through the parser and semantic analyzer, and emits
C# source (`.g.cs`) that the normal .NET toolchain can pick up.

This page documents `compile`, the verb you will use most. The CLI carries
four others: `format` re-prints source canonically (see [Known
limitations](#known-limitations)), `proto-import` and `proto-export` bridge
channels to protobuf (covered with [gRPC hosting](../hosting/grpc.md)), and
[`observe`](observe.md) attaches a live actor table to a running
Spek process.

## Installation

Run `spekc` from the repository's `src/Spek.Cli` project:

```bash
dotnet run --project src/Spek.Cli -- compile path/to/file.spek
```

The rest of this page uses the `spekc` form for brevity. The arguments are
identical either way.

## Usage

```
spekc compile <file.spek> [--out <dir>] [--base <dir>] [--check] [--ref <dll>] [--no-line-map] [--abs-line-map] [--tests]
spekc compile <dir>       [--out <dir>]   # compiles *.spek in directory
```

At least one input path is required. Inputs can be mixed; individual files
and directories can be passed in the same invocation:

```bash
spekc compile src/Messages.spek src/Actors/ --out build/generated
```

Everything passed in one invocation compiles as **one unit**: a type declared
in one file resolves when referenced from another, so an enum can live in
`State.spek` while the message that carries it is declared in `Messages.spek`. The
same name declared twice across files (in the same namespace) is a
[CE0013](errors.md#ce0013). Files that only make sense together must
therefore be compiled together; one invocation, not one per file. (The
MSBuild target already does this: it hands the whole project's `.spek` set to
a single `spekc` call.)

## Arguments

### `<path>` (positional, required)

One or more paths. Each path is either:

- A `.spek` file, compiled directly.
- A directory: every `.spek` file in the directory (top level only, not
  recursive) is compiled.

Unknown paths produce a diagnostic and a non-zero exit code.

### `--out <dir>`

Redirect generated `.g.cs` output to `<dir>` instead of emitting next to
each source file. The directory is created if it doesn't exist.

Without `--out`, each `foo.spek` produces `foo.g.cs` in the same directory.

### `--base <dir>`

With `--out`, mirror each input's path *relative to `<dir>`* under the output
directory, so `src/sub/Foo.spek` becomes `<out>/sub/Foo.g.cs` instead of
`<out>/Foo.g.cs`. This keeps two same-named files in different folders from
colliding. The MSBuild target passes the project directory here; direct CLI
use rarely needs it.

```bash
spekc compile src/A.spek src/sub/B.spek --out gen --base src
# gen/A.g.cs and gen/sub/B.g.cs
```

### `--check`

After emitting, compile the generated C# in memory with Roslyn and report
any errors **on the `.spek` source lines** (via the `#line` mapping below).
This catches problems that Spek deliberately delegates to the C# compiler
(generic-constraint violations, bad casts, type mismatches in passthrough
code) at `spekc` time instead of at `dotnet build` time.

The check references the BCL and `Spek.Runtime`. Code that uses other
packages (ASP.NET Core, NuGet libraries) needs those assemblies passed with
`--ref`; otherwise their types report as missing.

### `--ref <dll>`

Add a metadata reference, repeatable. It serves two passes:

- **Invisible async.** The rewriter is BCL-seeded, so it auto-awaits
  framework Task APIs on its own. A `--ref` lets it also see
  Task-returning methods from *that* assembly, so e.g. ASP.NET Core's
  `app.RunAsync()` / `context.Response.WriteAsync()` auto-await instead of
  being left as un-awaited statements.
- **`--check`.** The referenced types resolve during the Roslyn check
  instead of reporting as missing.

```bash
spekc compile api.spek --check \
  --ref "$(dotnet --list-runtimes | ...)/Microsoft.AspNetCore.dll" \
  --ref libs/My.Validation.dll
```

(With the MSBuild target, pass these through the `SpekcExtraArgs` property.)

### `--no-line-map`

By default the emitted `.g.cs` carries `#line` directives mapping every user
statement back to its `.spek` line, so C# compiler errors, analyzer
warnings, and debugger stepping land on the Spek source rather than the
generated file. The directive paths are written relative to the output
directory, so they stay machine-neutral in generated files and
resolve correctly in IDEs. Pass `--no-line-map` to emit without directives
(e.g. when diffing generated output).

### `--abs-line-map`

Emit the `#line` paths as **absolute** paths instead of relative. This is for
[debugging](debugging.md): the generated `.g.cs` lives under `obj/`, so
a debugger resolves the `.spek` source from the PDB more reliably with an
absolute path than a relative one. The `Spek.targets` integration passes this
automatically on Debug builds, so you rarely type it by hand.

### `--tests`

Treat `*Tests`-named modules and classes as native test containers: each
public method emits as a test the `Spek.Testing.Xunit` adapter discovers
under `dotnet test`. The MSBuild integration passes this automatically for
test projects (those that set `IsTestProject`), so you type it by hand only
when compiling a test file directly. See
[Testing actors](../language/testing.md).

## Building .spek inside `dotnet build`

Instead of running `spekc` by hand, import [build/Spek.targets](https://github.com/spek-lang/spek/blob/develop/build/Spek.targets)
in a csproj:

```xml
<Import Project="..\..\build\Spek.targets" />
```

Every `*.spek` in the project transpiles to a `.g.cs` under `obj/` (the
intermediate output, like a source generator) before the C# compiler runs,
incrementally (an unchanged `.spek` is skipped). Nothing is written beside your
source, so there's no `.g.cs` to check in or `.gitignore`, and the generated
files join the compilation automatically, so a clean checkout builds with plain
`dotnet build` / `dotnet run`. The target invokes the
built `spekc.dll` (build `Spek.Cli` once first); point the `SpekcDll`
property elsewhere for a packaged tool, and pass extra compiler flags via
`SpekcExtraArgs`. The `samples/HelloBank` project is wired this way.

## Exit codes

| Exit code | Meaning                                                     |
|-----------|-------------------------------------------------------------|
| `0`       | All inputs compiled successfully.                           |
| `1`       | One or more inputs failed (missing file, parse error, semantic error, or a `--check` C# error). |

## Diagnostic format

Diagnostics render as annotated source blocks: the code and message on the
first line, a `-->` locator with the file, line, and column, then the
offending line with a caret underline pointing at the exact span.

```
error[CE0011]: 'become Missing' targets an undeclared behavior.
 --> path/to/file.spek:4:14
  |
4 |     init() { become Missing; }
  |              ^^^^^^^^^^^^^^
  |
```

The locator line is the standard `file:line:col` form terminals and IDEs
link on. The full catalog of `CEXXXX` codes is on the
[errors page](errors.md).

## Typical workflow

In a project you don't run `spekc` by hand. Import `build/Spek.targets`
(above), and the build does the rest:

1. Write `.spek` source files in your project.
2. Run `dotnet build` (or `dotnet run` / `dotnet test`). The target transpiles
   every `.spek` into `obj/` and adds the generated C# to the compilation.

For a quick one-off outside a project, invoke the CLI directly and point
`--out` at a directory the C# build already compiles:

```bash
spekc compile src/ --out obj/generated   # writes .g.cs into obj/generated
dotnet build
```

## Known limitations

- Recursive directory walking is not supported for `compile`: `spekc compile
  src/` only compiles `.spek` files at the top level of `src/` (pass
  subdirectories explicitly). `spekc format` *does* walk recursively; the
  asymmetry is a known wart.
- There is no `--watch` mode; pair with an external watcher such as `dotnet
  watch` or a file-system tool if you want rebuild-on-save.
