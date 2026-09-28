---
title: Getting started
layout: default
nav_order: 3
permalink: /getting-started/
description: "Install the spekc toolchain and the MSBuild SDK, then compile and run a one-actor Spek program end to end."
---

# Getting started

Spek is an actor-model language that transpiles to C# and builds with the
ordinary `dotnet` pipeline. There's no runtime to install and no plugin to
your IDE: a `.spek` file becomes a `.g.cs` file, which the C# compiler turns
into the same IL it would have produced if you'd written the C# by hand.

This chapter gets that pipeline working on your machine. By the end you'll
have compiled and run a minimal one-actor program, and seen exactly what the
compiler emits. The [next chapter](language/first-actor.md) builds a real
program from scratch; this one is just the five-minute setup.

## Prerequisites

- **The .NET 10 SDK.** Spek targets `net10.0`. Check with `dotnet --version`.
- **Working knowledge of C#.** Spek is designed to feel familiar. Handler
  bodies are a [subset of C#](language/csharp-syntax.md), and Roslyn does the
  type-checking. If you can read C#, you can read Spek.
- **A C# editor** (Rider, Visual Studio, or VS Code with the C# Dev Kit). Any of
  them will pick up the generated C#. A Spek-aware [language server](reference/index.md)
  adds live diagnostics on the `.spek` source itself.

## Installing the toolchain

Spek is pre-release: the compiler is not on NuGet or distributed as a
`dotnet tool`. You run it from a working copy of the repository. There are two
pieces, and you only build the first one once.

**1. Build the compiler CLI (`spekc`).** It lives in the `src/Spek.Cli` project
and compiles a `.spek` file to a sibling `.g.cs`:

```bash
dotnet build src/Spek.Cli
```

That produces `spekc.dll`. You can invoke it directly, or through
`dotnet run`:

```bash
dotnet run --project src/Spek.Cli -- compile path/to/file.spek
```

The rest of this page writes `spekc compile …` for brevity. The arguments
are identical either way. See the [CLI reference](reference/cli.md) for the
full flag surface (`--out`, `--check`, `--ref`).

**2. Reference the runtime.** The generated C# uses types from
`Spek.Runtime`: `ActorSystem`, `ActorRef`, `ActorBase`. Any project that
hosts compiled Spek references it as an ordinary `ProjectReference`:

```xml
<ItemGroup>
  <ProjectReference Include="..\..\src\Spek.Runtime\Spek.Runtime.csproj" />
</ItemGroup>
```

## Your first program

Create a file `Hello.spek` with a single actor and an entry point:

<!-- spek-test: compile -->
```spek
namespace Hello;

message Greet(string name);

actor Greeter
{
    on Greet g =>
    {
        System.Console.WriteLine("Hello, " + g.name + "!");
    }
}

program Main
{
    var system = new ActorSystem("hello");
    ActorRef greeter = system.Spawn<Greeter>();

    greeter.Tell(new Greet("world"));

    system.AwaitTermination();
}
```

That's a complete program. Three declarations carry it, and each is one of
the language's core ideas. The chapters ahead cover them in depth; here they're
named just so the shape makes sense.

`message Greet(string name);` declares the only kind of value allowed to cross
an actor boundary. A `message` compiles to an immutable C# `record`, and sending
anything else is a compile error (see [messages](language/messages.md)).

`actor Greeter { … }` is an isolated unit of state and behavior. The
`on Greet g => { … }` handler says "when a `Greet` arrives, run this." A
single-handler actor needs no `behavior` wrapper. There's more on
[actors and behaviors](language/actors.md) later.

`program Main { … }` is the entry point. It creates an `ActorSystem` (the
runtime that hosts actors), `Spawn`s the `Greeter` to get an `ActorRef` (a
handle you can send to), `Tell`s it a message (fire-and-forget), and
`AwaitTermination()` blocks until the system shuts down.

Don't worry about absorbing all of that now. The [next chapter](language/first-actor.md)
introduces each piece one at a time. Right
now the goal is just to make it run.

## Compiling and running

Compiling is two steps: `spekc` turns the `.spek` into C#, then the normal
.NET toolchain takes over.

```bash
spekc compile Hello.spek
dotnet run
```

`spekc compile` writes `Hello.g.cs` next to `Hello.spek`. A directory argument
(`spekc compile src/`) batch-compiles every `.spek` file in that directory,
and `--out <dir>` redirects the emitted C# elsewhere. Run the program and you
should see:

```text
Hello, world!
```

### What the compiler emitted

Open `Hello.g.cs` once to see that there's no magic: Spek
lowers to exactly the C# you'd expect. The `message` became a `record`, the
`actor` became a class deriving from `Spek.ActorBase`, and `program Main`
became a normal `Main` entry point:

```csharp
public record Greet(string name);

internal sealed class Greeter : Spek.ActorBase
{
    // ... dispatch plumbing ...
    private async Task Default_HandleAsync(object _msg, Spek.ActorRef _sender)
    {
        switch (_msg)
        {
            case Greet g:
                System.Console.WriteLine(("Hello, " + g.name) + "!");
                break;
            // ...
        }
    }
}

public static class MainProgram
{
    public static async System.Threading.Tasks.Task Main(string[] args)
    {
        var system = new ActorSystem("hello");
        ActorRef greeter = system.Spawn<Greeter>();
        greeter.Tell(new Greet("world"));
        system.AwaitTermination();
    }
}
```

The emitted file carries `#line` directives (elided from the listing above)
that map every statement back to your `.spek` source, so compiler errors, analyzer warnings, and debugger
stepping all land on the Spek line, not the generated one. (Pass
`--no-line-map` to omit them, which is handy when diffing generated output.)

## Building inside `dotnet build`

Running `spekc` by hand is fine for a quick try, but you don't want to
remember it before every build. The **Spek MSBuild SDK** wires the
transpilation into `dotnet build` itself. Import `build/Spek.targets` from
your `.csproj`:

```xml
<Project Sdk="Microsoft.NET.Sdk">

  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net10.0</TargetFramework>
    <Nullable>enable</Nullable>
    <ImplicitUsings>enable</ImplicitUsings>
  </PropertyGroup>

  <Import Project="..\..\build\Spek.targets" />

  <ItemGroup>
    <ProjectReference Include="..\..\src\Spek.Runtime\Spek.Runtime.csproj" />
    <!-- Build-order only: ensures spekc.dll is built before the transpile runs. -->
    <ProjectReference Include="..\..\src\Spek.Cli\Spek.Cli.csproj" ReferenceOutputAssembly="false" />
  </ItemGroup>

</Project>
```

With that import in place, every `*.spek` in the project transpiles to a
`.g.cs` under `obj/` (like a source generator) *before* the C# compiler runs,
and the generated files join the compilation automatically. The build-order
reference to `Spek.Cli` means the compiler is built for you, so a clean
checkout, with no `.g.cs` beside your source or checked in, builds and runs in
one step:

```bash
dotnet run
```

The target is incremental (a `.spek` older than its `.g.cs` is skipped) and
passes your project's references to the compiler, so framework `Task` APIs
[auto-await](language/async.md) correctly. The `samples/HelloBank` project is
wired exactly this way. Copy its `.csproj` as a starting point.

If you ran `spekc compile` by hand earlier, delete the `.g.cs` it left beside
your source first: the SDK would compile both it and the `obj/` copy and report
duplicate types. With the target in place you never run `spekc` yourself.

{: .note }
> Because Spek emits ordinary C#, your project consumes NuGet packages,
> references other C# projects, and ships as a normal `dotnet` build artifact
> with no Spek-specific runtime step. The only build-time addition is the
> transpile, and the MSBuild SDK hides even that.

## Where to go next

You have a working pipeline. Now learn the language:

- [Build your first actor](language/first-actor.md): the hands-on tour. A
  counter grows into a bank account, one concept per step, with the compiler
  as your teacher.
- [Language overview](language/index.md): the full syntax, one page at a time.
- [Samples](samples.md): `HelloBank` and other runnable, real-world-shaped
  programs.
