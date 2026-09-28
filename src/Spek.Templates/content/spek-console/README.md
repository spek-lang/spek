# SpekHelloWorld

A minimal Spek console application scaffolded with `dotnet new spek-console`.

## Prerequisites

- .NET 10 SDK (or 9.0 / 8.0; pass `--framework` when scaffolding).

The project references the `Spek.Build` package, which runs the Spek
compiler on your `.spek` files during `dotnet build`. The compiler itself is
the `spekc` tool: install it once with `dotnet tool install -g Spek.Cli.Tool`,
or add a tools manifest (`dotnet new tool-manifest && dotnet tool install
Spek.Cli.Tool`) so the build restores it per-project.

## Build and run

```
dotnet run
```

Expected output:

```
Hello, world!
Goodbye.
```

## Project layout

| File                    | Purpose                                                              |
|-------------------------|----------------------------------------------------------------------|
| `Greeter.spek`          | The Spek source. Edit this; the build compiles it into `obj/`.     |
| `SpekHelloWorld.csproj` | Standard .NET SDK project; references `Spek.Runtime` and `Spek.Build`. |

## Next steps

- Add another `.spek` file; one actor per file is the Spek convention.
- Read `docs/language/actors.md` in the Spek repo for the language reference.
- Use `Spek.Testing` to write tests against your actors.
