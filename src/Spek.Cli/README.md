# Spek.Cli.Tool

`spekc` is the command-line compiler for the
[Spek](https://github.com/spek-lang/spek) actor language.

## Install

```
dotnet tool install -g Spek.Cli.Tool
```

## Usage

```
spekc compile MyActor.spek
spekc compile ./src --out ./gen
```

Run by hand, `spekc` writes each `foo.g.cs` next to its `foo.spek` (or into the
`--out` directory). Inside a project you normally don't call it directly: the
[`Spek.targets`](https://github.com/spek-lang/spek/blob/main/build/Spek.targets)
MSBuild integration compiles every `.spek` into `obj/` as a build artifact,
like a source generator, so nothing lands beside your source or gets checked in.
Either way the emitted C# references the `Spek.Runtime` NuGet package, so add
that to your `.csproj` for the compiled code to build.

## Status

Public **0.x preview**. Packages are versioned `0.<era>.<build>`; the whole 0.x
line is the preview, with no `-alpha` or `-preview` suffix. Pre-1.0, not for
production.

## License

Apache-2.0.
