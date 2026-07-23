# SpekTests

A Spek test project. Tests are written **in Spek**: every public method of a
`*Tests` class (or module) is an xUnit test, discovered by `dotnet test`, IDE test
explorers, and CI. There is no test keyword and no attributes; the `Tests` suffix
is the signal.

## Run the tests

```bash
dotnet tool restore   # installs the pinned `spekc` compiler (once)
dotnet test
```

`dotnet test` compiles the `.spek` files to C# (via the **Spek.Build** MSBuild
integration calling `spekc`) and runs the emitted tests.

## How it fits together

- **`CalculatorTests.spek`** — an actor and a `CalculatorTests` class. `init` runs
  before each test on a fresh instance, so state resets automatically; the
  `TestActorSystem` field is disposed after each test.
- **`.config/dotnet-tools.json`** — pins `Spek.Cli.Tool` (`spekc`) as a local
  tool, so the build is reproducible. `dotnet tool restore` fetches it.
- **`SpekTests.csproj`** — references `Spek.Runtime`, the `Spek.Testing.Xunit`
  adapter, and `Spek.Build` (which compiles `.spek` during the build).

## Write a test

Drive the actor under test through `TestActorSystem` / `TestProbe` and assert with
`Xunit.Assert` or the probe's `ExpectMsg<T>`. For supervision, `sys.ExpectStop` /
`sys.ExpectRestart` / `sys.RestartCountOf`. For per-test output, take an
`Xunit.Abstractions.ITestOutputHelper` parameter in `init`.

See the [testing guide](https://github.com/spek-lang/spek/blob/main/docs/language/testing.md).
