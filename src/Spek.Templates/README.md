# Spek.Templates

`dotnet new` templates for the [Spek](https://github.com/spek-lang/spek) actor language.

## Install

```
dotnet new install Spek.Templates
```

## Use

```
dotnet new spek-console -n MyApp
cd MyApp
dotnet run
```

See the template's own `README.md` inside the scaffolded project for full instructions.

## Templates included

- `spek-console`, a minimal Spek console application: one message, one actor, one
  `program Main` entry point. Demonstrates the expected project structure and the
  MSBuild-integrated build via Spek.Build.
- `spek-test`, a native Spek test project: `test "..."` blocks wired for
  `dotnet test`, with a tools manifest for the language server.

## License

Apache-2.0.
