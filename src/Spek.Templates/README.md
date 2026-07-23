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

- `spek-console` — a minimal Spek console application: one message, one actor, one
  `program Main` entry point. Demonstrates the expected project structure and the
  MSBuild-integrated build via Spek.Build.

## License

Apache-2.0.
