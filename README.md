# Spek

Spek is an actor-based, C#-inspired programming language for .NET, with
compile-time concurrency safety.

Shared mutable state, locks, and data races are gone by design. In their place
are isolated actors that talk only through immutable messages. Spek is a real
compiled language with its own syntax and toolchain: it plugs into the normal
`dotnet` build pipeline, produces ordinary .NET assemblies, and works with the
entire .NET ecosystem.

> [!WARNING]
> **Spek is pre-release software and is _not_ supported for production use, in any
> way, shape, or form.** It is shared in the open-source spirit: free to read, run,
> fork, and learn from, but with **no warranty, no support, and no stability
> guarantees**. Syntax, APIs, and on-disk/wire formats can change without notice.
> If you run it, **you are on your own.** ([Apache-2.0](LICENSE).)

<!-- spek-test: compile -->
```spek
message Greet(string name);
message Greeting(string text);

actor Greeter
{
    // Single-behavior actors need no `behavior { }` wrapper or `become`.
    on Greet g => { return new Greeting($"Hello, {g.name}!"); }
}
```

## Why Spek

Spek is for C# developers who want the actor model without the ceremony of
Akka.NET or Orleans, and with stronger compile-time guarantees than either.

The compiler enforces the rules you would otherwise hold in your head. There is
no shared mutable state, checked by `CE####` diagnostics rather than convention:
a value is either shared-and-immutable or owned-and-mutable, never both. Messages
are immutable because the `message` keyword compiles to a C# `record`, and only
declared messages can be sent.

Async is invisible. You write no `async` or `await`; task-returning calls are
auto-awaited where their value is used, independent work runs concurrently by
default, and cancellation is threaded into the generated code for you rather than
written by hand. When something fails, hierarchical supervision decides what
happens next under a "let it crash" model, with `Resume`, `Restart`, `Escalate`,
and `Stop` directives and retry budgets.

The rest is C# you already know: visibility modifiers, namespaces, generics, type
syntax, and NuGet packages consumed natively. A Language Server delivers live
diagnostics and quick-fixes to any LSP-aware editor, including VS Code, Neovim,
Emacs, Helix, and Zed.

## Status

Spek is in active pre-1.0 development and ships publicly as a phased 0.x preview.
Packages are versioned `0.<era>.<build>`; the whole 0.x line is the preview, with
no `-alpha` or `-preview` suffix. `1.0` is a later stability commitment, not
the launch. The language surface, runtime, persistence, clustering, hosting
adapters, test kit, and Language Server are all implemented and covered by a
large test suite. See the issue tracker for planned work and the GitHub
Releases page for what's shipped.

> ⚠️ The cluster TCP transport (`Spek.Cluster.Tcp`) is **not yet encrypted or
> authenticated** (TLS/mTLS is planned but not built yet). Keep clusters on a trusted
> network for now. See its README.

## Quick start (from source)

Requires the .NET 10 SDK.

```bash
# Build everything and run the test suite
dotnet build src/Spek.slnx
dotnet test src/Spek.slnx

# Compile a .spek file to C# with the spekc CLI
dotnet run --project src/Spek.Cli -- compile samples/HelloBank/HelloBank.spek

# Or just build a sample (the Spek.targets MSBuild integration regenerates the C#)
dotnet run --project samples/Modules/Modules.csproj
```

For day-to-day use, the MSBuild integration (`Spek.targets`) compiles `.spek`
files as part of a normal project build, so there's no manual `spekc` step.

## Documentation

- [Getting started: build your first actor](docs/language/first-actor.md)
- [Isolation & ownership](docs/language/isolation.md): the one idea the language
  follows from
- [Language reference](docs/language/): actors, messages, channels, modules,
  classes, generics, async, persistence, supervision, clustering, and more
- [Async & concurrency](docs/language/async.md): invisible async plus cancellation
- [Error codes (`CE####`)](docs/reference/errors.md): every diagnostic with a
  triggering example
- [Testing](docs/language/testing.md): write tests in Spek (a `*Tests`
  module/class; each public method is a test), run them with `dotnet test`
- The issue tracker: planned/incomplete work
- GitHub Releases: shipped history, release by release
- [Samples](samples/): `HelloBank`, `Modules`, `Concurrency`, `Transport`, and
  the REST/gRPC/web hosting examples

## Repository layout

```
src/
  Spek.Compiler/       lexer → parser → AST → semantic analysis → C# emitter
  Spek.Runtime/        ActorSystem, supervision, persistence, shared regions
  Spek.Persistence.*   snapshot stores (File / SQLite / append-only Log)
  Spek.Cluster.*       distributed actors (in-memory + TCP transports)
  Spek.Hosting.*       Console / AspNetCore / Windows Service / systemd / launchd
  Spek.Resilience.*    ingress policies (rate limiting)
  Spek.Testing/        TestActorSystem + TestProbe (xUnit-based)
  Spek.Cli/            the `spekc` command-line compiler
  Spek.LanguageServer/ LSP for editor diagnostics + quick-fixes
samples/               runnable example programs
tools/                 the VS Code extension
build/                 MSBuild glue that compiles `.spek` files in a project
```

## License

[Apache-2.0](LICENSE) © 2026 William Holroyd.
