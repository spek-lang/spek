# Contributing to Spek

Thanks for your interest in Spek, an actor-based, C#-inspired language for
.NET. The compiler lowers `.spek` source to C#, which builds against the Spek
runtime. This guide covers building, testing, how the compiler is organized,
and the conventions the codebase follows. It is the authoritative guide for
humans and coding agents alike.

## Prerequisites

- The **.NET 10 SDK**.
- **Java 11+**, once, to generate the ANTLR parser (below). The bundled script
  downloads the ANTLR jar on demand.

## First build

The generated ANTLR parser (`src/Spek.Compiler/Grammar/Generated/`) is not
committed, so a fresh clone needs one command before the first build:

```bash
./src/Spek.Compiler/Grammar/regenerate.sh
```

After that:

```bash
dotnet build src/Spek.slnx
dotnet test  src/Spek.slnx
./scripts/ci.sh          # build + test + samples, exactly what CI runs
```

The test suite is the authoritative health check, so keep it green. Run
`./scripts/ci.sh` before opening a PR.

## Where things live

```
src/                 all shipping projects + Spek.slnx (the solution)
  Spek.Compiler/     ANTLR grammar → AST → semantic analysis → C# emitter
  Spek.Runtime/      ActorSystem, supervision, persistence, shared regions
  Spek.Cli/          spekc — the command-line compiler
  Spek.Tests/        C# test suite (xUnit)
  Spek.Tests.Native/ the same behaviors tested in Spek itself (see COVERAGE.md)
samples/             runnable example programs (not in the solution)
build/Spek.targets   MSBuild glue that compiles .spek inside a csproj
tools/vscode-spek/   VS Code extension (TextMate grammar + LSP client)
```

## How the compiler is organized

A `.spek` file flows through `SpekLexer.g4`/`SpekParser.g4` (ANTLR) →
`Parser/AstBuilder` → `Semantic/SemanticAnalyzer` → `Emit/FileEmitter` →
`Emit/AsyncRewriter`. Semantic errors are `CE####` diagnostics created with
`Diagnostic.At(code, node.Span, …)` so the CLI and language server can render
caret underlines. A new check follows that pattern and ships with tests in
`src/Spek.Tests/Semantic/` covering both the flagged and the clean shapes.

Spek deliberately does not re-implement C#'s type system. Generics, interface
conformance, and similar features lower verbatim to C# and let Roslyn report
misuse. Prefer that passthrough approach over new compiler machinery whenever
the emitted C# can carry the check.

## Grammar changes

The grammar lives in `src/Spek.Compiler/Grammar/*.g4`. After editing, re-run
`regenerate.sh`. Never hand-edit files under `Grammar/Generated/` — they are
git-ignored and rebuilt from the `.g4` sources.

## Testing conventions

- **Tests with every feature.** No feature lands without coverage. Emitter
  changes that alter a non-trivial code shape get a Roslyn compile smoke test
  via `RoslynCompileHelper.TryCompile`.
- **Two suites, both green.** `Spek.Tests.Native` is dogfooding: behavioral
  coverage written in Spek itself. When you add a behavioral C# test, check
  `src/Spek.Tests.Native/COVERAGE.md` to see whether a native twin belongs
  there too. Never delete a C# test just because a native version exists.
- **Docs are compile-tested.** Every fenced ```` ```spek ```` block in the
  README is parse- or compile-checked by `DocSnippetTests`
  (`dotnet test --filter DocSnippet`). If you add a snippet, make it valid, or
  mark it with a `<!-- spek-test: ignore -->` directive when it's an
  intentional fragment. If you change language syntax, stale snippets fail the
  suite — fix the docs, not the test.
- **Known flakes.** A few timing-sensitive runtime tests (passivation,
  persistence respawn) can fail under full-suite parallel load. Re-run them in
  isolation before treating a failure as a regression.

## Conventions

- **C# idioms first, Spek sugar later.** Before adding grammar, check whether
  the pattern already works in plain C# via the emitted code. We prefer
  shipping the smaller piece and iterating.
- **Docs describe today.** Documentation states what Spek does *now*:
  capabilities and honest limitations, no forward-looking hints. Planned work
  belongs in a GitHub issue; shipped work is recorded on the Releases page.
- **Dogfooding.** Library code that can be expressed in Spek is written in
  `.spek` — extending a dogfooded project usually means writing Spek, not C#.
- **Public APIs are documented.** Packable projects enforce XML doc comments
  on public members (a missing doc is a CS1591 warning there); tests and
  samples are exempt.
- **Commit messages.** Short, imperative titles, one logical change per
  commit. Maintainers prefix with the release era (`0.1 - area: what
  changed`).
- Match the style of the file you are editing.

## Things not to do

- Don't commit build output: `bin/`, `obj/`, `*.g.cs`, `Grammar/Generated/`,
  and `*.vsix` are all gitignored for a reason.
- Don't add raw threading (`Task.Run`, `new Thread`, timers) to `.spek` code —
  CE0119 exists because "pure Spek is race-free by construction" is a promise
  the compiler enforces.
- Don't edit `samples/*/README.md` build instructions without running the
  sample; the samples double as integration tests of the MSBuild story.

## Proposing changes

Open an issue to discuss anything substantial before a large PR. For bugs,
include a minimal `.spek` snippet that reproduces the problem. PRs should keep
the suite green and update the docs for any user-visible change.

## License

Spek is licensed under [Apache-2.0](LICENSE). By submitting a contribution you
agree it is licensed under the same terms (section 5 of the license).
