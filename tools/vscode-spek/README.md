# Spek: VS Code extension

Syntax highlighting and language-server integration for `.spek` files, which
carry their own file icon in the explorer and editor tabs: a rounded tile
holding the smallest possible supervision tree, one actor node linked to two
children, in a light and a dark variant.

The grammar covers keywords, types, literals, operators, and comments, and is
always on. The language server adds live diagnostics, publishing CE codes as
squiggles, plus editor features like go-to-definition, hover, completion,
signature help, inlay hints for inferred `.Ask` reply types, and **"▶ Run test"
code lenses** on `…Tests` containers (clicking one runs `dotnet test` filtered to
that test in the enclosing project). A **"Spek: Show Emitted C#"** command is
available behind the `spek.experimental.showEmittedCSharp` setting (off by
default) for peeking at the compiler output. It launches `spek-lsp` from PATH on
activation. Install the language server as a global .NET tool:

```
dotnet tool install --global Spek.LanguageServer.Tool
```

Alternatively, point `spek.languageServer.path` at a locally built `spek-lsp`.
If the server isn't found, the extension falls back to grammar-only
highlighting.

## Run and debug from a .spek file

Press F5 in any `.spek` file, or use **Spek: Run Project of This File** and
**Spek: Debug Project of This File** from the command palette. The extension
works out which project runs the file: the nearest `.csproj` above the file is
the project that compiles it, and if that project is a library, the run target
is an executable project nearby that references it. The harness layout in
`demos/elevators` is the canonical case: `Elevators.spek` lives in the
`Elevators.Spek` library, and F5 finds and launches `Elevators.Harness`. When
several projects qualify, a quickpick asks once and the answer is remembered
for the workspace.

Debugging delegates to the C# extension's `coreclr` debugger. A `dotnet build`
task runs first, then the debugger launches the built assembly at the SDK's
default location, `bin/Debug/<TargetFramework>/<AssemblyName>.dll` (the
assembly name defaults to the project file name). A project that overrides
`OutputPath` or uses artifacts output needs its own `launch.json` with an
explicit `program`. Breakpoints set in `.spek` sources bind directly, because
Debug builds emit absolute `#line` directives into the generated C#.

While a Spek project is being debugged, the live actor panel (below) opens
automatically beside the editor, attached to the debuggee. The
`spek.observe.autoAttachOnDebug` setting controls this: `spekProjectsOnly`
(the default) attaches only when the debugged project contains or references
`.spek` sources, `always` attaches to every .NET debug session, and `never`
leaves attaching manual. Hitting a breakpoint suspends the debuggee's sampler,
so the panel notes "paused with debugger — tree frozen at last sample" until
execution resumes; when the debug session ends, the panel keeps its last tree
with a note instead of closing.

## Live actor introspection

**Spek: Attach to Process** connects the editor to a running Spek application.
Pick a .NET process from the list (discovered the same way `dotnet-counters ps`
finds attachable processes) or enter a pid, and the extension spawns
`spekc observe <pid> --json` — the read-only EventPipe transport, so attaching
never perturbs the target. Two things light up while attached.

The **actor panel** renders the live supervision tree: every actor with its
behavior, mailbox depth, restart count, and last dispatched message, refreshed
in place about once a second. Stopped actors are struck through, passivated
actors dimmed, and an actor with a deep mailbox (see
`spek.observe.mailboxWarningDepth`) is flagged — a queue that keeps growing
usually means a slow or stuck handler. Clicking an actor jumps to its
`actor` declaration in the workspace.

**Live CodeLens** appears above matching `actor X` declarations in open `.spek`
files — `● mailbox 3 · restarts 1 · last: Deposit` — updating with each sample.
The lens provider only exists while attached; detached, it contributes nothing
to editing.

Closing the panel (or **Spek: Detach from Process**) kills the observe child
process. A status bar item shows the attached system and its live actor count
throughout. The `spekc` CLI is resolved from PATH, or set `spek.spekc.path`.
For working on the panel itself without a live process, point `spek.spekc.path`
at `scripts/mock-observe.js`, which emits a synthetic sample stream.

## Error-code explanations

Every Spek diagnostic carries a `CE####` code, and the extension bundles the
full error-codes reference. Hovering a squiggle offers **Explain CE####**,
which opens that code's complete teaching text — trigger, rationale, severity,
a rejected example, and the fix — in a markdown preview, in the spirit of
`rustc --explain`. **Spek: Explain Error Code** does the same from the command
palette with a searchable list of every code. The catalog ships inside the
extension (`errors/catalog.json`, generated at build time from the
documentation's error-codes reference), so explanations work offline.
Regenerate it after the reference changes:

```
npm run compile
node out/tools/generateErrorCatalog.js <path-to-errors-reference.md>
```

## Install locally (for development)

```
cd tools/vscode-spek
npm install                   # pulls vscode-languageclient + TS compiler
npm run compile               # compiles src/ → out/
npm test                      # unit tests for the observe/catalog plumbing
npm install -g @vscode/vsce   # one-time
vsce package                  # produces vscode-spek-<version>.vsix
code --install-extension vscode-spek-0.1.0.vsix
```

Then open any `.spek` file and VS Code should pick up the grammar.

Alternatively, for rapid iteration on the grammar itself, symlink this
directory into `%USERPROFILE%/.vscode/extensions/vscode-spek-0.1.1` (Windows)
or `~/.vscode/extensions/vscode-spek-0.1.1` (macOS / Linux) and reload VS Code.

## Token scopes

The grammar classifies tokens into these TextMate scopes (so themes style them
correctly):

| Scope                                  | Examples                                                        |
|----------------------------------------|-----------------------------------------------------------------|
| `keyword.other.declaration.spek`       | `actor`, `message`, `interface`, `module`, `init`, `term`, `use` |
| `keyword.other.handler.spek`           | `on`, `emits`, `event`, `any`, `reader`, `writer`               |
| `keyword.control.actor.spek`           | `become`, `spawn`, `persist`, `passivate`, `supervise`          |
| `keyword.control.spek`                 | `if`, `foreach`, `switch`, `try`, `throw`, `when`, `return`     |
| `keyword.operator.expression.spek`     | `new`, `is`, `as`, `not`, `and`, `or`                           |
| `variable.language.spek`               | `self`, `sender`, `base`                                        |
| `entity.name.function.lifecycle.spek`  | `PreStart`, `PostStop`, `Restore`                               |
| `constant.language.supervision.spek`   | `OneForOne`, `Failure`, `Restart`, `Stop`, `Escalate`           |
| `storage.modifier.spek`                | `public`, `abstract`, `override`, `transient`, `deprecated`     |
| `storage.type.builtin.spek`            | `decimal`, `string`, `int`, `ActorRef`, `Outcome`, `TimeSpan`   |
| `entity.name.type.spek`                | Any PascalCase identifier                                       |
| `string.quoted.*.spek`                 | `"…"`, `$"…{x}…"`, `@"…"`, `$@"…"`, `"""…"""`, `'c'`            |
| `constant.numeric.*.spek`              | `10.00m`, `1_000_000L`, `0x1F`, `0b1010`, `1.5e3`               |
| `comment.block.documentation.spek`     | `/// doc comments`                                              |

## Verifying colorization

`examples/showcase.spek` (in this extension's folder) exercises every
colorized construct in one file — open it with the extension installed and
eyeball the result after any grammar change. The file is kept
**compile-valid** (`spekc compile examples/showcase.spek` from the repo root
must succeed, asserted by `TmLanguageSyncTests`), so it can't drift from the
real language.

## Where the grammar comes from

The grammar is derived from the authoritative ANTLR4 lexer in
`src/Spek.Compiler/Grammar/SpekLexer.g4`. When keywords change there, this
`.tmLanguage.json` needs to be updated by hand, since TextMate grammars can't
be mechanically regenerated from ANTLR.
