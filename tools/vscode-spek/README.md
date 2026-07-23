# Spek: VS Code extension

Syntax highlighting and language-server integration for `.spek` files.

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

## Install locally (for development)

```
cd tools/vscode-spek
npm install                   # pulls vscode-languageclient + TS compiler
npm run compile               # compiles src/extension.ts → out/extension.js
npm install -g @vscode/vsce   # one-time
vsce package                  # produces vscode-spek-0.1.2.vsix
code --install-extension vscode-spek-0.1.2.vsix
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
