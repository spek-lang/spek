# Spek.LanguageServer

Language Server (LSP) for the [Spek](https://github.com/spek-lang/spek) actor language.

Speaks LSP over stdio, so any LSP-aware editor can use it: VS Code, Neovim,
Emacs, Helix, Zed, Sublime, Kate, JetBrains Fleet.

## Install

```
dotnet tool install -g Spek.LanguageServer.Tool
```

That installs the `spek-lsp` command on your PATH.

## Features

| Feature            | Status | Notes                                                |
|--------------------|--------|------------------------------------------------------|
| Diagnostics        | ✅     | Every CE code published as a live squiggle, with a Rust-style severity + caret range. |
| Go to definition   | ✅     | Messages, actors, `become` targets, type references. |
| Hover              | ✅     | Message/actor/behavior signatures + field types.     |
| Document symbols   | ✅     | Outline view, symbol picker (Ctrl+O, Ctrl+Shift+O).  |
| Completion         | ✅     | Keywords + in-scope symbols (fields, behaviors, pattern bindings, message/actor types). |
| Rename / references| ✅     | `textDocument/rename` and find-references.           |
| Code actions / quick-fixes | ✅ | Closest-match and declare-stub fixes; footgun rewrites (e.g. Thread.Sleep → Task.Delay). |
| Formatting         | ✅     | `textDocument/formatting` (whole-document), plus the `spekc format` CLI. |
| Folding ranges     | ✅     | Declarations, actor members, behavior handlers.      |
| Semantic tokens    | ✅     | Resolved highlighting (actor / message / channel / enum / module). |
| Signature help     | ✅     | Message fields while typing `new Msg( … )`.          |
| Inlay hints        | ✅     | Inferred `.Ask` reply types shown inline.            |
| Call hierarchy     | ✅     | Message-flow graph over `on Msg` handlers.           |
| Code lens          | ✅     | "▶ Run test" on `…Tests` containers and their tests. |

## Editor setup

### VS Code

The `vscode-spek` extension (see `tools/vscode-spek/`) bundles a grammar and
will auto-launch `spek-lsp` once it's on your PATH.

### Neovim (nvim-lspconfig)

```lua
require'lspconfig'.spek.setup {
  cmd = {'spek-lsp'},
  filetypes = {'spek'},
  root_dir = require'lspconfig.util'.root_pattern('*.sln', '.git'),
}
```

### Emacs (eglot)

```elisp
(add-to-list 'eglot-server-programs '(spek-mode . ("spek-lsp")))
```

## License

Apache-2.0.
