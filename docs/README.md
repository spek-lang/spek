# Spek documentation site

This folder is the source of the Spek documentation website. It is written in
Markdown and built with Jekyll using the `just-the-docs` theme, then published to
GitHub Pages.

The theme gives us three things with no extra tooling: side navigation generated
from each page's front matter (`parent` and `nav_order`), client-side full-text
search (`search_enabled: true` in `_config.yml`), and styling built for technical
documentation. Contributors edit Markdown; there is no Node toolchain or local
build step required for a change to ship.

## Syntax highlighting

`_plugins/spek_lexer.rb` is a [Rouge](https://github.com/rouge-ruby/rouge) lexer
for Spek, so ` ```spek ` blocks get real keyword, type, string, and number
highlighting.

This affects how the site is built. GitHub Pages' default build runs Jekyll in
`--safe` mode and ignores `_plugins/`, so the custom lexer never loads there. The
live site is built instead by the `docs` GitHub Actions workflow
(`.github/workflows/docs.yml`), which runs `bundle exec jekyll build` against the
checked-in `Gemfile`.

## Preview locally

Install Ruby (3.x) and Bundler, then from this directory:

```bash
bundle install
bundle exec jekyll serve
```

Open `http://127.0.0.1:4000/`. Edits to `.md` files hot-reload. The `Gemfile`
uses the `github-pages` gem, so a local build runs the same Jekyll and Rouge
versions as CI.

## Snippets are compiled

Every ` ```spek ` block in these docs is checked against the real compiler by the
`DocSnippetTests` suite, so a published snippet cannot silently drift from the
language. `dotnet test` runs it; there is no separate build step.

A block that opens with a top-level keyword (`program`, `module`, `actor`,
`message`, `enum`, `shared`, `channel`, `using`, `namespace`) is syntax-checked.
Anything else, or a block with an ellipsis placeholder (`{ ... }`), is skipped.
Override per block with an HTML comment on the line before the fence:

| Directive | Effect |
|---|---|
| `<!-- spek-test: compile -->` | Full check: parse, semantic, emit, and Roslyn-compile. Use for self-contained snippets. |
| `<!-- spek-test: ignore -->` | Skip. Use for deliberately invalid demos or fragments. |
| `<!-- spek-test-default: ignore -->` | Set the default for a whole file. |

Prefer `compile` for any snippet that should build, and make it self-contained
(declare its own messages and actors) so the harness verifies the emitted C#.

## Publishing

The `docs` workflow builds this folder and deploys on every push that touches
`docs/**`. With the Pages source set to **GitHub Actions** (see above), the site
URL appears on the Pages settings page and on each workflow run.
