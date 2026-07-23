# Spek.Benchmarks.Toolchain

Latency benchmarks for the Spek toolchain: the compiler pipeline that
`spekc` runs on every build, the language-server paths that run on every
keystroke, the formatter, and the diagnostic error path. Built on
[BenchmarkDotNet](https://benchmarkdotnet.org/).

Spek.Benchmarks prices the runtime; this project prices the tools. The two
compound the same way. A build touches every file, an editor reparses the
whole document on every debounced keystroke, and semantic-token requests
arrive continuously while a file is open. In a large project the toolchain
is a hot loop the developer sits inside all day, so a regression here is
felt as directly as a slow mailbox: the compile that took two seconds now
takes six, the squiggles lag the cursor. These benchmarks make that cost a
number that can gate a release.

Every suite runs against deterministic synthetic source produced by
`SyntheticSpekSource`: a repeated cell of messages, two actors (guarded
switch arms, a `reader on` handler, `become` transitions), and a module,
modeled on the density of the shipped samples. Sizes are exact at 100,
1,000, and 5,000 lines, and identical bytes every run, so numbers compare
across machines and commits.

## Run

```bash
# All benchmarks (Release is mandatory — Debug numbers are meaningless):
dotnet run -c Release --project src/Spek.Benchmarks.Toolchain

# A subset:
dotnet run -c Release --project src/Spek.Benchmarks.Toolchain -- --filter '*Compiler*'
dotnet run -c Release --project src/Spek.Benchmarks.Toolchain -- --filter '*Lsp*'
dotnet run -c Release --project src/Spek.Benchmarks.Toolchain -- --filter '*Format*'
dotnet run -c Release --project src/Spek.Benchmarks.Toolchain -- --filter '*DiagnosticRender*'

# Faster, lower-fidelity pass while iterating:
dotnet run -c Release --project src/Spek.Benchmarks.Toolchain -- --job short
```

## What's measured

| Suite                        | Benchmark                                     | Captures                                                                                  |
|------------------------------|-----------------------------------------------|-------------------------------------------------------------------------------------------|
| `CompilerBenchmarks`         | `ParseOnly`                                   | ANTLR lex + parse + AST build, the floor under everything else                            |
|                              | `ParseAndAnalyze`                             | parse plus single-file semantic analysis (what an LSP didChange pays)                     |
|                              | `ParseAndEmit`                                | the full CliRunner path for one clean file: parse, combined symbols, analysis, emit       |
|                              | `EmitOnly`                                    | the backend alone against a pre-parsed tree, dominated by the invisible-async Roslyn pass |
| `LspBenchmarks`              | `DidChangeReparse`                            | the debounced-keystroke reparse of a 1,000-line document                                  |
|                              | `HoverOnLargeDoc` / `DefinitionOnLargeDoc`    | one feature request against the cached 5,000-line document                                |
|                              | `SemanticTokensFullDoc`                       | the full-document re-lex and classification editors request constantly                   |
| `FormatBenchmarks`           | `FormatDocument`                              | the token-stream formatter on 1,000 lines (the format-on-save cost)                       |
| `DiagnosticRenderBenchmarks` | `ParseClean` vs `ParseWithMidFileSyntaxError` | what ANTLR error recovery adds — or skips — when a big file is broken mid-edit            |
|                              | `RenderDiagnostic`                            | one Rust-style caret frame rendered against a 5,000-line source                           |

The figures quoted below are rough: single-iteration `--job dry` and
three-iteration `--job short` passes on an Apple M5, recorded to orient a
reader rather than to gate anything. The full-fidelity baseline run happens
separately; treat these as order-of-magnitude until it lands.

`[MemoryDiagnoser]` adds the `Allocated` column; parse allocations run about
8 to 10 MB per KLOC, so a 5,000-line reparse is a ~46 MB allocation event
per keystroke after debounce — watch that column as closely as the mean.

`CompilerBenchmarks` stages the pipeline so a regression names its own
stage. The scaling story is the headline: parse allocations stay linear
across all three sizes, but parse time does not. Short runs measure roughly
1.2, 7.9, and 90 ms at 100, 1k, and 5k lines — an 11x jump for the 5x step
from 1k to 5k. Emit is the other big number: the invisible-async pass
parses the emitted C# with Roslyn and builds a compilation over it, which
makes emit 70 to 90% of a clean single-file build and adds a ~1.4 s
process-cold floor to the first file a `spekc` invocation compiles. Large
single files pay for parse super-linearly and for emit always.

`LspBenchmarks` prices the editor loop. The didChange path re-parses the
whole document because sync is full-text, so keystroke latency is the
`ParseAndAnalyze` number in disguise (~11 ms and ~9.4 MB per keystroke on
1,000 lines in short runs). Hover and definition are cheap even on the
5,000-line document (each rebuilds the symbol table per request, which
sounds expensive and measures around 60 to 110 µs); semantic tokens
re-lexes the entire file per request (~6.8 ms and 7.3 MB at 5,000 lines)
and is the one editor-side cost that grows with document length rather than
with the edit.

`DiagnosticRenderBenchmarks` measures the state a file spends most of its
edited life in: broken. A mid-file syntax error currently makes the reparse
cheaper, not slower — once a syntax diagnostic exists the AST build and
semantic analysis are skipped, so the broken-file keystroke costs about
half the clean one (ratio 0.45 to 0.55). The render benchmark exists
because the caret frame re-splits the whole source per diagnostic rendered;
it is ~100 µs at one error on 5,000 lines but linear in both file size and
error count.

## Notes

Not part of `dotnet test` (benchmarks are long-running and run in their own
Release process). It is in `Spek.slnx` so it stays compiling.

The LSP feature handlers are internal to `Spek.LanguageServer`, so
`LspBenchmarks` constructs them by reflection during setup and drives them
through the public MediatR `IRequestHandler` interfaces — the same entry
point the server's dispatcher uses, with no per-operation reflection cost.
