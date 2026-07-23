# Working on Spek

Spek is an actor-model language for .NET: the compiler (`spekc`) lowers
`.spek` source to C#, which Roslyn compiles against `Spek.Runtime`.

Everything you need — layout, build, compiler architecture, testing
conventions, and the guardrails — is in [CONTRIBUTING.md](CONTRIBUTING.md).
It applies to agents verbatim. The short version:

```bash
./src/Spek.Compiler/Grammar/regenerate.sh   # fresh clone only (needs Java)
dotnet build src/Spek.slnx
dotnet test  src/Spek.slnx
./scripts/ci.sh                             # exactly what CI runs
```

The one gotcha worth repeating: the generated ANTLR parser is not committed,
so `regenerate.sh` must run once before the first build, and again after any
`.g4` grammar change.
