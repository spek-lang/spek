---
title: Samples
layout: default
nav_order: 5
permalink: /samples/
---

# Samples

Short, concrete Spek programs. The fixture files are the compiler's
regression inputs: they are deliberately minimal, and each targets a
specific slice of the language. `HelloBank` is a runnable end-to-end
program that stitches the features together.

## `HelloBank`: the runnable walkthrough

**Location:** `samples/HelloBank/HelloBank.spek`

A two-actor program: `Account` owns a balance and handles `Deposit`,
`Withdraw`, `GetBalance` messages; `Root` is the driver that spawns an
account, sends a handful of operations, and prints the responses. Ends
with `system.AwaitTermination();` so the process exits cleanly when all
mailboxes drain.

This is the sample walked through in [getting started](/getting-started/).

## `AlertHub`: contracts and inheritance, across files

**Location:** `samples/AlertHub/` (ten files, one type per file)

An ops alert router exercising every contract and inheritance form: an
`abstract message` family dispatched via `on AlertEvent` (with a specific
`ServiceDown` arm that wins), an `interface` whose implementation is swapped
mid-stream (`PlainFormatter` → `JsonFormatter`), an `abstract class`
template-method (`EscalationPolicy`/`PagerPolicy`) with
`init() : base(...)` chaining, and an `abstract actor` base
(`NotifierBase`) whose `abstract behavior` the concrete notifier fills in
with `override behavior`. Because each type lives in its own file, the
sample also demonstrates whole-project cross-file compilation; the layout
a real C#-convention project would use.

## `Watchdog`: supervision policies and passivation

**Location:** `samples/Watchdog/Watchdog.spek`

A `Foreman` supervises workers with a declarative `supervise` policy: typed
`on Failure(...)` arms, a `maxRetries`/`withinTime` budget, and a per-child
override that stops one worker where the default would restart. The output
shows a restart resetting state, dead-letters to the stopped sibling, and a
session-scoped cache passivating after two idle seconds and waking fresh.

## `Telemetry`: stream operators and reader/writer state

**Location:** `samples/Telemetry/Telemetry.spek`

Stream-shaped handlers rate-limit a noisy feed before the actor lock:
`distinct(by:)` collapses consecutive duplicate readings, `throttle`
passes one heartbeat per window. Tallies live in a `shared` region touched
by writer handlers and queried by a concurrent `reader on` handler, with a
`transient` field marker on the region.

## Fixtures

Each fixture lives under `src/Spek.Tests/Fixtures/`. They are loaded by
`FixtureLoader.cs` into parser / emitter unit tests.

### `01_messages_only.spek`: message declarations in isolation

**Location:** `src/Spek.Tests/Fixtures/01_messages_only.spek`

Exercises the message emitter on its own: ten message declarations with a
mix of required and default-valued fields, plus one generic message
(`Response<T>`). There are no actors and no runtime involved, only the `message`
→ `record` translation.

Useful as a reference for the full message-declaration surface in one
place.

### `02_simple_actor.spek`: the minimum viable actor

**Location:** `src/Spek.Tests/Fixtures/02_simple_actor.spek`

An `Echo` actor with one behavior and one handler. Exercises `actor`,
`init`, `behavior`, `on`, `become`, and `sender.Tell`. If this compiles,
the core actor wiring works.

### `03_become.spek`: behavior switching

**Location:** `src/Spek.Tests/Fixtures/03_become.spek`

A toggle switch with `On` and `Off` behaviors. Demonstrates:

- Multiple behaviors on one actor.
- Both inline (`on X => sender.Tell(...)`) and block (`on X => { ... }`)
  handler forms.
- `become` jumping between behaviors from inside a handler.

### `04_persist_passivate.spek`: persistence surface

**Location:** `src/Spek.Tests/Fixtures/04_persist_passivate.spek`

A `Wallet` actor that exercises every persistence construct:

- A typed field (`decimal balance`) and an id field (`string ownerId`).
- A `passivate after System.TimeSpan.FromMinutes(10);` declaration.
- `persist;` inside a handler.
- `on PreStart`, `on PostStop`, and `on Restore(Snapshot s)` lifecycle
  hooks.

See [persistence](/language/persistence/) for how these constructs behave at
runtime.

### `05_bank_account_full.spek`: the canonical integration test

**Location:** `src/Spek.Tests/Fixtures/05_bank_account_full.spek`

The largest fixture, and the integration test that exercises *every core
language feature*:

- `public` visibility modifier.
- Multi-field actors with typed fields.
- `init` with parameters.
- Two behaviors (`NormalOperation`, `Frozen`) with `become` between them.
- `persist;` inside multiple handlers.
- `passivate after System.TimeSpan.FromMinutes(30);`.
- All three lifecycle hooks: `PreStart`, `PostStop`, `Restore`.
- `spawn<BankAccount>(...)` from inside a handler.
- `sender.Tell(...)` for replies.
- `supervise OneForOne(on Failure: Restart, ...)` declaration wired
  end-to-end (default form, per-child overrides, exception-type arms;
  see [supervision](/language/supervision/)).
- A `program Main { ... }` entry block.

If you want to see what a "real" Spek program feels like, this is the
file to read. The [grammar](/spek-v1-grammar/) document's §16 ("Complete
Example") is built around the same program.

### `06_shared_region.spek`: shared regions and implicit Default

**Location:** `src/Spek.Tests/Fixtures/06_shared_region.spek`

Two actors attached to the same `MarketCache` shared region:
`PriceWriter` mutates under the writer lock; `PriceReader` reads
under the reader lock. Exercises:

- `shared X { fields ... init { ... } }`: region declaration with
  a lazy init block.
- `use MarketCache cache;`: actor-scope attachment.
- `writer on Update u => { cache.lastPrice = ... }`: the writer
  handler acquires the region's writer lock around its body.
- `reader on GetLast g => return new Reply(cache.lastPrice);`: the
  reader handler acquires the region's reader lock; multiple
  `PriceReader` instances run concurrently.
- Implicit `Default` behavior: neither actor declares a
  `behavior X { ... }` wrapper.

### `07_shared_region_persisted.spek`: Persisted shared regions

**Location:** `src/Spek.Tests/Fixtures/07_shared_region_persisted.spek`

A variant of `06_shared_region.spek` that uses the `: Persisted`
capability marker so the region survives process restart. Adds:

- `shared RequestMetrics : Persisted { ... }`: the capability marker
  selects the `Spek.PersistedRegion` runtime base class.
- `self.Name = "metrics-v1";` inside `init`: overrides the default
  snapshot key.
- A `program Main { ... }` block that registers the store via
  `system.RegisterPersistenceProvider<RequestMetrics>(store)`.
  Without it, [CE0098](/reference/errors/#ce0098) fails the build.

This fixture is the smallest end-to-end example of the host-driven
persistence model: the region declares the capability, the
`program` block configures the provider, and the compiler verifies
the wiring.

### `08_lambdas_and_linq.spek`: lambdas with LINQ inside a handler

**Location:** `src/Spek.Tests/Fixtures/08_lambdas_and_linq.spek`

A `BatchProcessor` actor that runs a LINQ method chain inside its
handler body. Exercises the full lambda-shape surface:

- Single bare-parameter lambdas (`x => x > threshold`).
- Typed local for a lambda value (`Func<int, int> compress = x => ...`).
- Capture of actor fields from inside a lambda.
- LINQ chain: `Where` → `Select` → `OrderBy` → `ToList`.
- Block-bodied lambda with multiple statements and a `return`.

## Where to go next

- [Getting started](/getting-started/): compile and run `HelloBank`
  yourself.
- [Demos](/demos/): the full-size runnable systems, with C# twins and a
  benchmark suite.
- [Language overview](/language/): the feature-by-feature breakdown.
- [Runtime reference](/reference/runtime/): the C# surface these programs
  lower onto.
