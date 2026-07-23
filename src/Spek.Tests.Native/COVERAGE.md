# Native-Spek test coverage

Tracks the 1:1 parity goal: every C# behavioral test in `Spek.Tests` that can be
expressed through Spek's actor-facing API gets a native Spek equivalent here. **Both
suites stay** until parity is complete. Compiler,
runtime-internal, wire-format, and store-format tests are C#-only (not expressible in
Spek) and are intentionally excluded.

**Status — convertible sweep complete.** 54 native tests across 19 files cover every
behaviorally-expressible area: the full supervise matrix (OneForOne/AllForOne,
default/per-child scope, untyped/typed/multiple arms, all four directives), the full
persistence surface (persist/restore, the `transient`/`deprecated`/`retired` field
modifiers, snapshot-store inspection, passivate→wake), reader/writer dispatch, and the
behavioral side of generics + modules. The remaining C# tests are either internal/white-box
(compiler, cluster, async-spawn timing, shutdown machinery), emitter-only by design (see the
feature-sweep note below), or feature-gap-blocked (`out var`, switch-expression patterns).
Both suites run in CI; the C# suite stays until the language gaps are closed.

Legend: ✅ converted · 🔲 pending · ⏳ partial

## Runtime

| C# test (`Spek.Tests/Runtime`) | Spek equivalent | Status |
|--------------------------------|-----------------|--------|
| TestingSmokeTests *(C# original superseded by the native version; probe APIs still exercised across the C# runtime suite)* | ProbeTests.spek | ✅ |
| SupervisionAssertionTests *(C# original superseded by the native version; ExpectStop/ExpectRestart still exercised by SupervisionDirectiveTests et al.)* | SupervisionTests.spek | ✅ |
| AwaitTerminationTests | AwaitTerminationTests.spek | ✅ |
| TestKitDeterminismTests | DeterminismTests.spek | ✅ |
| AsyncSpawnTests | — | C#-only — async-spawn timing via a custom DelayedStore; behavior overlaps PersistenceTests |
| ImplicitSenderTests | ImplicitSenderTests.spek | ✅ |
| SupervisionTests | FailureDirectiveTests.spek (Stop/Resume/Restart/Escalate) | ✅ Restart/Escalate unblocked by member-access keyword support |
| SupervisionDirectiveTests | SupervisionTests.spek (budget, Resume) | ✅ |
| AllForOneSupervisionTests | SuperviseStrategyTests.spek | ✅ behavior (sibling restart via state reset); emitted-C# reflection stays C# |
| PerChildSuperviseTests | SuperviseStrategyTests.spek | ✅ behavior (per-child Stop override vs default Restart) |
| ExceptionTypeSuperviseTests | SuperviseStrategyTests.spek | ✅ behavior (typed arms matched top-to-bottom) |
| RuntimeErgonomicsTests | AskTimeoutTests.spek (ask-timeout) | ⏳ GracefulShutdown stays C# (system-level) |
| TryCatchRuntimeTests | TryCatchTests.spek | ✅ |
| PrivateHandlerRuntimeTests | PrivateHandlerTests.spek | ✅ |
| RuntimeCoverageTests | — | C#-only — named roots / DeliverIncoming / remote refs / escalation are internal; the few behavioral bits overlap existing conversions |
| ActorInitiatedShutdownTests | — | C#-only — system-shutdown machinery (RequestShutdown/Dispose/terminal-state) |
| OutcomeTests | — | C#-only — uses `out var` + switch-expression type patterns Spek lacks |

## Language features

Feature-sweep result: which language features are *behaviorally* testable (→ native) vs
*emitter* concerns (→ C# stays). A feature is emitter-only when there's no runtime behavior
to observe through the test API — generics lower verbatim to C# (Roslyn type-checks), modules
emit as static classes, stream operators are timing-shaped, etc.

| C# test | Spek equivalent | Status |
|---------|-----------------|--------|
| ReaderConcurrencyTests | ReaderWriterTests.spek | ✅ behavior (every reader replies / writer runs); concurrency-overlap + Readers.Max/Strategy reflection stay C# |
| GenericEmitTests | TypeFeatureTests.spek | ✅ behavior (value round-trips through a `Box<T>` field); emitted-shape assertions stay C# |
| ModuleEmitTests | TypeFeatureTests.spek | ✅ behavior (module method called from a handler); static-class emit shape stays C# |
| InterfaceTests | InterfaceFeatureTests.spek | ✅ behavior (interface-typed field dispatches polymorphically, swappable impl); emit shape + CE0120/CE0121 stay C# |
| ClassInheritanceTests | ClassInheritanceFeatureTests.spek | ✅ behavior (abstract template-method base + subclass hook dispatch); emit shape + CE0122/CE0123 stay C# |
| MessageInheritanceTests | MessageInheritanceFeatureTests.spek | ✅ behavior (`on Base` receives every variant; specific arm wins); record emit + CE0124/CE0125 stay C# |
| ActorInheritanceTests | ActorInheritanceFeatureTests.spek | ✅ behavior (derived actor reuses base field/method + dispatches abstract hook); emit shape + CE0122/CE0123 stay C# |
| ActorInheritanceTests (behaviors) + CatchAllHandlerTests + StreamOperatorTests | DocumentedFeatureTests.spek | ✅ behavior (override-behavior dispatch, `on any` catch-all routing, `distinct` operator); emit shape stays C# |
| StreamOperatorTests | — | C#-only — `debounce`/`throttle`/`distinct` are timing-shaped; tested via emitted operator chain |
| SharedRegionTests | — | C#-only — concurrency *shape* (emit); the behavioral reader/writer slice is ReaderWriterTests |
| EventHandlerTests | — | C#-only — `on event` emit shape; external-event firing isn't surfaced through the test API |

## Persistence

| C# test | Spek equivalent | Status |
|---------|-----------------|--------|
| EndToEndPersistenceTests | PersistenceTests.spek | ✅ (InMemory store; File/SQLite variants stay C#) |
| (field modifiers + lifecycle) | PersistenceFeatureTests.spek | ✅ persist/restore, transient/deprecated/retired, snapshot-store inspection |
| PassivationTests | PersistenceFeatureTests.spek (passivate→wake) | ✅ behavior via `IsMaterialized` (short idle timeout); store-format internals stay C# |

## Resilience

| C# test | Spek equivalent | Status |
|---------|-----------------|--------|
| PolicyDecisionTests (pure value) | — | 🔲 |

## Cluster

All **C#-only** — these exercise cluster *infrastructure* (fabric/transport wiring,
`Cluster.Bind`, peer registration, remote-ref resolution, membership state machines,
consistent-hash placement), not actor behavior observable through the test API. A
Spek version would just transliterate C# cluster calls with no dogfooding value:
ClusterMemoryTransport, TcpClusterTransport, ClusterMembership, LocatedActorPlacement,
PlacementStability.

## Gaps surfaced by the conversion

Each conversion is dogfooding; these came out of it:
- **Fixed — actor methods were silently dropped.** The actor emitter had no
  method-emission step, so a hand-written `OnFailure` / `OnChildFailure` (the only
  way to return `Resume`) and any helper method parsed but vanished. Now emitted
  (`protected override` for ActorBase hooks). Guard: `Spek.Tests/Emit/ActorMethodEmitTests.cs`.
- **Fixed — invisible-async ref forwarding** (fire-and-forget assertions; BCL
  ambiguity from duplicate refs).
- **Resolved — `supervise` now expresses `Resume`.** `restartAction`
  covers `Resume`, so `supervise OneForOne(on Failure: Resume)` works; the native
  ResumingParent converted from an `OnChildFailure` override to the declarative form.
  The override stays as the imperative escape hatch.
- **Open — AllForOne sibling restarts are invisible to `RestartCountOf`.** Under
  AllForOne, a crash restarts every sibling via `ActorSlot.ScheduleRestart()`, which
  nulls `_current` (forcing re-materialization) but never calls `RecordRestart()` — so
  the sibling's `_restartLog`/`RestartCount` stays 0 and `sys.ExpectRestart` can't see
  it. The sibling genuinely restarts (state resets), so SuperviseStrategyTests observes
  it via a tally reset, like the C# test. Likely intentional for budget purposes
  (a sibling restart shouldn't count against the sibling's own `maxRetries`), but it
  conflates "restart budget" with "restart count" — observability/metrics would want
  every restart counted. Worth separating the two concerns later.
- **Resolved — reserved words.** `Stop`/`Restart`/`Escalate` work after `.`
  (member access). Removed as keywords entirely: `seconds`/`minutes`/`hours`/
  `maxRetries`/`withinTime`. `actor`/`message`/`channel`/`after`/`strategy` are
  usable as ordinary identifiers in every name-binding position
  (var/param/field/message-field/foreach/handler-binding) and as references —
  the `after`-as-a-local case that surfaced here now parses. Only declaration starters
  (`actor X { }`) and fixed contextual sequences (`passivate after`, `strategy:`) stay
  reserved. Handler-mode keywords (event/reader/writer) stay hard to avoid `on event`
  ambiguity.

## C#-only (not expressible in Spek)

These test the compiler or internals, so they have no Spek equivalent by design:
- **Compiler-side**: everything under `Emit`, `Parser`, `Semantic`, `Compiler`,
  `Format`, `Diagnostics`, `Docs`, `LanguageServer`.
- **Fixture + emitted-C# reflection** (really emitter tests): AllForOneSupervision,
  PerChildSupervise, ExceptionTypeSupervise, SuperviseDeclEndToEnd, EnumE2E,
  FullBankFixture, FullPipelineRuntime, ReaderConcurrency, **OptionDAskTests**
  (asserts `AskAsync<T>` on the emitted C# — inference is an emitter concern).
- **C# interop plumbing**: **ForwardTests** — exercises `event EventHandler<T>`,
  delegate identity, and `-=` unsubscribe, which Spek doesn't express.
- **Runtime internals**: Passivation *internals* (the C# PassivationTests' white-box
  IsMaterialized timing assertions; the observable passivate→wake behavior is now native
  in PersistenceFeatureTests), ShutdownPersistence, ShutdownTokenPolicy, ShutdownBehavior,
  LogicalClock (no actors).
- **Store format / limiter internals**: PersistenceTests, FileSnapshotStore,
  LogSnapshotStore, SqliteSnapshotStore, SnapshotStoreEdge, PersistenceRobustness,
  RateLimit*, IngressPolicyIntegration, PartitionedRateLimit.
- **Wire / topology**: TcpTransportFailure, ClusterFailover.
