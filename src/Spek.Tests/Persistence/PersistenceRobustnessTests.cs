using System.Collections.Generic;
using Spek;                 // PersistedRegion (capability marker), Snapshot wiring
using Spek.Compiler.Emit;   // FileEmitter
using Spek.Compiler.Parser; // SpekCompiler
using Spek.Persistence;     // ISnapshotStore, Snapshot
using Spek.Runtime;         // ActorSystem, ActorBase, ActorRef, dead-letter sinks, InMemorySnapshotStore
using Spek.Tests.Emit;      // RoslynCompileHelper
using Xunit;

namespace Spek.Tests.Persistence;

/// <summary>
/// Robustness coverage for <c>shared … : Persisted</c> regions beyond the
/// happy-path save/restore loop:
///
///   1. <b>Schema evolution</b> — restoring a snapshot whose key-set no longer
///      matches the region's fields. A field the snapshot is missing keeps its
///      compiler-emitted default ("graceful additive"); an extra snapshot key
///      that has no matching field is silently ignored (and may trigger the
///      <c>WarnOnDroppedKeys</c> stderr warning — not asserted on).
///   2. <b>transient field</b> — a <c>transient</c> region field is never
///      captured, so on restore it resets to its initialiser default while the
///      neighbouring persisted field is restored.
///   3. <b>Store failure → dead-letter</b> — when <c>SaveAsync</c> throws, the
///      region's catch routes a "save failed" record to the system's
///      <see cref="IDeadLetterSink"/>, and the region stays readable afterward.
///
/// These exercise <c>PersistedRegion.RestoreFields</c> /
/// <c>PersistedRegion.SaveAsync</c> end-to-end: real Spek source is compiled to
/// C#, loaded via Roslyn, and driven through actual <see cref="ActorSystem"/>
/// instances wired to the store under test. The snapshot key for a region is its
/// type name (<see cref="SharedRegion.Name"/> defaults to the simple type name).
/// </summary>
public sealed class PersistenceRobustnessTests
{
    // ── Scenario 1: schema-evolution restore ────────────────────────────────
    //
    // Region declares three persisted fields. We pre-seed a snapshot that:
    //   • carries DisplayName + LoginCount  (matching fields → restored)
    //   • is MISSING Score                  (no key → keeps default 0)
    //   • carries an EXTRA "legacyFlag" key (no field → ignored on restore)
    // A fresh system rehydrates the region on first reader access; the reader
    // replies with all three field values so we can assert the outcome.
    private const string SchemaSrc = """
        namespace SchemaEvo;

        shared Profile : Persisted
        {
            string DisplayName = "anonymous";
            int LoginCount = 0;
            int Score = 0;
        }

        message Read();
        message ProfileReply(string displayName, int loginCount, int score);

        actor Viewer
        {
            use Profile profile;

            reader on Read =>
            {
                sender.Tell(new ProfileReply(profile.DisplayName, profile.LoginCount, profile.Score));
            }
        }

        // Present only to satisfy CE0098 (a : Persisted region needs a registered
        // provider somewhere in the compilation). The test drives its own systems
        // and wires the store via the ActorSystem ctor, so this Main never runs.
        program Main
        {
            var system = new ActorSystem("schema-demo");
            system.RegisterPersistenceProvider<Profile>(new InMemorySnapshotStore());
        }
        """;

    [Fact]
    public async Task SchemaEvolution_MissingFieldKeepsDefault_ExtraKeyIgnoredAsync()
    {
        var asm = Compile(SchemaSrc, "SchemaEvo");
        var viewerType = asm.GetType("SchemaEvo.Viewer")!;
        var readType   = asm.GetType("SchemaEvo.Read")!;

        // Pre-seed the store under the region's type name ("Profile").
        // Old-schema snapshot: has DisplayName + LoginCount + an extra
        // "legacyFlag", but is missing the newer "Score" field.
        var store = new InMemorySnapshotStore();
        await store.SaveAsync("Profile", new Snapshot(new Dictionary<string, object?>
        {
            ["DisplayName"] = "ada",
            ["LoginCount"]  = 42,
            ["legacyFlag"]  = true,   // extra key — no matching field on the region
            // "Score" intentionally absent — added after this snapshot was written
        }));

        using var system = new ActorSystem("schema-rehydrate", snapshotStore: store);
        var viewer = system.Spawn(viewerType, System.Array.Empty<object>());

        var reply = await ReadReplyAsync(system, viewer, System.Activator.CreateInstance(readType)!);

        // Matching fields were overlaid from the snapshot.
        Assert.Equal("ada", Prop<string>(reply, "displayName"));
        Assert.Equal(42, Prop<int>(reply, "loginCount"));

        // Graceful-additive: the field the snapshot lacked keeps its
        // compiler-emitted initialiser default rather than throwing or
        // resetting the whole region.
        Assert.Equal(0, Prop<int>(reply, "score"));

        // The extra "legacyFlag" key caused no failure — restore completed and
        // the region is fully usable. (WarnOnDroppedKeys may have written a
        // stderr warning; we deliberately don't assert on Console.)
    }

    // ── Scenario 2: transient field resets on restore ───────────────────────
    //
    // Region has one normal persisted field and one transient field. A writer
    // mutates both, the region saves on writer-exit, then a fresh system
    // rehydrates from the same store. The normal field must come back; the
    // transient field must NOT be in the snapshot and so must reset to its
    // initialiser default.
    private const string TransientSrc = """
        namespace TransientEvo;

        shared Session : Persisted
        {
            int Durable = 0;
            transient int Ephemeral = 7;
        }

        message Touch();
        message Read();
        message SessionReply(int durable, int ephemeral);

        actor Worker
        {
            use Session session;

            writer on Touch =>
            {
                session.Durable = session.Durable + 5;        // persisted → in the snapshot
                session.Ephemeral = session.Ephemeral + 100;  // transient → never captured
            }

            reader on Read =>
            {
                sender.Tell(new SessionReply(session.Durable, session.Ephemeral));
            }
        }

        program Main
        {
            var system = new ActorSystem("transient-demo");
            system.RegisterPersistenceProvider<Session>(new InMemorySnapshotStore());
        }
        """;

    [Fact]
    public async Task TransientField_NotCaptured_ResetsToDefaultOnRestoreAsync()
    {
        var asm = Compile(TransientSrc, "TransientEvo");
        var workerType = asm.GetType("TransientEvo.Worker")!;
        var touchType  = asm.GetType("TransientEvo.Touch")!;
        var readType   = asm.GetType("TransientEvo.Read")!;

        var store = new InMemorySnapshotStore();

        // First lifetime: mutate both fields, then drain so the writer-exit
        // save lands (and Dispose flushes as a belt-and-braces final save).
        using (var system1 = new ActorSystem("transient-life-1", snapshotStore: store))
        {
            var worker = system1.Spawn(workerType, System.Array.Empty<object>());

            // Confirm the in-process mutation took effect before restart:
            // Durable 0→5, Ephemeral 7→107.
            worker.Tell(System.Activator.CreateInstance(touchType)!);
            var live = await ReadReplyAsync(system1, worker, System.Activator.CreateInstance(readType)!);
            Assert.Equal(5, Prop<int>(live, "durable"));
            Assert.Equal(107, Prop<int>(live, "ephemeral"));

            Assert.True(system1.AwaitTermination(System.TimeSpan.FromSeconds(10)));
        }

        // The persisted snapshot must carry "Durable" but never the transient
        // "Ephemeral" key.
        var saved = await store.LoadAsync("Session");
        Assert.NotNull(saved);
        Assert.True(saved!.Fields.ContainsKey("Durable"), "persisted field must be captured");
        Assert.False(saved.Fields.ContainsKey("Ephemeral"), "transient field must NOT be captured");
        Assert.Equal(5, saved.Get<int>("Durable"));

        // Second lifetime: fresh system + same store. The region rehydrates on
        // first access — Durable restores to 5, Ephemeral resets to its
        // initialiser default (7), NOT the 107 it reached at runtime.
        using var system2 = new ActorSystem("transient-life-2", snapshotStore: store);
        var reader = system2.Spawn(workerType, System.Array.Empty<object>());
        var reply = await ReadReplyAsync(system2, reader, System.Activator.CreateInstance(readType)!);

        Assert.Equal(5, Prop<int>(reply, "durable"));     // restored from snapshot
        Assert.Equal(7, Prop<int>(reply, "ephemeral"));   // back to initialiser, not 107
    }

    // ── Scenario 3: store-failure surfaces to dead-letters ───────────────────
    //
    // A store whose SaveAsync always throws is wired to the system together with
    // a recording dead-letter sink. A writer mutation triggers a save that
    // fails; PersistedRegion.SaveAsync's catch must route the failure to the
    // sink. Afterward the region must still be readable/usable (the in-memory
    // state was never rolled back; only durability was lost).
    private const string StoreFailureSrc = """
        namespace StoreFail;

        shared Ledger : Persisted
        {
            int Balance = 0;
        }

        message Deposit(int amount);
        message Read();
        message BalanceReply(int balance);

        actor Teller
        {
            use Ledger ledger;

            writer on Deposit d =>
            {
                ledger.Balance = ledger.Balance + d.amount;   // writer-exit kicks off the (failing) save
            }

            reader on Read =>
            {
                sender.Tell(new BalanceReply(ledger.Balance));
            }
        }

        program Main
        {
            var system = new ActorSystem("storefail-demo");
            system.RegisterPersistenceProvider<Ledger>(new InMemorySnapshotStore());
        }
        """;

    [Fact]
    public async Task StoreFailure_OnSave_RecordsDeadLetter_RegionStaysUsableAsync()
    {
        var asm = Compile(StoreFailureSrc, "StoreFail");
        var tellerType  = asm.GetType("StoreFail.Teller")!;
        var depositType = asm.GetType("StoreFail.Deposit")!;
        var readType    = asm.GetType("StoreFail.Read")!;

        var failingStore = new ThrowingOnSaveStore(
            new System.InvalidOperationException("disk is on fire"));
        var deadLetters = new RecordingDeadLetterSink();

        using var system = new ActorSystem(
            "storefail-rehydrate",
            snapshotStore: failingStore,
            deadLetterSink: deadLetters);

        var teller = system.Spawn(tellerType, System.Array.Empty<object>());

        // Writer mutation → writer-exit → fire-and-forget SaveAsync → throws →
        // caught and routed to the dead-letter sink.
        var deposit = System.Activator.CreateInstance(depositType, new object[] { 30 })!;
        teller.Tell(deposit);

        // Poll for the dead-letter (the save is fire-and-forget; bound the wait).
        var deadline = System.DateTime.UtcNow + System.TimeSpan.FromSeconds(10);
        DeadLetterRecord? record = null;
        while (System.DateTime.UtcNow < deadline)
        {
            record = FindSaveFailure(deadLetters);
            if (record is not null) break;
            await System.Threading.Tasks.Task.Delay(25);
        }

        Assert.NotNull(record);
        Assert.Contains("save failed", record!.Reason, System.StringComparison.OrdinalIgnoreCase);
        Assert.IsType<System.InvalidOperationException>(record.Cause);
        Assert.Equal("disk is on fire", record.Cause!.Message);

        // The region remains usable: the in-memory mutation is intact and a
        // subsequent read goes through despite persistence being broken.
        var reply = await ReadReplyAsync(system, teller, System.Activator.CreateInstance(readType)!);
        Assert.Equal(30, Prop<int>(reply, "balance"));

        // A second mutation also still works (read-after-write through the
        // failing store) — the failed save did not wedge the region's lock.
        teller.Tell(System.Activator.CreateInstance(depositType, new object[] { 12 })!);
        var afterDeadline = System.DateTime.UtcNow + System.TimeSpan.FromSeconds(10);
        int balance = -1;
        while (System.DateTime.UtcNow < afterDeadline)
        {
            var r = await ReadReplyAsync(system, teller, System.Activator.CreateInstance(readType)!);
            balance = Prop<int>(r, "balance");
            if (balance == 42) break;
            await System.Threading.Tasks.Task.Delay(25);
        }
        Assert.Equal(42, balance);
    }

    // ── Helpers ──────────────────────────────────────────────────────────────

    private static System.Reflection.Assembly Compile(string src, string asmName)
    {
        var parsed = SpekCompiler.Parse(src);
        Assert.True(parsed.Success,
            string.Join("\n", parsed.Diagnostics.Select(d => $"{d.Code} {d.Line}:{d.Column} {d.Message}")));
        var csharp = new FileEmitter().Emit(parsed.Tree!);
        return RoslynCompileHelper.CompileAndLoad(csharp, asmName);
    }

    /// <summary>Sends <paramref name="message"/> to <paramref name="target"/> with a
    /// one-shot capture actor (spawned in the SAME system) as the apparent sender,
    /// and returns the reply object. Bounded wait.</summary>
    private static async Task<object> ReadReplyAsync(ActorSystem system, ActorRef target, object message)
    {
        var tcs = new System.Threading.Tasks.TaskCompletionSource<object>();
        var capture = system.Spawn<ReplyCapture>(tcs);
        target.Tell(message, sender: capture);
        return await tcs.Task.WaitAsync(System.TimeSpan.FromSeconds(10));
    }

    private static T Prop<T>(object reply, string name)
        => (T)reply.GetType().GetProperty(name)!.GetValue(reply)!;

    private static DeadLetterRecord? FindSaveFailure(RecordingDeadLetterSink sink)
    {
        foreach (var r in sink.Records)
            if (r.Reason.Contains("save failed", System.StringComparison.OrdinalIgnoreCase))
                return r;
        return null;
    }

    private sealed class ReplyCapture : ActorBase
    {
        private readonly System.Threading.Tasks.TaskCompletionSource<object> _tcs;
        public ReplyCapture(System.Threading.Tasks.TaskCompletionSource<object> tcs) => _tcs = tcs;
        protected override System.Threading.Tasks.Task DispatchAsync(object message, ActorRef sender)
        {
            _tcs.TrySetResult(message);
            return System.Threading.Tasks.Task.CompletedTask;
        }
    }

    /// <summary>An <see cref="ISnapshotStore"/> whose <c>SaveAsync</c> always throws.
    /// Loads return null (nothing pre-seeded), so restore is a clean no-op and the
    /// region inits from its field defaults — leaving SaveAsync as the only failure
    /// point under test.</summary>
    private sealed class ThrowingOnSaveStore : ISnapshotStore
    {
        private readonly System.Exception _toThrow;
        public ThrowingOnSaveStore(System.Exception toThrow) => _toThrow = toThrow;

        public System.Threading.Tasks.Task SaveAsync(string key, Snapshot snapshot)
            => throw _toThrow;

        public System.Threading.Tasks.Task<Snapshot?> LoadAsync(string key)
            => System.Threading.Tasks.Task.FromResult<Snapshot?>(null);
    }
}

