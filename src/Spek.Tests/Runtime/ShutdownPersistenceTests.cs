using System.Reflection;
using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Spek.Persistence;   // ISnapshotStore, Snapshot
using Spek.Runtime;
using Spek.Tests.Emit;   // RoslynCompileHelper
using Xunit;

namespace Spek.Tests.Runtime;

/// <summary>
/// Durable shutdown for shared regions. A chain of actors all touch one
/// <c>shared … : Persisted</c> region; the leaf triggers <c>self.System.Shutdown()</c>.
/// The region's writer-exit saves are fire-and-forget, so shutdown flushes the
/// final state (ActorSystem.Dispose → PersistedRegion.FlushSave) — without that,
/// the last save races teardown and is lost. A second system over the same store
/// must rehydrate the final value.
/// </summary>
public sealed class ShutdownPersistenceTests
{
    private const string Src = """
        namespace ShutdownPersistDemo;

        shared Tally : Persisted
        {
            int Total = 0;
        }

        message Bump();
        message GetTotal();
        message TotalReply(int total);

        actor Link
        {
            use Tally tally;
            ActorRef? downstream;

            init(ActorRef? next) { downstream = next; }

            writer on Bump =>
            {
                tally.Total = tally.Total + 1;       // writer mutation → region save on exit
                if (downstream != null) { downstream.Tell(new Bump()); }
                else { self.System.Shutdown(); }     // the leaf initiates shutdown
            }

            reader on GetTotal => { sender.Tell(new TotalReply(tally.Total)); }
        }

        // Present only to satisfy CE0098 (a : Persisted region needs a registered
        // provider). The test drives its own systems and wires the store via the
        // ActorSystem ctor instead, so this Main is never run.
        program Main
        {
            var system = new ActorSystem("demo");
            system.RegisterPersistenceProvider<Tally>(new InMemorySnapshotStore());
        }
        """;

    [Fact]
    public async Task TenChainedActors_SharedPersistedRegion_FlushesOnShutdown_AndRehydratesAsync()
    {
        var parsed = SpekCompiler.Parse(Src);
        Assert.True(parsed.Success,
            string.Join("\n", parsed.Diagnostics.Select(d => $"{d.Code} {d.Line}:{d.Column} {d.Message}")));
        var csharp = new FileEmitter().Emit(parsed.Tree!);
        var asm    = RoslynCompileHelper.CompileAndLoad(csharp, "ShutdownPersist");

        var linkType  = asm.GetType("ShutdownPersistDemo.Link")!;
        var bumpType  = asm.GetType("ShutdownPersistDemo.Bump")!;
        var totalType = asm.GetType("ShutdownPersistDemo.GetTotal")!;

        var inner = new InMemorySnapshotStore();
        // Saves are *slow* — this is what makes the test prove the flush is
        // load-bearing: a fire-and-forget writer-exit save can't have landed by the
        // time shutdown returns, so only the synchronous shutdown flush makes the
        // final state durable. Loads stay fast.
        var store = new DelayingSnapshotStore(inner, saveDelay: TimeSpan.FromMilliseconds(300));

        // ── System 1: a 10-deep chain, all sharing Tally; the leaf shuts down ──
        var system1 = new ActorSystem("s1", snapshotStore: store);
        try
        {
            ActorRef? next = null;
            for (int i = 0; i < 10; i++)                       // leaf-first so each link gets its child
                next = system1.Spawn(linkType, new object[] { next! });
            var root = next!;                                  // depth 0

            root.Tell(Activator.CreateInstance(bumpType)!);    // propagates down → leaf shuts the system down

            Assert.True(system1.AwaitTermination(TimeSpan.FromSeconds(10)),
                "system should terminate after the leaf-initiated shutdown");
        }
        finally { system1.Dispose(); }   // idempotent; also flushes if the leaf path didn't

        // Durable on shutdown: the region's final state was flushed *before* teardown
        // completed, despite the slow store. Without the shutdown flush, the delayed
        // fire-and-forget saves wouldn't have landed yet and this would be null/stale.
        var saved = await inner.LoadAsync("Tally");
        Assert.NotNull(saved);
        Assert.Equal(10, saved!.Get<int>("Total"));

        // ── System 2: same store → the region rehydrates on first access ──
        using var system2 = new ActorSystem("s2", snapshotStore: store);
        var reader = system2.Spawn(linkType, new object[] { null! });   // downstream null; read-only
        var total  = await ReadTotalAsync(system2, reader, Activator.CreateInstance(totalType)!);

        Assert.Equal(10, total);   // all 10 bumps survived shutdown and rehydrated
    }

    private static async Task<int> ReadTotalAsync(ActorSystem system, ActorRef target, object getTotal)
    {
        var tcs     = new TaskCompletionSource<object>();
        var capture = system.Spawn<ReplyCapture>(tcs);     // capture lives in the target's system
        target.Tell(getTotal, sender: capture);
        var reply = await tcs.Task.WaitAsync(TimeSpan.FromSeconds(10));
        return (int)reply.GetType().GetProperty("total")!.GetValue(reply)!;
    }

    private sealed class ReplyCapture : ActorBase
    {
        private readonly TaskCompletionSource<object> _tcs;
        public ReplyCapture(TaskCompletionSource<object> tcs) => _tcs = tcs;
        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            _tcs.TrySetResult(message);
            return Task.CompletedTask;
        }
    }

    /// <summary>Wraps a store with a slow <c>SaveAsync</c> so the test can prove the
    /// shutdown flush actually waits for the final save (fast loads).</summary>
    private sealed class DelayingSnapshotStore(ISnapshotStore inner, TimeSpan saveDelay) : ISnapshotStore
    {
        public async Task SaveAsync(string key, Snapshot snapshot)
        {
            await Task.Delay(saveDelay).ConfigureAwait(false);
            await inner.SaveAsync(key, snapshot).ConfigureAwait(false);
        }

        public Task<Snapshot?> LoadAsync(string key) => inner.LoadAsync(key);
    }
}
