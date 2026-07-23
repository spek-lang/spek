using Spek;
using Spek.Persistence;
using Spek.Persistence.File;
using Spek.Persistence.Sqlite;
using Spek.Runtime;
using Xunit;

namespace Spek.Tests.Persistence;

/// <summary>
/// End-to-end coverage that the full
/// <c>ActorSystem → SpawnPersistent → persist → restart → OnRestore</c>
/// loop works through both file-system and SQLite snapshot stores.
/// This is the strongest correctness check for the persistence loop — the
/// scenario the packages were built to enable.
/// </summary>
public sealed class EndToEndPersistenceTests : IDisposable
{
    private readonly string _tempDir = Path.Combine(
        Path.GetTempPath(),
        "spek-e2e-persist-" + Guid.NewGuid().ToString("N"));

    public void Dispose()
    {
        Microsoft.Data.Sqlite.SqliteConnection.ClearAllPools();
        if (Directory.Exists(_tempDir))
            Directory.Delete(_tempDir, recursive: true);
    }

    public sealed record Increment();
    public sealed record Get();
    public sealed record Reply(int N);

    /// <summary>
    /// Persists `_count` after every Increment; restores it on
    /// rehydration. The minimal shape of a persistent actor.
    /// </summary>
    private sealed class CounterActor : ActorBase
    {
        private int _count;

        protected override async Task DispatchAsync(object message, ActorRef sender)
        {
            _currentSender = sender;
            switch (message)
            {
                case Increment: _count++; await PersistAsync(); break;
                case Get:       sender.Tell(new Reply(_count)); break;
            }
        }

        protected override IReadOnlyDictionary<string, object?> CaptureFields() =>
            new Dictionary<string, object?> { ["count"] = _count };

        protected override void OnRestore(Snapshot s) => _count = s.Get<int>("count");
    }

    [Fact]
    public async Task FileStore_State_Survives_System_Restart()
    {
        var store = new FileSnapshotStore(_tempDir);

        // First lifetime — increment three times.
        using (var system = new ActorSystem("svc-1", snapshotStore: store))
        {
            var actor = system.SpawnPersistent<CounterActor>("counter-A");
            actor.Tell(new Increment());
            actor.Tell(new Increment());
            actor.Tell(new Increment());
            Assert.True(system.AwaitTermination(TimeSpan.FromSeconds(10)));
        }

        // Second lifetime — fresh system + same store; count restores.
        using (var system = new ActorSystem("svc-2", snapshotStore: store))
        {
            var rehydrated = await system.SpawnPersistentAsync<CounterActor>("counter-A");

            // Use a probe-style receiver to capture the reply.
            var reply = await ReadReplyAsync(rehydrated, new Get());
            Assert.Equal(3, reply.N);
        }
    }

    [Fact]
    public async Task SqliteStore_State_Survives_System_Restart()
    {
        var dbPath = Path.Combine(_tempDir, "spek.db");
        Directory.CreateDirectory(_tempDir);

        using (var store = new SqliteSnapshotStore(dbPath))
        using (var system = new ActorSystem("svc-1", snapshotStore: store))
        {
            var actor = system.SpawnPersistent<CounterActor>("counter-B");
            actor.Tell(new Increment());
            actor.Tell(new Increment());
            Assert.True(system.AwaitTermination(TimeSpan.FromSeconds(10)));
        }

        Microsoft.Data.Sqlite.SqliteConnection.ClearAllPools();

        using (var store = new SqliteSnapshotStore(dbPath))
        using (var system = new ActorSystem("svc-2", snapshotStore: store))
        {
            var rehydrated = await system.SpawnPersistentAsync<CounterActor>("counter-B");
            var reply = await ReadReplyAsync(rehydrated, new Get());
            Assert.Equal(2, reply.N);
        }
    }

    /// <summary>
    /// Spawns a one-shot capture actor, sends <paramref name="message"/>
    /// to <paramref name="target"/> with the capture as the apparent
    /// sender, and awaits the reply. Mirrors what TestProbe does
    /// internally but without bringing the Spek.Testing dep into
    /// these tests.
    /// </summary>
    private static async Task<Reply> ReadReplyAsync(ActorRef target, object message)
    {
        var tcs = new TaskCompletionSource<Reply>();

        // Spin up a tiny ad-hoc system to host the capture actor —
        // actors can't span systems, so the capture lives next to the
        // sender in our test code, but the *target* may live in a
        // different ActorSystem since AskAsync is local-only and the
        // testing pattern here uses Tell-with-explicit-sender.
        // For simplicity we reuse the target's slot via Tell-with-self.
        // Build a minimal receiver via a captured TCS pattern.
        var captureSystem = new ActorSystem("capture");
        var capture       = captureSystem.Spawn<ReplyCapture>(tcs);
        target.Tell(message, sender: capture);

        var reply = await tcs.Task.WaitAsync(TimeSpan.FromSeconds(10));
        captureSystem.Dispose();
        return reply;
    }

    private sealed class ReplyCapture : ActorBase
    {
        private readonly TaskCompletionSource<Reply> _tcs;
        public ReplyCapture(TaskCompletionSource<Reply> tcs) => _tcs = tcs;

        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            if (message is Reply r) _tcs.TrySetResult(r);
            return Task.CompletedTask;
        }
    }
}
