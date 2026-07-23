using Microsoft.Data.Sqlite;
using Spek.Persistence;
using Spek.Persistence.Sqlite;
using Xunit;

namespace Spek.Tests.Persistence;

/// <summary>
/// Coverage for the embedded SQLite <see cref="SqliteSnapshotStore"/>
/// — durable single-node persistence with WAL-mode concurrency.
/// </summary>
public sealed class SqliteSnapshotStoreTests : IDisposable
{
    private readonly string _dbPath = Path.Combine(
        Path.GetTempPath(),
        "spek-sqlite-store-tests-" + Guid.NewGuid().ToString("N") + ".db");

    public void Dispose()
    {
        // Pool clearing happens in store.Dispose; tests still need
        // belt+suspenders here in case a test bails before disposing.
        SqliteConnection.ClearAllPools();
        if (System.IO.File.Exists(_dbPath)) System.IO.File.Delete(_dbPath);

        // WAL leaves -wal and -shm sidecars; clean them too.
        foreach (var sidecar in new[] { "-wal", "-shm" })
        {
            var path = _dbPath + sidecar;
            if (System.IO.File.Exists(path)) System.IO.File.Delete(path);
        }
    }

    [Fact]
    public async Task Save_then_Load_roundtrips_a_single_field()
    {
        using var store = new SqliteSnapshotStore(_dbPath);
        var snap = new Snapshot(new Dictionary<string, object?>
        {
            ["balance"] = 12345L,
        });

        await store.SaveAsync("Wallet/user-1", snap);
        var loaded = await store.LoadAsync("Wallet/user-1");

        Assert.NotNull(loaded);
        Assert.Equal(12345L, loaded!.Get<long>("balance"));
    }

    [Fact]
    public async Task Load_returns_null_for_unknown_key()
    {
        using var store = new SqliteSnapshotStore(_dbPath);
        var loaded = await store.LoadAsync("never-saved");
        Assert.Null(loaded);
    }

    [Fact]
    public async Task Upsert_replaces_prior_snapshot_for_same_key()
    {
        using var store = new SqliteSnapshotStore(_dbPath);
        await store.SaveAsync("k", new Snapshot(new Dictionary<string, object?> { ["v"] = 1L }));
        await store.SaveAsync("k", new Snapshot(new Dictionary<string, object?> { ["v"] = 2L }));

        var loaded = await store.LoadAsync("k");
        Assert.NotNull(loaded);
        Assert.Equal(2L, loaded!.Get<long>("v"));
    }

    [Fact]
    public async Task Snapshot_survives_store_disposal_and_reopen()
    {
        // The whole point of durable persistence — process-restart-style
        // cycle should round-trip the data via the on-disk file.
        using (var store = new SqliteSnapshotStore(_dbPath))
        {
            await store.SaveAsync("survivor", new Snapshot(new Dictionary<string, object?>
            {
                ["v"] = 7L,
            }));
        }

        SqliteConnection.ClearAllPools();

        using var reopened = new SqliteSnapshotStore(_dbPath);
        var loaded = await reopened.LoadAsync("survivor");
        Assert.NotNull(loaded);
        Assert.Equal(7L, loaded!.Get<long>("v"));
    }
}
