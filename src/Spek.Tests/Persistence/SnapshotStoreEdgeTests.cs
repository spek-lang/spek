using System.Text.Json;
using Microsoft.Data.Sqlite;
using Spek.Persistence;
using Spek.Persistence.File;
using Spek.Persistence.Log;
using Spek.Persistence.Sqlite;
using Spek.Runtime;
using Xunit;

namespace Spek.Tests.Persistence;

/// <summary>
/// Cross-store edge-case coverage for the four <see cref="ISnapshotStore"/>
/// implementations (InMemory / File / Sqlite / Log). The existing per-store
/// tests round-trip a single <c>long</c> field and the obvious happy path;
/// this suite targets the gaps that bite in production:
///
///   • round-trip fidelity of mixed field types (int / long / double / bool /
///     string / null) through the JSON-backed stores, where the only path
///     back is <c>JsonElement.Deserialize<T></c>;
///   • the <see cref="Snapshot"/> contract itself (null-field default,
///     missing-key throw, empty-snapshot is distinct from never-saved);
///   • per-store divergences the abstraction deliberately allows — InMemory
///     stores the live object (reference identity), Log appends history while
///     the other three collapse to latest-wins, and key validation differs
///     across stores;
///   • the Log store's garbage-segment robustness fallback in NextSequence.
///
/// Temp dirs / db files are created per-test-class instance and cleaned in
/// <see cref="Dispose"/>.
/// </summary>
public sealed class SnapshotStoreEdgeTests : IDisposable
{
    private readonly string _root = Path.Combine(
        Path.GetTempPath(),
        "spek-snapshot-edge-" + Guid.NewGuid().ToString("N"));

    public SnapshotStoreEdgeTests() => Directory.CreateDirectory(_root);

    public void Dispose()
    {
        // SQLite pools hold the db file open; clear so the dir delete succeeds.
        SqliteConnection.ClearAllPools();
        if (Directory.Exists(_root))
        {
            try { Directory.Delete(_root, recursive: true); }
            catch (IOException) { /* best-effort cleanup */ }
        }
    }

    private string FileDir => Path.Combine(_root, "file");
    private string LogDir => Path.Combine(_root, "log");
    private string DbPath => Path.Combine(_root, "store.db");

    private static Snapshot Mixed() => new(new Dictionary<string, object?>
    {
        ["i"]    = 7,
        ["l"]    = 9_000_000_000L,   // outside int range — must stay long
        ["d"]    = 3.5,
        ["flag"] = true,
        ["name"] = "ada",
        ["nil"]  = null,
    });

    /// <summary>Asserts every field of <see cref="Mixed"/> reads back at the
    /// right type and value through whatever serialization the store used.</summary>
    private static void AssertMixed(Snapshot? loaded)
    {
        Assert.NotNull(loaded);
        Assert.Equal(7, loaded!.Get<int>("i"));
        Assert.Equal(9_000_000_000L, loaded.Get<long>("l"));
        Assert.Equal(3.5, loaded.Get<double>("d"));
        Assert.True(loaded.Get<bool>("flag"));
        Assert.Equal("ada", loaded.Get<string>("name"));
        // A persisted null comes back as the type default rather than throwing.
        Assert.Null(loaded.Get<string?>("nil"));
    }

    // ───────────────────────── InMemory store ─────────────────────────

    [Fact]
    public async Task InMemory_Load_returns_null_for_unknown_key()
    {
        var store = new InMemorySnapshotStore();
        Assert.Null(await store.LoadAsync("never-saved"));
    }

    [Fact]
    public async Task InMemory_second_Save_overwrites_latest_wins()
    {
        var store = new InMemorySnapshotStore();
        await store.SaveAsync("k", new Snapshot(new Dictionary<string, object?> { ["v"] = 1 }));
        await store.SaveAsync("k", new Snapshot(new Dictionary<string, object?> { ["v"] = 2 }));

        var loaded = await store.LoadAsync("k");
        Assert.NotNull(loaded);
        Assert.Equal(2, loaded!.Get<int>("v"));
    }

    [Fact]
    public async Task InMemory_round_trips_mixed_types_and_null()
    {
        var store = new InMemorySnapshotStore();
        await store.SaveAsync("k", Mixed());
        AssertMixed(await store.LoadAsync("k"));
    }

    [Fact]
    public async Task InMemory_returns_the_same_live_Snapshot_instance()
    {
        // Divergence vs the JSON-backed stores: InMemory keeps the live object,
        // so Get<int> sees the *boxed int* directly (the `value is T` fast path),
        // and the loaded reference is the very snapshot that was saved — no
        // serialization round-trip in between.
        var store = new InMemorySnapshotStore();
        var saved = new Snapshot(new Dictionary<string, object?> { ["v"] = 41 });
        await store.SaveAsync("k", saved);

        var loaded = await store.LoadAsync("k");
        Assert.Same(saved, loaded);
        Assert.Equal(41, loaded!.Get<int>("v"));
    }

    // ───────────────────────── File store ─────────────────────────

    [Fact]
    public async Task File_Load_returns_null_for_unknown_key()
    {
        var store = new FileSnapshotStore(FileDir);
        Assert.Null(await store.LoadAsync("never-saved"));
    }

    [Fact]
    public async Task File_round_trips_mixed_types_and_null()
    {
        var store = new FileSnapshotStore(FileDir);
        await store.SaveAsync("acct/1", Mixed());
        AssertMixed(await store.LoadAsync("acct/1"));
    }

    [Fact]
    public async Task File_overwrite_with_different_shape_only_keeps_latest()
    {
        // Latest-wins also means the *shape* is replaced: a key the first
        // snapshot carried but the second omits must be gone after overwrite
        // (the file is atomically replaced, not merged).
        var store = new FileSnapshotStore(FileDir);
        await store.SaveAsync("k", new Snapshot(new Dictionary<string, object?>
        {
            ["a"] = 1, ["b"] = "first",
        }));
        await store.SaveAsync("k", new Snapshot(new Dictionary<string, object?>
        {
            ["a"] = 2,   // 'b' deliberately dropped
        }));

        var loaded = await store.LoadAsync("k");
        Assert.NotNull(loaded);
        Assert.Equal(2, loaded!.Get<int>("a"));
        Assert.False(loaded.Fields.ContainsKey("b"));
    }

    [Fact]
    public async Task File_empty_snapshot_round_trips_to_empty_not_null()
    {
        // A saved-but-empty snapshot must be distinguishable from a key that
        // was never written: LoadAsync returns a non-null Snapshot with zero
        // fields, NOT null (which would look like "nothing persisted").
        var store = new FileSnapshotStore(FileDir);
        await store.SaveAsync("empty", new Snapshot(new Dictionary<string, object?>()));

        var loaded = await store.LoadAsync("empty");
        Assert.NotNull(loaded);
        Assert.Empty(loaded!.Fields);
    }

    [Fact]
    public async Task File_load_throws_on_empty_key()
    {
        // File resolves the path eagerly (ArgumentException.ThrowIfNullOrEmpty
        // inside ResolvePath), so an empty key is rejected on Load too — not
        // silently treated as a missing key.
        var store = new FileSnapshotStore(FileDir);
        await Assert.ThrowsAsync<ArgumentException>(() => store.LoadAsync(""));
    }

    // ───────────────────────── Sqlite store ─────────────────────────

    [Fact]
    public async Task Sqlite_Load_returns_null_for_unknown_key()
    {
        using var store = new SqliteSnapshotStore(DbPath);
        Assert.Null(await store.LoadAsync("never-saved"));
    }

    [Fact]
    public async Task Sqlite_round_trips_mixed_types_and_null()
    {
        using var store = new SqliteSnapshotStore(DbPath);
        await store.SaveAsync("acct/1", Mixed());
        AssertMixed(await store.LoadAsync("acct/1"));
    }

    [Fact]
    public async Task Sqlite_upsert_with_different_shape_only_keeps_latest()
    {
        using var store = new SqliteSnapshotStore(DbPath);
        await store.SaveAsync("k", new Snapshot(new Dictionary<string, object?>
        {
            ["a"] = 1, ["b"] = "first",
        }));
        await store.SaveAsync("k", new Snapshot(new Dictionary<string, object?>
        {
            ["a"] = 2,
        }));

        var loaded = await store.LoadAsync("k");
        Assert.NotNull(loaded);
        Assert.Equal(2, loaded!.Get<int>("a"));
        Assert.False(loaded.Fields.ContainsKey("b"));
    }

    [Fact]
    public async Task Sqlite_save_and_load_reject_empty_key()
    {
        using var store = new SqliteSnapshotStore(DbPath);
        await Assert.ThrowsAsync<ArgumentException>(() =>
            store.SaveAsync("", new Snapshot(new Dictionary<string, object?>())));
        await Assert.ThrowsAsync<ArgumentException>(() => store.LoadAsync(""));
    }

    // ───────────────────────── Log store ─────────────────────────

    [Fact]
    public async Task Log_Load_returns_null_for_unknown_key()
    {
        var store = new LogSnapshotStore(LogDir);
        Assert.Null(await store.LoadAsync("never-saved"));
    }

    [Fact]
    public async Task Log_round_trips_mixed_types_and_null()
    {
        var store = new LogSnapshotStore(LogDir);
        await store.SaveAsync("acct/1", Mixed());
        AssertMixed(await store.LoadAsync("acct/1"));
    }

    [Fact]
    public async Task Log_appends_history_while_Load_returns_latest()
    {
        // Divergence vs the latest-wins stores: the same two saves that
        // *replace* in File/Sqlite are both *retained* in the Log. LoadAsync
        // still returns only the latest, but ReadHistoryAsync sees both shapes
        // in chronological order.
        var store = new LogSnapshotStore(LogDir);
        await store.SaveAsync("k", new Snapshot(new Dictionary<string, object?> { ["v"] = "first" }));
        await store.SaveAsync("k", new Snapshot(new Dictionary<string, object?> { ["v"] = "second" }));

        var latest = await store.LoadAsync("k");
        Assert.Equal("second", latest!.Get<string>("v"));

        var history = new List<string>();
        await foreach (var snap in store.ReadHistoryAsync("k"))
            history.Add(snap.Get<string>("v"));
        Assert.Equal(new[] { "first", "second" }, history);
    }

    [Fact]
    public async Task Log_NextSequence_appends_past_a_garbage_segment_file()
    {
        // Append-side robustness: NextSequence has an explicit fallback for a
        // non-numeric file sorting to the end of the key directory — it walks
        // back to the highest *parseable* sequence and appends after it rather
        // than colliding or resetting to 1. (Read-side tolerance is a separate,
        // currently-broken concern — see the skipped bug test below.)
        var store = new LogSnapshotStore(LogDir);
        await store.SaveAsync("k", new Snapshot(new Dictionary<string, object?> { ["v"] = 1 }));
        await store.SaveAsync("k", new Snapshot(new Dictionary<string, object?> { ["v"] = 2 }));

        // Drop a junk file that lexicographically sorts AFTER the real
        // zero-padded sequence segments (digits sort before 'z' under Ordinal).
        var keyDir = Path.Combine(LogDir, "k");
        var junk = Path.Combine(keyDir, "zzz-garbage.snapshot.json");
        await System.IO.File.WriteAllTextAsync(junk, "{ not valid sequence }");

        // The next real append must land on sequence 3 — above the highest
        // parseable one (2) — not collide with an existing segment or restart.
        await store.SaveAsync("k", new Snapshot(new Dictionary<string, object?> { ["v"] = 3 }));

        var expectedSegment = Path.Combine(keyDir, "0000000003.snapshot.json");
        Assert.True(System.IO.File.Exists(expectedSegment),
            $"expected NextSequence to advance to 3; key dir contained: "
            + string.Join(", ", Directory.GetFiles(keyDir).Select(Path.GetFileName)));

        // SegmentCount now counts only valid numbered segments — the stray junk
        // file isn't a segment — so 3, not 4. (Same EnumerateSegments fix that lets
        // LoadAsync skip the junk; see Log_Load_should_skip_a_garbage_segment_file.)
        Assert.Equal(3, store.SegmentCount("k"));
    }

    // Fixed: EnumerateSegments now filters to numbered segments + orders numerically,
    // so a stray non-numeric *.snapshot.json is skipped instead of JSON-parsed.
    [Fact]
    public async Task Log_Load_should_skip_a_garbage_segment_file()
    {
        var store = new LogSnapshotStore(LogDir);
        await store.SaveAsync("k", new Snapshot(new Dictionary<string, object?> { ["v"] = 1 }));
        await store.SaveAsync("k", new Snapshot(new Dictionary<string, object?> { ["v"] = 2 }));

        var keyDir = Path.Combine(LogDir, "k");
        var junk = Path.Combine(keyDir, "zzz-garbage.snapshot.json");
        await System.IO.File.WriteAllTextAsync(junk, "{ not valid sequence }");

        // LoadAsync should ignore the unparseable, non-numbered file and return
        // the real latest numbered segment. Today it throws instead.
        var latest = await store.LoadAsync("k");
        Assert.NotNull(latest);
        Assert.Equal(2, latest!.Get<int>("v"));
    }

    [Fact]
    public async Task Log_CompactAsync_to_zero_clears_all_history()
    {
        // keepLast: 0 is explicitly allowed (only < 0 throws). It must drop
        // every segment, after which Load returns null again.
        var store = new LogSnapshotStore(LogDir);
        for (int i = 1; i <= 4; i++)
            await store.SaveAsync("k", new Snapshot(new Dictionary<string, object?> { ["v"] = i }));

        await store.CompactAsync("k", keepLast: 0);

        Assert.Equal(0, store.SegmentCount("k"));
        Assert.Null(await store.LoadAsync("k"));
    }

    [Fact]
    public async Task Log_CompactAsync_rejects_negative_keepLast()
    {
        var store = new LogSnapshotStore(LogDir);
        await store.SaveAsync("k", new Snapshot(new Dictionary<string, object?> { ["v"] = 1 }));
        await Assert.ThrowsAsync<ArgumentOutOfRangeException>(
            () => store.CompactAsync("k", keepLast: -1));
    }

    // ───────────── Snapshot contract (store-independent) ─────────────

    [Fact]
    public void Snapshot_Get_throws_KeyNotFound_for_absent_field()
    {
        var snap = new Snapshot(new Dictionary<string, object?> { ["present"] = 1 });
        Assert.Throws<KeyNotFoundException>(() => snap.Get<int>("missing"));
    }

    [Fact]
    public void Snapshot_Get_coerces_a_JsonElement_number_to_the_requested_type()
    {
        // Mirrors exactly what the JSON-backed stores hand back: a field whose
        // raw value is a JsonElement number must deserialize to the requested
        // numeric type, not throw an InvalidCastException.
        using var doc = JsonDocument.Parse("{\"n\": 42}");
        var element = doc.RootElement.GetProperty("n").Clone();
        var snap = new Snapshot(new Dictionary<string, object?> { ["n"] = element });

        Assert.Equal(42, snap.Get<int>("n"));
        Assert.Equal(42L, snap.Get<long>("n"));
        Assert.Equal(42.0, snap.Get<double>("n"));
    }
}

