using Spek.Persistence;
using Spek.Persistence.Log;
using Xunit;

namespace Spek.Tests.Persistence;

/// <summary>
/// Coverage for the append-only segmented <see cref="LogSnapshotStore"/>.
/// Verifies ISnapshotStore compatibility (latest-segment-wins),
/// history replay, retention bounding, and concurrent-append safety.
/// </summary>
public sealed class LogSnapshotStoreTests : IDisposable
{
    private readonly string _tempDir = Path.Combine(
        Path.GetTempPath(),
        "spek-log-store-tests-" + Guid.NewGuid().ToString("N"));

    public void Dispose()
    {
        if (Directory.Exists(_tempDir))
            Directory.Delete(_tempDir, recursive: true);
    }

    private static Snapshot SnapshotOf(long v) =>
        new(new Dictionary<string, object?> { ["v"] = v });

    [Fact]
    public async Task Save_then_Load_returns_latest_segmentAsync()
    {
        var store = new LogSnapshotStore(_tempDir);
        await store.SaveAsync("k", SnapshotOf(1));
        await store.SaveAsync("k", SnapshotOf(2));
        await store.SaveAsync("k", SnapshotOf(3));

        var latest = await store.LoadAsync("k");
        Assert.NotNull(latest);
        Assert.Equal(3L, latest!.Get<long>("v"));
    }

    [Fact]
    public async Task Load_returns_null_for_unknown_keyAsync()
    {
        var store  = new LogSnapshotStore(_tempDir);
        var loaded = await store.LoadAsync("never-saved");
        Assert.Null(loaded);
    }

    [Fact]
    public async Task SaveAsync_appends_a_new_segment_each_timeAsync()
    {
        var store = new LogSnapshotStore(_tempDir);
        for (int i = 1; i <= 5; i++)
            await store.SaveAsync("k", SnapshotOf(i));

        Assert.Equal(5, store.SegmentCount("k"));
    }

    [Fact]
    public async Task ReadHistory_yields_segments_in_chronological_orderAsync()
    {
        var store = new LogSnapshotStore(_tempDir);
        for (int i = 1; i <= 4; i++)
            await store.SaveAsync("k", SnapshotOf(i));

        var values = new List<long>();
        await foreach (var snap in store.ReadHistoryAsync("k"))
            values.Add(snap.Get<long>("v"));

        Assert.Equal(new[] { 1L, 2L, 3L, 4L }, values);
    }

    [Fact]
    public async Task ReadHistory_yields_empty_for_unknown_keyAsync()
    {
        var store  = new LogSnapshotStore(_tempDir);
        var count  = 0;
        await foreach (var _ in store.ReadHistoryAsync("never-saved")) count++;
        Assert.Equal(0, count);
    }

    [Fact]
    public async Task MaxSegments_caps_retention_per_keyAsync()
    {
        var store = new LogSnapshotStore(_tempDir, maxSegments: 3);
        for (int i = 1; i <= 7; i++)
            await store.SaveAsync("k", SnapshotOf(i));

        Assert.Equal(3, store.SegmentCount("k"));

        // The three retained should be the most recent three.
        var values = new List<long>();
        await foreach (var snap in store.ReadHistoryAsync("k"))
            values.Add(snap.Get<long>("v"));
        Assert.Equal(new[] { 5L, 6L, 7L }, values);
    }

    [Fact]
    public async Task CompactAsync_trims_oldest_segmentsAsync()
    {
        var store = new LogSnapshotStore(_tempDir);
        for (int i = 1; i <= 10; i++)
            await store.SaveAsync("k", SnapshotOf(i));

        await store.CompactAsync("k", keepLast: 2);

        Assert.Equal(2, store.SegmentCount("k"));
        var values = new List<long>();
        await foreach (var snap in store.ReadHistoryAsync("k"))
            values.Add(snap.Get<long>("v"));
        Assert.Equal(new[] { 9L, 10L }, values);
    }

    [Fact]
    public async Task SequenceNumbers_keep_incrementing_after_compactionAsync()
    {
        // Compaction trims oldest; the next append must still get a
        // sequence > the highest remaining one (not restart at 1).
        var store = new LogSnapshotStore(_tempDir);
        for (int i = 1; i <= 5; i++) await store.SaveAsync("k", SnapshotOf(i));
        await store.CompactAsync("k", keepLast: 2);

        await store.SaveAsync("k", SnapshotOf(99));
        var latest = await store.LoadAsync("k");

        Assert.NotNull(latest);
        Assert.Equal(99L, latest!.Get<long>("v"));
        Assert.Equal(3, store.SegmentCount("k"));
    }

    [Fact]
    public async Task Concurrent_appends_to_same_key_produce_distinct_segmentsAsync()
    {
        var store = new LogSnapshotStore(_tempDir);

        var tasks = Enumerable.Range(0, 50).Select(i =>
            store.SaveAsync("k", SnapshotOf(i)));
        await Task.WhenAll(tasks);

        // 50 distinct segments — sequence numbers stay strictly
        // monotonic under contention because of the per-key lock.
        Assert.Equal(50, store.SegmentCount("k"));
    }

    [Fact]
    public async Task Concurrent_appends_to_different_keys_are_independentAsync()
    {
        var store = new LogSnapshotStore(_tempDir);

        var tasks = Enumerable.Range(0, 50).Select(i =>
            store.SaveAsync($"k-{i}", SnapshotOf(i)));
        await Task.WhenAll(tasks);

        for (int i = 0; i < 50; i++)
        {
            var loaded = await store.LoadAsync($"k-{i}");
            Assert.NotNull(loaded);
            Assert.Equal((long)i, loaded!.Get<long>("v"));
        }
    }

    [Fact]
    public void Constructor_rejects_zero_or_negative_maxSegments()
    {
        Assert.Throws<ArgumentOutOfRangeException>(() =>
            new LogSnapshotStore(_tempDir, maxSegments: 0));
        Assert.Throws<ArgumentOutOfRangeException>(() =>
            new LogSnapshotStore(_tempDir, maxSegments: -1));
    }

    [Fact]
    public async Task SlashKey_creates_subdirectory_layoutAsync()
    {
        var store = new LogSnapshotStore(_tempDir);
        await store.SaveAsync("Account/user-42", SnapshotOf(7));
        await store.SaveAsync("Account/user-42", SnapshotOf(8));

        var keyDir = Path.Combine(_tempDir, "Account", "user-42");
        Assert.True(Directory.Exists(keyDir));
        Assert.Equal(2, Directory.GetFiles(keyDir, "*.snapshot.json").Length);

        var latest = await store.LoadAsync("Account/user-42");
        Assert.Equal(8L, latest!.Get<long>("v"));
    }
}
