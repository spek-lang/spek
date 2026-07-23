using Spek.Persistence;
using Spek.Persistence.File;
using Xunit;

namespace Spek.Tests.Persistence;

/// <summary>
/// Coverage for the JSON-on-disk <see cref="FileSnapshotStore"/> —
/// the default "no database needed" persistence path for desktop /
/// CLI / single-node service deployments.
/// </summary>
public sealed class FileSnapshotStoreTests : IDisposable
{
    private readonly string _tempDir = Path.Combine(
        Path.GetTempPath(),
        "spek-file-store-tests-" + Guid.NewGuid().ToString("N"));

    public void Dispose()
    {
        if (Directory.Exists(_tempDir))
            Directory.Delete(_tempDir, recursive: true);
    }

    [Fact]
    public async Task Save_then_Load_roundtrips_a_single_fieldAsync()
    {
        var store = new FileSnapshotStore(_tempDir);
        var snap  = new Snapshot(new Dictionary<string, object?>
        {
            ["balance"] = 12345L,
        });

        await store.SaveAsync("Wallet/user-1", snap);
        var loaded = await store.LoadAsync("Wallet/user-1");

        Assert.NotNull(loaded);
        Assert.Equal(12345L, loaded!.Get<long>("balance"));
    }

    [Fact]
    public async Task Load_returns_null_for_unknown_keyAsync()
    {
        var store  = new FileSnapshotStore(_tempDir);
        var loaded = await store.LoadAsync("never-saved");
        Assert.Null(loaded);
    }

    [Fact]
    public async Task Save_overwrites_prior_snapshot_for_same_keyAsync()
    {
        var store = new FileSnapshotStore(_tempDir);
        await store.SaveAsync("k", new Snapshot(new Dictionary<string, object?> { ["v"] = 1L }));
        await store.SaveAsync("k", new Snapshot(new Dictionary<string, object?> { ["v"] = 2L }));

        var loaded = await store.LoadAsync("k");
        Assert.NotNull(loaded);
        Assert.Equal(2L, loaded!.Get<long>("v"));
    }

    [Fact]
    public async Task Slash_in_key_creates_subdirectoryAsync()
    {
        var store = new FileSnapshotStore(_tempDir);
        await store.SaveAsync("Account/user-42", new Snapshot(new Dictionary<string, object?>
        {
            ["balance"] = 100L,
        }));

        // The on-disk path is implementation detail, but the
        // subdirectory layout should mirror the slash structure.
        var path = Path.Combine(_tempDir, "Account", "user-42.snapshot.json");
        Assert.True(System.IO.File.Exists(path),
            $"Expected snapshot at {path}; tree contained: {string.Join(", ", Directory.EnumerateFiles(_tempDir, "*", SearchOption.AllDirectories))}");
    }

    [Fact]
    public async Task Unsafe_filename_chars_are_percent_encodedAsync()
    {
        // Keys may contain characters that aren't path-safe. The
        // store should encode them rather than refuse the write.
        var store = new FileSnapshotStore(_tempDir);
        await store.SaveAsync("user:42@host", new Snapshot(new Dictionary<string, object?>
        {
            ["v"] = 99L,
        }));

        var loaded = await store.LoadAsync("user:42@host");
        Assert.NotNull(loaded);
        Assert.Equal(99L, loaded!.Get<long>("v"));
    }

    [Fact]
    public async Task Concurrent_saves_to_different_keys_do_not_corrupt_each_otherAsync()
    {
        var store = new FileSnapshotStore(_tempDir);

        var saves = Enumerable.Range(0, 50).Select(i =>
            store.SaveAsync($"k-{i}", new Snapshot(new Dictionary<string, object?>
            {
                ["i"] = (long)i,
            })));
        await Task.WhenAll(saves);

        for (int i = 0; i < 50; i++)
        {
            var loaded = await store.LoadAsync($"k-{i}");
            Assert.NotNull(loaded);
            Assert.Equal((long)i, loaded!.Get<long>("i"));
        }
    }
}
