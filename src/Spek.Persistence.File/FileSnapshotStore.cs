using System.Text;
using System.Text.Json;

namespace Spek.Persistence.File;

/// <summary>
/// File-system-backed <see cref="ISnapshotStore"/>. Each snapshot
/// serialises to a single JSON file at
/// <c>{baseDirectory}/{sanitizedKey}.snapshot.json</c>; writes go
/// through a temp file plus an atomic rename so a crash mid-write
/// never corrupts an existing snapshot.
///
/// Thread-safe — concurrent saves to different keys are independent;
/// concurrent saves to the same key serialise via a per-key lock so
/// the last write wins without races.
/// </summary>
public sealed class FileSnapshotStore : ISnapshotStore
{
    private static readonly JsonSerializerOptions SerializerOptions = new()
    {
        WriteIndented = false,
        PropertyNameCaseInsensitive = true,
    };

    private readonly string _baseDirectory;

    // One lock per key so concurrent SaveAsync calls on different
    // actors don't contend on a global mutex. Per-key locks are
    // created lazily and never reclaimed — the working set is bounded
    // by the number of distinct persistence keys, which is bounded by
    // the number of persistent actors a host has ever seen.
    private readonly Dictionary<string, SemaphoreSlim> _keyLocks =
        new(StringComparer.Ordinal);
    private readonly object _keyLocksGate = new();

    /// <summary>
    /// Creates a store rooted at <paramref name="baseDirectory"/>,
    /// creating the directory if it does not exist. Keys containing
    /// <c>/</c> map to subdirectories beneath it.
    /// </summary>
    public FileSnapshotStore(string baseDirectory)
    {
        ArgumentException.ThrowIfNullOrEmpty(baseDirectory);
        _baseDirectory = baseDirectory;
        Directory.CreateDirectory(_baseDirectory);
    }

    /// <summary>
    /// Writes <paramref name="snapshot"/> as the JSON file for
    /// <paramref name="key"/>, replacing any previous snapshot.
    /// Durable on return: the payload is flushed to disk before the
    /// atomic rename, so a crash at any point leaves either the old
    /// snapshot intact or the new one complete — never a torn file.
    /// </summary>
    public async Task SaveAsync(string key, Snapshot snapshot)
    {
        ArgumentNullException.ThrowIfNull(snapshot);
        var path = ResolvePath(key);

        var dir = Path.GetDirectoryName(path);
        if (!string.IsNullOrEmpty(dir)) Directory.CreateDirectory(dir);

        // The pattern: serialize to {file}.tmp, fsync, then atomically
        // rename onto {file}. POSIX guarantees rename is atomic
        // wrt other observers; Windows gives the same guarantee on
        // the same volume.
        var temp = path + ".tmp";

        var gate = GetOrCreateKeyLock(key);
        await gate.WaitAsync().ConfigureAwait(false);
        try
        {
            await using (var fs = new FileStream(
                temp, FileMode.Create, FileAccess.Write, FileShare.None,
                bufferSize: 4096, useAsync: true))
            {
                await JsonSerializer.SerializeAsync(fs, snapshot.Fields, SerializerOptions)
                    .ConfigureAwait(false);
                fs.Flush(flushToDisk: true);
            }

            // File.Move with overwrite=true gives us atomic replace on
            // both POSIX and Windows (the latter via SetFileInformationByHandle
            // since .NET 5).
            System.IO.File.Move(temp, path, overwrite: true);
        }
        finally
        {
            gate.Release();
        }
    }

    /// <summary>
    /// Reads the latest snapshot for <paramref name="key"/>, or
    /// <c>null</c> if none has been saved. Field values come back as
    /// <see cref="JsonElement"/>s; <see cref="Snapshot.Get{T}"/>
    /// deserialises them on access.
    /// </summary>
    public async Task<Snapshot?> LoadAsync(string key)
    {
        var path = ResolvePath(key);
        if (!System.IO.File.Exists(path)) return null;

        await using var fs = new FileStream(
            path, FileMode.Open, FileAccess.Read, FileShare.Read,
            bufferSize: 4096, useAsync: true);

        var fields = await JsonSerializer
            .DeserializeAsync<Dictionary<string, object?>>(fs, SerializerOptions)
            .ConfigureAwait(false);

        return fields is null ? null : new Snapshot(fields);
    }

    /// <summary>The directory snapshots are written to.</summary>
    public string BaseDirectory => _baseDirectory;

    private string ResolvePath(string key)
    {
        ArgumentException.ThrowIfNullOrEmpty(key);
        // `actor/persistence-id` becomes a subdirectory; other unsafe
        // filename characters are percent-encoded so any key the
        // runtime ever produces is path-safe.
        var parts = key.Split('/', StringSplitOptions.RemoveEmptyEntries);
        var safeParts = parts.Select(SanitizePart).ToArray();
        var combined = Path.Combine(safeParts);
        return Path.Combine(_baseDirectory, combined + ".snapshot.json");
    }

    private static string SanitizePart(string segment)
    {
        var sb = new StringBuilder(segment.Length);
        foreach (var c in segment)
        {
            // Conservative whitelist — letters, digits, '-', '_', '.'
            // pass through; everything else is percent-encoded.
            if (char.IsLetterOrDigit(c) || c == '-' || c == '_' || c == '.')
                sb.Append(c);
            else
                sb.Append('%').Append(((int)c).ToString("X2"));
        }
        return sb.ToString();
    }

    private SemaphoreSlim GetOrCreateKeyLock(string key)
    {
        lock (_keyLocksGate)
        {
            if (!_keyLocks.TryGetValue(key, out var gate))
            {
                gate = new SemaphoreSlim(1, 1);
                _keyLocks[key] = gate;
            }
            return gate;
        }
    }
}
