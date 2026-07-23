using System.Globalization;
using System.Text;
using System.Text.Json;

namespace Spek.Persistence.Log;

/// <summary>
/// Append-only segmented <see cref="ISnapshotStore"/>. Each call to
/// <see cref="SaveAsync"/> writes a new immutable segment file
/// (<c>{baseDir}/{key}/{seq:D10}.snapshot.json</c>) instead of
/// overwriting the prior snapshot. Modeled on Cassandra's SSTable
/// shape: many immutable segments per key, latest wins on read,
/// older segments available for replay and event-sourcing patterns.
///
/// Implements <see cref="ISnapshotStore"/> directly so existing
/// actors work unchanged (they see only the latest segment via
/// <see cref="LoadAsync"/>). Actors that need history replay use
/// <see cref="ReadHistoryAsync"/> directly on this type.
///
/// Thread-safe — concurrent appends to different keys are
/// independent; concurrent appends to the same key serialise via
/// per-key locks so sequence numbers stay strictly monotonic.
/// </summary>
public sealed class LogSnapshotStore : ISnapshotStore
{
    private const string SegmentExtension = ".snapshot.json";
    private const int    SegmentDigits    = 10;   // pads up to 10^10 segments per key

    private static readonly JsonSerializerOptions SerializerOptions = new()
    {
        WriteIndented              = false,
        PropertyNameCaseInsensitive = true,
    };

    private readonly string _baseDirectory;
    private readonly int?   _maxSegments;

    // Per-key write lock. Reads of immutable segments are lock-free;
    // only the append path needs serialisation to keep sequence
    // numbers strictly monotonic per key.
    private readonly Dictionary<string, SemaphoreSlim> _keyLocks =
        new(StringComparer.Ordinal);
    private readonly object _keyLocksGate = new();

    /// <param name="baseDirectory">Root directory for all keys.</param>
    /// <param name="maxSegments">If set, after each append the oldest
    /// segments are deleted so at most this many remain per key.
    /// Null = retain forever (compact manually via
    /// <see cref="CompactAsync"/>).</param>
    public LogSnapshotStore(string baseDirectory, int? maxSegments = null)
    {
        ArgumentException.ThrowIfNullOrEmpty(baseDirectory);
        if (maxSegments is { } m && m <= 0)
            throw new ArgumentOutOfRangeException(nameof(maxSegments),
                "maxSegments must be positive (or null for unbounded retention).");

        _baseDirectory = baseDirectory;
        _maxSegments   = maxSegments;
        Directory.CreateDirectory(_baseDirectory);
    }

    /// <summary>The directory segments are written under.</summary>
    public string BaseDirectory => _baseDirectory;

    /// <summary>
    /// Appends <paramref name="snapshot"/> as a new segment for
    /// <paramref name="key"/>. The sequence number is one greater
    /// than the highest existing segment, or 1 if no segments exist
    /// yet.
    /// </summary>
    public async Task SaveAsync(string key, Snapshot snapshot)
    {
        ArgumentNullException.ThrowIfNull(snapshot);

        var keyDir = ResolveKeyDir(key);
        Directory.CreateDirectory(keyDir);

        var gate = GetOrCreateKeyLock(key);
        await gate.WaitAsync().ConfigureAwait(false);
        try
        {
            var nextSeq = NextSequence(keyDir);
            var path    = SegmentPath(keyDir, nextSeq);
            var temp    = path + ".tmp";

            await using (var fs = new FileStream(
                temp, FileMode.Create, FileAccess.Write, FileShare.None,
                bufferSize: 4096, useAsync: true))
            {
                await JsonSerializer.SerializeAsync(fs, snapshot.Fields, SerializerOptions)
                    .ConfigureAwait(false);
                fs.Flush(flushToDisk: true);
            }

            // Atomic rename — segment is observable only after this
            // returns, so concurrent readers never see a partial file.
            System.IO.File.Move(temp, path, overwrite: true);

            if (_maxSegments is { } cap)
                TrimOldestBeyond(keyDir, cap);
        }
        finally
        {
            gate.Release();
        }
    }

    /// <summary>
    /// Returns the most recent segment for <paramref name="key"/>,
    /// or null if no segments exist. Lock-free — segments are
    /// immutable once written.
    /// </summary>
    public async Task<Snapshot?> LoadAsync(string key)
    {
        var keyDir = ResolveKeyDir(key);
        if (!Directory.Exists(keyDir)) return null;

        var latest = EnumerateSegments(keyDir).LastOrDefault();
        if (latest is null) return null;

        return await ReadSegmentAsync(latest).ConfigureAwait(false);
    }

    /// <summary>
    /// Streams every segment for <paramref name="key"/> in
    /// chronological order (oldest first). Use this for event-sourcing
    /// rehydration — fold the snapshots into the actor's current
    /// state. Returns an empty stream if the key has no segments.
    /// </summary>
    public async IAsyncEnumerable<Snapshot> ReadHistoryAsync(string key)
    {
        var keyDir = ResolveKeyDir(key);
        if (!Directory.Exists(keyDir)) yield break;

        foreach (var path in EnumerateSegments(keyDir))
        {
            var snap = await ReadSegmentAsync(path).ConfigureAwait(false);
            if (snap is not null) yield return snap;
        }
    }

    /// <summary>
    /// Deletes all but the most recent <paramref name="keepLast"/>
    /// segments for <paramref name="key"/>. Useful when retention
    /// wasn't bounded at construction time, or for manual compaction
    /// after an event-sourcing checkpoint.
    /// </summary>
    public Task CompactAsync(string key, int keepLast)
    {
        if (keepLast < 0)
            throw new ArgumentOutOfRangeException(nameof(keepLast),
                "keepLast must be zero or positive.");

        var keyDir = ResolveKeyDir(key);
        if (!Directory.Exists(keyDir)) return Task.CompletedTask;

        // Compact under the write lock so a concurrent SaveAsync
        // doesn't observe a transient empty state.
        var gate = GetOrCreateKeyLock(key);
        return Task.Run(async () =>
        {
            await gate.WaitAsync().ConfigureAwait(false);
            try { TrimOldestBeyond(keyDir, keepLast); }
            finally { gate.Release(); }
        });
    }

    /// <summary>The number of segments currently retained for
    /// <paramref name="key"/> (zero if the key has no history).</summary>
    public int SegmentCount(string key)
    {
        var keyDir = ResolveKeyDir(key);
        return Directory.Exists(keyDir) ? EnumerateSegments(keyDir).Count() : 0;
    }

    private static async Task<Snapshot?> ReadSegmentAsync(string path)
    {
        await using var fs = new FileStream(
            path, FileMode.Open, FileAccess.Read, FileShare.Read,
            bufferSize: 4096, useAsync: true);

        var fields = await JsonSerializer
            .DeserializeAsync<Dictionary<string, object?>>(fs, SerializerOptions)
            .ConfigureAwait(false);
        return fields is null ? null : new Snapshot(fields);
    }

    /// <summary>
    /// Returns segment paths sorted chronologically (oldest first).
    /// Sequence-number lexicographic sort = chronological because of
    /// zero-padding.
    /// </summary>
    private static IEnumerable<string> EnumerateSegments(string keyDir)
    {
        // Only real numbered segments ("{seq}.snapshot.json"), ordered by their
        // NUMERIC sequence. Skipping unparseable filenames here means LoadAsync /
        // ReadHistoryAsync / SegmentCount never mistake a stray "garbage.snapshot.json"
        // for the latest segment (which previously crashed ReadSegmentAsync's JSON
        // parse). Numeric ordering is also robust to non-zero-padded names.
        return Directory
            .EnumerateFiles(keyDir, "*" + SegmentExtension, SearchOption.TopDirectoryOnly)
            .Select(p => (Path: p, Seq: ParseSeqOrNegative(p)))
            .Where(t => t.Seq >= 0)
            .OrderBy(t => t.Seq)
            .Select(t => t.Path);
    }

    private static int NextSequence(string keyDir)
    {
        var highest = EnumerateSegments(keyDir).LastOrDefault();
        if (highest is null) return 1;

        var name = Path.GetFileNameWithoutExtension(highest);
        // Strip trailing ".snapshot" from "{seq}.snapshot.json".
        var dot = name.LastIndexOf('.');
        var seqStr = dot >= 0 ? name[..dot] : name;

        if (!int.TryParse(seqStr, NumberStyles.Integer,
                CultureInfo.InvariantCulture, out var seq))
        {
            // Garbage file in the directory — skip it; sequence keeps
            // walking back through the sorted list.
            return EnumerateSegments(keyDir)
                .Reverse()
                .Skip(1)
                .Select(ParseSeqOrNegative)
                .FirstOrDefault(s => s > 0) is var prior && prior > 0
                    ? prior + 1
                    : 1;
        }
        return seq + 1;
    }

    private static int ParseSeqOrNegative(string path)
    {
        var name   = Path.GetFileNameWithoutExtension(path);
        var dot    = name.LastIndexOf('.');
        var seqStr = dot >= 0 ? name[..dot] : name;
        return int.TryParse(seqStr, NumberStyles.Integer,
            CultureInfo.InvariantCulture, out var s) ? s : -1;
    }

    private static void TrimOldestBeyond(string keyDir, int keepLast)
    {
        var segments = EnumerateSegments(keyDir).ToList();
        if (segments.Count <= keepLast) return;

        var toDelete = segments.Count - keepLast;
        for (int i = 0; i < toDelete; i++)
        {
            try { System.IO.File.Delete(segments[i]); }
            catch (IOException) { /* best-effort; next compaction will retry */ }
        }
    }

    private static string SegmentPath(string keyDir, int seq) =>
        Path.Combine(keyDir,
            seq.ToString($"D{SegmentDigits}", CultureInfo.InvariantCulture)
            + SegmentExtension);

    private string ResolveKeyDir(string key)
    {
        ArgumentException.ThrowIfNullOrEmpty(key);
        var parts     = key.Split('/', StringSplitOptions.RemoveEmptyEntries);
        var safeParts = parts.Select(SanitizePart).ToArray();
        return Path.Combine(_baseDirectory, Path.Combine(safeParts));
    }

    private static string SanitizePart(string segment)
    {
        var sb = new StringBuilder(segment.Length);
        foreach (var c in segment)
        {
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
