using System.Text.Json;
using Microsoft.Data.Sqlite;

namespace Spek.Persistence.Sqlite;

/// <summary>
/// Embedded SQLite-backed <see cref="ISnapshotStore"/>. Snapshots are
/// JSON-encoded into a single <c>snapshots</c> table inside a local
/// database file. WAL mode is enabled so readers don't block writers
/// and the durability story matches what serious single-node services
/// expect.
///
/// Thread-safe — SQLite serialises writers internally; the store
/// pools its own connections so concurrent <see cref="SaveAsync"/> /
/// <see cref="LoadAsync"/> calls scale up to the SQLite engine's
/// limits.
/// </summary>
public sealed class SqliteSnapshotStore : ISnapshotStore, IDisposable
{
    private static readonly JsonSerializerOptions SerializerOptions = new()
    {
        WriteIndented = false,
        PropertyNameCaseInsensitive = true,
    };

    private readonly string _connectionString;

    /// <summary>
    /// Opens (creating if needed) the database file at
    /// <paramref name="databasePath"/>, creating parent directories,
    /// enabling WAL mode, and ensuring the <c>snapshots</c> table
    /// exists — the store is ready to save and load on return.
    /// </summary>
    public SqliteSnapshotStore(string databasePath)
    {
        ArgumentException.ThrowIfNullOrEmpty(databasePath);

        var dir = Path.GetDirectoryName(databasePath);
        if (!string.IsNullOrEmpty(dir)) Directory.CreateDirectory(dir);

        _connectionString = new SqliteConnectionStringBuilder
        {
            DataSource = databasePath,
            Mode       = SqliteOpenMode.ReadWriteCreate,
            Cache      = SqliteCacheMode.Shared,
        }.ToString();

        InitialiseSchema();
    }

    /// <summary>The SQLite connection string used to access the
    /// underlying database. Exposed for diagnostics; the store owns
    /// the schema and lifecycle.</summary>
    public string ConnectionString => _connectionString;

    /// <summary>
    /// Upserts the JSON-encoded snapshot into the row for
    /// <paramref name="key"/> — one row per key, last write wins —
    /// stamped with an updated-at timestamp. The write is committed on
    /// return; under <c>synchronous = NORMAL</c> an app crash cannot
    /// lose it, though an ill-timed power loss can roll the database
    /// back to the previous snapshot.
    /// </summary>
    public async Task SaveAsync(string key, Snapshot snapshot)
    {
        ArgumentException.ThrowIfNullOrEmpty(key);
        ArgumentNullException.ThrowIfNull(snapshot);

        var payload = JsonSerializer.SerializeToUtf8Bytes(snapshot.Fields, SerializerOptions);

        await using var conn = OpenConnection();
        await conn.OpenAsync().ConfigureAwait(false);

        await using var cmd = conn.CreateCommand();
        cmd.CommandText = """
            INSERT INTO snapshots (key, payload, updated_at)
            VALUES ($key, $payload, $updated_at)
            ON CONFLICT(key) DO UPDATE
              SET payload = excluded.payload,
                  updated_at = excluded.updated_at;
            """;
        cmd.Parameters.AddWithValue("$key", key);
        cmd.Parameters.AddWithValue("$payload", payload);
        cmd.Parameters.AddWithValue("$updated_at",
            DateTimeOffset.UtcNow.ToUnixTimeMilliseconds());

        await cmd.ExecuteNonQueryAsync().ConfigureAwait(false);
    }

    /// <summary>
    /// Reads the current snapshot for <paramref name="key"/>, or
    /// <c>null</c> if none has been saved. Field values come back as
    /// <see cref="JsonElement"/>s; <see cref="Snapshot.Get{T}"/>
    /// deserialises them on access.
    /// </summary>
    public async Task<Snapshot?> LoadAsync(string key)
    {
        ArgumentException.ThrowIfNullOrEmpty(key);

        await using var conn = OpenConnection();
        await conn.OpenAsync().ConfigureAwait(false);

        await using var cmd = conn.CreateCommand();
        cmd.CommandText = "SELECT payload FROM snapshots WHERE key = $key";
        cmd.Parameters.AddWithValue("$key", key);

        await using var reader = await cmd.ExecuteReaderAsync().ConfigureAwait(false);
        if (!await reader.ReadAsync().ConfigureAwait(false)) return null;

        var payload = (byte[])reader.GetValue(0);
        var fields  = JsonSerializer.Deserialize<Dictionary<string, object?>>(
            payload, SerializerOptions);
        return fields is null ? null : new Snapshot(fields);
    }

    /// <summary>
    /// Releases the database file by clearing SQLite's connection
    /// pools — pooled connections otherwise keep the file open and
    /// locked after the store is done, blocking moves and deletes.
    /// Note this clears every SQLite pool in the process, not just
    /// this store's.
    /// </summary>
    public void Dispose()
    {
        // Pool-aware: SqliteConnection's pooling holds open file
        // handles to the DB until the pool is cleared. Clearing here
        // releases the file lock so the DB can be moved/deleted in
        // tests + tooling.
        SqliteConnection.ClearAllPools();
    }

    private SqliteConnection OpenConnection() => new(_connectionString);

    private void InitialiseSchema()
    {
        using var conn = OpenConnection();
        conn.Open();

        // WAL mode lets readers run concurrently with the single
        // writer; synchronous=NORMAL is the standard durability/perf
        // sweet spot for embedded stores (full power-loss durability
        // is `FULL` but adds an fsync per commit).
        using (var pragma = conn.CreateCommand())
        {
            pragma.CommandText = "PRAGMA journal_mode = WAL; PRAGMA synchronous = NORMAL;";
            pragma.ExecuteNonQuery();
        }

        using var cmd = conn.CreateCommand();
        cmd.CommandText = """
            CREATE TABLE IF NOT EXISTS snapshots (
              key        TEXT PRIMARY KEY,
              payload    BLOB NOT NULL,
              updated_at INTEGER NOT NULL
            ) WITHOUT ROWID;
            """;
        cmd.ExecuteNonQuery();
    }
}
