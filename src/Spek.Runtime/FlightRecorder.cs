using System.Collections.Concurrent;
using System.Text.Json;

namespace Spek.Runtime;

/// <summary>
/// The always-on-capable flight recorder (D2): a bounded ring buffer at
/// the runtime's ingress boundary. It journals messages entering the
/// system from outside the actor world — host sends and channel adapters,
/// recognized as enqueues that carry no actor sender — because when
/// execution is deterministic, journaling only ingress suffices:
/// re-execution re-derives the internal traffic. Attach at system
/// construction (<c>new ActorSystem(..., trace: recorder)</c>), dump on
/// incident, replay with <c>SimulatedActorSystem.ReplayIngress</c>.
/// </summary>
/// <remarks>
/// Payloads are JSON-serialized at capture (the same contract clustering
/// imposes on remote messages). Types that fail to serialize are recorded
/// as gaps and reported in <see cref="UnserializableTypes"/> — never
/// silently dropped. Traces contain real payloads: treat trace files as
/// production data.
/// </remarks>
public sealed class FlightRecorder
{
    private readonly ConcurrentQueue<TraceEvent> _ring = new();
    private readonly int _capacity;
    private long _seq;
    private readonly ConcurrentDictionary<string, byte> _unserializable = new();

    /// <summary>Creates a recorder keeping the last <paramref name="capacity"/> ingress events.</summary>
    public FlightRecorder(int capacity = 10_000)
    {
        ArgumentOutOfRangeException.ThrowIfLessThan(capacity, 1);
        _capacity = capacity;
    }

    /// <summary>Message types that could not be captured (not JSON-serializable).</summary>
    public IReadOnlyCollection<string> UnserializableTypes => _unserializable.Keys.ToArray();

    internal void RecordIngress(string target, object message)
    {
        var type = message.GetType();
        string json;
        try { json = JsonSerializer.Serialize(message, type); }
        catch (Exception)
        {
            _unserializable.TryAdd(type.FullName ?? type.Name, 0);
            return;
        }

        _ring.Enqueue(new TraceEvent(
            Interlocked.Increment(ref _seq), target,
            type.AssemblyQualifiedName ?? type.FullName ?? type.Name, json));
        while (_ring.Count > _capacity) _ring.TryDequeue(out _);
    }

    /// <summary>Snapshot of the buffered window, oldest first.</summary>
    public SpekTrace Snapshot() => new(BuildFingerprint(), _ring.ToArray());

    /// <summary>Writes the buffered window to <paramref name="path"/> as JSON.</summary>
    public void Dump(string path) =>
        File.WriteAllText(path, JsonSerializer.Serialize(Snapshot(),
            new JsonSerializerOptions { WriteIndented = true }));

    internal static string BuildFingerprint()
    {
        var entry = System.Reflection.Assembly.GetEntryAssembly()?.GetName();
        var runtime = typeof(FlightRecorder).Assembly.GetName();
        return $"{entry?.Name ?? "?"}/{entry?.Version} spek/{runtime.Version}";
    }
}

/// <summary>One recorded ingress event.</summary>
/// <param name="Seq">Global arrival sequence number (monotonic).</param>
/// <param name="Target">Display identity of the receiving actor (as in <see cref="ActorSnapshot.Path"/>).</param>
/// <param name="MessageType">Assembly-qualified payload type, for rehydration.</param>
/// <param name="PayloadJson">The payload, JSON-serialized at capture.</param>
public sealed record TraceEvent(long Seq, string Target, string MessageType, string PayloadJson);

/// <summary>A dumped trace: build fingerprint plus the recorded window.</summary>
public sealed record SpekTrace(string Fingerprint, TraceEvent[] Events)
{
    /// <summary>Loads a trace dumped by <see cref="FlightRecorder.Dump"/>.</summary>
    public static SpekTrace Load(string path) =>
        JsonSerializer.Deserialize<SpekTrace>(File.ReadAllText(path))
        ?? throw new InvalidDataException($"'{path}' is not a Spek trace.");

    /// <summary>Rehydrates one event's payload.</summary>
    internal object Rehydrate(TraceEvent e)
    {
        var type = Type.GetType(e.MessageType) ?? throw new InvalidOperationException(
            $"trace event #{e.Seq}: type '{e.MessageType}' not found in this build — " +
            "replay the trace against the build that recorded it " +
            $"(recorded on: {Fingerprint}).");
        return JsonSerializer.Deserialize(e.PayloadJson, type) ?? throw new InvalidOperationException(
            $"trace event #{e.Seq}: payload failed to rehydrate as {type.Name}.");
    }
}
