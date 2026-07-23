using System.Diagnostics.Tracing;
using System.Text.Json;

namespace Spek.Runtime;

/// <summary>
/// The live-introspection attach surface: an <see cref="EventSource"/>
/// (provider name <c>Spek-Introspection</c>) that rides the .NET
/// diagnostics IPC channel — the same transport as <c>dotnet-counters</c> —
/// so no listening port opens and the attach boundary is the OS user.
/// While at least one EventPipe session is enabled, a pump emits one
/// <see cref="ActorTable"/> event per system per second carrying the
/// JSON-serialized <see cref="ActorSystem.SnapshotActors"/> table. When
/// nobody is attached, the only cost is a live weak reference per system.
/// </summary>
[EventSource(Name = "Spek-Introspection")]
internal sealed class SpekIntrospectionEventSource : EventSource
{
    /// <summary>Singleton instance — EventSource registration is per-name.</summary>
    public static readonly SpekIntrospectionEventSource Instance = new();

    private static readonly List<WeakReference<ActorSystem>> Systems = [];
    private static readonly object Gate = new();
    private Timer? _pump;

    private SpekIntrospectionEventSource() { }

    internal static void Register(ActorSystem system)
    {
        lock (Gate)
        {
            Systems.RemoveAll(w => !w.TryGetTarget(out _));
            Systems.Add(new WeakReference<ActorSystem>(system));
        }
    }

    internal static void Unregister(ActorSystem system)
    {
        lock (Gate)
            Systems.RemoveAll(w => !w.TryGetTarget(out var s) || ReferenceEquals(s, system));
    }

    /// <inheritdoc />
    protected override void OnEventCommand(EventCommandEventArgs command)
    {
        if (command.Command == EventCommand.Enable)
        {
            // Real timer on purpose: the pump serves an external observer's
            // wall-clock cadence, not program semantics, so it stays outside
            // the (possibly virtual) system clock.
            _pump ??= new Timer(static _ => Instance.EmitSnapshots(),
                null, dueTime: 0, period: 1000);
        }
        else if (command.Command == EventCommand.Disable)
        {
            _pump?.Dispose();
            _pump = null;
        }
    }

    [NonEvent]
    private void EmitSnapshots()
    {
        if (!IsEnabled()) return;
        ActorSystem[] systems;
        lock (Gate)
            systems = Systems
                .Select(w => w.TryGetTarget(out var s) ? s : null)
                .Where(s => s is not null)
                .Cast<ActorSystem>()
                .ToArray();

        foreach (var system in systems)
        {
            string json;
            try { json = JsonSerializer.Serialize(system.SnapshotActors()); }
            catch (ObjectDisposedException) { continue; }
            ActorTable(system.Name, json);
        }
    }

    /// <summary>One introspection sample: the full actor table of one system.</summary>
    [Event(1, Level = EventLevel.Informational)]
    public void ActorTable(string systemName, string actorsJson)
        => WriteEvent(1, systemName, actorsJson);
}
