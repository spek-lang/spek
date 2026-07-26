using System.Diagnostics.Tracing;
using System.Text.Json;
using Spek;
using Spek.Runtime;
using Spek.Testing;
using Xunit;

namespace Spek.Tests.Runtime;

/// <summary>
/// The attach surface behind <c>spekc observe</c>: enabling the
/// Spek-Introspection provider starts a pump that emits one ActorTable
/// event per system per second, carrying the JSON snapshot table. An
/// in-proc <see cref="EventListener"/> exercises the same enable/emit
/// path an out-of-proc EventPipe session does.
/// </summary>
public sealed class IntrospectionEventSourceTests
{
    private sealed record Ping();

    private sealed class Sink : ActorBase
    {
        protected override Task DispatchAsync(object message, ActorRef sender)
            => Task.CompletedTask;
    }

    private sealed class TableListener : EventListener
    {
        private readonly List<(string System, string Json)> _tables = [];
        private readonly object _gate = new();

        public IReadOnlyList<(string System, string Json)> Tables
        {
            get { lock (_gate) return _tables.ToArray(); }
        }

        protected override void OnEventSourceCreated(EventSource eventSource)
        {
            if (eventSource.Name == "Spek-Introspection")
                EnableEvents(eventSource, EventLevel.Informational);
        }

        protected override void OnEventWritten(EventWrittenEventArgs eventData)
        {
            if (eventData.EventName != "ActorTable" || eventData.Payload is null) return;
            lock (_gate)
                _tables.Add(((string)eventData.Payload[0]!, (string)eventData.Payload[1]!));
        }
    }

    [Fact]
    public async Task EnabledListener_ReceivesActorTables_WithValidSnapshotsAsync()
    {
        using var system = new ActorSystem("observe-pump");
        system.Spawn<Sink>();

        using var listener = new TableListener();

        await TestActorSystem.WaitUntilAsync(
            () => listener.Tables.Any(t => t.System == "observe-pump"),
            timeout: TimeSpan.FromSeconds(10),
            description: "pump emitted this system's table");

        var (_, json) = listener.Tables.First(t => t.System == "observe-pump");
        var snapshots = JsonSerializer.Deserialize<List<ActorSnapshot>>(json)!;
        var snap = Assert.Single(snapshots);
        Assert.Equal("Sink", snap.Path);
        Assert.True(snap.IsMaterialized);
    }
}
