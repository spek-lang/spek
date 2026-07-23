using System.Text.Json;
using Microsoft.Diagnostics.NETCore.Client;
using Microsoft.Diagnostics.Tracing;
using Spek.Runtime;

namespace Spek.Cli;

/// <summary>
/// <c>spekc observe &lt;pid&gt;</c> — live actor introspection. Attaches an
/// EventPipe session to the target process over the .NET diagnostics IPC
/// channel (the <c>dotnet-counters</c> transport: no listening port, OS
/// user boundary) and renders the Spek-Introspection provider's ActorTable
/// samples. Read-only and non-perturbing by construction. <c>--json</c>
/// swaps the rendered tables for line-delimited JSON samples (and JSON
/// error objects) so tooling — the VS Code extension's live actor panel —
/// can consume the stream without parsing prose.
/// </summary>
internal static class ObserveCommand
{
    public static int Run(string[] args, TextWriter @out, TextWriter error)
    {
        if (args.Length < 1 || !int.TryParse(args[0], out var pid))
        {
            error.WriteLine("Usage: spekc observe <pid> [--actor <path>] [--once] [--json]");
            error.WriteLine("         --actor <path>  drill into one actor (path as shown in the table)");
            error.WriteLine("         --once          print one sample and exit (default: refresh until Ctrl-C)");
            error.WriteLine("         --json          one JSON object per sample on stdout (NDJSON) instead of tables;");
            error.WriteLine("                         errors become {\"error\": ...} objects, never prose");
            return 1;
        }

        string? actorFilter = null;
        bool once = false;
        bool json = false;
        for (int i = 1; i < args.Length; i++)
        {
            switch (args[i])
            {
                case "--actor" when i + 1 < args.Length: actorFilter = args[++i]; break;
                case "--once": once = true; break;
                case "--json": json = true; break;
                default:
                    error.WriteLine($"spekc observe: unknown option '{args[i]}'");
                    return 1;
            }
        }

        try
        {
            var client = new DiagnosticsClient(pid);
            var provider = new EventPipeProvider(
                "Spek-Introspection", System.Diagnostics.Tracing.EventLevel.Informational);
            using var session = client.StartEventPipeSession(provider, requestRundown: false);
            using var source = new EventPipeEventSource(session.EventStream);
            var sampled = false;
            source.Dynamic.All += evt =>
            {
                if (evt.EventName != "ActorTable") return;
                var systemName = (string)evt.PayloadByName("systemName");
                var actorsJson = (string)evt.PayloadByName("actorsJson");
                var snapshots = JsonSerializer.Deserialize<ActorSnapshot[]>(actorsJson) ?? [];
                @out.WriteLine(json
                    ? RenderJson(systemName, snapshots, actorFilter)
                    : Render(systemName, snapshots, actorFilter, DateTimeOffset.UtcNow));
                sampled = true;
                if (once) session.Stop();
            };
            source.Process();
            if (!sampled)
            {
                const string noSample = "session ended before a sample arrived — is this a Spek process?";
                if (json) @out.WriteLine(RenderJsonError(noSample));
                else error.WriteLine($"spekc observe: {noSample}");
            }
            return sampled ? 0 : 1;
        }
        catch (Exception ex) when (ex is ServerNotAvailableException or TimeoutException)
        {
            var attachError = $"cannot attach to pid {pid} — {ex.Message}";
            if (json) @out.WriteLine(RenderJsonError(attachError));
            else error.WriteLine($"spekc observe: {attachError}");
            return 1;
        }
    }

    /// <summary>
    /// Renders one sample as a single NDJSON line —
    /// <c>{"system": name, "actors": [ActorSnapshot...]}</c>. The snapshots
    /// serialize with the same default System.Text.Json contract the
    /// introspection EventSource puts on the wire (PascalCase members), so
    /// consumers see one shape end to end. An <c>--actor</c> filter narrows
    /// <c>actors</c> to the one match, or yields an error object when the
    /// path is unknown. Internal for direct unit testing.
    /// </summary>
    internal static string RenderJson(
        string systemName, ActorSnapshot[] snapshots, string? actorFilter)
    {
        if (actorFilter is not null)
        {
            var snap = snapshots.FirstOrDefault(s => s.Path == actorFilter);
            if (snap is null)
                return RenderJsonError($"no actor '{actorFilter}' (paths: " +
                    string.Join(", ", snapshots.Select(s => s.Path)) + ")");
            snapshots = [snap];
        }
        return JsonSerializer.Serialize(new { system = systemName, actors = snapshots });
    }

    /// <summary>
    /// A machine-readable <c>{"error": message}</c> line for --json mode —
    /// consumers of the JSON stream never have to parse prose.
    /// </summary>
    internal static string RenderJsonError(string message)
        => JsonSerializer.Serialize(new { error = message });

    /// <summary>Renders one sample. Internal for direct unit testing.</summary>
    internal static string Render(
        string systemName, ActorSnapshot[] snapshots, string? actorFilter, DateTimeOffset now)
    {
        var sb = new System.Text.StringBuilder();
        if (actorFilter is not null)
        {
            var snap = snapshots.FirstOrDefault(s => s.Path == actorFilter);
            if (snap is null)
                return $"[{systemName}] no actor '{actorFilter}' (paths: " +
                       string.Join(", ", snapshots.Select(s => s.Path)) + ")";
            var uptime = now - snap.SpawnedAt;
            sb.AppendLine($"{snap.Path}  ({snap.ActorType}, up {uptime:hh\\:mm\\:ss})");
            sb.AppendLine($"  behavior:  {snap.Behavior ?? "-"}");
            sb.AppendLine($"  mailbox:   {snap.MailboxDepth} pending" +
                (snap.MailboxHead.Length > 0
                    ? "; head: " + string.Join(", ", snap.MailboxHead
                          .GroupBy(t => t).Select(g => $"{g.Key} x{g.Count()}"))
                    : ""));
            sb.AppendLine($"  restarts:  {snap.Restarts}");
            sb.AppendLine($"  last msg:  {snap.LastMessageType ?? "-"}");
            sb.AppendLine($"  state:     {(snap.IsStopped ? "stopped" : snap.IsMaterialized ? "live" : "passivated")}");
            if (snap.Children.Length > 0)
                sb.AppendLine($"  children:  {string.Join(", ", snap.Children)}");
            return sb.ToString().TrimEnd();
        }

        sb.AppendLine($"[{systemName}]");
        sb.AppendLine($"{"ACTOR",-24} {"BEHAVIOR",-12} {"MAILBOX",8} {"RESTARTS",9}  LAST MSG");
        foreach (var s in snapshots)
        {
            var state = s.IsStopped ? " (stopped)" : s.IsMaterialized ? "" : " (passivated)";
            sb.AppendLine(
                $"{s.Path,-24} {s.Behavior ?? "-",-12} {s.MailboxDepth,8} {s.Restarts,9}  " +
                $"{s.LastMessageType ?? "-"}{state}");
        }
        return sb.ToString().TrimEnd();
    }
}
