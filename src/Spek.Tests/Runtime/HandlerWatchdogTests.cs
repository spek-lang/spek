using Spek.Runtime;
using Spek.Testing;
using Xunit;

namespace Spek.Tests.Runtime;

/// <summary>
/// The slow-handler watchdog: a wedged handler (writer or reader arm) is
/// reported to the dead-letter sink exactly once per occurrence, detection
/// never interferes with the actor, fast handlers under sustained load are
/// never flagged, and a null threshold disables the sweep entirely.
///
/// Timing discipline: tests shorten the sweep period (internal hook) and
/// the threshold so detection lands in a few hundred milliseconds, but all
/// waits are condition-polled with generous ceilings — under the fully
/// parallel suite a timer callback can be scheduled seconds late, so
/// nothing here asserts a tight real-time window.
/// </summary>
public class HandlerWatchdogTests
{
    private static readonly TimeSpan SweepPeriod = TimeSpan.FromMilliseconds(50);

    public sealed record Ping();
    public sealed record Pong();
    /// <summary>Fast do-nothing message for the sustained-load test (no
    /// reply, so a senderless Tell doesn't spam the NoSender dead-letter
    /// line twenty thousand times).</summary>
    public sealed record Nop();
    /// <summary>Parks the handler on <paramref name="Released"/> — the wedge
    /// under test. Completing it un-wedges the handler.</summary>
    public sealed record Wedge(TaskCompletionSource Released);

    /// <summary>Default (writer) dispatch: a Wedge parks the mailbox; a Ping
    /// replies so tests can prove the actor still serves after detection.</summary>
    private sealed class WedgeableWriter : ActorBase
    {
        protected override async Task DispatchAsync(object message, ActorRef sender)
        {
            switch (message)
            {
                case Wedge w: await w.Released.Task; break;
                case Ping:    sender.Tell(new Pong()); break;
                case Nop:     break;
            }
        }
    }

    /// <summary>Hand-written reader-arm actor: classifying every message as
    /// a reader drives the concurrent (off-drain-loop) dispatch path, the
    /// same one emitted `reader on` handlers take.</summary>
    private sealed class WedgeableReader : ActorBase
    {
        protected override bool ClassifyAsReader(object message) => true;

        protected override async Task DispatchAsync(object message, ActorRef sender)
        {
            switch (message)
            {
                case Wedge w: await w.Released.Task; break;
                case Ping:    sender.Tell(new Pong()); break;
            }
        }
    }

    private static ActorSystem NewSystem(
        string name, RecordingDeadLetterSink sink, TimeSpan? threshold)
    {
        var system = new ActorSystem(name, deadLetterSink: sink);
        // Before the first spawn — the watchdog reads the period at creation.
        WatchdogTestHooks.SetSlowHandlerSweepPeriod(system, SweepPeriod);
        system.SlowHandlerThreshold = threshold;
        return system;
    }

    private static IReadOnlyList<DeadLetterRecord> WatchdogReports(RecordingDeadLetterSink sink)
        => sink.Records.Where(r => r.Message is SlowHandlerReport).ToArray();

    [Fact]
    public async Task WedgedWriter_ReportsExactlyOnce_AndDetectionNeverBreaksTheActorAsync()
    {
        var sink = new RecordingDeadLetterSink();
        using var system = NewSystem("watchdog-writer", sink, TimeSpan.FromMilliseconds(150));
        var released = new TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously);
        try
        {
            var actor = system.Spawn<WedgeableWriter>();
            actor.Tell(new Wedge(released));

            await TestActorSystem.WaitUntilAsync(
                () => WatchdogReports(sink).Count >= 1,
                TimeSpan.FromSeconds(10), "watchdog report for the wedged writer");

            // Several more sweep periods: the SAME occurrence must not repeat.
            await Task.Delay(SweepPeriod * 8);
            var reports = WatchdogReports(sink);
            var record = Assert.Single(reports);
            Assert.Contains("WedgeableWriter", record.Reason);
            Assert.Contains("possible wedge", record.Reason);
            Assert.Null(record.Cause);
            var report = Assert.IsType<SlowHandlerReport>(record.Message);
            Assert.Equal(nameof(Wedge), report.LastMessageType);
            Assert.True(report.ObservedRunningFor >= TimeSpan.FromMilliseconds(150));

            // Detection only: release the wedge and the actor serves the
            // queued message normally — nothing was cancelled or stopped.
            released.TrySetResult();
            var pong = await actor.AskAsync<Pong>(new Ping(), TimeSpan.FromSeconds(10));
            Assert.NotNull(pong);
        }
        finally
        {
            released.TrySetResult();   // never leak a parked handler
        }
    }

    [Fact]
    public async Task WedgedReader_StaleReaderPhaseStamp_IsReportedAsync()
    {
        var sink = new RecordingDeadLetterSink();
        using var system = NewSystem("watchdog-reader", sink, TimeSpan.FromMilliseconds(150));
        var released = new TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously);
        try
        {
            var actor = system.Spawn<WedgeableReader>();
            actor.Tell(new Wedge(released));

            await TestActorSystem.WaitUntilAsync(
                () => WatchdogReports(sink).Count >= 1,
                TimeSpan.FromSeconds(10), "watchdog report for the wedged reader");

            var record = WatchdogReports(sink)[0];
            Assert.Contains("reader handler", record.Reason);
            Assert.Contains("WedgeableReader", record.Reason);
            Assert.Contains("possible wedge", record.Reason);
        }
        finally
        {
            released.TrySetResult();
        }
    }

    [Fact]
    public async Task FastHandlersUnderSustainedLoad_ProduceZeroReportsAsync()
    {
        // Writer dispatch re-stamps per message, so sustained fast traffic
        // never lets a stamp age past the threshold. (Reader phases are the
        // documented approximation — a continuously-busy overlap of fast
        // readers CAN age the phase stamp — so this drives the writer arm.)
        var sink = new RecordingDeadLetterSink();
        using var system = NewSystem("watchdog-load", sink, TimeSpan.FromSeconds(1));
        var actor = system.Spawn<WedgeableWriter>();

        var until = DateTime.UtcNow + TimeSpan.FromSeconds(2);
        while (DateTime.UtcNow < until)
        {
            for (int i = 0; i < 200; i++)
                actor.Tell(new Nop());
            await Task.Delay(20);
        }

        await TestActorSystem.WaitUntilAsync(
            () => system.IsIdle, TimeSpan.FromSeconds(20), "load drained");
        Assert.Empty(WatchdogReports(sink));
    }

    [Fact]
    public async Task NullThreshold_DisablesDetection_WedgeIsNeverReportedAsync()
    {
        var sink = new RecordingDeadLetterSink();
        using var system = NewSystem("watchdog-off", sink, threshold: null);
        var released = new TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously);
        try
        {
            var actor = system.Spawn<WedgeableWriter>();
            actor.Tell(new Wedge(released));

            // ~20 sweep periods and ~6x the enabled tests' threshold: if
            // detection were on, the report would long since have landed.
            await Task.Delay(1_000);
            Assert.Empty(WatchdogReports(sink));
        }
        finally
        {
            released.TrySetResult();
        }
    }

    [Fact]
    public async Task NewWedgeAfterRecovery_ReportsAgain_OncePerOccurrenceAsync()
    {
        var sink = new RecordingDeadLetterSink();
        using var system = NewSystem("watchdog-rewedge", sink, TimeSpan.FromMilliseconds(150));
        var first  = new TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously);
        var second = new TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously);
        try
        {
            var actor = system.Spawn<WedgeableWriter>();

            actor.Tell(new Wedge(first));
            await TestActorSystem.WaitUntilAsync(
                () => WatchdogReports(sink).Count >= 1,
                TimeSpan.FromSeconds(10), "first wedge reported");
            first.TrySetResult();

            // Round-trip proves the first wedge fully resolved (its stamp
            // cleared) before the second begins.
            await actor.AskAsync<Pong>(new Ping(), TimeSpan.FromSeconds(10));

            actor.Tell(new Wedge(second));
            await TestActorSystem.WaitUntilAsync(
                () => WatchdogReports(sink).Count >= 2,
                TimeSpan.FromSeconds(10), "second wedge reported");

            // Once per OCCURRENCE, not once per actor: two wedges, two
            // reports — and no more than two after further sweeps.
            await Task.Delay(SweepPeriod * 8);
            Assert.Equal(2, WatchdogReports(sink).Count);
        }
        finally
        {
            first.TrySetResult();
            second.TrySetResult();
        }
    }
}
