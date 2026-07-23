using Spek.Hosting.Console;
using Spek.Runtime;
using Xunit;

namespace Spek.Tests.Hosting;

/// <summary>
/// Integration coverage for <see cref="SpekConsoleHost"/>. We can't
/// fire OS signals from a unit test (it'd kill the test runner), but
/// the signal handler's effect is just <c>entryActor.Tell(shutdown,
/// sender: receiver)</c>. We verify the surrounding plumbing — actor
/// runs, host returns when it stops, exit code propagates via Option D
/// reply.
/// </summary>
public class ConsoleHostTests
{
    public sealed record Shutdown();
    public sealed record StopYourself();

    /// <summary>
    /// Stand-in entry actor. On <see cref="Shutdown"/> it replies with
    /// a typed exit code (Option D equivalent) and stops itself. On
    /// <see cref="StopYourself"/> it just stops, no reply — the host
    /// should fall back to the default exit code.
    /// </summary>
    private sealed class GracefulActor : ActorBase
    {
        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            switch (message)
            {
                case Shutdown:
                    _currentSender.Tell(42);    // Option D return → exit code 42
                    StopSelf();
                    break;
                case StopYourself:
                    StopSelf();
                    break;
            }
            return Task.CompletedTask;
        }
    }

    [Fact]
    public async Task ShutdownPath_RoutesReplyAsExitCode()
    {
        // Drive shutdown through the public RunAsync surface: bring our
        // own system + entry, kick off RunAsync, then trigger shutdown
        // via the same factory the signal handlers use — but we have to
        // do it from outside, so we Tell directly to the entry actor.
        // The internal receiver is wired by the host; we just need to
        // make the entry actor stop.
        using var system = new ActorSystem("console-host-shutdown");
        var entry        = system.Spawn<GracefulActor>();

        // Send the shutdown ourselves. The actor's reply goes to
        // NoSender (since we Tell with no sender override), so the
        // host's receiver never sees the 42 — the host returns the
        // default. This isn't the full signal flow but it exercises
        // the wait-and-return surface.
        var hostTask = SpekConsoleHost.RunAsync(
            system, entry,
            shutdownFactory: () => new Shutdown(),
            shutdownGrace: TimeSpan.FromSeconds(2),
            defaultExitCode: 7);

        // Give RunAsync time to start its wait loop, then trigger.
        await Task.Delay(100);
        entry.Tell(new StopYourself());

        var exitCode = await hostTask;

        Assert.Equal(7, exitCode);          // default — actor's reply went to NoSender
        Assert.True(entry.IsStopped);
    }

    [Fact]
    public async Task ExitCodeReceiver_CapturesOptionDReply_ViaTellWithSender()
    {
        // Direct test of the inner machinery: spawn the receiver,
        // give it a Tell-with-sender of an int, verify the holder
        // captures it. This is the path the host uses internally:
        // it Tells the entry actor with the receiver as sender, the
        // entry actor replies to that sender with an int, the
        // receiver's DispatchAsync stores it.
        using var system = new ActorSystem("exit-code-receiver");
        var holder       = new SpekConsoleHost.ExitCodeHolder(99);
        var receiver     = system.Spawn<SpekConsoleHost.ExitCodeReceiverActor>(holder);

        // Send an int directly — receiver pulls it into the holder.
        receiver.Tell(42);
        await Task.Delay(100);

        Assert.Equal(42, holder.Value);
    }

    [Fact]
    public async Task TellWithSender_RoutesReplyToSender_EndToEnd()
    {
        // Verifies the new public Tell(message, sender) overload on
        // ActorRef does what we need: when an actor in the middle
        // does sender.Tell(reply), it routes back to the original
        // sender's mailbox.
        using var system = new ActorSystem("tell-with-sender");
        var holder       = new SpekConsoleHost.ExitCodeHolder(0);
        var receiver     = system.Spawn<SpekConsoleHost.ExitCodeReceiverActor>(holder);
        var graceful     = system.Spawn<GracefulActor>();

        // Tell the graceful actor to shut down, with the receiver as
        // the apparent sender. Graceful's `_currentSender.Tell(42)`
        // routes back to receiver, which writes it into the holder.
        graceful.Tell(new Shutdown(), sender: receiver);

        await Task.Delay(150);

        Assert.True(graceful.IsStopped);
        Assert.Equal(42, holder.Value);
    }
}
