using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Spek.Testing;
using Spek.Tests.Emit;
using Xunit;

namespace Spek.Tests.Runtime;

/// <summary>
/// End-to-end proof of the documented debounce semantics through the FULL
/// pipeline: parse a stream-shaped handler, emit C#, Roslyn-compile, spawn,
/// and drive it through a real mailbox. Pins the two halves of the defect
/// this exists to guard against:
/// <list type="bullet">
///   <item>A burst of messages collapses to ONE body execution (the latest
///         message), per docs/language/streams.md — not one per message.</item>
///   <item>The mailbox is never stalled by the quiet window: draining the
///         burst is fast even though the debounce interval is long.</item>
/// </list>
/// </summary>
public class StreamDebounceE2ETests
{
    private const string Source = """
        using Spek.Streams;

        message Nudge(int id);
        message GetFired();
        message Fired(int count, int lastId);

        actor Debouncer
        {
            int fired = 0;
            int lastId = 0;

            on Nudge n
                => debounce(500)
                => {
                    fired = fired + 1;
                    lastId = n.id;
                }

            on GetFired => return new Fired(fired, lastId);
        }
        """;

    [Fact]
    public async Task Burst_FiresBodyOnceWithLatest_WithoutStallingTheMailboxAsync()
    {
        var parse = SpekCompiler.Parse(Source);
        Assert.True(parse.Success,
            string.Join("\n", parse.Diagnostics.Select(d => $"  {d.Code} {d.Line}:{d.Column} {d.Message}")));

        var csharp   = new FileEmitter().Emit(parse.Tree!);
        var assembly = RoslynCompileHelper.CompileAndLoad(csharp, "DebounceE2E");
        var actorType = assembly.GetType("Debouncer")
            ?? assembly.GetTypes().Single(t => t.Name == "Debouncer");
        var nudgeType = assembly.GetTypes().Single(t => t.Name == "Nudge");
        var getType   = assembly.GetTypes().Single(t => t.Name == "GetFired");

        using var system = new TestActorSystem();
        var actor = system.Spawn(actorType);

        // A 5-message burst. With the old inline-delay operator the body
        // fired five times, once per message. (The mailbox-stall half of
        // that defect is pinned deterministically by the unit test
        // Offer_ReturnsWithoutWaitingOutTheWindow — a wall-clock assertion
        // here can't tell a stall from full-suite thread-pool saturation,
        // so this test pins only the burst-collapse semantics.)
        for (int i = 1; i <= 5; i++)
            actor.Tell(Activator.CreateInstance(nudgeType, i)!);
        await system.WhenIdleAsync();

        // Semantics half: after the quiet window, the body ran exactly once,
        // with the burst's final message. (Bounded poll — timer-thread emit.)
        var deadline = DateTime.UtcNow + TimeSpan.FromSeconds(10);
        (int count, int lastId) state;
        do
        {
            var reply = await actor.AskAsync<object>(Activator.CreateInstance(getType)!);
            var t = reply.GetType();
            state = ((int)t.GetProperty("count")!.GetValue(reply)!,
                     (int)t.GetProperty("lastId")!.GetValue(reply)!);
            if (state.count > 0) break;
            await Task.Delay(20);
        } while (DateTime.UtcNow < deadline);

        Assert.Equal(1, state.count);
        Assert.Equal(5, state.lastId);

        // And it stays at one — no straggler emissions from the burst.
        await Task.Delay(300);
        var final = await actor.AskAsync<object>(Activator.CreateInstance(getType)!);
        Assert.Equal(1, (int)final.GetType().GetProperty("count")!.GetValue(final)!);
    }

    [Fact]
    public async Task Burst_UnderVirtualTime_FiresOnceExactlyAtTheWindowAsync()
    {
        // Same pipeline, virtual clock: the emitted actor's operator chain
        // takes the system clock through the generated Configure call, so a
        // TestActorSystem(virtualTime: true) controls the debounce window
        // with AdvanceClock — no wall-clock margins, and the window edge is
        // assertable exactly (499ms silent, 500ms fires).
        var parse = SpekCompiler.Parse(Source);
        Assert.True(parse.Success,
            string.Join("\n", parse.Diagnostics.Select(d => $"  {d.Code} {d.Line}:{d.Column} {d.Message}")));

        var csharp   = new FileEmitter().Emit(parse.Tree!);
        var assembly = RoslynCompileHelper.CompileAndLoad(csharp, "DebounceVirtualE2E");
        var actorType = assembly.GetTypes().Single(t => t.Name == "Debouncer");
        var nudgeType = assembly.GetTypes().Single(t => t.Name == "Nudge");
        var getType   = assembly.GetTypes().Single(t => t.Name == "GetFired");

        using var system = new TestActorSystem(virtualTime: true);
        var actor = system.Spawn(actorType);

        async Task<(int count, int lastId)> StateAsync()
        {
            var reply = await actor.AskAsync<object>(Activator.CreateInstance(getType)!);
            var t = reply.GetType();
            return ((int)t.GetProperty("count")!.GetValue(reply)!,
                    (int)t.GetProperty("lastId")!.GetValue(reply)!);
        }

        // Burst of five; drain the first hops so the quiet-window timer is
        // armed at the current virtual instant.
        for (int i = 1; i <= 5; i++)
            actor.Tell(Activator.CreateInstance(nudgeType, i)!);
        await system.WhenIdleAsync();

        // Virtual time is frozen: the window can never elapse on its own.
        Assert.Equal((0, 0), await StateAsync());

        // One tick short of the 500ms window — still silent.
        system.AdvanceClock(TimeSpan.FromMilliseconds(499));
        await system.WhenIdleAsync();
        Assert.Equal((0, 0), await StateAsync());

        // The tick that completes the window fires the body exactly once,
        // with the burst's final message.
        system.AdvanceClock(TimeSpan.FromMilliseconds(1));
        await system.WhenIdleAsync();
        Assert.Equal((1, 5), await StateAsync());

        // And it stays at one — hours of virtual time produce no straggler.
        system.AdvanceClock(TimeSpan.FromHours(1));
        await system.WhenIdleAsync();
        Assert.Equal((1, 5), await StateAsync());
    }
}
