using Spek.Testing;
using Xunit;

namespace Spek.Tests.Runtime;

/// <summary>
/// Virtual time: a TestActorSystem constructed with <c>virtualTime: true</c>
/// runs the runtime's semantic clocks (passivation idleness, self.Clock) on a
/// manual provider — minutes of idle time become one Advance call, and
/// wall-clock reads inside handlers are deterministic. Real-time systems are
/// untouched by default (Clock is null).
/// </summary>
public sealed class VirtualTimeTests
{
    public sealed record Poke();
    public sealed record WhatTime();
    public sealed record ItIs(DateTimeOffset now);

    private sealed class ClockReader : Spek.ActorBase
    {
        protected override Task DispatchAsync(object message, Spek.ActorRef sender)
        {
            if (message is WhatTime) sender.Tell(new ItIs(Clock.GetUtcNow()), _selfRef);
            return Task.CompletedTask;
        }
    }

    [Fact]
    public async Task SelfClock_UnderVirtualTime_IsDeterministicAsync()
    {
        using var sys = new TestActorSystem(virtualTime: true);
        var actor = sys.Spawn<ClockReader>();

        var first = await actor.AskAsync<ItIs>(new WhatTime());
        sys.AdvanceClock(TimeSpan.FromHours(1));
        var second = await actor.AskAsync<ItIs>(new WhatTime());

        Assert.Equal(TimeSpan.FromHours(1), second.now - first.now);
    }

    private sealed class IdleActor : Spek.ActorBase
    {
        protected override Task DispatchAsync(object message, Spek.ActorRef sender)
            => Task.CompletedTask;

        protected override TimeSpan? PassivationTimeout => TimeSpan.FromMinutes(10);
    }

    [Fact]
    public async Task Passivation_UnderVirtualTime_IsOneAdvanceCallAsync()
    {
        // A ten-minute idle window costs one AdvanceClock call, not a sleep:
        // the passivation check timer and the idleness reads both run on the
        // manual clock. (The docs' virtual-time passivation example mirrors
        // this shape — keep them in sync.)
        using var sys = new TestActorSystem(virtualTime: true);
        var actor = sys.Spawn<IdleActor>();
        actor.Tell(new Poke());
        await sys.WhenIdleAsync();

        Assert.True(actor.IsMaterialized);
        sys.AdvanceClock(TimeSpan.FromMinutes(11));

        await TestActorSystem.WaitUntilAsync(
            () => !actor.IsMaterialized, description: "actor passivated");
        Assert.False(actor.IsStopped);   // passivation is not termination
    }

    [Fact]
    public void DirectTimeRead_InActor_WarnsCE0134_ButStillCompiles()
    {
        const string src = "namespace X;\n" +
            "message T();\n" +
            "actor A { behavior Default { on T t => { var n = DateTime.UtcNow; } } }";
        var parsed = Spek.Compiler.Parser.SpekCompiler.Parse(src);
        Assert.True(parsed.Success, "warnings must not fail the parse");
        var diag = Assert.Single(parsed.Diagnostics, d => d.Code == "CE0134");
        Assert.Equal(Spek.Compiler.Semantic.DiagnosticSeverity.Warning, diag.Severity);
        Assert.Contains("self.Clock", diag.Message);
    }

    [Fact]
    public void SelfClock_InSpekSource_IsNotCE0012()
    {
        // The idiom CE0134 teaches must itself compile: `self.Clock` is an
        // ambient self-accessor (like self.Log), not a read on an ActorRef.
        const string src = "namespace X;\n" +
            "message T();\n" +
            "actor A { behavior Default { on T t => { var n = self.Clock.GetUtcNow(); } } }";
        var parsed = Spek.Compiler.Parser.SpekCompiler.Parse(src);
        Assert.True(parsed.Success);
        Assert.DoesNotContain(parsed.Diagnostics, d => d.Code == "CE0012");
        Assert.DoesNotContain(parsed.Diagnostics, d => d.Code == "CE0134");
    }

    [Fact]
    public void DirectTimeRead_InProgramBlock_IsHostSide_NoWarning()
    {
        const string src = "namespace X;\n" +
            "module M { public long F() { return DateTime.UtcNow.Ticks; } }";
        var parsed = Spek.Compiler.Parser.SpekCompiler.Parse(src);
        Assert.True(parsed.Success);
        Assert.DoesNotContain(parsed.Diagnostics, d => d.Code == "CE0134");
    }

    [Fact]
    public void RealTimeDefault_HasNoManualClock()
    {
        using var sys = new TestActorSystem();
        Assert.Null(sys.Clock);
        Assert.Throws<InvalidOperationException>(
            () => sys.AdvanceClock(TimeSpan.FromSeconds(1)));
    }
}
