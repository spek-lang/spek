using Spek;
using Spek.Runtime;
using Spek.Testing;
using Xunit;

namespace Spek.Tests.Runtime;

/// <summary>
/// Property-based actor testing with integrated shrinking (D3): properties
/// run under the deterministic simulator, failures shrink to a minimal
/// (input, schedule) pair, and every repro is seed-exact.
/// </summary>
public sealed class PropertyTestingTests
{
    private abstract record CounterMsg;
    private sealed record Add(int N) : CounterMsg;
    private sealed record Reset : CounterMsg;
    private sealed record GetTotal;
    private sealed record Total(int Value);

    private sealed class Counter : ActorBase
    {
        private int _total;
        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            switch (message)
            {
                case Add a: _total += a.N; break;
                case Reset: _total = 0; break;
                case GetTotal: sender.Tell(new Total(_total), _selfRef!); break;
            }
            return Task.CompletedTask;
        }
    }

    /// <summary>The bug: quietly saturates — deposits past 100 are lost.</summary>
    private sealed class SaturatingCounter : ActorBase
    {
        private int _total;
        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            switch (message)
            {
                case Add a when _total + a.N <= 100: _total += a.N; break;
                case Add: break;                     // silently dropped
                case Reset: _total = 0; break;
                case GetTotal: sender.Tell(new Total(_total), _selfRef!); break;
            }
            return Task.CompletedTask;
        }
    }

    // Weighted toward deposits (a 50/50 Reset mix would almost never let
    // the total accumulate); shrink order still puts Add first.
    private static readonly Gen<CounterMsg> AddGen =
        Gen.Int(0, 50).Select(n => (CounterMsg)new Add(n));

    private static readonly Gen<List<CounterMsg>> Msgs =
        Gen.Sequence(
            Gen.OneOf(AddGen, AddGen, AddGen, AddGen, AddGen,
                      Gen.OneOf<CounterMsg>(new Reset())),
            upTo: 30);

    private static bool AgreesWithModel<TActor>(List<CounterMsg> msgs, SimulatedActorSystem sim)
        where TActor : ActorBase
    {
        var counter = sim.Spawn<TActor>();
        var model = 0;
        foreach (var m in msgs)
        {
            counter.Tell(m);
            if (m is Add a) model += a.N;
            if (m is Reset) model = 0;
        }
        sim.Run();
        return sim.Ask<Total>(counter, new GetTotal()).Value == model;
    }

    [Fact]
    public void CorrectCounter_AgreesWithTheModel_AcrossGeneratedSequences()
    {
        var result = Prop.ForAll(Msgs, AgreesWithModel<Counter>, runs: 60);
        Assert.False(result.Falsified);
        result.Assert();   // no-op on success
    }

    [Fact]
    public void BuggyCounter_IsFalsified_AndShrinksToAMinimalSequence()
    {
        var result = Prop.ForAll(Msgs, AgreesWithModel<SaturatingCounter>, runs: 200);

        Assert.True(result.Falsified);
        Assert.True(result.MinimalChoiceCount < result.OriginalChoiceCount,
            "shrinking made no progress");
        // The minimal repro needs only enough deposits to cross 100 and one
        // that gets dropped — a handful of messages, not a generated haystack.
        Assert.NotNull(result.MinimalInput);
        var text = result.MinimalInput!;
        var messages = text.Split("Add").Length - 1 + text.Split("Reset").Length - 1;
        Assert.InRange(messages, 1, 6);

        var ex = Assert.Throws<PropertyFailedException>(result.Assert);
        Assert.Contains($"seed {result.Seed}", ex.Message);
    }

    [Fact]
    public void SameSeed_ProducesTheIdenticalFailureReport()
    {
        var first = Prop.ForAll(Msgs, AgreesWithModel<SaturatingCounter>, runs: 200, seed: 99);
        var second = Prop.ForAll(Msgs, AgreesWithModel<SaturatingCounter>, runs: 200, seed: 99);

        Assert.Equal(first.Seed, second.Seed);
        Assert.Equal(first.MinimalInput, second.MinimalInput);
        Assert.Equal(first.ShrinkProbes, second.ShrinkProbes);
    }

    [Fact]
    public void IntegerShrinking_DescendsToTheBoundary()
    {
        // Pure-data property: ints below 10 pass. The shrinker should land
        // exactly on the boundary counterexample.
        var result = Prop.ForAll(
            Gen.Int(0, 1000), (n, _) => n < 10, runs: 50, seed: 7);

        Assert.True(result.Falsified);
        Assert.Equal("10", result.MinimalInput);
    }
}
