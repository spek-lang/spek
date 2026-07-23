using System.Text;
using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Configs;
using Spek.Runtime;

namespace Spek.Benchmarks.Language;

/// <summary>
/// What behavior-shaped Spek source costs per message, through the real
/// pipeline: the .spek source below is parsed, emitted to C#, Roslyn-
/// compiled, and spawned once in GlobalSetup; each measured operation is a
/// plain Tell into the emitted dispatch switch.
///
/// The Become pair prices the behavior-switch write: <c>Flipper</c> executes
/// a <c>become</c> to the other of two behaviors on EVERY message, while
/// <c>SingleBehavior</c> handles the same message rate with no transition.
/// The delta is what a per-message behavior change adds on top of a plain
/// dispatch — the switch-field write plus whatever the emitted dispatch
/// table does with it.
///
/// DispatchArmScaling asks whether the emitted switch scales: actors with
/// 2, 8, and 32 <c>on</c> handlers in one behavior, always sent the
/// LAST-declared message type, so a sequential type-test chain would show
/// up as growth across the three arms.
/// </summary>
[MemoryDiagnoser]
[GroupBenchmarksBy(BenchmarkLogicalGroupRule.ByCategory)]
[CategoriesColumn]
public class BehaviorBenchmarks
{
    private const int MessagesPerInvoke = 10_000;

    private const string Source = """
        message Ping();

        actor Flipper
        {
            int count = 0;

            init() { become A; }

            behavior A
            {
                on Ping => { count = count + 1; become B; }
            }

            behavior B
            {
                on Ping => { count = count + 1; become A; }
            }
        }

        actor SingleBehavior
        {
            int count = 0;

            on Ping => { count = count + 1; }
        }
        """;

    private ActorSystem _system = null!;
    private ActorRef _flipper = null!;
    private ActorRef _single = null!;
    private object _ping = null!;
    private readonly Dictionary<int, (ActorRef Actor, object Message)> _scaling = new();

    [GlobalSetup]
    public void Setup()
    {
        var assembly = SpekSourceCompiler.Compile(Source + ScalingSource(), "BehaviorShapes");
        _system  = new ActorSystem("langbench-behavior");
        _flipper = _system.Spawn(SpekSourceCompiler.TypeNamed(assembly, "Flipper"));
        _single  = _system.Spawn(SpekSourceCompiler.TypeNamed(assembly, "SingleBehavior"));
        _ping    = SpekSourceCompiler.NewMessage(assembly, "Ping");

        foreach (var handlers in new[] { 2, 8, 32 })
        {
            var actor = _system.Spawn(SpekSourceCompiler.TypeNamed(assembly, $"Wide{handlers}"));
            var last  = SpekSourceCompiler.NewMessage(assembly, $"M{handlers - 1}");
            _scaling[handlers] = (actor, last);
        }

        // Prime every actor once so one-time work (Initialize, init-block
        // become, first dispatch) happens before measurement.
        _flipper.Tell(_ping);
        _single.Tell(_ping);
        foreach (var (actor, message) in _scaling.Values)
            actor.Tell(message);
        _system.AwaitTermination();
    }

    [GlobalCleanup]
    public void Cleanup() => _system.Dispose();

    /// <summary>Baseline: same message rate as <see cref="BecomeTransition"/>,
    /// same handler body, no behavior change.</summary>
    [BenchmarkCategory("Become"), Benchmark(Baseline = true, OperationsPerInvoke = MessagesPerInvoke)]
    public void SingleBehaviorDispatch()
    {
        for (int i = 0; i < MessagesPerInvoke; i++)
            _single.Tell(_ping);
        _system.AwaitTermination();
    }

    /// <summary>Every message executes <c>become</c> to the other of two
    /// behaviors. The ratio against the baseline is the per-message price of
    /// a behavior transition in emitted code.</summary>
    [BenchmarkCategory("Become"), Benchmark(OperationsPerInvoke = MessagesPerInvoke)]
    public void BecomeTransition()
    {
        for (int i = 0; i < MessagesPerInvoke; i++)
            _flipper.Tell(_ping);
        _system.AwaitTermination();
    }

    /// <summary>Dispatch through a behavior with 2 / 8 / 32 <c>on</c> arms,
    /// always sending the last-declared message type — the worst case for a
    /// sequential type-test chain.</summary>
    [BenchmarkCategory("Arms"), Benchmark(OperationsPerInvoke = MessagesPerInvoke)]
    [Arguments(2)]
    [Arguments(8)]
    [Arguments(32)]
    public void DispatchArmScaling(int handlers)
    {
        var (actor, message) = _scaling[handlers];
        for (int i = 0; i < MessagesPerInvoke; i++)
            actor.Tell(message);
        _system.AwaitTermination();
    }

    /// <summary>Generates 32 empty message types plus Wide2 / Wide8 / Wide32,
    /// actors whose single behavior holds that many identical arms. Shared
    /// message declarations keep the arms honest: each actor's switch still
    /// contains exactly its own arm count.</summary>
    private static string ScalingSource()
    {
        var sb = new StringBuilder();
        sb.AppendLine();
        for (int i = 0; i < 32; i++)
            sb.AppendLine($"message M{i}();");

        foreach (var n in new[] { 2, 8, 32 })
        {
            sb.AppendLine();
            sb.AppendLine($"actor Wide{n}");
            sb.AppendLine("{");
            sb.AppendLine("    int count = 0;");
            for (int i = 0; i < n; i++)
            {
                sb.AppendLine();
                sb.AppendLine($"    on M{i} => {{ count = count + 1; }}");
            }
            sb.AppendLine("}");
        }
        return sb.ToString();
    }
}
