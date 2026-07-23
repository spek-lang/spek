using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Spek.Runtime;
using Spek.Testing;
using Spek.Tests.Emit;
using Spek.Tests.Fixtures;
using Xunit;

namespace Spek.Tests.Runtime;

/// <summary>
/// Full-pipeline integration tests: take a .spek fixture, parse it, emit C#,
/// compile that C# with Roslyn, load the resulting assembly, spawn the
/// compiled actor, send it a real message, and assert on the reply.
/// This is the highest-confidence signal that the whole toolchain works.
/// </summary>
public class FullPipelineRuntimeTests
{
    [Fact]
    public void Fixture02_Echo_CompiledAndExecuted_RepliesWithPong()
    {
        var source = FixtureLoader.Load("02_simple_actor.spek");
        var parse  = SpekCompiler.Parse(source);
        Assert.True(parse.Success,
            string.Join("\n", parse.Diagnostics.Select(d => $"  {d.Code} {d.Line}:{d.Column} {d.Message}")));

        var csharp   = new FileEmitter().Emit(parse.Tree!);
        var assembly = RoslynCompileHelper.CompileAndLoad(csharp, "Fixture02");

        var echoType = assembly.GetType("Samples.Echo")
            ?? throw new InvalidOperationException("Samples.Echo not in compiled assembly");
        var pingType = assembly.GetType("Samples.Ping")
            ?? throw new InvalidOperationException("Samples.Ping not in compiled assembly");
        var pongType = assembly.GetType("Samples.Pong")
            ?? throw new InvalidOperationException("Samples.Pong not in compiled assembly");

        using var system = new TestActorSystem();
        var probe = system.CreateProbe();

        var echoRef = system.Spawn(echoType);
        var ping    = Activator.CreateInstance(pingType)!;

        probe.Send(echoRef, ping);

        var pong = probe.ExpectMsg(pongType);
        Assert.IsType(pongType, pong);
    }
}
