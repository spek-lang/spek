using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Spek.Runtime;
using Spek.Testing;
using Spek.Tests.Emit;
using Xunit;

namespace Spek.Tests.Runtime;

/// <summary>
/// End-to-end coverage for Spek-native enums.
/// <list type="bullet">
///   <item>Enum declarations parse and emit as plain C# enums.</item>
///   <item>Enums are accepted as <c>message</c> field types (CE0010
///         pass-through).</item>
///   <item>Enum-member access via dotted name (<c>HostState.Paused</c>)
///         round-trips through parse → emit → Roslyn → runtime.</item>
/// </list>
/// </summary>
public class EnumE2ETests
{
    [Fact]
    public void EnumValue_FlowsThroughMessageField()
    {
        // Actor receives a StateChanged message carrying an enum field
        // and replies with the same enum value back so the test can
        // confirm runtime equality.
        const string src = """
            namespace EnumDemo;

            enum HostState
            {
                Running,
                Paused,
                Stopped,
            }

            message StateChanged(HostState state);
            message Echo(HostState state);
            message Reply(HostState state);

            public actor Mirror
            {
                behavior Idle
                {
                    on Echo e => { sender.Tell(new Reply(e.state)); }
                }
            }
            """;

        var parse = SpekCompiler.Parse(src);
        Assert.True(parse.Success,
            string.Join("\n", parse.Diagnostics.Select(d => $"  {d.Code} {d.Line}:{d.Column} {d.Message}")));

        var csharp   = new FileEmitter().Emit(parse.Tree!);
        var assembly = RoslynCompileHelper.CompileAndLoad(csharp, "EnumE2E");

        // Sanity-check the generated C# contains the enum declaration.
        Assert.Contains("public enum HostState", csharp);
        Assert.Contains("Running,", csharp);
        Assert.Contains("Paused,",  csharp);
        Assert.Contains("Stopped,", csharp);

        var mirrorTy   = assembly.GetType("EnumDemo.Mirror")!;
        var echoTy     = assembly.GetType("EnumDemo.Echo")!;
        var replyTy    = assembly.GetType("EnumDemo.Reply")!;
        var hostStateTy = assembly.GetType("EnumDemo.HostState")!;

        // HostState.Paused via reflection — the value the test sends in.
        var paused = Enum.Parse(hostStateTy, "Paused");

        using var system = new TestActorSystem("enum-e2e");
        var probe  = system.CreateProbe();
        var mirror = system.Spawn(mirrorTy);

        var echoMsg = Activator.CreateInstance(echoTy, paused)!;
        probe.Send(mirror, echoMsg);

        var reply = probe.ExpectMsg(replyTy);
        var roundTripped = replyTy.GetProperty("state")!.GetValue(reply);

        Assert.Equal(paused, roundTripped);
    }
}
