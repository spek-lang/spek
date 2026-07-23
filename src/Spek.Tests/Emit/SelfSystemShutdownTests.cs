using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Xunit;
using Xunit.Abstractions;

namespace Spek.Tests.Emit;

/// <summary>
/// `self.System.Shutdown()` is the actor-reachable graceful node-shutdown
/// verb (the supported replacement for the CE0084 process escapes). It lowers
/// to `this.SpekSystem.Shutdown()` — the `SpekSystem` property name avoids
/// shadowing the `System` namespace inside generated actor code — and compiles
/// against `ActorBase`.
/// </summary>
public sealed class SelfSystemShutdownTests(ITestOutputHelper output)
{
    private static string EmitCSharp(string source)
    {
        var parsed = SpekCompiler.Parse(source);
        Assert.True(parsed.Success,
            string.Join("\n", parsed.Diagnostics.Select(d => $"{d.Code} {d.Message}")));
        return new FileEmitter().Emit(parsed.Tree!);
    }

    [Fact]
    public void SelfSystemShutdown_LowersToSpekSystemProperty_AndCompiles()
    {
        const string src = """
            message Boom();
            actor Watchdog
            {
                on Boom => { self.System.Shutdown(); }
            }
            """;
        var code = EmitCSharp(src);

        Assert.Contains("this.SpekSystem.Shutdown()", code);
        Assert.DoesNotContain("_selfRef.System", code);   // NOT an ActorRef member

        var (ok, summary, _) = RoslynCompileHelper.TryCompile(code, "SelfSystemShutdown");
        if (!ok) { output.WriteLine(code); output.WriteLine(summary); }
        Assert.True(ok, $"Emitted C# did not compile:\n{summary}");
    }
}
