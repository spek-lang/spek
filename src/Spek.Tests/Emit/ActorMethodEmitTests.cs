using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Xunit;

namespace Spek.Tests.Emit;

/// <summary>
/// Actor bodies may declare ordinary methods and override ActorBase hooks
/// (OnFailure / OnChildFailure / …). These must be EMITTED — previously the actor
/// emitter had no method step at all, so they parsed but were silently dropped.
/// That broke the documented OnFailure override and made <c>FailureDirective.Resume</c>
/// (only reachable via an OnChildFailure override) impossible to express.
/// </summary>
public sealed class ActorMethodEmitTests
{
    private const string Src = """
        namespace M;

        message Go();

        actor Worker
        {
            init() { become R; }

            behavior R { on Go => { } }

            int Bump(int x) { return x + 1; }

            FailureDirective OnFailure(Exception ex, object msg) { return FailureDirective.Resume; }

            FailureDirective OnChildFailure(ActorRef child, Exception cause, object msg) { return FailureDirective.Resume; }

            void OnPreRestart() { }
        }
        """;

    [Fact]
    public void ActorMethodsAndOverrideHooks_AreEmitted()
    {
        var parse = SpekCompiler.Parse(Src);
        Assert.True(parse.Success,
            string.Join("\n", parse.Diagnostics.Select(d => $"  {d.Code} {d.Message}")));

        var csharp = new FileEmitter().Emit(parse.Tree!);

        // Ordinary method keeps its declared (default-private) visibility.
        Assert.Contains("int Bump(int x)", csharp);

        // ActorBase hooks emit as `protected override` — otherwise they don't take
        // effect (the failure mode this test guards against).
        Assert.Contains("protected override FailureDirective OnFailure(", csharp);
        Assert.Contains("protected override FailureDirective OnChildFailure(", csharp);

        // OnPreRestart is NOT an ActorBase virtual — it must emit as a plain method,
        // never `protected override` (which would not compile, CS0115).
        Assert.Contains("private void OnPreRestart()", csharp);
        Assert.DoesNotContain("override void OnPreRestart", csharp);
    }
}
