using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Spek.Compiler.Semantic;
using Spek.Tests.Emit;
using Xunit;

namespace Spek.Tests.Semantic;

/// <summary>
/// Red-team H2 + emit-A (round 2): the stream-operator chain
/// (<c>on X =&gt; throttle(window) =&gt; distinct(by: x =&gt; …) =&gt; { }</c>) was a
/// walker blind spot shared by THREE passes — WalkExpr (CE0119 raw concurrency),
/// CheckConversionTargets (CE0127/CE0129), and CheckTimeApis (CE0134) — none of
/// which descended into operator args or selector lambdas, so those bans all
/// leaked inside a selector that runs OFF the actor's turn. The emitter had the
/// same shape: it built the chain-step ExpressionEmitter with an EMPTY field set,
/// so a field read in an operator arg emitted bare (<c>window</c> vs <c>_window</c>)
/// → CS0103 on the generated C#. These fences pin the closure of both.
/// </summary>
public sealed class RedTeamStreamSelectorTests
{
    private static bool Has(string src, string code) =>
        SpekCompiler.Parse(src).Diagnostics.Any(d => d.Code == code);

    private static string Wrap(string chain) => $$"""
        using Spek.Streams;
        using System.Threading.Tasks;

        message Reading(string sensor, int value);

        actor Sensor
        {
            int window = 200;

            init() { become Active; }

            behavior Active
            {
                on Reading r => {{chain}} =>
                {
                    Console.WriteLine($"reading {r.value}");
                }
            }
        }
        """;

    // ─── H2: the CE family fires inside a stream selector ────────────────────

    [Fact]
    public void CE0119_TaskRunInStreamSelector_Errors() =>
        Assert.True(Has(Wrap("distinct<Reading, int>(by: x => Task.Run<int>(() => x.value).Result)"), "CE0119"),
            "raw concurrency (Task.Run) inside a selector must not leak past CE0119");

    [Fact]
    public void CE0129_CastInStreamSelector_Errors() =>
        Assert.True(Has(Wrap("distinct<Reading, long>(by: x => (long)x.value)"), "CE0129"),
            "a cast inside a selector must not leak past CE0129");

    [Fact]
    public void CE0127_UnknownConversionTargetInStreamSelector_Errors() =>
        Assert.True(Has(Wrap("distinct<Reading, System.Guid>(by: x => x.value.To<System.Guid>())"), "CE0127"),
            "an unroutable conversion target inside a selector must not leak past CE0127");

    [Fact]
    public void CE0134_TimeReadInStreamSelector_Warns() =>
        Assert.True(Has(Wrap("distinct<Reading, System.DateTime>(by: x => System.DateTime.UtcNow)"), "CE0134"),
            "a direct clock read inside a selector must not leak past CE0134");

    [Fact]
    public void CE0119_TaskRunInOperatorArg_Errors() =>
        Assert.True(Has(Wrap("throttle(Task.Run<int>(() => 200).Result)"), "CE0119"),
            "raw concurrency in an operator ARG (not a selector) must not leak either");

    // ─── False-positive guard: a legitimate selector stays clean ─────────────

    [Fact]
    public void CleanStreamSelector_NoConcurrencyOrConversionOrTimeDiagnostics()
    {
        var diags = SpekCompiler.Parse(Wrap("distinct<Reading, int>(by: x => x.value)")).Diagnostics;
        Assert.DoesNotContain(diags, d => d.Code is "CE0119" or "CE0127" or "CE0129" or "CE0134");
    }

    // ─── emit-B: the bare distinct(by:) shorthand infers BOTH type args ──────
    // distinct<T, TKey> needs two type args; injecting only <T> gave CS0305.
    // Typing the selector's parameter lets C# infer both from the lambda.
    [Fact]
    public void BareDistinctShorthand_TypesSelectorParam_AndCompiles()
    {
        var src = Wrap("distinct(by: x => x.value)");
        var parsed = SpekCompiler.Parse(src);
        Assert.True(parsed.Success,
            string.Join("\n", parsed.Diagnostics.Select(d => $"  {d.Code} {d.Message}")));

        var code = new FileEmitter().Emit(parsed.Tree!);
        Assert.Contains("distinct(by: (Reading x) => x.value)", code);   // typed selector
        Assert.DoesNotContain("distinct<Reading>(", code);               // NOT wrong-arity <T>

        var (ok, summary, _) = RoslynCompileHelper.TryCompile(code, "BareDistinct");
        Assert.True(ok, $"emitted C# must compile:\n{summary}");
    }

    // ─── emit-A: a field read in an operator arg emits the prefixed field ────

    [Fact]
    public void StreamOperatorArg_ReadingActorField_EmitsPrefixedFieldAndCompiles()
    {
        var src = Wrap("throttle(window)");   // `window` is an actor field
        var parsed = SpekCompiler.Parse(src);
        Assert.True(parsed.Success,
            "the field-reading stream handler must parse + analyze cleanly:\n" +
            string.Join("\n", parsed.Diagnostics.Select(d => $"  {d.Code} {d.Message}")));

        var code = new FileEmitter().Emit(parsed.Tree!);
        // The operator arg must reference the generated field, not a bare name.
        Assert.Contains("throttle<Reading>(_window)", code);
        Assert.DoesNotContain("throttle<Reading>(window)", code);

        // And the emitted C# must actually compile — the bug was green Spek,
        // then CS0103 on the generated field reference.
        var (ok, summary, _) = RoslynCompileHelper.TryCompile(code, "StreamEmitAField");
        Assert.True(ok, $"emitted C# must compile:\n{summary}");
    }
}
