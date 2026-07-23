using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Xunit;

namespace Spek.Tests.Emit;

/// <summary>
/// Coverage for stream-shaped event policies. Handlers may
/// chain operator factories with `=>` between the message pattern
/// and the body. Operators ship in <c>Spek.Streams</c>; the
/// compiler emits per-handler scaffolding (lazy operator field,
/// synthetic body-trigger record, and dispatch routing).
/// </summary>
public sealed class StreamOperatorTests
{
    private static string EmitCSharp(string source)
    {
        var parsed = SpekCompiler.Parse(source);
        Assert.True(parsed.Success,
            "Source must parse; diagnostics: " +
            string.Join("\n", parsed.Diagnostics.Select(d => $"{d.Code} {d.Line}:{d.Column} {d.Message}")));
        return new FileEmitter().Emit(parsed.Tree!);
    }

    [Fact]
    public void Handler_WithoutOperators_EmitsUnchanged()
    {
        // Sanity check: a handler that doesn't use any operator chain
        // still emits as a normal dispatch arm. Confirms the
        // grammar change is fully backward-compatible.
        const string src = """
            message Tick();
            actor A
            {
                int n = 0;
                on Tick =>
                {
                    n = n + 1;
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.DoesNotContain("__FireBody_", code);
        Assert.DoesNotContain("Spek.Streams", code);
    }

    [Fact]
    public void Handler_WithSingleOperator_EmitsScaffolding()
    {
        const string src = """
            using Spek.Streams;

            message Tick();
            actor A
            {
                int n = 0;
                on Tick =>
                    debounce(500) =>
                    {
                        n = n + 1;
                    }
            }
            """;
        var code = EmitCSharp(src);

        // Synthetic body-trigger record.
        Assert.Contains("private sealed record __FireBody_Default_Tick(Tick Inner);", code);

        // Lazy operator field + getter.
        Assert.Contains("private Spek.Streams.StreamOperator<Tick>? __ops_Default_Tick;", code);
        Assert.Contains("Ops_Default_Tick()", code);

        // Single-step chain assigns to an explicit-typed variable so the
        // factory's <T> is bound from the declaration site. The factory
        // call gets an explicit `<Tick>` injected by the emitter.
        Assert.Contains("Spek.Streams.StreamOperator<Tick> op = debounce<Tick>(500)", code);

        // Dispatch arm routes to operator chain.
        Assert.Contains("await Ops_Default_Tick().OfferAsync", code);

        // Synthetic body-trigger arm runs the user body.
        Assert.Contains("case __FireBody_Default_Tick __trig:", code);
    }

    [Fact]
    public void Handler_WithMultipleOperators_UsesCompose()
    {
        // Two chain steps: the compiler wraps in compose<T>(...) so
        // both operators run in declaration order with shared state.
        const string src = """
            using Spek.Streams;

            message Update(int value);
            actor A
            {
                int latest = 0;
                on Update u =>
                    throttle(16) =>
                    debounce(100) =>
                    {
                        latest = u.value;
                    }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("Spek.Streams.StreamOperators.compose<Update>(", code);
        Assert.Contains("throttle<Update>(16)", code);
        Assert.Contains("debounce<Update>(100)", code);
    }

    [Fact]
    public void Handler_WithOperator_NamedBindPattern_RestoresBinding()
    {
        // For `on Update u`, the body should still see `u` after the
        // chain emits. The synthetic body-trigger arm restores the
        // binding from the trigger record's Inner field.
        const string src = """
            using Spek.Streams;

            message Update(int value);
            actor A
            {
                int latest = 0;
                on Update u =>
                    debounce(100) =>
                    {
                        latest = u.value;
                    }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("var u = __trig.Inner;", code);
    }

    [Fact]
    public void Handler_WithOperator_NoBindPattern_NoBindingNeeded()
    {
        // For `on Tick` (no binding), the body doesn't reference the
        // message — no restore needed in the synthetic arm.
        const string src = """
            using Spek.Streams;

            message Tick();
            actor A
            {
                int n = 0;
                on Tick =>
                    debounce(50) =>
                    {
                        n = n + 1;
                    }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("case __FireBody_Default_Tick __trig:", code);
        // No `var ___ = __trig.Inner;` line for unbound patterns.
        Assert.DoesNotContain("var _ = __trig.Inner", code);
    }

    [Fact]
    public void Handler_StreamShaped_RoundTripsThroughRoslyn()
    {
        const string src = """
            using Spek.Streams;

            message Tick();
            message Reply(int n);
            actor Counter
            {
                int n = 0;
                on Tick =>
                    debounce(50) =>
                    {
                        n = n + 1;
                        return new Reply(n);
                    }
            }
            """;
        var code = EmitCSharp(src);
        var (success, errors, _) = RoslynCompileHelper.TryCompile(code, "StreamSingleSmoke");
        Assert.True(success,
            "Emitted C# did not compile:\n" + errors + "\n--- emitted ---\n" + code);
    }

    [Fact]
    public void Handler_MultipleStreamSteps_RoundTripsThroughRoslyn()
    {
        const string src = """
            using Spek.Streams;

            message Tick();
            actor Counter
            {
                int n = 0;
                on Tick =>
                    throttle(16) =>
                    debounce(100) =>
                    {
                        n = n + 1;
                    }
            }
            """;
        var code = EmitCSharp(src);
        var (success, errors, _) = RoslynCompileHelper.TryCompile(code, "StreamMultiSmoke");
        Assert.True(success,
            "Emitted C# did not compile:\n" + errors + "\n--- emitted ---\n" + code);
    }

    [Fact]
    public void NonStreamHandler_StillCompiles()
    {
        // Regression check: an actor with handlers that DON'T use the
        // chain still emits cleanly with no Spek.Streams references.
        const string src = """
            message Tick();
            actor A
            {
                int n = 0;
                on Tick =>
                {
                    n = n + 1;
                }
            }
            """;
        var code = EmitCSharp(src);
        var (success, errors, _) = RoslynCompileHelper.TryCompile(code, "NonStreamSmoke");
        Assert.True(success,
            "Emitted C# did not compile:\n" + errors + "\n--- emitted ---\n" + code);
    }

    [Fact]
    public void ExplicitlyTypedFactoryCall_Compiles_AndIsNotDoubleInjected()
    {
        // `debounce<Reading>(500)` is the explicitly-annotated form of the
        // documented `debounce(500)` — the shape the chain emitter itself
        // generates. It used to crash the compiler with a raw .NET stack
        // trace (InvocationExpr had nowhere to carry type args, so
        // VisitTypedCallExpr threw). It must parse, and the chain emitter
        // must respect the author's annotation rather than appending its own.
        const string src = """
            using Spek.Streams;
            message Reading(int v);
            actor Mon
            {
                on Reading r
                    => debounce<Reading>(500)
                    => { }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("debounce<Reading>(500)", code);
        Assert.DoesNotContain("debounce<Reading><Reading>", code);
    }
}
