using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Spek.Compiler.Semantic;
using Spek.Runtime;
using Spek.Testing;
using Spek.Tests.Emit;
using Xunit;
using Xunit.Abstractions;

namespace Spek.Tests.Semantic;

/// <summary>
/// Coverage fills for compiler behaviors that existing suites assert in only
/// one direction (error-only or happy-only), and emit shapes that prior tests
/// pin with a string match but never round-trip through Roslyn / the runtime.
///
/// Each scenario was verified against <c>Spek.Compiler</c> before being
/// written — the comments record the exact source line / branch each test
/// exercises.
///
/// <list type="bullet">
///   <item><b>CE0103 (sealed-enum exhaustiveness)</b> — the documented subject
///         shapes and pattern shapes that <c>SealedEnumTests</c> never touch:
///         the <i>bare</i> variant pattern (<c>Active</c> with no <c>Status.</c>
///         prefix), a switch <i>mixing</i> qualified + bare arms, an enum-typed
///         method <i>parameter</i> subject, and the fail-open behaviour of an
///         untyped <c>var</c> local subject.</item>
///   <item><b>ask reply-type inference</b> — <c>OptionDAskTests</c> covers an
///         inline return and a top-level block return. These pin the two
///         control-flow branches of <c>SymbolTable.FindFirstReturnType</c> that
///         are otherwise untested (a <c>return</c> inside an <c>if</c> and
///         inside a <c>foreach</c>), plus a <i>generic</i> reply type
///         (<c>Box<int></c>) flowing through inference into
///         <c>AskAsync<Box<int>></c> and running end-to-end.</item>
/// </list>
/// </summary>
public sealed class CompilerCoverageTests(ITestOutputHelper output)
{
    // ── helpers (mirror SealedEnumTests / OptionDAskTests) ──────────────────

    private static CompilationResult Parse(string source) => SpekCompiler.Parse(source);

    private static IReadOnlyList<Diagnostic> Diagnose(string source) =>
        SpekCompiler.Parse(source).Diagnostics;

    private static void AssertParsesClean(string source)
    {
        var parsed = SpekCompiler.Parse(source);
        Assert.True(parsed.Success,
            "Expected clean parse + analysis; diagnostics:\n" +
            string.Join("\n", parsed.Diagnostics.Select(d => $"  {d.Code} {d.Line}:{d.Column} {d.Message}")));
    }

    private static string EmitCSharp(string source)
    {
        var parsed = SpekCompiler.Parse(source);
        Assert.True(parsed.Success,
            "Source must parse + analyze; diagnostics:\n" +
            string.Join("\n", parsed.Diagnostics.Select(d => $"  {d.Code} {d.Line}:{d.Column} {d.Message}")));
        return new FileEmitter().Emit(parsed.Tree!);
    }

    // ════════════════════════════════════════════════════════════════════════
    //  CE0103 — sealed-enum exhaustiveness, untested subject / pattern shapes
    // ════════════════════════════════════════════════════════════════════════

    // SealedEnumTests only ever uses the qualified `Status.Active` pattern. The
    // analyzer (SemanticAnalyzer.AddIfMatchesEnum) explicitly supports a BARE
    // single-part name too — `Active` resolving to a variant of the subject's
    // enum. These pin both directions of that bare-name branch.

    [Fact]
    public void CE0103_BareVariantPattern_Exhaustive_NoReport()
    {
        // Every variant covered by its bare name (no `Status.` qualifier). The
        // analyzer accepts this as exhaustive (AddIfMatchesEnum handles the
        // single-part case). NOTE: this source analyzes clean but does NOT
        // emit valid C# — see the skipped round-trip test below for the bug.
        const string src = """
            enum Status { Active, Inactive }
            message Tick();
            actor A
            {
                on Tick =>
                {
                    Status s = Status.Active;
                    var x = s switch {
                        Active   => 1,
                        Inactive => 2,
                    };
                }
            }
            """;
        Assert.DoesNotContain(Diagnose(src), d => d.Code == "CE0103");
        AssertParsesClean(src);
    }

    [Fact]
    public void CE0103_BareVariantPattern_NonExhaustive_Reports()
    {
        // Bare `Active` covers Active; Inactive is left uncovered. The CE must
        // still fire and name the missing variant — proving bare names are
        // counted as covers, not silently ignored (which would mask the gap).
        const string src = """
            enum Status { Active, Inactive }
            message Tick();
            actor A
            {
                on Tick =>
                {
                    Status s = Status.Active;
                    var x = s switch {
                        Active => 1,
                    };
                }
            }
            """;
        var diag = Assert.Single(Diagnose(src), d => d.Code == "CE0103");
        Assert.Contains("Inactive", diag.Message);
    }

    [Fact]
    public void CE0103_MixedQualifiedAndBareArms_Exhaustive_NoReport()
    {
        // One switch combining `Status.Active`, bare `Inactive`, and
        // `Status.Pending`. Both pattern shapes must contribute to the
        // covered-set in the same pass.
        const string src = """
            enum Status { Active, Inactive, Pending }
            message Tick();
            actor A
            {
                on Tick =>
                {
                    Status s = Status.Active;
                    var x = s switch {
                        Status.Active  => 1,
                        Inactive       => 2,
                        Status.Pending => 3,
                    };
                }
            }
            """;
        Assert.DoesNotContain(Diagnose(src), d => d.Code == "CE0103");
        AssertParsesClean(src);
    }

    // The CE0103 docstring lists "parameters" as a supported subject shape, but
    // SealedEnumTests only exercises typed locals and actor fields. These pin an
    // enum-typed *method parameter* as the switch subject (TryGetType resolves
    // it via RegisterParameter), in both directions.

    [Fact]
    public void CE0103_EnumMethodParameterSubject_NonExhaustive_Reports()
    {
        const string src = """
            enum Status { Active, Inactive }
            message Tick();
            actor A
            {
                int Classify(Status s)
                {
                    return s switch {
                        Status.Active => 1,
                    };
                }

                on Tick => { }
            }
            """;
        var diag = Assert.Single(Diagnose(src), d => d.Code == "CE0103");
        Assert.Contains("Inactive", diag.Message);
    }

    [Fact]
    public void CE0103_EnumMethodParameterSubject_Exhaustive_NoReport()
    {
        const string src = """
            enum Status { Active, Inactive }
            message Tick();
            actor A
            {
                int Classify(Status s)
                {
                    return s switch {
                        Status.Active   => 1,
                        Status.Inactive => 2,
                    };
                }

                on Tick => { }
            }
            """;
        Assert.DoesNotContain(Diagnose(src), d => d.Code == "CE0103");
        AssertParsesClean(src);
    }

    [Fact]
    public void CE0103_UntypedVarLocalSubject_IsNotChecked_FailsOpen()
    {
        // CheckExhaustiveSwitchOverEnum relies on ExpressionTyper.TryGetType,
        // which resolves only EXPLICITLY-typed locals (RegisterLocalWithType)
        // and actor fields. A `var s = Status.Active;` local is registered with
        // its ExprKind but NOT its TypeRef, so TryGetType returns null and the
        // check bails (fail-open, the documented convention for the rest of the
        // semantic walker).
        //
        // This pins that behaviour: a non-exhaustive switch over a `var` local
        // produces NO CE0103. (The class docstring on CheckExhaustiveSwitchOverEnum
        // describes opportunistic var-local classification, but the implementation
        // does not realize it — see couldNotTest in the run report. The contract
        // the analyzer actually upholds is "no false positives", which this
        // verifies by asserting the source still parses cleanly.)
        const string src = """
            enum Status { Active, Inactive }
            message Tick();
            actor A
            {
                on Tick =>
                {
                    var s = Status.Active;
                    var x = s switch {
                        Status.Active => 1,
                    };
                }
            }
            """;
        Assert.DoesNotContain(Diagnose(src), d => d.Code == "CE0103");
        AssertParsesClean(src);
    }

    // Fixed: the switch emitter now qualifies a bare enum-variant pattern
    // (`Active =>` → `Status.Active =>`) when the name resolves to a unique enum
    // member, so the bare form round-trips through Roslyn.
    [Fact]
    public void CE0103_ExhaustiveBareSwitch_RoundTripsThroughRoslyn()
    {
        // The bare-variant arms must also lower to valid C#. (SealedEnumTests
        // only round-trips the qualified form.)
        const string src = """
            enum Status { Active, Inactive, Pending }
            message Tick();
            actor A
            {
                int n = 0;
                on Tick =>
                {
                    Status s = Status.Active;
                    n = s switch {
                        Active   => 1,
                        Inactive => 2,
                        Pending  => 3,
                    };
                }
            }
            """;
        var code = EmitCSharp(src);
        var (ok, errors, _) = RoslynCompileHelper.TryCompile(code, "BareEnumSmoke");
        Assert.True(ok, "Emitted C# did not compile:\n" + errors + "\n--- emitted ---\n" + code);
    }

    // ════════════════════════════════════════════════════════════════════════
    //  ask reply-type inference — FindFirstReturnType control-flow branches
    // ════════════════════════════════════════════════════════════════════════

    // OptionDAskTests covers an inline `=> return new Pong()` and a block whose
    // return sits at the top level. SymbolTable.FindFirstReturnType also walks
    // INTO an `if` (IfStmt) and INTO loop bodies (ForeachStmt/ForStmt/...).
    // These two prove the reply type is still inferred when the only `return`
    // is nested inside control flow.

    [Fact]
    public void AskInference_ReturnInsideIf_InfersReplyType()
    {
        const string src = """
            namespace AskIf;
            message Ping();
            message Pong();
            message Kick();
            actor Ponger
            {
                behavior Idle
                {
                    on Ping =>
                    {
                        if (true) { return new Pong(); }
                        return new Pong();
                    }
                }
            }
            actor Caller
            {
                ActorRef peer;
                init(ActorRef p) { peer = p; }
                behavior Ready
                {
                    on Kick => { var r = peer.Ask(new Ping()); }
                }
            }
            """;
        var code = EmitCSharp(src);
        // Inference reached the `return` nested in the `if` — typed, not object.
        Assert.Contains("AskAsync<Pong>", code);
        Assert.DoesNotContain("AskAsync<object>", code);
    }

    [Fact]
    public void AskInference_ReturnInsideForeach_InfersReplyType()
    {
        const string src = """
            namespace AskForeach;
            message Ping();
            message Pong();
            message Kick();
            actor Ponger
            {
                behavior Idle
                {
                    on Ping =>
                    {
                        foreach (var i in new int[] { 1 })
                        {
                            return new Pong();
                        }
                        return new Pong();
                    }
                }
            }
            actor Caller
            {
                ActorRef peer;
                init(ActorRef p) { peer = p; }
                behavior Ready
                {
                    on Kick => { var r = peer.Ask(new Ping()); }
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("AskAsync<Pong>", code);
        Assert.DoesNotContain("AskAsync<object>", code);
    }

    // A GENERIC reply type. OptionDAskTests only ever infers a plain message
    // type. FindFirstReturnType captures `newExpr.TypeArgs`, so a
    // `return new Box<int>(...)` should infer `Box<int>` — and the emitted
    // `AskAsync<Box<int>>` must compile AND route correctly at runtime.

    [Fact]
    public void AskInference_GenericReplyType_EmitsGenericAskType()
    {
        const string src = """
            namespace GenAskEmit;
            message Ping();
            message Box<T>(T value);
            message Kick();
            actor Server
            {
                behavior Idle { on Ping => { return new Box<int>(7); } }
            }
            actor Caller
            {
                ActorRef peer;
                init(ActorRef p) { peer = p; }
                behavior Ready
                {
                    on Kick => { var b = peer.Ask(new Ping()); }
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("AskAsync<Box<int>>", code);
        Assert.DoesNotContain("AskAsync<object>", code);
    }

    [Fact]
    public void AskInference_GenericReplyType_RoundTripsAndRoutesAtRuntime()
    {
        // End-to-end: a generic reply type flows through inference, the emitted
        // C# compiles via Roslyn, and the actual ask round-trips the
        // Box<int>(42) back to the caller, which reports `value` to the probe.
        const string src = """
            namespace GenAskRun;
            message Ping();
            message Box<T>(T value);
            message Start();
            message Done(int got);

            actor Server
            {
                behavior Idle { on Ping => { return new Box<int>(42); } }
            }

            public actor Client
            {
                ActorRef peer;
                init(ActorRef p) { peer = p; }
                behavior Ready
                {
                    on Start =>
                    {
                        var b = peer.Ask(new Ping());
                        sender.Tell(new Done(b.value));
                    }
                }
            }
            """;
        var parse = Parse(src);
        Assert.True(parse.Success,
            string.Join("\n", parse.Diagnostics.Select(d => $"  {d.Code} {d.Line}:{d.Column} {d.Message}")));

        var csharp = new FileEmitter().Emit(parse.Tree!);
        Assert.Contains("AskAsync<Box<int>>", csharp);

        var assembly = RoslynCompileHelper.CompileAndLoad(csharp, "GenAskRun");
        var clientTy = assembly.GetType("GenAskRun.Client")!;
        var serverTy = assembly.GetType("GenAskRun.Server")!;
        var startTy  = assembly.GetType("GenAskRun.Start")!;
        var doneTy   = assembly.GetType("GenAskRun.Done")!;

        using var system = new TestActorSystem("genask-run");
        var probe  = system.CreateProbe();
        var server = system.Spawn(serverTy);
        var client = system.Spawn(clientTy, server);

        probe.Send(client, Activator.CreateInstance(startTy)!);

        var done = probe.ExpectMsg(doneTy);
        Assert.Equal(42, doneTy.GetProperty("got")!.GetValue(done));
    }
}

