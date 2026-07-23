using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Spek.Compiler.Semantic;
using Spek.Tests.Emit;
using Xunit;
using Xunit.Abstractions;

namespace Spek.Tests.Semantic;

/// <summary>
/// Coverage-gap fills for the semantic analyzer and the Spek-to-C# emitter.
/// Each test pairs an existing "error case" with the missing happy-path
/// contrast, or exercises a branch the existing suites skip:
/// <list type="bullet">
///   <item>CE0112 happy path — an *immutable*-typed shared-region field
///         (message / primitive) is accepted (only the mutable-class error
///         case is covered in <c>ClassConfinementTests</c>).</item>
///   <item>CE0087 over an index assignment (<c>field[i] = x</c>) from a
///         reader handler — the existing tests only cover bare-field and
///         <c>field.member</c> assignment, not index mutation.</item>
///   <item>Generics with two type parameters, each carrying its own
///         <c>where</c> clause — existing emit tests only do single-param
///         constraints.</item>
///   <item>A <c>switch</c> expression arm combining a relational pattern AND
///         a <c>when</c> guard — existing tests cover each in isolation.</item>
/// </list>
/// </summary>
public sealed class SemanticAndEmitGapTests(ITestOutputHelper output)
{
    // ── diagnostic helpers (mirror SemanticAnalyzerTests / ClassConfinementTests) ──

    private static IReadOnlyList<Diagnostic> Diagnose(string source) =>
        SpekCompiler.Parse(source).Diagnostics;

    private static void AssertParsesClean(string source)
    {
        var parsed = SpekCompiler.Parse(source);
        Assert.True(parsed.Success,
            "Expected source to parse + analyze cleanly; diagnostics:\n" +
            string.Join("\n", parsed.Diagnostics.Select(d => $"  {d.Code} {d.Line}:{d.Column} {d.Message}")));
    }

    // ── emit helpers (mirror GenericEmitTests / SwitchExpressionTests) ──

    private static string EmitCSharp(string source)
    {
        var parsed = SpekCompiler.Parse(source);
        Assert.True(parsed.Success,
            "Source must parse + analyze; diagnostics:\n" +
            string.Join("\n", parsed.Diagnostics.Select(d => $"  {d.Code} {d.Line}:{d.Column} {d.Message}")));
        return new FileEmitter().Emit(parsed.Tree!);
    }

    private void AssertCompiles(string source, string name)
    {
        var code = EmitCSharp(source);
        var (ok, summary, _) = RoslynCompileHelper.TryCompile(code, name);
        if (!ok)
        {
            output.WriteLine("─── emitted C# ───");
            output.WriteLine(code);
            output.WriteLine("─── diagnostics ───");
            output.WriteLine(summary);
        }
        Assert.True(ok, $"Emitted C# did not compile:\n{summary}");
    }

    // ─── Scenario 1 — CE0112 HAPPY PATH ──────────────────────────────────────
    // ClassConfinementTests pins only the *error* case (a mutable `class` as a
    // shared-region field). CE0112 fires solely when the field type resolves to
    // a declared `class` (SemanticAnalyzer.CheckRegionFieldsArentMutableClasses
    // → symbols.ResolveClass(...)). An immutable field type — a `message` or a
    // primitive — must therefore be ACCEPTED with no CE0112.

    [Fact]
    public void CE0112_PrimitiveRegionField_Accepted()
    {
        // A primitive field never resolves to a class symbol, so CE0112
        // cannot fire — sharing a primitive is exactly what regions are for.
        const string src = """
            shared Counters
            {
                int hits = 0;
                long total = 0;
            }
            """;
        Assert.DoesNotContain(Diagnose(src), d => d.Code == "CE0112");
        AssertParsesClean(src);
    }

    [Fact]
    public void CE0112_MessageTypedRegionField_Accepted()
    {
        // A `message` is immutable by construction (its own fields are CE0010-
        // checked). Holding one in a shared region is safe — no CE0112.
        const string src = """
            message Snapshot(int version, string label);
            shared Cache
            {
                Snapshot latest = new Snapshot(0, "init");
            }
            """;
        Assert.DoesNotContain(Diagnose(src), d => d.Code == "CE0112");
        AssertParsesClean(src);
    }

    // ─── Scenario 2 — CE0087 over an INDEX assignment from a reader handler ───
    // SemanticAnalyzerTests covers `field = X` and `field.member = X`. The rule
    // (TryGetActorFieldRoot) also claims to cover `field[i] = X` (IndexExpr
    // rooted at a field). These tests prove that branch is live for both an
    // actor-field array and a shared-region-local array.

    [Fact]
    public void CE0087_ReaderHandler_ActorFieldIndexAssign_Reported()
    {
        // `buf[0] = 5` mutates the actor's array field from a reader handler —
        // an index write is still a write to actor state.
        const string src = """
            message Tick();
            actor A
            {
                int[] buf = new int[] { 0, 0, 0 };
                behavior Idle
                {
                    reader on Tick => { buf[0] = 5; }
                }
            }
            """;
        // Parse must succeed (no CE0001) before we can assert the semantic code.
        Assert.DoesNotContain(Diagnose(src), d => d.Code == "CE0001");
        var d = Assert.Single(Diagnose(src), x => x.Code == "CE0087");
        Assert.Contains("buf", d.Message);
    }

    [Fact]
    public void CE0087_ReaderHandler_RegionLocalIndexAssign_Reported()
    {
        // The same rule applies to a shared-region array reached through a
        // `use` local: `cache[0] = 5` writes region state under the reader
        // lock, which would race other concurrent readers.
        const string src = """
            message Tick();
            shared Cache
            {
                int[] slots = new int[] { 0, 0 };
            }
            actor A
            {
                use Cache cache;
                reader on Tick => { cache.slots[0] = 5; }
            }
            """;
        Assert.DoesNotContain(Diagnose(src), d => d.Code == "CE0001");
        Assert.Contains(Diagnose(src), d => d.Code == "CE0087");
    }

    [Fact]
    public void CE0087_WriterHandler_ActorFieldIndexAssign_Clean()
    {
        // Contrast: the identical index write from a writer handler is fine —
        // writers hold the exclusive lock. (Confirms the reader-mode gate, not
        // a blanket ban on index writes.)
        const string src = """
            message Tick();
            actor A
            {
                int[] buf = new int[] { 0, 0, 0 };
                behavior Idle
                {
                    writer on Tick => { buf[0] = 5; }
                }
            }
            """;
        Assert.DoesNotContain(Diagnose(src), d => d.Code == "CE0087");
    }

    // ─── Scenario 3 — two type params, each with its OWN where clause ────────
    // GenericEmitTests does single-param constraints and `class, new()` on one
    // param. This pins the multi-clause form `where T : ... where U : ...` on
    // both a function and a class, and round-trips through Roslyn.

    [Fact]
    public void GenericFunction_TwoTypeParams_EachWithWhere_RoundTrips()
    {
        const string src = """
            module Algo
            {
                public U Pick<T, U>(T key, U value)
                    where T : System.IComparable<T>
                    where U : class
                {
                    return value;
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("where T : System.IComparable<T>", code);
        Assert.Contains("where U : class", code);
        AssertCompiles(src, "TwoParamConstrainedFunction");
    }

    [Fact]
    public void GenericClass_TwoTypeParams_EachWithWhere_RoundTrips()
    {
        const string src = """
            class Registry<TKey, TValue>
                where TKey : System.IComparable<TKey>
                where TValue : class, new()
            {
                public TValue Fresh() { return new TValue(); }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("where TKey : System.IComparable<TKey>", code);
        Assert.Contains("where TValue : class, new()", code);
        AssertCompiles(src, "TwoParamConstrainedClass");
    }

    // ─── Scenario 4 — relational pattern + `when` guard on one arm ───────────
    // SwitchExpressionTests covers relational patterns and `when` guards
    // separately. This combines them: `>= 80 when score < 90 => "B"` — a
    // banded grader that needs both the relational lower bound and the guard.

    [Fact]
    public void SwitchExpression_RelationalPatternWithWhenGuard_Emits()
    {
        const string src = """
            message Grade(int score);
            message Reply(string label);
            actor Grader
            {
                behavior Idle
                {
                    on Grade g =>
                    {
                        var grade = g.score switch {
                            >= 90                  => "A",
                            >= 80 when g.score < 90 => "B",
                            >= 70                  => "C",
                            _                      => "F"
                        };
                        return new Reply(grade);
                    }
                }
            }
            """;
        var code = EmitCSharp(src);
        // The combined arm emits the relational pattern, the C# `when` keyword,
        // and the (parenthesised) guard expression, all on one arm.
        Assert.Contains(">= 80 when (g.score < 90) =>", code);
    }

    [Fact]
    public void SwitchExpression_RelationalPatternWithWhenGuard_RoundTripsThroughRoslyn()
    {
        const string src = """
            message Grade(int score);
            message Reply(string label);
            actor Grader
            {
                behavior Idle
                {
                    on Grade g =>
                    {
                        var grade = g.score switch {
                            >= 90                  => "A",
                            >= 80 when g.score < 90 => "B",
                            >= 70                  => "C",
                            _                      => "F"
                        };
                        return new Reply(grade);
                    }
                }
            }
            """;
        AssertCompiles(src, "RelationalWhenSmoke");
    }
}

