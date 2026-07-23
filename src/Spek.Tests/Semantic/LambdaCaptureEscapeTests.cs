using Spek.Compiler.Parser;
using Spek.Compiler.Semantic;
using Xunit;

namespace Spek.Tests.Semantic;

/// <summary>
/// CE0135. A lambda that writes actor state may not escape the handler that
/// wrote it: whoever holds it afterwards can invoke it on a thread the actor
/// does not own, writing actor state outside the mailbox's serialization.
/// Read-only capture is the common, safe case and stays unrestricted.
/// (CE0010 already closes the message-shaped routes; this rule closes the
/// fourth — registration with something that outlives the turn.)
/// </summary>
public sealed class LambdaCaptureEscapeTests
{
    private static bool HasError(CompilationResult r, string code) =>
        r.Diagnostics.Any(d => d.Code == code && d.Severity == DiagnosticSeverity.Error);

    /// <summary>A plain class that stores a callback — the "outlives the
    /// handler" shape the rule exists for.</summary>
    private const string Registry = """
        class Registry
        {
            public void Register(System.Action cb) { }
        }
        """;

    [Fact]
    public void CE0135_IssueExample_RegisterTaintedLocal_Errors()
    {
        // The G8 issue's headline example, verbatim: binding is fine, direct
        // invocation is fine, registration is the escape.
        const string src = $$"""
            {{Registry}}

            message Up();
            actor Auditor
            {
                int seen = 0;
                Registry registry = new Registry();

                on Up u =>
                {
                    var bump = () => seen = seen + 1;
                    bump();
                    registry.Register(bump);
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.False(parsed.Success, "CE0135 is an error — it must fail the build.");
        Assert.True(HasError(parsed, "CE0135"));
    }

    [Fact]
    public void CE0135_InlineStateMutatingLambdaArgument_Errors()
    {
        // The same escape without the intermediate local.
        const string src = $$"""
            {{Registry}}

            message Up();
            actor Auditor
            {
                int seen = 0;
                Registry registry = new Registry();

                on Up u =>
                {
                    registry.Register(() => seen = seen + 1);
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.True(HasError(parsed, "CE0135"));
    }

    [Fact]
    public void CE0135_DirectInvocation_NotFlagged()
    {
        // The allowed line: declaring and calling a state-capturing lambda runs
        // it here, on the thread that owns the actor, inside the turn.
        const string src = """
            message Up();
            actor Auditor
            {
                int seen = 0;

                on Up u =>
                {
                    var bump = () => seen = seen + 1;
                    bump();
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.DoesNotContain(parsed.Diagnostics, d => d.Code == "CE0135");
    }

    [Fact]
    public void CE0135_ReadOnlyCapture_NotFlagged()
    {
        // The overwhelmingly common case: a LINQ predicate closing over actor
        // state to READ it. Nothing escapes that could race.
        const string src = """
            using System.Linq;
            using System.Collections.Generic;

            message Sweep();
            actor Filter
            {
                int filterId = 3;
                List<int> items = new List<int>();

                on Sweep =>
                {
                    var hits = items.Where(x => x == filterId).ToList();
                    var n = items.Count(x => x > filterId);
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.DoesNotContain(parsed.Diagnostics, d => d.Code == "CE0135");
    }

    [Fact]
    public void CE0135_StoredInActorField_Errors()
    {
        // Route two: an actor field outlives every turn, so parking the lambda
        // there is the same hazard as handing it to a registry.
        const string src = """
            message Up();
            actor Auditor
            {
                int seen = 0;
                System.Action pending = null;

                on Up u =>
                {
                    var bump = () => seen = seen + 1;
                    pending = bump;
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.True(HasError(parsed, "CE0135"));
    }

    [Fact]
    public void CE0135_ReturnedFromActorMethod_Errors()
    {
        // Route three: returning hands the lambda to a caller outside the turn.
        const string src = """
            using System;

            message Up();
            actor Auditor
            {
                int seen = 0;

                private Action Bumper()
                {
                    return () => seen = seen + 1;
                }

                on Up u => { }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.True(HasError(parsed, "CE0135"));
    }

    [Fact]
    public void CE0135_MutatingSiblingMethodCall_Errors()
    {
        // A lambda need not assign directly: calling one of the actor's own
        // methods that writes state is the same escape. The mutating-method set
        // is the shared StateMutation classifier, reused from CE0087.
        const string src = $$"""
            {{Registry}}

            message Up();
            actor Auditor
            {
                int seen = 0;
                Registry registry = new Registry();

                private void Bump() { seen = seen + 1; }

                on Up u =>
                {
                    registry.Register(() => Bump());
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.True(HasError(parsed, "CE0135"));
    }

    [Fact]
    public void CE0135_ReadOnlyLambdaPassedToRegistry_NotFlagged()
    {
        // Escape alone is not the violation — escape of a *state-writing*
        // lambda is. A callback that only reads may travel freely.
        const string src = $$"""
            {{Registry}}

            message Up();
            actor Auditor
            {
                int seen = 0;
                Registry registry = new Registry();

                on Up u =>
                {
                    registry.Register(() => System.Console.WriteLine(seen));
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.DoesNotContain(parsed.Diagnostics, d => d.Code == "CE0135");
    }

    [Fact]
    public void CE0135_ListForEach_Errors_KnownOverApproximation()
    {
        // PINNED BEHAVIOUR — a deliberate, accepted false positive.
        //
        // `List.ForEach` invokes its callback immediately and synchronously, so
        // this particular write is safe in fact. The analysis cannot know that:
        // a foreign callee taking a delegate is indistinguishable from one that
        // stores it. Rather than special-case a BCL allowlist (which would go
        // stale and would still miss every third-party equivalent), the rule
        // assumes the worst and the diagnostic points at `foreach`, which Spek
        // has and which is the better code here anyway.
        const string src = """
            using System.Collections.Generic;

            message Sum();
            actor Totals
            {
                int total = 0;
                List<int> items = new List<int>();

                on Sum =>
                {
                    items.ForEach(x => total = total + x);
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.True(HasError(parsed, "CE0135"));

        // The message must steer to the idiom that fixes it.
        var d = parsed.Diagnostics.First(x => x.Code == "CE0135");
        Assert.Contains("foreach", d.Message);
    }

    [Fact]
    public void CE0135_ForeachLoop_IsTheCleanRewrite()
    {
        // The fix the ForEach diagnostic names must itself compile clean —
        // otherwise the rule would have no way out.
        const string src = """
            using System.Collections.Generic;

            message Sum();
            actor Totals
            {
                int total = 0;
                List<int> items = new List<int>();

                on Sum =>
                {
                    foreach (var x in items) { total = total + x; }
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.DoesNotContain(parsed.Diagnostics, d => d.Code == "CE0135");
    }

    [Fact]
    public void CE0135_CapturedLocalOnly_NotFlagged()
    {
        // Capturing locals, parameters, and messages is unrestricted — none of
        // it is actor state, so none of it can race.
        const string src = $$"""
            {{Registry}}

            message Up(int Delta);
            actor Auditor
            {
                Registry registry = new Registry();

                on Up u =>
                {
                    var tally = 0;
                    registry.Register(() => tally = tally + u.Delta);
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.DoesNotContain(parsed.Diagnostics, d => d.Code == "CE0135");
    }

    [Fact]
    public void CE0135_FiresInInit_NotJustHandlers()
    {
        // Wiring a callback up at construction is the most natural place to
        // write this bug; the rule covers every actor body, as CE0119 does.
        const string src = $$"""
            {{Registry}}

            message Up();
            actor Auditor
            {
                int seen = 0;
                Registry registry = new Registry();

                init()
                {
                    registry.Register(() => seen = seen + 1);
                }

                on Up u => { }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.True(HasError(parsed, "CE0135"));
    }

    [Fact]
    public void CE0135_ModuleLambda_NotFlagged()
    {
        // The rule is about ACTOR state. A module has none, so its lambdas —
        // including ones returned as values — are ordinary C#.
        const string src = """
            using System;

            module Adjustments
            {
                public Func<int, int> For(int mode)
                {
                    return mode switch {
                        0 => x => x + 1,
                        _ => x => x - 1
                    };
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.DoesNotContain(parsed.Diagnostics, d => d.Code == "CE0135");
    }

    [Fact]
    public void CE0135_MutatingClassMethodOnActorField_Errors()
    {
        // Gap (a), closed: the lambda writes no field itself — it calls a
        // mutating method on a confined-class actor field. Same receiver
        // shapes and the same ClassSymbols.MutatingMethods set CE0087 uses
        // for reader handlers.
        const string src = $$"""
            {{Registry}}

            class Counter
            {
                int n = 0;
                public void Bump() { n = n + 1; }
            }

            message Up();
            actor Auditor
            {
                Counter helper = new Counter();
                Registry registry = new Registry();

                on Up u =>
                {
                    registry.Register(() => helper.Bump());
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.True(HasError(parsed, "CE0135"));

        var d = parsed.Diagnostics.First(x => x.Code == "CE0135");
        Assert.Contains("helper.Bump", d.Message);
    }

    [Fact]
    public void CE0135_SelfQualifiedClassFieldMutation_Errors()
    {
        // The `self.helper.Bump()` spelling of the same escape.
        const string src = $$"""
            {{Registry}}

            class Counter
            {
                int n = 0;
                public void Bump() { n = n + 1; }
            }

            message Up();
            actor Auditor
            {
                Counter helper = new Counter();
                Registry registry = new Registry();

                on Up u =>
                {
                    registry.Register(() => self.helper.Bump());
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.True(HasError(parsed, "CE0135"));
    }

    [Fact]
    public void CE0135_RegionHandleWrite_Errors()
    {
        // Gap (b), closed: a lambda that writes through a `use` handle. A
        // foreign thread invoking this write holds neither the region's
        // reader nor its writer lock — the reader/writer discipline is
        // bypassed entirely, which is worse than an actor-field race.
        const string src = $$"""
            {{Registry}}

            shared Tally
            {
                int hits = 0;
            }

            message Up();
            actor Auditor
            {
                use Tally tally;
                Registry registry = new Registry();

                on Up u =>
                {
                    registry.Register(() => tally.hits = tally.hits + 1);
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.True(HasError(parsed, "CE0135"));

        var d = parsed.Diagnostics.First(x => x.Code == "CE0135");
        Assert.Contains("shared-region", d.Message);
    }

    [Fact]
    public void CE0135_ForeignTypedFieldMethodCall_NotFlagged()
    {
        // The documented residue: a method call on a FOREIGN-typed field.
        // Whether StringBuilder.Append mutates is unknowable without foreign
        // type resolution, so CE0135 stays silent — that remainder belongs to
        // the runtime turn guard (in design).
        const string src = $$"""
            {{Registry}}

            message Up();
            actor Auditor
            {
                System.Text.StringBuilder log = new System.Text.StringBuilder();
                Registry registry = new Registry();

                on Up u =>
                {
                    registry.Register(() => log.Append("up"));
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.DoesNotContain(parsed.Diagnostics, d => d.Code == "CE0135");
    }

    [Fact]
    public void CE0135_RegionHandleTaintedLocal_Errors()
    {
        // The region-handle write flows through the same intra-body taint
        // tracking as an actor-field write.
        const string src = $$"""
            {{Registry}}

            shared Tally
            {
                int hits = 0;
            }

            message Up();
            actor Auditor
            {
                use Tally tally;
                Registry registry = new Registry();

                on Up u =>
                {
                    var bump = () => tally.hits = tally.hits + 1;
                    registry.Register(bump);
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.True(HasError(parsed, "CE0135"));
    }

    [Fact]
    public void CE0135_ReportsOnceForNestedEscape()
    {
        // A mutating lambda nested inside an escaping one escapes *with* it;
        // the outer boundary is the actionable report, so there is exactly one.
        const string src = $$"""
            using System.Collections.Generic;
            {{Registry}}

            message Up();
            actor Auditor
            {
                int seen = 0;
                Registry registry = new Registry();
                List<int> items = new List<int>();

                on Up u =>
                {
                    registry.Register(() => { items.ForEach(x => seen = seen + x); });
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.Single(parsed.Diagnostics.Where(d => d.Code == "CE0135"));
    }
}
