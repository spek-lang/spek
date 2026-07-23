using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Xunit;

namespace Spek.Tests.Emit;

/// <summary>
/// Coverage for the switch expression. Shipped patterns: constant,
/// type with optional binding, discard, relational, property, and the
/// logical combinators (`not`, `and`, `or`, parenthesisation), plus
/// optional `when` guards. Tuple/list patterns are still deferred to
/// a follow-up that needs the underlying language features first.
/// </summary>
public sealed class SwitchExpressionTests
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
    public void ConstantArms_EmitCSharpSwitch()
    {
        const string src = """
            message Tick();
            actor A
            {
                int code = 0;
                behavior Idle
                {
                    on Tick => {
                        var name = code switch {
                            1 => "one",
                            2 => "two",
                            _ => "other"
                        };
                    }
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("switch", code);
        Assert.Contains("1 => \"one\"", code);
        Assert.Contains("2 => \"two\"", code);
        Assert.Contains("_ => \"other\"", code);
    }

    [Fact]
    public void TypePatternWithBinding_EmitsBoundVariable()
    {
        const string src = """
            message Foo(int x);
            message Bar();
            message Tick();
            actor A
            {
                behavior Idle
                {
                    on Tick => {
                        object obj = new Foo(1);
                        var label = obj switch {
                            Foo f => "foo",
                            Bar => "bar",
                            _ => "other"
                        };
                    }
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("Foo f =>", code);
        Assert.Contains("Bar =>", code);
        Assert.Contains("_ =>", code);
    }

    [Fact]
    public void WhenGuard_EmitsCSharpWhenClause()
    {
        const string src = """
            message Tick();
            actor A
            {
                int n = 0;
                behavior Idle
                {
                    on Tick => {
                        var label = n switch {
                            0 when n == 0 => "zero",
                            _ => "other"
                        };
                    }
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("when", code);
        Assert.Contains("\"zero\"", code);
    }

    [Fact]
    public void DiscardPattern_EmitsUnderscore()
    {
        const string src = """
            message Tick();
            actor A
            {
                int n = 0;
                behavior Idle
                {
                    on Tick => {
                        var label = n switch {
                            _ => "anything"
                        };
                    }
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("_ => \"anything\"", code);
    }

    [Fact]
    public void RelationalPattern_GreaterThan_EmitsRelationalSyntax()
    {
        const string src = """
            message Tick();
            actor A
            {
                int n = 0;
                behavior Idle
                {
                    on Tick => {
                        var label = n switch {
                            > 100 => "big",
                            > 10  => "medium",
                            _     => "small"
                        };
                    }
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("> 100 => \"big\"", code);
        Assert.Contains("> 10 => \"medium\"", code);
    }

    [Fact]
    public void RelationalPattern_AllOperators_Emit()
    {
        const string src = """
            message Tick();
            actor A
            {
                int n = 0;
                behavior Idle
                {
                    on Tick => {
                        var label = n switch {
                            < 0    => "negative",
                            <= 5   => "small",
                            == 10  => "ten",
                            != 20  => "not twenty",
                            >= 100 => "huge",
                            > 50   => "big",
                            _      => "other"
                        };
                    }
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("< 0 =>", code);
        Assert.Contains("<= 5 =>", code);
        Assert.Contains("== 10 =>", code);
        Assert.Contains("!= 20 =>", code);
        Assert.Contains(">= 100 =>", code);
        Assert.Contains("> 50 =>", code);
    }

    [Fact]
    public void RelationalPattern_RoundTripsThroughRoslyn()
    {
        const string src = """
            message Tick();
            message Reply(string label);
            actor Grader
            {
                behavior Idle
                {
                    on Tick => {
                        int score = 75;
                        var grade = score switch {
                            >= 90 => "A",
                            >= 80 => "B",
                            >= 70 => "C",
                            _     => "F"
                        };
                        return new Reply(grade);
                    }
                }
            }
            """;
        var code = EmitCSharp(src);
        var (success, errors, _) = RoslynCompileHelper.TryCompile(code, "RelationalSmoke");
        Assert.True(success,
            "Emitted C# did not compile:\n" + errors + "\n--- emitted ---\n" + code);
    }

    [Fact]
    public void PropertyPattern_SingleProperty_Emits()
    {
        const string src = """
            message Order(int total, string tier);
            actor Triage
            {
                behavior Idle
                {
                    on Order o => {
                        var label = o switch {
                            { total: 0 } => "empty",
                            _            => "other"
                        };
                    }
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("{ total: 0 } =>", code);
    }

    [Fact]
    public void PropertyPattern_MultipleProperties_WithRelational_Emit()
    {
        const string src = """
            message Order(int total, string tier);
            actor Triage
            {
                behavior Idle
                {
                    on Order o => {
                        var label = o switch {
                            { total: > 100, tier: "Gold" } => "vip",
                            { total: > 50 }                => "discount",
                            _                              => "standard"
                        };
                    }
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("{ total: > 100, tier: \"Gold\" } =>", code);
        Assert.Contains("{ total: > 50 } =>", code);
    }

    [Fact]
    public void PropertyPattern_ExtendedDottedPath_Emits()
    {
        const string src = """
            message Order(int total, string tier);
            message Wrapper(Order inner);
            actor Triage
            {
                behavior Idle
                {
                    on Wrapper w => {
                        var label = w switch {
                            { inner.tier: "Gold" } => "vip",
                            _                      => "other"
                        };
                    }
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("{ inner.tier: \"Gold\" } =>", code);
    }

    [Fact]
    public void PropertyPattern_Empty_EmitsBraces()
    {
        const string src = """
            message Tick();
            actor A
            {
                object o = new object();
                behavior Idle
                {
                    on Tick => {
                        var label = o switch {
                            { } => "non-null",
                            _   => "null"
                        };
                    }
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("{ } =>", code);
    }

    [Fact]
    public void PropertyPattern_RoundTripsThroughRoslyn()
    {
        const string src = """
            message Order(int total, string tier);
            message Reply(string label);
            actor Triage
            {
                behavior Idle
                {
                    on Order o => {
                        var label = o switch {
                            { total: > 100, tier: "Gold" } => "vip",
                            { total: > 50 }                => "discount",
                            _                              => "standard"
                        };
                        return new Reply(label);
                    }
                }
            }
            """;
        var code = EmitCSharp(src);
        var (success, errors, _) = RoslynCompileHelper.TryCompile(code, "PropertySmoke");
        Assert.True(success,
            "Emitted C# did not compile:\n" + errors + "\n--- emitted ---\n" + code);
    }

    [Fact]
    public void NotPattern_NegatesAtom_Emits()
    {
        const string src = """
            message Tick();
            actor A
            {
                int n = 0;
                behavior Idle
                {
                    on Tick => {
                        var label = n switch {
                            not 0 => "non-zero",
                            _     => "zero"
                        };
                    }
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("not 0 =>", code);
    }

    [Fact]
    public void OrPattern_AlternativeConstants_Emit()
    {
        const string src = """
            message Tick();
            actor A
            {
                int n = 0;
                behavior Idle
                {
                    on Tick => {
                        var label = n switch {
                            1 or 2 or 3 => "small",
                            _           => "other"
                        };
                    }
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("1 or 2 or 3 =>", code);
    }

    [Fact]
    public void AndPattern_TwoRelationals_Emit()
    {
        const string src = """
            message Tick();
            actor A
            {
                int n = 0;
                behavior Idle
                {
                    on Tick => {
                        var label = n switch {
                            >= 1 and <= 9 => "single digit",
                            _             => "other"
                        };
                    }
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains(">= 1 and <= 9 =>", code);
    }

    [Fact]
    public void NotPattern_OverOr_AddsParens()
    {
        const string src = """
            message Tick();
            actor A
            {
                string s = "";
                behavior Idle
                {
                    on Tick => {
                        var label = s switch {
                            not ("low" or "medium") => "high-or-other",
                            _                       => "low-or-medium"
                        };
                    }
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("not (\"low\" or \"medium\") =>", code);
    }

    [Fact]
    public void AndPattern_OverOr_AddsParens()
    {
        const string src = """
            message Tick();
            actor A
            {
                int n = 0;
                behavior Idle
                {
                    on Tick => {
                        var label = n switch {
                            (1 or 2) and not 3 => "small-not-three",
                            _                  => "other"
                        };
                    }
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("(1 or 2) and not 3 =>", code);
    }

    [Fact]
    public void LogicalPatterns_AllForms_RoundTripThroughRoslyn()
    {
        const string src = """
            message Tick();
            message Reply(string label);
            actor Categoriser
            {
                behavior Idle
                {
                    on Tick => {
                        int code = 5;
                        var label = code switch {
                            < 0           => "negative",
                            0 or 1 or 2   => "small",
                            >= 3 and <= 9 => "single-digit",
                            not 100       => "other",
                            _             => "hundred"
                        };
                        return new Reply(label);
                    }
                }
            }
            """;
        var code = EmitCSharp(src);
        var (success, errors, _) = RoslynCompileHelper.TryCompile(code, "LogicalSmoke");
        Assert.True(success,
            "Emitted C# did not compile:\n" + errors + "\n--- emitted ---\n" + code);
    }

    [Fact]
    public void PropertyPattern_WithLogicalCombinators_RoundTrips()
    {
        const string src = """
            message Order(int total, string tier);
            message Reply(string label);
            actor Triage
            {
                behavior Idle
                {
                    on Order o => {
                        var label = o switch {
                            { total: > 100, tier: "Gold" or "Platinum" } => "vip",
                            { tier: not "Bronze" }                       => "premium",
                            _                                            => "standard"
                        };
                        return new Reply(label);
                    }
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("\"Gold\" or \"Platinum\"", code);
        Assert.Contains("not \"Bronze\"", code);
        var (success, errors, _) = RoslynCompileHelper.TryCompile(code, "PropertyLogicalSmoke");
        Assert.True(success,
            "Emitted C# did not compile:\n" + errors + "\n--- emitted ---\n" + code);
    }

    [Fact]
    public void SwitchExpression_AsAssignmentRhs_Compiles()
    {
        // End-to-end smoke test that the emitted code is valid C#.
        const string src = """
            message Tick();
            message Reply(string label);
            actor Labeler
            {
                behavior Idle
                {
                    on Tick => {
                        int code = 1;
                        var name = code switch {
                            1 => "one",
                            2 => "two",
                            _ => "other"
                        };
                        sender.Tell(new Reply(name));
                    }
                }
            }
            """;
        var code = EmitCSharp(src);
        var (success, errors, _) = RoslynCompileHelper.TryCompile(code, "SwitchSmoke");
        Assert.True(success,
            "Emitted C# did not compile:\n" + errors + "\n--- emitted ---\n" + code);
    }
}
