using Spek.Compiler.Parser;
using Xunit;

namespace Spek.Tests.Semantic;

/// <summary>
/// Every Spek `enum` is sealed by default. A `switch` over an
/// enum value must cover every variant or include a `_` discard arm.
/// CE0103 fires when neither holds.
///
/// Headline rationale: cross-actor versioning during rolling deploys.
/// When half the
/// nodes have the old enum and half have the new one, exhaustiveness
/// catches the mismatch on the new code at compile time — no silent
/// "default arm hit" surprises in production.
/// </summary>
public sealed class SealedEnumTests
{
    [Fact]
    public void ExhaustiveSwitch_OverTypedLocal_NoReport()
    {
        const string src = """
            enum Status { Active, Inactive, Pending }
            message Tick();
            actor A
            {
                on Tick =>
                {
                    Status s = Status.Active;
                    var label = s switch {
                        Status.Active   => "a",
                        Status.Inactive => "i",
                        Status.Pending  => "p",
                    };
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.True(parsed.Success,
            "Diagnostics: " +
            string.Join("\n", parsed.Diagnostics.Select(d => $"{d.Code} {d.Line}:{d.Column} {d.Message}")));
    }

    [Fact]
    public void NonExhaustiveSwitch_OverTypedLocal_Reports()
    {
        const string src = """
            enum Status { Active, Inactive, Pending }
            message Tick();
            actor A
            {
                on Tick =>
                {
                    Status s = Status.Active;
                    var label = s switch {
                        Status.Active   => "a",
                        Status.Inactive => "i",
                        // missing: Pending
                    };
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.False(parsed.Success);
        var diag = Assert.Single(parsed.Diagnostics, d => d.Code == "CE0103");
        Assert.Contains("Pending", diag.Message);
    }

    [Fact]
    public void NonExhaustiveSwitch_WithDiscard_NoReport()
    {
        // The `_` discard arm is the explicit opt-out. Adding it
        // makes the switch trivially exhaustive — the developer has
        // signalled "I know this isn't covering every variant and
        // that's fine."
        const string src = """
            enum Status { Active, Inactive, Pending }
            message Tick();
            actor A
            {
                on Tick =>
                {
                    Status s = Status.Active;
                    var label = s switch {
                        Status.Active => "a",
                        _             => "other",
                    };
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.True(parsed.Success,
            "Diagnostics: " +
            string.Join("\n", parsed.Diagnostics.Select(d => $"{d.Code} {d.Line}:{d.Column} {d.Message}")));
    }

    [Fact]
    public void ExhaustiveSwitch_OverActorField_NoReport()
    {
        const string src = """
            enum Status { Active, Inactive }
            message Tick();
            actor A
            {
                Status current = Status.Active;

                on Tick =>
                {
                    var label = current switch {
                        Status.Active   => "a",
                        Status.Inactive => "i",
                    };
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.True(parsed.Success,
            "Diagnostics: " +
            string.Join("\n", parsed.Diagnostics.Select(d => $"{d.Code} {d.Line}:{d.Column} {d.Message}")));
    }

    [Fact]
    public void NonExhaustiveSwitch_OverActorField_Reports()
    {
        const string src = """
            enum Status { Active, Inactive, Pending }
            message Tick();
            actor A
            {
                Status current = Status.Active;

                on Tick =>
                {
                    var label = current switch {
                        Status.Active => "a",
                    };
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.False(parsed.Success);
        Assert.Contains(parsed.Diagnostics, d => d.Code == "CE0103");
    }

    [Fact]
    public void NonExhaustiveSwitch_MissingMultipleVariants_ListsAll()
    {
        const string src = """
            enum Direction { North, South, East, West }
            message Tick();
            actor A
            {
                on Tick =>
                {
                    Direction d = Direction.North;
                    var s = d switch {
                        Direction.North => "n",
                    };
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.False(parsed.Success);
        var diag = Assert.Single(parsed.Diagnostics, d => d.Code == "CE0103");
        Assert.Contains("South",  diag.Message);
        Assert.Contains("East",   diag.Message);
        Assert.Contains("West",   diag.Message);
    }

    [Fact]
    public void NonEnumSwitch_NoReport()
    {
        // Sanity check: switching on a non-enum value (an int here)
        // doesn't touch CE0103.
        const string src = """
            message Tick();
            actor A
            {
                int counter = 0;

                on Tick =>
                {
                    var s = counter switch {
                        0 => "zero",
                        _ => "nonzero",
                    };
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.True(parsed.Success,
            "Diagnostics: " +
            string.Join("\n", parsed.Diagnostics.Select(d => $"{d.Code} {d.Line}:{d.Column} {d.Message}")));
    }

    [Fact]
    public void SwitchWithWhenGuard_DoesNotCountAsCover()
    {
        // A `when` guard means the arm matches conditionally. The
        // compiler can't prove the guard is always true, so the arm
        // doesn't count as a definitive cover for the variant —
        // exhaustiveness still requires either a `_` arm or another
        // unguarded arm for the same variant.
        const string src = """
            enum Status { Active, Inactive }
            message Tick();
            actor A
            {
                on Tick =>
                {
                    Status s = Status.Active;
                    var label = s switch {
                        Status.Active when true => "a",
                        Status.Inactive         => "i",
                    };
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.False(parsed.Success);
        var diag = Assert.Single(parsed.Diagnostics, d => d.Code == "CE0103");
        Assert.Contains("Active", diag.Message);
    }

    [Fact]
    public void ExhaustiveSwitch_RoundTripsThroughRoslyn()
    {
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
                        Status.Active   => 1,
                        Status.Inactive => 2,
                        Status.Pending  => 3,
                    };
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.True(parsed.Success);
        var code = new Spek.Compiler.Emit.FileEmitter().Emit(parsed.Tree!);
        var (success, errors, _) = Spek.Tests.Emit.RoslynCompileHelper.TryCompile(code, "SealedEnumSmoke");
        Assert.True(success,
            "Emitted C# did not compile:\n" + errors + "\n--- emitted ---\n" + code);
    }
}
