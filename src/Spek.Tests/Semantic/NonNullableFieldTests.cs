using Spek.Compiler.Parser;
using Spek.Compiler.Semantic;
using Xunit;

namespace Spek.Tests.Semantic;

/// <summary>
/// CE0109. Non-nullable reference-typed fields without an
/// initializer leave the underlying C# field as `null` at runtime,
/// which surfaces as a `NullReferenceException` on first read.
/// CE0109 catches the case at the Spek layer rather than relying on
/// Roslyn's nullability warning at the C# layer.
///
/// The check is conservative: it skips owners with an `init` block
/// (presumed to assign fields there) and skips known value-like types.
/// </summary>
public sealed class NonNullableFieldTests
{
    [Fact]
    public void CE0109_ActorWithoutInit_NonNullableStringField_Warns()
    {
        const string src = """
            message Tick();
            actor A
            {
                string name;
                on Tick =>
                {
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.True(parsed.Success, "Warning shouldn't fail the build.");
        Assert.Contains(parsed.Diagnostics, d =>
            d.Code == "CE0109" && d.Severity == DiagnosticSeverity.Warning);
    }

    [Fact]
    public void CE0109_ActorWithInit_SuppressesWarning()
    {
        // The init block is presumed to assign the field. CE0109's
        // conservative check skips owners with any init block.
        const string src = """
            message Tick();
            actor A
            {
                string name;
                init(string n) { name = n; }
                on Tick =>
                {
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.DoesNotContain(parsed.Diagnostics, d => d.Code == "CE0109");
    }

    [Fact]
    public void CE0109_NullableField_NoWarning()
    {
        // `string?` is explicitly nullable — null is a valid value.
        const string src = """
            message Tick();
            actor A
            {
                string? name;
                on Tick =>
                {
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.DoesNotContain(parsed.Diagnostics, d => d.Code == "CE0109");
    }

    [Fact]
    public void CE0109_PrimitiveField_NoWarning()
    {
        // `int` has a natural default (0); no warning.
        const string src = """
            message Tick();
            actor A
            {
                int n;
                on Tick =>
                {
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.DoesNotContain(parsed.Diagnostics, d => d.Code == "CE0109");
    }

    [Fact]
    public void CE0109_FieldWithInitializer_NoWarning()
    {
        const string src = """
            message Tick();
            actor A
            {
                string name = "anon";
                on Tick =>
                {
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.DoesNotContain(parsed.Diagnostics, d => d.Code == "CE0109");
    }

    [Fact]
    public void CE0109_RegionWithoutInit_Warns()
    {
        const string src = """
            shared Cache
            {
                string lastKey;
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.True(parsed.Success);
        Assert.Contains(parsed.Diagnostics, d =>
            d.Code == "CE0109" && d.Severity == DiagnosticSeverity.Warning);
    }

    [Fact]
    public void CE0109_RegionWithInit_SuppressesWarning()
    {
        const string src = """
            shared Cache
            {
                string lastKey;
                init { lastKey = ""; }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.DoesNotContain(parsed.Diagnostics, d => d.Code == "CE0109");
    }

    [Fact]
    public void CE0109_ActorRefField_WithoutInit_Warns()
    {
        // ActorRef is a reference type — holding one without
        // initialising it likely deads-letters or NPEs on first use.
        const string src = """
            message Tick();
            actor A
            {
                ActorRef peer;
                on Tick =>
                {
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.Contains(parsed.Diagnostics, d => d.Code == "CE0109");
    }
}
