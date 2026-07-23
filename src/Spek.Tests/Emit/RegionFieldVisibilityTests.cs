using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Xunit;

namespace Spek.Tests.Emit;

/// <summary>
/// CE0108. Field-level visibility modifiers on shared regions
/// now flow through to the emitted C# class. The default (no
/// modifier) is `public` — preserves the existing
/// "attaching actors can read/write" model. Explicit `private`
/// makes the field internal to the region (set in init / used by
/// other fields, not reachable from attaching actors).
/// </summary>
public sealed class RegionFieldVisibilityTests
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
    public void Region_FieldWithoutModifier_EmitsPublic()
    {
        // Default: no modifier → public (preserves the
        // "attaching actors can read" contract).
        const string src = """
            shared Cache
            {
                int hits = 0;
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("public int hits = 0;", code);
    }

    [Fact]
    public void Region_PrivateField_EmitsPrivate()
    {
        const string src = """
            shared Cache
            {
                private string _config = "";
                public string lastValue = "";
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("private string _config = \"\";", code);
        Assert.Contains("public string lastValue = \"\";", code);
    }

    [Fact]
    public void Region_InternalField_EmitsInternal()
    {
        const string src = """
            shared Cache
            {
                internal int internalCounter = 0;
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("internal int internalCounter = 0;", code);
    }

    [Fact]
    public void Region_PrivateField_RoundTripsThroughRoslyn()
    {
        // The region uses a private field internally; init reads/writes
        // it. Roslyn accepts the shape.
        const string src = """
            shared Cache
            {
                private string _config = "";
                public string lastValue = "";

                init
                {
                    _config = "initial";
                    lastValue = _config;
                }
            }
            """;
        var code = EmitCSharp(src);
        var (success, errors, _) = RoslynCompileHelper.TryCompile(code, "RegionPrivateFieldSmoke");
        Assert.True(success,
            "Emitted C# did not compile:\n" + errors + "\n--- emitted ---\n" + code);
    }
}
