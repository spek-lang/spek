using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Xunit;
using Xunit.Abstractions;

namespace Spek.Tests.Emit;

/// <summary>
/// Language-completeness — properties on a <c>class</c>: auto
/// (<c>{ get; set; }</c>), init-only (<c>{ get; init; }</c>), accessor
/// visibility (<c>{ get; private set; }</c>), expression-bodied
/// (<c>{ get =&gt; expr; }</c>), and auto + initializer (<c>= 0;</c>). The
/// accessor keywords get/set/init are contextual — they never collide with C#
/// member names. Round-trips prove the emitted C# compiles.
/// </summary>
public sealed class PropertyTests(ITestOutputHelper output)
{
    private static (bool ok, string code, string diags, IReadOnlyList<string> codes) Compile(string source)
    {
        var parsed = SpekCompiler.Parse(source);
        var codes = parsed.Diagnostics.Select(d => d.Code).ToList();
        if (!parsed.Success) return (false, "", string.Join("\n", parsed.Diagnostics.Select(d => $"{d.Code} {d.Message}")), codes);
        return (true, new FileEmitter().Emit(parsed.Tree!), "", codes);
    }

    private void AssertCompiles(string source, string name, params string[] mustContain)
    {
        var (ok, code, diags, _) = Compile(source);
        Assert.True(ok, $"Source must parse + analyze:\n{diags}");
        foreach (var s in mustContain) Assert.Contains(s, code);
        var (rok, summary, _) = RoslynCompileHelper.TryCompile(code, name);
        if (!rok)
        {
            output.WriteLine(code);
            output.WriteLine(summary);
        }
        Assert.True(rok, $"Emitted C# did not compile:\n{summary}");
    }

    [Fact]
    public void AutoProperty_And_AccessorVisibility_RoundTrip()
    {
        const string src = """
            class Account
            {
                public int    Balance { get; private set; }
                public string Owner   { get; set; }
            }
            """;
        AssertCompiles(src, "AutoProp", "public int Balance { get; private set; }", "public string Owner { get; set; }");
    }

    [Fact]
    public void InitOnlyProperty_RoundTrips()
    {
        const string src = """
            class Config { public string Name { get; init; } }
            """;
        AssertCompiles(src, "InitProp", "{ get; init; }");
    }

    [Fact]
    public void ExpressionBodiedProperty_RoundTrips()
    {
        const string src = """
            class Account
            {
                int balance = 0;
                public bool IsOverdrawn { get => balance < 0; }
            }
            """;
        AssertCompiles(src, "ExprProp", "get => (balance < 0);");
    }

    [Fact]
    public void AutoPropertyWithInitializer_RoundTrips()
    {
        const string src = """
            class Doc { public int Version { get; set; } = 1; }
            """;
        AssertCompiles(src, "PropInit", "{ get; set; } = 1;");
    }

    [Fact]
    public void PropertyNameClashingWithField_IsCE0013()
    {
        const string src = """
            class Bad
            {
                int X = 0;
                public int X { get; set; }
            }
            """;
        var (_, _, _, codes) = Compile(src);
        Assert.Contains("CE0013", codes);
    }
}
