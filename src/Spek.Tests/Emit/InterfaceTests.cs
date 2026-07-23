using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Xunit;
using Xunit.Abstractions;

namespace Spek.Tests.Emit;

/// <summary>
/// The class-side <c>interface</c> contract: the method-based sibling of
/// <c>channel</c>. Covers declaration, class implementation (base list),
/// interface inheritance, and the no-behavior rule (CE0120) plus the
/// handlers-dispatch-on-messages-never-contracts rule (CE0121). Roslyn
/// smoke-compiles the emitted C# so the passthrough type-check (a class must
/// actually satisfy its interface, surfaced as CS0535) is exercised too.
///
/// The rule throughout: a contract declares shape, never behavior.
/// </summary>
public sealed class InterfaceTests(ITestOutputHelper output)
{
    private static string EmitCSharp(string source)
    {
        var parsed = SpekCompiler.Parse(source);
        Assert.True(parsed.Success,
            string.Join("\n", parsed.Diagnostics.Select(d => $"{d.Code} {d.Message}")));
        return new FileEmitter().Emit(parsed.Tree!);
    }

    private void AssertCompiles(string code, string label)
    {
        var (ok, summary, _) = RoslynCompileHelper.TryCompile(code, label);
        if (!ok) { output.WriteLine(code); output.WriteLine(summary); }
        Assert.True(ok, $"Emitted C# did not compile:\n{summary}");
    }

    // ─── declaration + emit ─────────────────────────────────────────────

    [Fact]
    public void InterfaceDecl_EmitsCSharpInterface_SignaturesOnly()
    {
        const string src = """
            namespace X;

            interface Validator
            {
                bool IsValid(string input);
                string Describe();
                int MinLength { get; }
            }
            """;
        var code = EmitCSharp(src);

        Assert.Contains("interface Validator", code);
        Assert.Contains("bool IsValid(string input);", code);
        Assert.Contains("string Describe();", code);
        Assert.Contains("int MinLength { get; }", code);
        // No default-method bodies: a method signature must not open a brace.
        Assert.DoesNotContain("bool IsValid(string input)\n", code);
        AssertCompiles(code, "InterfaceDeclSmoke");
    }

    [Fact]
    public void ClassImplementsInterface_EmitsBaseList_AndCompiles()
    {
        const string src = """
            namespace X;

            interface Validator
            {
                bool IsValid(string input);
                int MinLength { get; }
            }

            class EmailValidator : Validator
            {
                public bool IsValid(string input) { return input.Contains("@"); }
                public int MinLength { get => 3; }
            }
            """;
        var code = EmitCSharp(src);

        Assert.Contains("class EmailValidator : Validator", code);
        AssertCompiles(code, "ClassImplementsInterface");
    }

    [Fact]
    public void ClassImplementsMultipleInterfaces_EmitsAllInBaseList()
    {
        const string src = """
            namespace X;

            interface Named  { string Name { get; } }
            interface Sized  { int Size { get; } }

            class Widget : Named, Sized
            {
                public string Name { get => "w"; }
                public int Size    { get => 1; }
            }
            """;
        var code = EmitCSharp(src);

        Assert.Contains("class Widget : Named, Sized", code);
        AssertCompiles(code, "ClassImplementsMultiple");
    }

    [Fact]
    public void InterfaceExtendsInterface_EmitsBaseList_AndCompiles()
    {
        const string src = """
            namespace X;

            interface Named { string Name { get; } }

            interface Describable : Named
            {
                string Describe();
            }

            class Thing : Describable
            {
                public string Name     { get => "t"; }
                public string Describe() { return "thing"; }
            }
            """;
        var code = EmitCSharp(src);

        Assert.Contains("interface Describable : Named", code);
        AssertCompiles(code, "InterfaceExtends");
    }

    [Fact]
    public void GenericInterface_LowersTypeParamsVerbatim()
    {
        const string src = """
            namespace X;

            interface Box<T>
            {
                T Get();
            }
            """;
        var code = EmitCSharp(src);

        Assert.Contains("interface Box<T>", code);
        Assert.Contains("T Get();", code);
        AssertCompiles(code, "GenericInterface");
    }

    // ─── CE0120: no behavior, no state ──────────────────────────────────

    [Fact]
    public void InterfaceWithMethodBody_ReportsCE0120()
    {
        const string src = """
            namespace X;
            interface I { bool Ok() { return true; } }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.False(parsed.Success);
        var diag = Assert.Single(parsed.Diagnostics, d => d.Code == "CE0120");
        Assert.Contains("Ok", diag.Message);
        Assert.Contains("body", diag.Message, StringComparison.OrdinalIgnoreCase);
    }

    [Fact]
    public void InterfaceWithField_ReportsCE0120()
    {
        const string src = """
            namespace X;
            interface I { int count; bool Ok(); }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.False(parsed.Success);
        var diag = Assert.Single(parsed.Diagnostics, d => d.Code == "CE0120");
        Assert.Contains("count", diag.Message);
    }

    [Fact]
    public void InterfacePropertyAccessorBody_ReportsCE0120()
    {
        const string src = """
            namespace X;
            interface I { int N { get => 3; } }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.False(parsed.Success);
        Assert.Contains(parsed.Diagnostics, d => d.Code == "CE0120");
    }

    // ─── CE0121: handlers dispatch on messages, never on contracts ──────

    [Fact]
    public void OnInterfaceHandler_ReportsCE0121()
    {
        const string src = """
            namespace X;
            interface Greet { string Hello(); }
            actor A
            {
                on Greet => { }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.False(parsed.Success);
        var diag = Assert.Single(parsed.Diagnostics, d => d.Code == "CE0121");
        Assert.Contains("interface", diag.Message);
    }

    [Fact]
    public void PrivateOnChannelHandler_ReportsCE0121()
    {
        // Even a private handler — which otherwise escapes the declared-message
        // rule and may bind BCL types — cannot dispatch on a channel.
        const string src = """
            namespace X;
            message M();
            channel Ch { on M; }
            actor A
            {
                private on Ch => { }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.False(parsed.Success);
        var diag = Assert.Single(parsed.Diagnostics, d => d.Code == "CE0121");
        Assert.Contains("channel", diag.Message);
    }

    // ─── CE0093: unknown base interface ─────────────────────────────────

    [Fact]
    public void InterfaceExtendsUnknown_ReportsCE0093()
    {
        const string src = """
            namespace X;
            interface I : Nonexistent { bool Ok(); }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.False(parsed.Success);
        var diag = Assert.Single(parsed.Diagnostics, d => d.Code == "CE0093");
        Assert.Contains("Nonexistent", diag.Message);
    }

    // ─── passthrough type-check: Roslyn enforces conformance ────────────

    [Fact]
    public void MissingInterfaceMember_SurfacesAsCS0535_ViaRoslyn()
    {
        // Spek does no interface-conformance checking (passthrough style): a
        // class that omits an interface member emits fine but fails the Roslyn
        // smoke-compile with CS0535, exactly as C# would report it.
        const string src = """
            namespace X;
            interface I { bool Ok(); }
            class C : I { }
            """;
        var code = EmitCSharp(src);
        var (ok, summary, _) = RoslynCompileHelper.TryCompile(code, "MissingMember");
        Assert.False(ok, "a class omitting an interface member must not compile");
        Assert.Contains("CS0535", summary);
    }
}
