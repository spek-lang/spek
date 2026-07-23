using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Xunit;
using Xunit.Abstractions;

namespace Spek.Tests.Emit;

/// <summary>
/// Modules emit as C# <c>static class</c>; their methods
/// become <c>static</c> methods. Visibility flows through verbatim,
/// nested modules emit as nested static classes, and the emitted C#
/// must Roslyn-compile.
/// </summary>
public sealed class ModuleEmitTests(ITestOutputHelper output)
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
    public void EmptyModule_EmitsPublicStaticClass()
    {
        var code = EmitCSharp("module Empty { }");
        Assert.Contains("public static class Empty", code);
    }

    [Fact]
    public void InternalModule_EmitsInternalStaticClass()
    {
        var code = EmitCSharp("internal module Hidden { }");
        Assert.Contains("internal static class Hidden", code);
    }

    [Fact]
    public void Module_WithMethod_EmitsStaticMethod()
    {
        const string src = """
            module Validators
            {
                public bool IsValidEmail(string s)
                {
                    return s.Contains("@");
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("public static class Validators",         code);
        Assert.Contains("public static bool IsValidEmail(string s)", code);
    }

    [Fact]
    public void Module_VoidFunction_EmitsVoidReturn()
    {
        const string src = """
            module Sinks
            {
                public void LogIt(string s) { return; }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("public static void LogIt(string s)", code);
    }

    [Fact]
    public void Module_AllVisibilityLevels_EmitVerbatim()
    {
        const string src = """
            module Format
            {
                public   string Trim1(string s)  { return s; }
                internal string Trim2(string s)  { return s; }
                private  string Trim3(string s)  { return s; }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("public static string Trim1(string s)",   code);
        Assert.Contains("internal static string Trim2(string s)", code);
        Assert.Contains("private static string Trim3(string s)",  code);
    }

    [Fact]
    public void NestedModule_EmitsAsNestedStaticClass()
    {
        const string src = """
            module Outer
            {
                public int A() { return 1; }

                module Inner
                {
                    public int B() { return 2; }
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("public static class Outer",        code);
        Assert.Contains("public static int A()",            code);
        Assert.Contains("public static class Inner",        code);
        Assert.Contains("public static int B()",            code);
    }

    [Fact]
    public void Module_EmittedCSharpCompiles()
    {
        const string src = """
            module Mathy
            {
                public int Double(int n)   { return n + n; }
                public int Square(int n)   { return n * n; }
                public int Cube(int n)     { return n * n * n; }
            }
            """;
        var code = EmitCSharp(src);
        var (success, summary, _) = RoslynCompileHelper.TryCompile(code, "ModuleEmitTest");
        if (!success)
        {
            output.WriteLine("─── emitted C# ───");
            output.WriteLine(code);
            output.WriteLine("─── diagnostics ───");
            output.WriteLine(summary);
        }
        Assert.True(success, $"Emitted C# for module did not compile:\n{summary}");
    }

    [Fact]
    public void Module_NestedAndAlongsideActor_AllCompile()
    {
        const string src = """
            message Tick();

            module Helpers
            {
                public int Double(int n) { return n + n; }

                module Nested
                {
                    public int Triple(int n) { return n + n + n; }
                }
            }

            actor Counter
            {
                int n = 0;
                on Tick => { n = n + 1; }
            }
            """;
        var code = EmitCSharp(src);
        var (success, summary, _) = RoslynCompileHelper.TryCompile(code, "ModuleEmitNestedTest");
        if (!success)
        {
            output.WriteLine("─── emitted C# ───");
            output.WriteLine(code);
            output.WriteLine("─── diagnostics ───");
            output.WriteLine(summary);
        }
        Assert.True(success, $"Emitted C# did not compile:\n{summary}");
    }
}
