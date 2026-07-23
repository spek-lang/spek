using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Xunit;
using Xunit.Abstractions;

namespace Spek.Tests.Emit;

/// <summary>
/// `in` / `ref` / `out` parameter modifiers emit verbatim into
/// the C# method signature, and call-site arguments restate the
/// keyword. The end-to-end case must Roslyn-compile, which transitively
/// proves the modifier matches between declaration and call site (C#
/// enforces that itself).
/// </summary>
public sealed class ParamModifierEmitTests(ITestOutputHelper output)
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
    public void Modifiers_EmitInSignature()
    {
        const string src = """
            module Buffers
            {
                public void Mix(in int read, ref int aliased, out int written)
                {
                    written = read + aliased;
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("public static void Mix(in int read, ref int aliased, out int written)", code);
    }

    [Fact]
    public void CallSiteModifier_EmitsKeyword()
    {
        const string src = """
            module Calc
            {
                public void AddInto(int a, int b, out int result) { result = a + b; }

                public int Compute(int x)
                {
                    int r = 0;
                    AddInto(x, x, out r);
                    return r;
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("AddInto(x, x, out r)", code);
    }

    [Fact]
    public void DeclarationAndCall_RoundTrip_Compiles()
    {
        const string src = """
            module Calc
            {
                public void AddInto(in int a, ref int acc, out int result)
                {
                    acc = acc + a;
                    result = acc;
                }

                public int Run(int x)
                {
                    int acc = 0;
                    int result = 0;
                    AddInto(in x, ref acc, out result);
                    return result;
                }
            }
            """;
        var code = EmitCSharp(src);
        var (success, summary, _) = RoslynCompileHelper.TryCompile(code, "ParamModifierEmitTest");
        if (!success)
        {
            output.WriteLine("─── emitted C# ───");
            output.WriteLine(code);
            output.WriteLine("─── diagnostics ───");
            output.WriteLine(summary);
        }
        Assert.True(success, $"Emitted C# did not compile:\n{summary}");
    }

    [Fact]
    public void OutVar_EmitsAndCompiles()
    {
        // The inline `out var n` must introduce `n` for the rest of the
        // scope — proven by `return n` compiling.
        const string src = """
            module P
            {
                public bool TryGet(string s, out int n) { n = 1; return true; }
                public int Use(string s)
                {
                    TryGet(s, out var n);
                    return n;
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("TryGet(s, out var n)", code);

        var (success, summary, _) = RoslynCompileHelper.TryCompile(code, "OutVarEmitTest");
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
