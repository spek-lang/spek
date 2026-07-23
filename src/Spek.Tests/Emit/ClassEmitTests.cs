using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Xunit;
using Xunit.Abstractions;

namespace Spek.Tests.Emit;

/// <summary>
/// A Spek <c>class</c> is a mutable, single-owner instance type. It
/// lowers to a plain C# instance class: fields (plain names), an optional
/// <c>init(params)</c> constructor, and methods. <c>self</c> emits as
/// <c>this</c>; invisible async applies to class method bodies like any
/// other method.
/// </summary>
public sealed class ClassEmitTests(ITestOutputHelper output)
{
    private static string EmitCSharp(string source)
    {
        var parsed = SpekCompiler.Parse(source);
        Assert.True(parsed.Success,
            "Source must parse/analyze; diagnostics: " +
            string.Join("\n", parsed.Diagnostics.Select(d => $"{d.Code} {d.Line}:{d.Column} {d.Message}")));
        return new FileEmitter().Emit(parsed.Tree!);
    }

    [Fact]
    public void Class_EmitsInstanceClass_WithFieldsAndMethods()
    {
        const string src = """
            class Counter
            {
                int n = 0;
                public void Inc() { n = n + 1; }
                public int Value() { return n; }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("sealed class Counter", code);
        Assert.Contains("private int n = 0;", code);     // plain name, no underscore
        Assert.Contains("public void Inc()", code);
        Assert.Contains("public int Value()", code);
    }

    [Fact]
    public void Class_DefaultVisibility_IsInternal()
    {
        var code = EmitCSharp("class Box { int v = 0; }");
        Assert.Contains("internal sealed class Box", code);
    }

    [Fact]
    public void Class_PublicVisibility_EmitsPublic()
    {
        var code = EmitCSharp("public class Box { int v = 0; }");
        Assert.Contains("public sealed class Box", code);
    }

    [Fact]
    public void Class_InitBlock_BecomesConstructorWithParams()
    {
        const string src = """
            class Account
            {
                decimal balance;
                init(string owner, decimal opening) { balance = opening; }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("public Account(string owner, decimal opening)", code);
        Assert.Contains("balance = opening;", code);
    }

    [Fact]
    public void Class_SelfEmitsAsThis()
    {
        const string src = """
            class Box
            {
                int value = 0;
                public int Get() { return self.value; }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("return this.value;", code);
    }

    [Fact]
    public void Class_Method_AutoAwaitsTaskCall()
    {
        // Invisible async runs over the whole emitted file, so a Task-returning
        // call inside a class method is auto-awaited and the method goes async.
        const string src = """
            class Loader
            {
                public int ReadLen(string path)
                {
                    string content = System.IO.File.ReadAllTextAsync(path);
                    return content.Length;
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("await System.IO.File.ReadAllTextAsync(path)", code);
        Assert.Contains("async", code);
    }

    [Fact]
    public void Class_RoundTripsThroughRoslyn()
    {
        // A complete class + a program that constructs and drives it must emit
        // C# that actually compiles.
        const string src = """
            class Account
            {
                decimal balance;
                string owner;

                init(string ownerName, decimal opening)
                {
                    owner = ownerName;
                    balance = opening;
                }

                public void Deposit(decimal amount) { balance = balance + amount; }
                public decimal Balance() { return balance; }
            }

            program Main
            {
                var a = new Account("alice", 100.00m);
                a.Deposit(50.00m);
                System.Console.WriteLine(a.Balance());
            }
            """;
        var code = EmitCSharp(src);
        var (success, summary, _) = RoslynCompileHelper.TryCompile(code, "ClassEmitSmoke");
        if (!success)
        {
            output.WriteLine("─── emitted C# ───");
            output.WriteLine(code);
            output.WriteLine("─── diagnostics ───");
            output.WriteLine(summary);
        }
        Assert.True(success, $"Emitted class C# did not compile:\n{summary}");
    }
}
