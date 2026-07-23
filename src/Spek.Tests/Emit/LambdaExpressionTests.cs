using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Xunit;

namespace Spek.Tests.Emit;

/// <summary>
/// Coverage for first-class lambda expressions. The emit path is
/// verbatim — Spek lambdas lower one-to-one to C# lambdas; type
/// inference, capture, and target-typing are handled by Roslyn.
/// </summary>
public sealed class LambdaExpressionTests
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
    public void Lambda_SingleBareParam_ExpressionBody_Emits()
    {
        const string src = """
            using System.Linq;
            using System.Collections.Generic;

            message Tick();
            actor A
            {
                List<int> items = new List<int>();
                on Tick =>
                {
                    var actives = items.Where(x => x > 0).ToList();
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("x => (x > 0)", code);
    }

    [Fact]
    public void Lambda_NoParams_Emits()
    {
        // Use a typed local so type inference picks up the lambda's
        // shape — Spek doesn't have C# cast expressions like (Func<…>)(…).
        const string src = """
            using System;

            message Tick();
            actor A
            {
                on Tick =>
                {
                    System.Func<long> nowSec = () => DateTimeOffset.UtcNow.ToUnixTimeSeconds();
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("() => DateTimeOffset.UtcNow.ToUnixTimeSeconds()", code);
    }

    [Fact]
    public void Lambda_ParenList_TwoParams_Emits()
    {
        const string src = """
            using System;

            message Tick();
            actor A
            {
                on Tick =>
                {
                    System.Func<int, int, int> add = (x, y) => x + y;
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("(x, y) => (x + y)", code);
    }

    [Fact]
    public void Lambda_TypedParenList_Emits()
    {
        const string src = """
            using System;

            message Tick();
            actor A
            {
                on Tick =>
                {
                    System.Func<int, int, int> add = (int x, int y) => x + y;
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("(int x, int y) => (x + y)", code);
    }

    [Fact]
    public void Lambda_BlockBody_Emits()
    {
        const string src = """
            using System;

            message Tick();
            actor A
            {
                on Tick =>
                {
                    System.Func<int, int> transform = x =>
                    {
                        var doubled = x * 2;
                        return doubled + 1;
                    };
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("x =>", code);
        Assert.Contains("var doubled =", code);
    }

    [Fact]
    public void Lambda_LinqChain_RoundTripsThroughRoslyn()
    {
        // The headline use case: LINQ method chains inside a handler.
        const string src = """
            using System.Linq;
            using System.Collections.Generic;

            message Tick();
            actor A
            {
                List<int> items = new List<int>();
                on Tick =>
                {
                    var doubled = items
                        .Where(x => x > 0)
                        .Select(x => x * 2)
                        .OrderBy(x => x)
                        .ToList();
                }
            }
            """;
        var code = EmitCSharp(src);
        var (success, errors, _) = RoslynCompileHelper.TryCompile(code, "LambdaLinqSmoke");
        Assert.True(success,
            "Emitted C# did not compile:\n" + errors + "\n--- emitted ---\n" + code);
    }

    [Fact]
    public void Lambda_NestedInsideSwitchArm_Parses()
    {
        // A switch arm whose result is a lambda. The outer => is the
        // arm separator; the inner one is consumed by the lambda parser.
        const string src = """
            using System;

            message Tick();
            actor A
            {
                int n = 0;
                on Tick =>
                {
                    System.Func<int, int> f = n switch {
                        0 => x => x + 1,
                        _ => x => x - 1
                    };
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.True(parsed.Success,
            "Source must parse; diagnostics: " +
            string.Join("\n", parsed.Diagnostics.Select(d => $"{d.Code} {d.Line}:{d.Column} {d.Message}")));
    }

    [Fact]
    public void Lambda_CapturesActorField_RoundTripsThroughRoslyn()
    {
        const string src = """
            using System.Linq;
            using System.Collections.Generic;

            message Tick();
            actor A
            {
                int threshold = 5;
                List<int> items = new List<int>();
                on Tick =>
                {
                    var filtered = items.Where(x => x > threshold).ToList();
                }
            }
            """;
        var code = EmitCSharp(src);
        var (success, errors, _) = RoslynCompileHelper.TryCompile(code, "LambdaCaptureSmoke");
        Assert.True(success,
            "Emitted C# did not compile:\n" + errors + "\n--- emitted ---\n" + code);
    }
}
