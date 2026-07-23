using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Xunit;

namespace Spek.Tests.Emit;

/// <summary>
/// Coverage for the try/catch/throw emitter. Mirrors C#:
/// typed catches with optional bindings, optional `when` guards,
/// optional finally, bare `throw;` rethrow.
/// </summary>
public sealed class TryCatchEmitTests
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
    public void TryCatchTyped_EmitsTryCatchBlocks()
    {
        const string src = """
            using System;
            message Tick();
            actor A
            {
                behavior Idle
                {
                    on Tick => {
                        try {
                            int x = 1;
                        } catch (InvalidOperationException ex) {
                            int y = 2;
                        }
                    }
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("try", code);
        Assert.Contains("catch (InvalidOperationException ex)", code);
    }

    [Fact]
    public void TryCatchWhenGuard_EmitsCatchWhenClause()
    {
        // C#-shape: `when` clause lives outside the catch parens.
        const string src = """
            using System;
            message Tick();
            actor A
            {
                behavior Idle
                {
                    on Tick => {
                        try {
                            int x = 1;
                        } catch (InvalidOperationException ex) when (ex.Message == "x") {
                            int y = 2;
                        }
                    }
                }
            }
            """;
        var code = EmitCSharp(src);
        // BinaryExpr emits with surrounding parens for precedence;
        // the catch's `when (...)` adds its own outer pair, so we
        // accept either single- or double-parens form.
        Assert.True(
            code.Contains("when (ex.Message == \"x\")")
            || code.Contains("when ((ex.Message == \"x\"))"),
            "Expected 'when (...)' clause; emitted code was:\n" + code);
        Assert.Contains("catch (InvalidOperationException ex)", code);
    }

    [Fact]
    public void TryFinally_EmitsFinallyBlock()
    {
        const string src = """
            using System;
            message Tick();
            actor A
            {
                behavior Idle
                {
                    on Tick => {
                        try {
                            int x = 1;
                        } finally {
                            int y = 2;
                        }
                    }
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("try", code);
        Assert.Contains("finally", code);
    }

    [Fact]
    public void TryCatchFinally_EmitsAllThree()
    {
        const string src = """
            using System;
            message Tick();
            actor A
            {
                behavior Idle
                {
                    on Tick => {
                        try {
                            int x = 1;
                        } catch (Exception ex) {
                            int y = 2;
                        } finally {
                            int z = 3;
                        }
                    }
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("try", code);
        Assert.Contains("catch (Exception ex)", code);
        Assert.Contains("finally", code);
    }

    [Fact]
    public void Throw_WithExpression_EmitsThrowExpression()
    {
        const string src = """
            using System;
            message Tick();
            actor A
            {
                behavior Idle
                {
                    on Tick => {
                        throw new InvalidOperationException("boom");
                    }
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("throw new InvalidOperationException(\"boom\")", code);
    }

    [Fact]
    public void BareThrow_EmitsRethrow()
    {
        const string src = """
            using System;
            message Tick();
            actor A
            {
                behavior Idle
                {
                    on Tick => {
                        try {
                            int x = 1;
                        } catch (Exception ex) {
                            throw;
                        }
                    }
                }
            }
            """;
        var code = EmitCSharp(src);
        // Bare `throw;` (no expression) — exact textual match for the rethrow.
        Assert.Contains("throw;", code);
    }
}
