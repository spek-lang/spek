using Spek.Compiler.AST;
using Spek.Compiler.Parser;
using Xunit;

namespace Spek.Tests.Compiler;

/// <summary>
/// `in` / `ref` / `out` parameter modifiers on module methods.
/// Parsing layer: the modifier is recorded on the <see cref="Param"/>
/// AST node, and call-site arguments carrying a modifier parse into a
/// <see cref="RefArgExpr"/>.
/// </summary>
public sealed class ParamModifierTests
{
    private static SpekFile Parse(string source)
    {
        var parsed = SpekCompiler.Parse(source);
        Assert.True(parsed.Success,
            "Source must parse; diagnostics: " +
            string.Join("\n", parsed.Diagnostics.Select(d => $"{d.Code} {d.Line}:{d.Column} {d.Message}")));
        return parsed.Tree!;
    }

    [Fact]
    public void NoModifier_DefaultsToNone()
    {
        var tree = Parse("module M { public int Id(int n) { return n; } }");
        var p = tree.Declarations.OfType<ModuleDecl>().Single().Methods.Single().Parameters.Single();
        Assert.Equal(ParamModifier.None, p.Modifier);
    }

    [Fact]
    public void InRefOut_AreRecordedOnParameters()
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
        var fn = Parse(src).Declarations.OfType<ModuleDecl>().Single().Methods.Single();
        Assert.Equal(ParamModifier.In,  fn.Parameters[0].Modifier);
        Assert.Equal(ParamModifier.Ref, fn.Parameters[1].Modifier);
        Assert.Equal(ParamModifier.Out, fn.Parameters[2].Modifier);
    }

    [Fact]
    public void CallSiteModifier_ParsesAsRefArgExpr()
    {
        // A module method that calls a sibling with `ref` / `out`.
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
        var module = Parse(src).Declarations.OfType<ModuleDecl>().Single();
        var compute = module.Methods.Single(f => f.Name == "Compute");

        // Find the AddInto(...) invocation inside Compute's body.
        var call = compute.Body.Statements
            .OfType<ExpressionStmt>()
            .Select(s => s.Expr)
            .OfType<InvocationExpr>()
            .Single(i => i.Callee == "AddInto");

        // Third argument is `out r` → RefArgExpr(Out, NameExpr r).
        var outArg = Assert.IsType<RefArgExpr>(call.Args[2]);
        Assert.Equal(ParamModifier.Out, outArg.Modifier);
    }

    [Fact]
    public void OutVar_ParsesAsOutVarExpr()
    {
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
        var module = Parse(src).Declarations.OfType<ModuleDecl>().Single();
        var use    = module.Methods.Single(f => f.Name == "Use");
        var call   = use.Body.Statements
            .OfType<ExpressionStmt>()
            .Select(s => s.Expr)
            .OfType<InvocationExpr>()
            .Single(i => i.Callee == "TryGet");

        var outVar = Assert.IsType<OutVarExpr>(call.Args[1]);
        Assert.Equal("n", outVar.Name);
    }
}
