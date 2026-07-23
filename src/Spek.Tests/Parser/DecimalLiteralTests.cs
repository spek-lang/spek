using Spek.Compiler.AST;
using Spek.Compiler.Parser;
using Xunit;

namespace Spek.Tests.Parser;

/// <summary>
/// Decimal literals match C#: an integer with the <c>m</c> suffix
/// (<c>0m</c>, <c>100m</c>) is a decimal, alongside the fractional forms
/// (<c>1.5</c>, <c>1.5m</c>). A bare integer with no <c>.</c> and no
/// <c>m</c> stays an integer literal.
/// </summary>
public sealed class DecimalLiteralTests
{
    private static Expr DefaultOf(string src)
    {
        var r = SpekCompiler.Parse(src);
        Assert.True(r.Success,
            string.Join("\n", r.Diagnostics.Select(d => $"{d.Code} {d.Line}:{d.Column} {d.Message}")));
        var m = r.Tree!.Declarations.OfType<MessageDecl>().Single();
        return m.Fields[0].DefaultValue!;
    }

    [Fact]
    public void IntegerWithMSuffix_IsDecimal()
    {
        var lit = Assert.IsType<DecimalLiteralExpr>(DefaultOf("message M(decimal x = 0m);"));
        Assert.Equal(0m, lit.Value);
        Assert.True(lit.HasSuffix);
    }

    [Fact]
    public void LargerIntegerWithMSuffix_IsDecimal()
    {
        var lit = Assert.IsType<DecimalLiteralExpr>(DefaultOf("message M(decimal x = 100m);"));
        Assert.Equal(100m, lit.Value);
        Assert.True(lit.HasSuffix);
    }

    [Fact]
    public void FractionalForms_StillParse()
    {
        var noSuffix = Assert.IsType<DecimalLiteralExpr>(DefaultOf("message M(decimal x = 1.5);"));
        Assert.False(noSuffix.HasSuffix);

        var suffixed = Assert.IsType<DecimalLiteralExpr>(DefaultOf("message M(decimal x = 2.50m);"));
        Assert.True(suffixed.HasSuffix);
        Assert.Equal(2.50m, suffixed.Value);
    }

    [Fact]
    public void BareInteger_StaysInteger()
    {
        Assert.IsType<IntLiteralExpr>(DefaultOf("message M(int x = 5);"));
    }
}
