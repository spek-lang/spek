using Spek.Compiler.Parser;
using Spek.Compiler.Semantic;
using Xunit;

namespace Spek.Tests.Semantic;

/// <summary>
/// Red-team H1 (round 2): <c>To&lt;T&gt;</c> promises lossless conversion, but its
/// identity lowering (<c>Conversions.To&lt;T&gt;(T value) =&gt; value</c>) leans on
/// C#'s implicit numeric conversions — which INCLUDE lossy widenings
/// (int/long → float, long → double: the target's mantissa is narrower than
/// the source's integer range). So <c>16777217.To&lt;float&gt;()</c> == 16777216
/// compiled silently, falsifying the marquee "you cannot silently lose data".
/// CE0138 now rejects those pairs and steers to <c>TryTo</c>. These fences pin
/// which sources it can see (literal, local/field, msg.field) and, critically,
/// that lossless widenings stay clean.
/// </summary>
public sealed class RedTeamLossyWideningTests
{
    // A handler over `Reading r` (int + byte fields), with int/long/float
    // actor fields, so a single wrapper reaches every source shape.
    private static string Wrap(string stmt) => $$"""
        message Reading(int value, byte small);

        actor A
        {
            int i = 5;
            long lng = 5;
            float f = 1.0;

            init() { become Active; }

            behavior Active
            {
                on Reading r => { {{stmt}} }
            }
        }
        """;

    private static bool HasCE0138(string stmt) =>
        SpekCompiler.Parse(Wrap(stmt)).Diagnostics.Any(
            d => d.Code == "CE0138" && d.Severity == DiagnosticSeverity.Error);

    // ─── fires: the lossy-implicit widenings, across every source shape ──────

    [Fact]
    public void CE0138_IntLiteral_To_Float_Errors() =>
        Assert.True(HasCE0138("var x = 16777217.To<float>();"));

    [Fact]
    public void CE0138_IntField_To_Float_Errors() =>
        Assert.True(HasCE0138("var x = i.To<float>();"));

    [Fact]
    public void CE0138_MessageField_Int_To_Float_Errors() =>
        Assert.True(HasCE0138("var x = r.value.To<float>();"));

    [Fact]
    public void CE0138_LongField_To_Double_Errors() =>
        Assert.True(HasCE0138("var x = lng.To<double>();"));

    [Fact]
    public void CE0138_LongLiteral_To_Double_Errors() =>
        Assert.True(HasCE0138("var x = 10000000000L.To<double>();"));

    // ─── stays silent: LOSSLESS widenings (the false-positive guard) ─────────

    [Fact]
    public void CE0138_IntToDouble_IsLossless_NoError() =>
        Assert.False(HasCE0138("var x = i.To<double>();"));

    [Fact]
    public void CE0138_IntToLong_IsLossless_NoError() =>
        Assert.False(HasCE0138("var x = i.To<long>();"));

    [Fact]
    public void CE0138_FloatToDouble_IsLossless_NoError() =>
        Assert.False(HasCE0138("var x = f.To<double>();"));

    [Fact]
    public void CE0138_ByteToFloat_IsLossless_NoError() =>
        Assert.False(HasCE0138("var x = r.small.To<float>();"));

    [Fact]
    public void CE0138_IntToDecimal_IsLossless_NoError() =>
        Assert.False(HasCE0138("var x = i.To<decimal>();"));

    // ─── stays silent when the source can't be pinned down (documented gap) ──
    // A member chain the resolver can't follow (`.ToString().Length`) leaves
    // the source unknown; CE0138 declines to guess rather than risk a false
    // positive. Documents the residue, not an endorsement.
    [Fact]
    public void CE0138_UnresolvableSource_StaysSilent() =>
        Assert.False(HasCE0138("var x = i.ToString().Length.To<float>();"));

    // TryTo — the prescribed replacement — is never flagged.
    [Fact]
    public void CE0138_TryTo_IsAlwaysAllowed() =>
        Assert.False(HasCE0138("var x = 16777217.TryTo<float>();"));
}
