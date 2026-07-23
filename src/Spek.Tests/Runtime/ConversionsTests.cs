using Spek;
using Xunit;

namespace Spek.Tests.Runtime;

/// <summary>
/// Direct runtime-semantics coverage for the conversion family backing the
/// language's <c>to</c> / <c>to!</c> operators (Spek.Runtime/Conversions.cs).
/// The emitter-side lowering is covered in Emit/ConversionFamilyTests; this
/// file exercises the halves of the runtime surface those tests skip:
/// <list type="bullet">
///   <item><see cref="Conversions.To{T}"/> — the lossless identity/widening
///         carrier.</item>
///   <item>Every numeric <c>TryTo</c> source overload (int, uint, long,
///         ulong, float, double, decimal) on both the exactly-representable
///         and the null branch — truncation, precision loss, and range
///         overflow all yield null, never a wrong value.</item>
///   <item>The float / double / decimal rounding-strategy overloads,
///         including the non-finite guard and the rounded-but-out-of-range
///         null.</item>
///   <item><see cref="EnumConversions"/> — the long-source overload, the
///         underlying-type overflow branch, and flags combinations through
///         the long path.</item>
///   <item>The <see cref="FlagsEnumExtensions"/> arities the flags tests
///         don't touch, plus a ulong-underlying enum with the high bit set
///         (the UInt64 branch of the bit-image helper).</item>
/// </list>
/// </summary>
public class ConversionsTests
{
    // ─── To: lossless carrier ────────────────────────────────────────────────

    [Fact]
    public void To_NumericWidening_CarriesTheValue()
    {
        // The widening happens as ordinary argument conversion at the call
        // site; To itself is the identity that anchors the target type.
        Assert.Equal(42L, Conversions.To<long>(42));
        Assert.Equal(42d, Conversions.To<double>(42));
        Assert.Equal(0.5d, Conversions.To<double>(0.5f));
    }

    [Fact]
    public void To_ReferenceUpcast_IsTheSameInstance()
    {
        var s = "spek";
        Assert.Same(s, Conversions.To<object>(s));
    }

    // ─── TryTo: int source ───────────────────────────────────────────────────

    [Fact]
    public void TryTo_FromInt_ExactOrNull()
    {
        Assert.Equal((byte)200, Conversions.TryTo<byte>(200));
        Assert.Null(Conversions.TryTo<byte>(256));            // range overflow
        Assert.Null(Conversions.TryTo<uint>(-1));              // sign loss
        Assert.Equal(7u, Conversions.TryTo<uint>(7));

        // int → float is NOT always exact: 2^24 + 1 rounds in the mantissa.
        Assert.Equal(16_777_216f, Conversions.TryTo<float>(16_777_216));
        Assert.Null(Conversions.TryTo<float>(16_777_217));
    }

    // ─── TryTo: uint source ──────────────────────────────────────────────────

    [Fact]
    public void TryTo_FromUInt_ExactOrNull()
    {
        Assert.Equal(7, Conversions.TryTo<int>(7u));
        Assert.Null(Conversions.TryTo<int>(uint.MaxValue));    // > int.MaxValue
        Assert.Equal(4_000_000_000L, Conversions.TryTo<long>(4_000_000_000u));
    }

    // ─── TryTo: long source ──────────────────────────────────────────────────

    [Fact]
    public void TryTo_FromLong_ExactOrNull()
    {
        Assert.Equal(int.MaxValue, Conversions.TryTo<int>((long)int.MaxValue));
        Assert.Null(Conversions.TryTo<int>((long)int.MaxValue + 1));

        // long → double: exact up to 2^53, rounds beyond.
        Assert.Equal(9_007_199_254_740_992d, Conversions.TryTo<double>(1L << 53));
        Assert.Null(Conversions.TryTo<double>((1L << 53) + 1));
    }

    // ─── TryTo: ulong source ─────────────────────────────────────────────────

    [Fact]
    public void TryTo_FromULong_ExactOrNull()
    {
        Assert.Equal(42L, Conversions.TryTo<long>(42UL));
        Assert.Null(Conversions.TryTo<long>(ulong.MaxValue)); // > long.MaxValue
        Assert.Equal((byte)200, Conversions.TryTo<byte>(200UL));
        Assert.Null(Conversions.TryTo<byte>(300UL));
    }

    // ─── TryTo: float source ─────────────────────────────────────────────────

    [Fact]
    public void TryTo_FromFloat_ExactOrNull()
    {
        Assert.Equal(2, Conversions.TryTo<int>(2f));
        Assert.Null(Conversions.TryTo<int>(2.5f));             // fraction: no silent truncation
        Assert.Null(Conversions.TryTo<int>(float.NaN));
        Assert.Null(Conversions.TryTo<byte>(300f));            // range overflow
        Assert.Equal(0.5d, Conversions.TryTo<double>(0.5f));   // float → double widens exactly
    }

    // ─── TryTo: double source (the halves the emit tests skip) ───────────────

    [Fact]
    public void TryTo_FromDouble_ExactOrNull()
    {
        Assert.Equal((sbyte)127, Conversions.TryTo<sbyte>(127d));
        Assert.Null(Conversions.TryTo<sbyte>(128d));           // just out of range
        Assert.Null(Conversions.TryTo<int>(double.NaN));
        Assert.Null(Conversions.TryTo<long>(double.PositiveInfinity));
    }

    // ─── TryTo: decimal source ───────────────────────────────────────────────

    [Fact]
    public void TryTo_FromDecimal_ExactOrNull()
    {
        Assert.Equal(42, Conversions.TryTo<int>(42.0m));
        Assert.Null(Conversions.TryTo<int>(42.5m));            // fraction
        Assert.Null(Conversions.TryTo<long>(decimal.MaxValue)); // range overflow
        Assert.Equal(255UL, Conversions.TryTo<ulong>(255m));
    }

    // ─── TryTo with a rounding strategy ──────────────────────────────────────

    [Fact]
    public void TryTo_FloatWithRounding_RoundsThenConverts()
    {
        Assert.Equal(3, Conversions.TryTo<int>(2.5f, MidpointRounding.AwayFromZero));
        Assert.Equal(2, Conversions.TryTo<int>(2.5f, MidpointRounding.ToEven));
        Assert.Equal(2, Conversions.TryTo<int>(2.9f, MidpointRounding.ToZero));

        // Non-finite is null even with a strategy — there is nothing to round.
        Assert.Null(Conversions.TryTo<int>(float.NaN, MidpointRounding.ToZero));
        Assert.Null(Conversions.TryTo<int>(float.PositiveInfinity, MidpointRounding.ToZero));
    }

    [Fact]
    public void TryTo_DoubleWithRounding_NonFiniteAndOutOfRangeAreNull()
    {
        Assert.Equal(3, Conversions.TryTo<int>(2.5d, MidpointRounding.AwayFromZero));
        Assert.Null(Conversions.TryTo<int>(double.PositiveInfinity, MidpointRounding.AwayFromZero));
        // Rounding succeeds but the result doesn't fit the target.
        Assert.Null(Conversions.TryTo<byte>(300.4d, MidpointRounding.ToZero));
    }

    [Fact]
    public void TryTo_DecimalWithRounding_RoundsThenConverts()
    {
        Assert.Equal(3, Conversions.TryTo<int>(2.5m, MidpointRounding.AwayFromZero));
        Assert.Equal(2, Conversions.TryTo<int>(2.5m, MidpointRounding.ToEven));
        Assert.Null(Conversions.TryTo<byte>(300.4m, MidpointRounding.ToZero));  // rounded, still too big
    }

    // ─── EnumConversions: the long-source overload + overflow branch ─────────

    private enum Color { Red = 0, Green = 1, Blue = 2 }
    private enum Tiny : byte { A = 1, B = 2 }

    [Flags]
    private enum Perms { None = 0, Read = 1, Write = 2, Execute = 4 }

    [Flags]
    private enum Wide : ulong { Lo = 1UL, Hi = 0x8000_0000_0000_0000UL }

    [Fact]
    public void EnumTryTo_FromLong_DefinedOrNull()
    {
        Assert.Equal(Color.Blue, EnumConversions.TryTo<Color>(2L));
        Assert.Null(EnumConversions.TryTo<Color>(3L));          // undefined member
    }

    [Fact]
    public void EnumTryTo_UnderlyingTypeOverflow_IsNull()
    {
        // The value can't even be converted to the enum's underlying type —
        // the OverflowException branch, not the IsDefined branch.
        Assert.Null(EnumConversions.TryTo<Color>(long.MaxValue));   // int underlying
        Assert.Null(EnumConversions.TryTo<Tiny>(-1));               // byte underlying, int source
        Assert.Null(EnumConversions.TryTo<Tiny>(256L));             // byte underlying, long source
        Assert.Equal(Tiny.A, EnumConversions.TryTo<Tiny>(1));
    }

    [Fact]
    public void EnumTryTo_FlagsThroughTheLongPath_EverySetBitMustBeDefined()
    {
        Assert.Equal(Perms.Read | Perms.Write, EnumConversions.TryTo<Perms>(3L));
        Assert.Equal(Perms.None, EnumConversions.TryTo<Perms>(0L));
        Assert.Null(EnumConversions.TryTo<Perms>(8L));    // undefined bit
        Assert.Null(EnumConversions.TryTo<Perms>(9L));    // defined + undefined mix
    }

    // ─── FlagsEnumExtensions: uncovered arities + the UInt64 bit image ───────

    [Fact]
    public void HasAnyFlags_ThreeFlagOverload_IsTheUnionIntersection()
    {
        var p = Perms.Execute;
        Assert.True(p.HasAnyFlags(Perms.Read, Perms.Write, Perms.Execute));
        Assert.False(Perms.Read.HasAnyFlags(Perms.Write, Perms.Execute));
        Assert.False(Perms.None.HasAnyFlags(Perms.Read, Perms.Write, Perms.Execute));
    }

    [Fact]
    public void FlagsQueries_ULongUnderlying_HighBitSurvivesTheBitImage()
    {
        // Wide.Hi is above long.MaxValue — the UInt64 branch of the bit-image
        // helper. A sign-extension bug here would corrupt every query.
        var both = Wide.Lo | Wide.Hi;
        Assert.True(Wide.Hi.HasAnyFlags(Wide.Hi));
        Assert.True(both.HasOnlyFlags(Wide.Lo, Wide.Hi));
        Assert.False(both.HasOnlyFlags(Wide.Lo));
        Assert.True(both.HasAnyFlags(Wide.Lo, Wide.Hi));
    }
}
