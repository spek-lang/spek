using System.Numerics;

namespace Spek;

/// <summary>
/// The Spek conversion family: <c>To</c> accepts only conversions C# can
/// prove lossless (implicit conversions — numeric widening and reference
/// upcasts); <c>TryTo</c> performs the fallible directions and returns
/// <c>null</c> rather than throwing, wrapping, or truncating.
/// </summary>
/// <remarks>
/// spekc lowers <c>x.To&lt;T&gt;()</c> / <c>x.TryTo&lt;T&gt;()</c> method
/// syntax to these static calls. The static form is load-bearing: extension
/// receivers accept only identity/reference/boxing conversions, so the
/// identity trick that makes <c>To</c> reject narrowing would never see a
/// widening receiver — ordinary argument conversion does. Reference
/// downcasts never reach this class; spekc lowers
/// <c>x.TryTo&lt;SomeClass&gt;()</c> to <c>x as SomeClass</c> so Roslyn keeps
/// its unrelated-types error. The <c>TryTo</c> contract is <em>exactly
/// representable</em>: a conversion that would change the value yields null
/// unless a rounding strategy was requested explicitly.
/// </remarks>
public static class Conversions
{
    /// <summary>
    /// Lossless conversion. Compiles only when an implicit conversion exists
    /// from the argument to <typeparamref name="T"/> — numeric widening and
    /// reference upcasts. Anything that could lose information is a compile
    /// error; use <c>TryTo</c> for the fallible directions.
    /// </summary>
    /// <typeparam name="T">The conversion target.</typeparam>
    /// <param name="value">The value to convert.</param>
    /// <returns>The value, converted losslessly.</returns>
    public static T To<T>(T value) => value;

    // ── numeric TryTo: exactly-representable or null ─────────────────────

    /// <summary>Fallible numeric conversion from <see cref="int"/>; null unless the value is exactly representable in <typeparamref name="T"/>.</summary>
    public static T? TryTo<T>(int value) where T : struct, INumberBase<T> => Exact<int, T>(value);

    /// <summary>Fallible numeric conversion from <see cref="uint"/>; null unless exactly representable.</summary>
    public static T? TryTo<T>(uint value) where T : struct, INumberBase<T> => Exact<uint, T>(value);

    /// <summary>Fallible numeric conversion from <see cref="long"/>; null unless exactly representable.</summary>
    public static T? TryTo<T>(long value) where T : struct, INumberBase<T> => Exact<long, T>(value);

    /// <summary>Fallible numeric conversion from <see cref="ulong"/>; null unless exactly representable.</summary>
    public static T? TryTo<T>(ulong value) where T : struct, INumberBase<T> => Exact<ulong, T>(value);

    /// <summary>Fallible numeric conversion from <see cref="float"/>; null unless exactly representable (fractions do not truncate — request a rounding strategy instead).</summary>
    public static T? TryTo<T>(float value) where T : struct, INumberBase<T> => Exact<float, T>(value);

    /// <summary>Fallible numeric conversion from <see cref="double"/>; null unless exactly representable (fractions do not truncate — request a rounding strategy instead).</summary>
    public static T? TryTo<T>(double value) where T : struct, INumberBase<T> => Exact<double, T>(value);

    /// <summary>Fallible numeric conversion from <see cref="decimal"/>; null unless exactly representable.</summary>
    public static T? TryTo<T>(decimal value) where T : struct, INumberBase<T> => Exact<decimal, T>(value);

    // ── floating → integral with an explicit rounding strategy ──────────

    /// <summary>Rounds by <paramref name="mode"/> first, then converts; null only when the rounded result does not fit <typeparamref name="T"/>.</summary>
    public static T? TryTo<T>(float value, MidpointRounding mode) where T : struct, INumberBase<T>
        => float.IsFinite(value) ? Exact<float, T>(MathF.Round(value, mode)) : null;

    /// <summary>Rounds by <paramref name="mode"/> first, then converts; null only when the rounded result does not fit <typeparamref name="T"/>.</summary>
    public static T? TryTo<T>(double value, MidpointRounding mode) where T : struct, INumberBase<T>
        => double.IsFinite(value) ? Exact<double, T>(Math.Round(value, mode)) : null;

    /// <summary>Rounds by <paramref name="mode"/> first, then converts; null only when the rounded result does not fit <typeparamref name="T"/>.</summary>
    public static T? TryTo<T>(decimal value, MidpointRounding mode) where T : struct, INumberBase<T>
        => Exact<decimal, T>(Math.Round(value, mode));

    // ── machinery ────────────────────────────────────────────────────────

    // Exactness by round-trip: convert, convert back, compare. One rule
    // covers truncation (2.9 → 2 → 2.0 ≠ 2.9), precision loss
    // (double → float → double drift), and range overflow (CreateChecked
    // throws). Never throws to the caller.
    private static TTarget? Exact<TSource, TTarget>(TSource value)
        where TSource : struct, INumberBase<TSource>
        where TTarget : struct, INumberBase<TTarget>
    {
        try
        {
            var converted = TTarget.CreateChecked(value);
            return TSource.CreateChecked(converted).Equals(value) ? converted : null;
        }
        catch (OverflowException) { return null; }
        catch (NotSupportedException) { return null; }
    }

}

/// <summary>
/// The enum side of the Spek conversion family. Separate from
/// <see cref="Conversions"/> because C# treats generic constraints as
/// invisible to method signatures — a numeric and an enum
/// <c>TryTo&lt;T&gt;(int)</c> in one class would be duplicate definitions.
/// spekc routes calls here when the conversion target is a declared enum.
/// </summary>
public static class EnumConversions
{
    /// <summary>Fallible enum conversion; null when the enum does not define the value.</summary>
    public static TEnum? TryTo<TEnum>(int value) where TEnum : struct, Enum => ToEnum<TEnum, int>(value);

    /// <summary>Fallible enum conversion; null when the enum does not define the value.</summary>
    public static TEnum? TryTo<TEnum>(long value) where TEnum : struct, Enum => ToEnum<TEnum, long>(value);

    private static TEnum? ToEnum<TEnum, TSource>(TSource value)
        where TEnum : struct, Enum
        where TSource : struct, IConvertible
    {
        object boxed;
        try { boxed = Convert.ChangeType(value, Enum.GetUnderlyingType(typeof(TEnum))); }
        catch (OverflowException) { return null; }

        // Flags enums define combinations, not just members: Read | Write is
        // valid without being named. The rule is "every set bit corresponds
        // to a defined flag" — Enum.IsDefined would wrongly reject it.
        if (typeof(TEnum).IsDefined(typeof(FlagsAttribute), inherit: false))
        {
            var bits = EnumBits.Of((TEnum)boxed);
            return (bits & ~DefinedMask<TEnum>.Value) == 0UL ? (TEnum)boxed : null;
        }

        return Enum.IsDefined(typeof(TEnum), boxed) ? (TEnum)boxed : null;
    }

    private static class DefinedMask<TEnum> where TEnum : struct, Enum
    {
        public static readonly ulong Value =
            Enum.GetValues<TEnum>().Aggregate(0UL, (acc, v) => acc | EnumBits.Of(v));
    }
}

/// <summary>
/// The two flag-set query verbs the BCL lacks. <c>HasFlag</c> asks "is
/// everything in the argument set in me?" (superset); <c>HasAnyFlags</c> asks
/// "does anything overlap?" (intersection); <c>HasOnlyFlags</c> asks "is
/// nothing set outside these?" (subset — the authorization-boundary test).
/// Equality covers exactness. spekc rejects the degenerate calls (None
/// argument, non-flags enum) at compile time.
/// </summary>
public static class FlagsEnumExtensions
{
    /// <summary>True when any bit of <paramref name="mask"/> is set in <paramref name="value"/>.</summary>
    public static bool HasAnyFlags<T>(this T value, T mask) where T : struct, Enum
        => (EnumBits.Of(value) & EnumBits.Of(mask)) != 0UL;

    /// <summary>True when any bit of either flag is set in <paramref name="value"/>.</summary>
    public static bool HasAnyFlags<T>(this T value, T f1, T f2) where T : struct, Enum
        => (EnumBits.Of(value) & (EnumBits.Of(f1) | EnumBits.Of(f2))) != 0UL;

    /// <summary>True when any bit of any of the three flags is set in <paramref name="value"/>.</summary>
    public static bool HasAnyFlags<T>(this T value, T f1, T f2, T f3) where T : struct, Enum
        => (EnumBits.Of(value) & (EnumBits.Of(f1) | EnumBits.Of(f2) | EnumBits.Of(f3))) != 0UL;

    /// <summary>True when no bit outside <paramref name="mask"/> is set — the subset test. Note: an empty value is a subset of everything (correct for allow-lists); require non-empty explicitly when needed.</summary>
    public static bool HasOnlyFlags<T>(this T value, T mask) where T : struct, Enum
        => (EnumBits.Of(value) & ~EnumBits.Of(mask)) == 0UL;

    /// <summary>Subset test against the union of the two flags.</summary>
    public static bool HasOnlyFlags<T>(this T value, T f1, T f2) where T : struct, Enum
        => (EnumBits.Of(value) & ~(EnumBits.Of(f1) | EnumBits.Of(f2))) == 0UL;

    /// <summary>Subset test against the union of the three flags.</summary>
    public static bool HasOnlyFlags<T>(this T value, T f1, T f2, T f3) where T : struct, Enum
        => (EnumBits.Of(value) & ~(EnumBits.Of(f1) | EnumBits.Of(f2) | EnumBits.Of(f3))) == 0UL;
}

internal static class EnumBits
{
    // Bit image of an enum value regardless of underlying type. Signed
    // underlyings pass through two's complement unchanged; ulong-underlying
    // enums with values above long.MaxValue take the UInt64 path.
    internal static ulong Of<T>(T value) where T : struct, Enum
    {
        var c = (IConvertible)value;
        return c.GetTypeCode() == TypeCode.UInt64
            ? c.ToUInt64(null)
            : unchecked((ulong)c.ToInt64(null));
    }
}
