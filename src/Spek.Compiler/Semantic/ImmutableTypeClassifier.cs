using Spek.Compiler.AST;

namespace Spek.Compiler.Semantic;

/// <summary>
/// Classifies a <see cref="TypeRef"/> as immutable-by-design for the purpose of
/// CE0010. The set of accepted types is deliberately narrow:
///
///   * primitives (<c>int</c>, <c>long</c>, <c>decimal</c>, <c>bool</c>, ...)
///   * immutable system value/reference types (<c>string</c>, <c>DateTime</c>,
///     <c>Guid</c>, <c>Uri</c>, <c>TimeSpan</c>, <c>Version</c>, ...)
///   * <c>ActorRef</c> — opaque capability handle from Spek.Runtime
///   * Spek-declared <c>message</c> types (records; immutable by construction)
///   * type parameters of the enclosing message declaration
///   * generic types from <c>System.Collections.Immutable.*</c> when all type
///     args are themselves immutable (recursive check)
///
/// Everything else — <c>List&lt;T&gt;</c>, <c>Dictionary&lt;K,V&gt;</c>, plain
/// arrays, <c>IEnumerable&lt;T&gt;</c>, <c>IReadOnlyList&lt;T&gt;</c>, and any
/// user-declared class not in the table — is rejected. The readonly interfaces
/// are rejected deliberately because the underlying implementation can still be
/// a mutable collection; callers should use the concrete <c>Immutable*</c>
/// types when a collection is required in a message.
///
/// Reflection is explicitly NOT a concern of this classifier — that belongs to
/// a future hostile-import rule (reserved as CE0080).
/// </summary>
public static class ImmutableTypeClassifier
{
    // ─── Scalar & reference types blessed as immutable by design ────────────

    private static readonly HashSet<string> ImmutableSimpleTypes = new(StringComparer.Ordinal)
    {
        // Spek / C# primitive keywords
        "bool", "byte", "sbyte", "char",
        "short", "ushort", "int", "uint", "long", "ulong",
        "float", "double", "decimal",
        "string",

        // C# BCL names (writers often use these interchangeably)
        "Boolean", "Byte", "SByte", "Char",
        "Int16", "UInt16", "Int32", "UInt32", "Int64", "UInt64",
        "Single", "Double", "Decimal",
        "String",

        // Immutable value/reference types from the BCL that callers commonly reach for
        "DateTime", "DateTimeOffset", "DateOnly", "TimeOnly", "TimeSpan",
        "Guid", "Uri", "Version",

        // Spek.Runtime — opaque capability handle, internally immutable
        "ActorRef",
    };

    // Generic containers that are immutable by design. Still recursively
    // checked — all type args must be immutable too.
    private static readonly HashSet<string> ImmutableGenericContainers = new(StringComparer.Ordinal)
    {
        "ImmutableArray",
        "ImmutableList",
        "ImmutableDictionary",
        "ImmutableSortedDictionary",
        "ImmutableHashSet",
        "ImmutableSortedSet",
        "ImmutableQueue",
        "ImmutableStack",
    };

    public enum Classification { Allowed, Disallowed }

    /// <summary>
    /// Classifies <paramref name="type"/> against the whitelist rules.
    /// <paramref name="enclosingTypeParameters"/> lets the classifier accept
    /// type parameter references (e.g. <c>T</c> in <c>Response&lt;T&gt;</c>).
    /// </summary>
    public static Classification Classify(
        TypeRef type,
        SymbolTable symbols,
        IReadOnlyList<TypeParameter> enclosingTypeParameters)
    {
        var simple = type.Name.Simple;

        // Arrays are mutable regardless of element type — `int[]` elements can
        // be reassigned in place — so they can never be an immutable message
        // field (use an ImmutableArray/ImmutableList instead). Reject before any
        // element-type check.
        if (type.ArrayRank > 0)
            return Classification.Disallowed;

        // Type parameter — allowed (actual type substituted at instantiation
        // time; instantiation-site checks are future work).
        if (enclosingTypeParameters.Any(tp => tp.Name == simple))
            return Classification.Allowed;

        // Spek-declared message — records, immutable by construction.
        if (symbols.ResolveMessage(type.Name) is not null)
            return Classification.Allowed;

        // Spek-declared enum — value type, immutable by construction.
        if (symbols.ResolveEnum(type.Name) is not null)
            return type.TypeArgs.Count == 0 ? Classification.Allowed : Classification.Disallowed;

        // Simple immutable types — no generic args expected.
        if (ImmutableSimpleTypes.Contains(simple))
            return type.TypeArgs.Count == 0 ? Classification.Allowed : Classification.Disallowed;

        // Immutable generic containers — all type args must also be immutable.
        if (ImmutableGenericContainers.Contains(simple))
        {
            foreach (var arg in type.TypeArgs)
            {
                if (Classify(arg, symbols, enclosingTypeParameters) == Classification.Disallowed)
                    return Classification.Disallowed;
            }
            return Classification.Allowed;
        }

        // Everything else — rejected. This is intentional: unknown types are
        // treated as disallowed for CE0010 to keep the "no shared mutable
        // state" guarantee. The user can broaden the whitelist if a type is
        // proven immutable (open question for the future).
        return Classification.Disallowed;
    }
}
