namespace Spek.Hosting.AspNetCore.Rest;

/// <summary>
/// Emit-type → HTTP status code lookup. Built-in defaults
/// cover the common shapes; users register additional mappings via
/// <see cref="Add"/>.
///
/// The convention: a Spek handler that returns a message named
/// <c>NotFound</c> automatically gets a 404 response. Same for
/// <c>BadRequest</c> (400), <c>Unauthorized</c> (401), and so on.
/// Anything not on the list defaults to 200.
///
/// Scoped per-host (registered as a singleton on the DI container)
/// so different `WebApplication` instances can have different
/// mappings.
/// </summary>
public sealed class SpekStatusCodeMap
{
    private readonly Dictionary<Type, int> _map;

    public SpekStatusCodeMap()
    {
        _map = new Dictionary<Type, int>();
    }

    /// <summary>
    /// Convenience constructor pre-populated with the built-in
    /// mappings. The mapping table is a copy — extending it via
    /// <see cref="Add{T}"/> doesn't affect other hosts.
    /// </summary>
    public static SpekStatusCodeMap WithBuiltInDefaults()
    {
        var m = new SpekStatusCodeMap();
        m.AddByName("NotFound",            404);
        m.AddByName("BadRequest",          400);
        m.AddByName("Unauthorized",        401);
        m.AddByName("Forbidden",           403);
        m.AddByName("Conflict",            409);
        m.AddByName("UnprocessableEntity", 422);
        m.AddByName("TooManyRequests",     429);
        return m;
    }

    /// <summary>
    /// Map a specific message type to a status code. Replaces any
    /// existing mapping for the type.
    /// </summary>
    public SpekStatusCodeMap Add<T>(int statusCode)
    {
        _map[typeof(T)] = statusCode;
        return this;
    }

    /// <summary>
    /// Resolve a status code by reply type. Returns
    /// <see cref="StatusCodes.Status200OK"/> when no mapping exists
    /// — the standard "this is a normal response" case.
    /// </summary>
    public int ResolveFor(Type replyType)
        => _map.TryGetValue(replyType, out var code) ? code : 200;

    /// <summary>
    /// Internal: register a mapping by simple type name. Used by
    /// <see cref="WithBuiltInDefaults"/> for the convention names
    /// (`NotFound`, `BadRequest`, etc.) which the user defines as
    /// regular Spek messages. The actual <see cref="Type"/> is
    /// resolved lazily via reflection at first use, but for now
    /// we register a placeholder mapping by name and
    /// the resolver matches on type-name comparison.
    /// </summary>
    private void AddByName(string typeName, int statusCode)
        => _byName[typeName] = statusCode;

    private readonly Dictionary<string, int> _byName = new(StringComparer.Ordinal);

    /// <summary>
    /// Resolve a status code with name fallback. Tried in order:
    /// type-equality match, then type-simple-name match against
    /// the convention names.
    /// </summary>
    internal int ResolveWithNameFallback(Type replyType)
    {
        if (_map.TryGetValue(replyType, out var code))     return code;
        if (_byName.TryGetValue(replyType.Name, out var c)) return c;
        return 200;
    }
}
