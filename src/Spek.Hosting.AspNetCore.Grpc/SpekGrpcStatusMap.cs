using Grpc.Core;

namespace Spek.Hosting.AspNetCore.Grpc;

/// <summary>
/// Emit-type → gRPC <see cref="StatusCode"/> lookup,
/// the gRPC counterpart to <c>Spek.Hosting.AspNetCore.Rest</c>'s
/// <c>SpekStatusCodeMap</c>. Built-in defaults mirror the REST
/// table, mapped onto gRPC's standard status codes
/// (<see href="https://grpc.io/docs/guides/status-codes/"/>):
///
/// <list type="bullet">
///   <item><c>NotFound</c> message → <see cref="StatusCode.NotFound"/></item>
///   <item><c>BadRequest</c> message → <see cref="StatusCode.InvalidArgument"/></item>
///   <item><c>Unauthorized</c> message → <see cref="StatusCode.Unauthenticated"/></item>
///   <item><c>Forbidden</c> message → <see cref="StatusCode.PermissionDenied"/></item>
///   <item><c>Conflict</c> message → <see cref="StatusCode.AlreadyExists"/></item>
///   <item><c>UnprocessableEntity</c> message → <see cref="StatusCode.FailedPrecondition"/></item>
///   <item><c>TooManyRequests</c> message → <see cref="StatusCode.ResourceExhausted"/></item>
///   <item>Anything else → <see cref="StatusCode.OK"/></item>
/// </list>
///
/// Matches by simple type name so any user-defined `NotFound`
/// message — regardless of namespace — picks up the right gRPC
/// status. Override via <see cref="Add{T}"/> when a particular
/// message type maps to a non-standard status.
/// </summary>
public sealed class SpekGrpcStatusMap
{
    private readonly Dictionary<Type, StatusCode>   _byType = new();
    private readonly Dictionary<string, StatusCode> _byName = new(StringComparer.Ordinal);

    /// <summary>
    /// Constructs an empty map. Use <see cref="WithBuiltInDefaults"/>
    /// to start from the conventional table — most apps want that
    /// and only add custom entries on top.
    /// </summary>
    public SpekGrpcStatusMap() { }

    public static SpekGrpcStatusMap WithBuiltInDefaults()
    {
        var m = new SpekGrpcStatusMap();
        m._byName["NotFound"]            = StatusCode.NotFound;
        m._byName["BadRequest"]          = StatusCode.InvalidArgument;
        m._byName["Unauthorized"]        = StatusCode.Unauthenticated;
        m._byName["Forbidden"]           = StatusCode.PermissionDenied;
        m._byName["Conflict"]            = StatusCode.AlreadyExists;
        m._byName["UnprocessableEntity"] = StatusCode.FailedPrecondition;
        m._byName["TooManyRequests"]     = StatusCode.ResourceExhausted;
        return m;
    }

    /// <summary>
    /// Pin a specific reply type to a status code. Replaces any
    /// existing mapping for the type.
    /// </summary>
    public SpekGrpcStatusMap Add<T>(StatusCode status)
    {
        _byType[typeof(T)] = status;
        return this;
    }

    /// <summary>
    /// Resolve the gRPC status for a reply object. Tried in order:
    /// type-equality match, simple-name match against the convention
    /// table, then <see cref="StatusCode.OK"/> as the default.
    /// </summary>
    public StatusCode ResolveFor(Type replyType)
    {
        if (_byType.TryGetValue(replyType, out var code))      return code;
        if (_byName.TryGetValue(replyType.Name, out var named)) return named;
        return StatusCode.OK;
    }
}
