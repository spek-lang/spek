using System.Reflection;
using Spek.Hosting;

namespace Spek.Hosting.AspNetCore.Rest;

/// <summary>
/// Given a Spek channel type (the marker interface emitted
/// by the compiler), produce the set of HTTP routes the channel
/// expects, applying the convention rules below and any user
/// overrides registered through <see cref="RouteConfigurator{T}"/>.
///
/// <para>
/// <b>Convention rules:</b>
/// </para>
/// <list type="bullet">
///   <item><b>Verb from message-name prefix.</b> <c>Get*</c> /
///   <c>List*</c> → GET; <c>Create*</c> / <c>Add*</c> → POST;
///   <c>Update*</c> / <c>Replace*</c> → PUT; <c>Patch*</c> /
///   <c>Change*</c> → PATCH; <c>Delete*</c> / <c>Remove*</c> →
///   DELETE. Default fallback: POST.</item>
///
///   <item><b>Path placeholders from `id` field.</b> If the input
///   message has a constructor parameter named <c>id</c> (case
///   insensitive), the route appends <c>/{id}</c>. List-style
///   handlers (<c>List*</c> prefix) skip this even when an `id`
///   is present, because list endpoints don't take a path id.</item>
///
///   <item><b>Field source (path / query / body).</b> Fields whose
///   names match the route's path placeholders bind from the path.
///   For verbs without a body (GET, DELETE, HEAD, OPTIONS), all
///   other fields bind from the query string. For body-bearing
///   verbs (POST, PUT, PATCH), all other fields bind from the
///   JSON request body.</item>
/// </list>
///
/// User overrides take precedence over the convention. The
/// configurator is the only place to express off-convention routes
/// — the channel and message decls themselves stay
/// transport-agnostic.
/// </summary>
internal sealed class ChannelRouteResolver<TChannel>
{
    private readonly Dictionary<Type, RouteOverride> _overrides = new();

    public RouteConfigurator<TChannel> Configurator { get; }

    public ChannelRouteResolver()
    {
        Configurator = new RouteConfigurator<TChannel>(_overrides);
    }

    /// <summary>
    /// Walks the channel's <see cref="SpekChannelMetadataAttribute"/>
    /// and yields a <see cref="RouteCandidate"/> for each input
    /// message type. Overrides registered through the configurator
    /// replace the convention-derived verb / template for that
    /// message type.
    /// </summary>
    public IEnumerable<RouteCandidate> Routes()
    {
        var metadata = typeof(TChannel).GetCustomAttribute<SpekChannelMetadataAttribute>()
            ?? throw new InvalidOperationException(
                $"Type '{typeof(TChannel)}' has no [SpekChannelMetadata] attribute. " +
                $"MapChannel only works on Spek-emitted channel marker interfaces. " +
                $"Did you pass an actor type or a regular C# interface by mistake?");

        foreach (var inputType in metadata.Inputs)
        {
            if (_overrides.TryGetValue(inputType, out var ov))
            {
                yield return new RouteCandidate(inputType, ov.Verb, ov.Template);
                continue;
            }

            var verb = DeriveVerb(inputType.Name);
            var template = DeriveTemplate(inputType, verb);
            yield return new RouteCandidate(inputType, verb, template);
        }
    }

    private static HttpVerb DeriveVerb(string messageName)
    {
        if (StartsWith(messageName, "Get") || StartsWith(messageName, "List"))
            return HttpVerb.Get;
        if (StartsWith(messageName, "Create") || StartsWith(messageName, "Add"))
            return HttpVerb.Post;
        if (StartsWith(messageName, "Update") || StartsWith(messageName, "Replace"))
            return HttpVerb.Put;
        if (StartsWith(messageName, "Patch") || StartsWith(messageName, "Change"))
            return HttpVerb.Patch;
        if (StartsWith(messageName, "Delete") || StartsWith(messageName, "Remove"))
            return HttpVerb.Delete;
        return HttpVerb.Post; // sensible default for unrecognised shapes
    }

    /// <summary>
    /// True when <paramref name="name"/> starts with
    /// <paramref name="prefix"/> AND the prefix is followed by an
    /// uppercase letter (so "List" matches "ListUsers" but not
    /// "ListenerStarted"). Avoids surprising false positives.
    /// </summary>
    private static bool StartsWith(string name, string prefix)
    {
        if (name.Length <= prefix.Length) return false;
        if (!name.StartsWith(prefix, StringComparison.Ordinal)) return false;
        return char.IsUpper(name[prefix.Length]);
    }

    private static string DeriveTemplate(Type inputType, HttpVerb verb)
    {
        // List-shaped GETs collect from the base path; no path id
        // even if the message has one for some reason.
        var isListLike = StartsWith(inputType.Name, "List");
        if (isListLike) return "/";

        var hasId = inputType.GetProperties()
            .Any(p => string.Equals(p.Name, "id", StringComparison.OrdinalIgnoreCase));
        return hasId ? "/{id}" : "/";
    }
}

/// <summary>
/// Describes a single (verb, path, message type) tuple
/// derived from a channel input. Internal to the resolver →
/// endpoint-mapping pipeline.
/// </summary>
internal sealed record RouteCandidate(Type MessageType, HttpVerb Verb, string Template);

internal sealed record RouteOverride(HttpVerb Verb, string Template);
