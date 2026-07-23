using Spek.Hosting;
using Spek.Hosting.AspNetCore.Rest;
using Xunit;

namespace Spek.Tests.Hosting;

/// <summary>
/// Unit tests for the REST convention engine and the
/// status-code map. Doesn't boot a host; just exercises the
/// internal types directly via the same hand-rolled channel
/// metadata the compiler will emit.
/// </summary>
public sealed class RestConventionTests
{
    public sealed record GetUser(string id);
    public sealed record CreateUser(string name);
    public sealed record UpdateUser(string id, string name);
    public sealed record DeleteUser(string id);
    public sealed record ListUsers(int page = 1);
    public sealed record ChangeEmail(string id, string email);
    public sealed record User(string id);
    public sealed record NotFound(string reason);

    [SpekChannelMetadata(
        inputs: new[] { typeof(GetUser), typeof(CreateUser), typeof(UpdateUser),
                        typeof(DeleteUser), typeof(ListUsers), typeof(ChangeEmail) },
        emits: new[]  { typeof(User), typeof(NotFound) },
        emitsAny: false)]
    public interface UserApi { }

    // ─── Convention engine ─────────────────────────────────────────

    [Fact]
    public void Convention_GetUser_BecomesGetWithIdPlaceholder()
    {
        var routes = new ChannelRouteResolver<UserApi>().Routes().ToList();
        var r = Assert.Single(routes, r => r.MessageType == typeof(GetUser));
        Assert.Equal(HttpVerb.Get, r.Verb);
        Assert.Equal("/{id}", r.Template);
    }

    [Fact]
    public void Convention_CreateUser_BecomesPostWithoutId()
    {
        var routes = new ChannelRouteResolver<UserApi>().Routes().ToList();
        var r = Assert.Single(routes, r => r.MessageType == typeof(CreateUser));
        Assert.Equal(HttpVerb.Post, r.Verb);
        Assert.Equal("/", r.Template);
    }

    [Fact]
    public void Convention_UpdateUser_BecomesPutWithId()
    {
        var routes = new ChannelRouteResolver<UserApi>().Routes().ToList();
        var r = Assert.Single(routes, r => r.MessageType == typeof(UpdateUser));
        Assert.Equal(HttpVerb.Put, r.Verb);
        Assert.Equal("/{id}", r.Template);
    }

    [Fact]
    public void Convention_DeleteUser_BecomesDeleteWithId()
    {
        var routes = new ChannelRouteResolver<UserApi>().Routes().ToList();
        var r = Assert.Single(routes, r => r.MessageType == typeof(DeleteUser));
        Assert.Equal(HttpVerb.Delete, r.Verb);
        Assert.Equal("/{id}", r.Template);
    }

    [Fact]
    public void Convention_ListUsers_BecomesGetWithoutId()
    {
        // List-shaped GETs collect from the base path, even if a
        // message has an id field — the convention rule explicitly
        // skips the id placeholder for List* prefix.
        var routes = new ChannelRouteResolver<UserApi>().Routes().ToList();
        var r = Assert.Single(routes, r => r.MessageType == typeof(ListUsers));
        Assert.Equal(HttpVerb.Get, r.Verb);
        Assert.Equal("/", r.Template);
    }

    [Fact]
    public void Convention_ChangeEmail_DefaultsToPatchWithId()
    {
        // `Change*` prefix → PATCH; message has an `id` field, so the
        // path includes /{id}. Without an override, this is the
        // default route.
        var routes = new ChannelRouteResolver<UserApi>().Routes().ToList();
        var r = Assert.Single(routes, r => r.MessageType == typeof(ChangeEmail));
        Assert.Equal(HttpVerb.Patch, r.Verb);
        Assert.Equal("/{id}", r.Template);
    }

    [Fact]
    public void Override_ReplacesConvention()
    {
        var resolver = new ChannelRouteResolver<UserApi>();
        resolver.Configurator.Override<ChangeEmail>(HttpVerb.Patch, "/{id}/email");

        var r = Assert.Single(resolver.Routes().ToList(),
            r => r.MessageType == typeof(ChangeEmail));
        Assert.Equal("/{id}/email", r.Template);
    }

    // ─── Status code map ───────────────────────────────────────────

    [Fact]
    public void StatusMap_BuiltInDefaults_ResolveByName()
    {
        var map = SpekStatusCodeMap.WithBuiltInDefaults();
        Assert.Equal(404, map.ResolveWithNameFallback(typeof(NotFound)));
    }

    [Fact]
    public void StatusMap_UserType_DefaultsTo200()
    {
        var map = SpekStatusCodeMap.WithBuiltInDefaults();
        Assert.Equal(200, map.ResolveWithNameFallback(typeof(User)));
    }

    [Fact]
    public void StatusMap_UserOverride_TakesPrecedence()
    {
        var map = SpekStatusCodeMap.WithBuiltInDefaults();
        map.Add<User>(201);   // CreateUser convention → 201 Created
        Assert.Equal(201, map.ResolveWithNameFallback(typeof(User)));
    }
}
