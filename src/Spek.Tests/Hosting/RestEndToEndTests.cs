using System.Net;
using System.Net.Http.Json;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Spek;
using Spek.Hosting;
using Spek.Hosting.AspNetCore.Rest;
using Spek.Runtime;
using Xunit;

namespace Spek.Tests.Hosting;

/// <summary>
/// End-to-end smoke tests for the REST hosting bridge.
/// Boot a real <c>WebApplication</c> in-process, fire real HTTP
/// requests via <c>HttpClient</c>, assert status codes and JSON
/// payloads. Exercises the full path:
/// HTTP → ASP.NET Core endpoint routing → <c>SpekMessageBinder</c>
/// → actor system <c>ask</c> → reply → <c>SpekStatusCodeMap</c>
/// → JSON response.
///
/// Channels and actors here are hand-written C# instead of
/// Spek-emitted code so the tests don't depend on the compiler
/// pipeline being clean. The compiler emit is covered separately
/// in <see cref="Emit.ChannelEmitTests"/>.
/// </summary>
public sealed class RestEndToEndTests
{
    // ─── Test channel + actor (hand-written analogue of compiler emit) ──────

    public sealed record GetUser(string id);
    public sealed record CreateUser(string name, string email);
    public sealed record ListUsers(int page = 1, int pageSize = 20);
    public sealed record User(string id, string name, string email);
    public sealed record NotFound(string reason);

    [SpekChannelMetadata(
        inputs: new[] { typeof(GetUser), typeof(CreateUser), typeof(ListUsers) },
        emits: new[]  { typeof(User), typeof(NotFound) },
        emitsAny: false)]
    public interface UserApi { }

    public sealed class UserService : ActorBase, UserApi
    {
        protected override Task DispatchAsync(object message, ActorRef sender)
        {
            object reply = message switch
            {
                GetUser g     => g.id == "nope"
                                    ? new NotFound($"user {g.id} not found")
                                    : new User(g.id, "Alice", "alice@example.com"),
                CreateUser c  => new User("u-" + c.name, c.name, c.email),
                ListUsers l   => new User("u-1", "Alice", "alice@example.com"),
                _             => new NotFound("unhandled"),
            };
            sender.Tell(reply, _selfRef!);
            return Task.CompletedTask;
        }
    }

    // ─── Test harness ──────────────────────────────────────────────────────

    /// <summary>
    /// Spawns a <c>WebApplication</c> on an arbitrary free port,
    /// returns an HttpClient + a disposable that shuts it down.
    /// </summary>
    private static async Task<(HttpClient Http, IAsyncDisposable Lifetime)> StartHostAsync()
    {
        var builder = WebApplication.CreateBuilder();
        builder.Services.AddSpekActorSystem("test-rest-host");
        // Pick a free ephemeral port — avoids collisions with anything
        // running on the dev machine.
        builder.WebHost.UseUrls("http://127.0.0.1:0");

        var app = builder.Build();
        app.MapChannel<UserApi, UserService>("/users");

        await app.StartAsync();

        var addr = app.Urls.FirstOrDefault()
            ?? throw new InvalidOperationException("No address bound by WebApplication.");
        var http = new HttpClient { BaseAddress = new Uri(addr) };

        return (http, new HostLifetime(http, app));
    }

    private sealed class HostLifetime(HttpClient http, WebApplication app) : IAsyncDisposable
    {
        public async ValueTask DisposeAsync()
        {
            http.Dispose();
            await app.StopAsync();
            await app.DisposeAsync();
        }
    }

    // ─── Tests ─────────────────────────────────────────────────────────────

    [Fact]
    public async Task Get_HappyPath_Returns200WithUser()
    {
        var (http, lifetime) = await StartHostAsync();
        await using var _ = lifetime;

        var resp = await http.GetAsync("/users/u-1");
        Assert.Equal(HttpStatusCode.OK, resp.StatusCode);

        var user = await resp.Content.ReadFromJsonAsync<User>();
        Assert.NotNull(user);
        Assert.Equal("u-1", user!.id);
        Assert.Equal("Alice", user.name);
    }

    [Fact]
    public async Task Get_NotFound_Returns404FromEmitTypeMapping()
    {
        var (http, lifetime) = await StartHostAsync();
        await using var _ = lifetime;

        var resp = await http.GetAsync("/users/nope");
        Assert.Equal(HttpStatusCode.NotFound, resp.StatusCode);

        var body = await resp.Content.ReadFromJsonAsync<NotFound>();
        Assert.NotNull(body);
        Assert.Contains("nope", body!.reason);
    }

    [Fact]
    public async Task Post_CreatesUser_Returns200WithCreatedRecord()
    {
        var (http, lifetime) = await StartHostAsync();
        await using var _ = lifetime;

        var resp = await http.PostAsJsonAsync("/users",
            new { name = "Bob", email = "bob@example.com" });

        Assert.Equal(HttpStatusCode.OK, resp.StatusCode);
        var user = await resp.Content.ReadFromJsonAsync<User>();
        Assert.NotNull(user);
        Assert.Equal("Bob", user!.name);
        Assert.Equal("bob@example.com", user.email);
    }

    [Fact]
    public async Task ListUsers_BindsQueryParams()
    {
        var (http, lifetime) = await StartHostAsync();
        await using var _ = lifetime;

        var resp = await http.GetAsync("/users?page=2&pageSize=50");
        Assert.Equal(HttpStatusCode.OK, resp.StatusCode);

        var user = await resp.Content.ReadFromJsonAsync<User>();
        Assert.NotNull(user);
        // The handler returns a fixed user; we just verify the
        // request bound and the actor produced a reply with the
        // expected emit type.
        Assert.Equal("u-1", user!.id);
    }
}
