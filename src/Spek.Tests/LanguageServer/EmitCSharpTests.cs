using OmniSharp.Extensions.LanguageServer.Protocol;
using Spek.LanguageServer;
using Xunit;

namespace Spek.Tests.LanguageServer;

/// <summary>
/// Coverage for <see cref="SpekEmitCSharpHandler"/> — the custom
/// <c>spek/emitCSharp</c> request behind the extension's experimental
/// "Show emitted C#" command.
/// </summary>
public class EmitCSharpTests
{
    private static async Task<EmitCSharpResult> EmitAsync(string? source)
    {
        var cache = new DocumentCache();
        var uri   = DocumentUri.From("file:///emit.spek");
        if (source is not null) cache.Update(uri, source);

        var handler = new SpekEmitCSharpHandler(cache);
        return await handler.Handle(new EmitCSharpParams { Uri = uri.ToString() }, CancellationToken.None);
    }

    [Fact]
    public async Task ValidSource_ReturnsEmittedCSharpAsync()
    {
        var result = await EmitAsync("""
            message Ping();
            actor Server
            {
                on Ping => { }
            }
            """);

        Assert.Null(result.Error);
        Assert.NotNull(result.CSharp);
        Assert.Contains("class Server", result.CSharp);   // the actor lowers to a C# class
        Assert.Contains("Ping", result.CSharp);           // the message type is present
    }

    [Fact]
    public async Task BrokenSource_ReturnsErrorAsync()
    {
        var result = await EmitAsync("actor {{{ this is not valid");

        Assert.Null(result.CSharp);
        Assert.NotNull(result.Error);
    }

    [Fact]
    public async Task UnopenedDocument_ReturnsErrorAsync()
    {
        var result = await EmitAsync(source: null);   // nothing put in the cache

        Assert.Null(result.CSharp);
        Assert.NotNull(result.Error);
    }
}
