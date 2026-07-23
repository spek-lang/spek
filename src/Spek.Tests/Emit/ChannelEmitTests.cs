using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Xunit;

namespace Spek.Tests.Emit;

/// <summary>
/// Channels emit as marker C# interfaces decorated with
/// <c>[Spek.Hosting.SpekChannelMetadata]</c>. Hosting adapters
/// (REST, future gRPC, queue, etc.) read the attribute via
/// reflection to recover the channel's input/emit message sets.
/// Actors that implement a channel emit as <c>: ChannelName</c> on
/// the C# class so the implementation relationship survives at
/// runtime — letting reflection on the actor type discover its
/// channels through the standard <c>type.GetInterfaces()</c> path.
/// </summary>
public sealed class ChannelEmitTests
{
    private static string EmitCSharp(string source)
    {
        var parsed = SpekCompiler.Parse(source);
        Assert.True(parsed.Success,
            "Source must parse; diagnostics: " +
            string.Join("\n", parsed.Diagnostics.Select(d => $"{d.Code} {d.Line}:{d.Column} {d.Message}")));
        return new FileEmitter().Emit(parsed.Tree!);
    }

    [Fact]
    public void Channel_EmitsMarkerInterface()
    {
        const string src = """
            message GetUser(string id);
            message User(string id, string name);

            channel UserApi
            {
                on GetUser;
                emits User;
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("internal interface UserApi", code);
    }

    [Fact]
    public void Channel_EmitsSpekChannelMetadataAttribute_WithInputsAndEmits()
    {
        const string src = """
            message GetUser(string id);
            message CreateUser(string name);
            message User(string id, string name);
            message NotFound(string reason);

            channel UserApi
            {
                on GetUser;
                on CreateUser;
                emits User;
                emits NotFound;
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("[global::Spek.Hosting.SpekChannelMetadata(", code);
        Assert.Contains("typeof(GetUser)",    code);
        Assert.Contains("typeof(CreateUser)", code);
        Assert.Contains("typeof(User)",       code);
        Assert.Contains("typeof(NotFound)",   code);
        Assert.Contains("emitsAny: false",    code);
    }

    [Fact]
    public void Channel_EmitsAnyAdvisory_SetsEmitsAnyTrue()
    {
        const string src = """
            message GetUser(string id);

            channel UserApi
            {
                on GetUser;
                emits any;
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("emitsAny: true", code);
        // Empty emits array when the channel uses advisory mode. Must be an
        // array-creation expression, not Array.Empty<T>() — attribute
        // arguments can't be method calls (CS0182).
        Assert.Contains("emits: new global::System.Type[] { }", code);
    }

    [Fact]
    public void Channel_NoMembers_EmitsEmptyArrays()
    {
        const string src = """
            channel Ping { }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("inputs: new global::System.Type[] { }", code);
        Assert.Contains("emits: new global::System.Type[] { }",  code);
        Assert.Contains("emitsAny: false", code);
    }

    [Fact]
    public void Channel_PublicVisibility_EmitsPublicInterface()
    {
        const string src = """
            message GetUser(string id);

            public channel UserApi
            {
                on GetUser;
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("public interface UserApi", code);
    }

    [Fact]
    public void ChannelInheritance_FlattensInputsAndEmits()
    {
        // Derived channels add inputs/emits to the base set; the
        // emitted metadata carries the linearised flat set.
        const string src = """
            message Ping();
            message Pong();
            message Reset();

            channel BasicApi
            {
                on Ping;
                emits Pong;
            }

            channel ExtendedApi : BasicApi
            {
                on Reset;
            }
            """;
        var code = EmitCSharp(src);

        // ExtendedApi's metadata should include inputs from itself
        // (Reset) and from BasicApi (Ping). Same for emits (Pong
        // inherited).
        var extendedSection = code.Substring(code.IndexOf("interface ExtendedApi"));
        // Walk back from the interface name to find its attribute.
        var attrStart = code.LastIndexOf("[global::Spek.Hosting.SpekChannelMetadata(",
            code.IndexOf("interface ExtendedApi"));
        Assert.NotEqual(-1, attrStart);
        var extendedAttr = code.Substring(attrStart,
            code.IndexOf("interface ExtendedApi") - attrStart);

        Assert.Contains("typeof(Reset)", extendedAttr);
        Assert.Contains("typeof(Ping)",  extendedAttr);
        Assert.Contains("typeof(Pong)",  extendedAttr);
    }

    [Fact]
    public void Actor_ImplementingChannel_EmitsChannelInBaseList()
    {
        const string src = """
            message GetUser(string id);
            message User(string id, string name);

            channel UserApi
            {
                on GetUser;
                emits User;
            }

            actor UserService : UserApi
            {
                on GetUser g => return new User(g.id, "Alice");
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("class UserService : Spek.ActorBase, UserApi", code);
    }

    [Fact]
    public void Actor_ImplementingMultipleChannels_EmitsAllInBaseList()
    {
        const string src = """
            message Ping();
            message Pong();
            message Tick();

            channel PingApi  { on Ping; emits Pong; }
            channel TickApi  { on Tick; }

            actor PingTicker : PingApi, TickApi
            {
                on Ping => return new Pong();
                on Tick => { }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("class PingTicker : Spek.ActorBase, PingApi, TickApi", code);
    }

    [Fact]
    public void ChannelEmit_RoundTripsThroughRoslyn()
    {
        // The marker interface + attribute must compile cleanly
        // under Roslyn with the Spek.Hosting.Abstractions reference
        // available (where SpekChannelMetadataAttribute lives).
        const string src = """
            message GetUser(string id);
            message User(string id, string name);

            channel UserApi
            {
                on GetUser;
                emits User;
            }

            actor UserService : UserApi
            {
                on GetUser g => return new User(g.id, "Alice");
            }
            """;
        var code = EmitCSharp(src);
        var (success, errors, _) = RoslynCompileHelper.TryCompile(code, "ChannelEmitSmoke");
        Assert.True(success,
            "Emitted C# did not compile:\n" + errors + "\n--- emitted ---\n" + code);
    }

    [Fact]
    public void ChannelWithNoEmits_RoundTripsThroughRoslyn()
    {
        // A channel with inputs but no emits must still emit valid C#. The
        // empty emits array has to be an array-creation expression — emitting
        // Array.Empty<T>() (a method call) is a CS0182 inside the attribute,
        // so any inputs-only (or emits-only) channel produced uncompilable C#.
        const string src = """
            message Shutdown();
            message Reboot();

            channel ConsoleHost
            {
                on Shutdown;
                on Reboot;
            }

            actor MyApp : ConsoleHost
            {
                on Shutdown => { }
                on Reboot   => { }
            }
            """;
        var code = EmitCSharp(src);
        var (success, errors, _) = RoslynCompileHelper.TryCompile(code, "ChannelNoEmitsSmoke");
        Assert.True(success,
            "Emitted C# did not compile:\n" + errors + "\n--- emitted ---\n" + code);
    }
}
