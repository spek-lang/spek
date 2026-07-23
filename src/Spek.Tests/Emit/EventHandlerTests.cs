using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Xunit;

namespace Spek.Tests.Emit;

/// <summary>
/// Coverage for the `on event Name(params) => body` form. The
/// compiler must:
///   1. Parse the new grammar alternative
///   2. Synthesise a private nested message record per event handler
///   3. Emit a bridge method on the actor class with the user's
///      chosen name and the verbatim delegate signature
///   4. Add a dispatch arm that unboxes the synthetic message back
///      into the user's parameter names
/// </summary>
public sealed class EventHandlerTests
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
    public void EventHandler_Parses()
    {
        const string src = """
            actor LogProcessor
            {
                behavior Idle
                {
                    on event FileChanged(object s, System.IO.FileSystemEventArgs e) => { }
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.True(parsed.Success,
            "Source must parse; diagnostics: " +
            string.Join("\n", parsed.Diagnostics.Select(d => $"{d.Code} {d.Line}:{d.Column} {d.Message}")));
    }

    [Fact]
    public void EventHandler_EmitsSyntheticRecord()
    {
        const string src = """
            actor LogProcessor
            {
                behavior Idle
                {
                    on event FileChanged(object s, System.IO.FileSystemEventArgs e) => { }
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("private sealed record __Event_FileChanged(object s, System.IO.FileSystemEventArgs e)", code);
    }

    [Fact]
    public void EventHandler_EmitsBridgeMethod()
    {
        const string src = """
            actor LogProcessor
            {
                behavior Idle
                {
                    on event FileChanged(object s, System.IO.FileSystemEventArgs e) => { }
                }
            }
            """;
        var code = EmitCSharp(src);
        // OnHandler default visibility is public (matches existing convention).
        // Bridge method visibility tracks the on-handler.
        Assert.Contains("public void FileChanged(object s, System.IO.FileSystemEventArgs e)", code);
        Assert.Contains("=> _selfRef.Tell(new __Event_FileChanged(s, e))", code);
    }

    [Fact]
    public void EventHandler_EmitsDispatchArm()
    {
        const string src = """
            actor LogProcessor
            {
                behavior Idle
                {
                    on event FileChanged(object s, System.IO.FileSystemEventArgs e) => { }
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("case __Event_FileChanged __ev:", code);
        Assert.Contains("var s = __ev.s;", code);
        Assert.Contains("var e = __ev.e;", code);
    }

    [Fact]
    public void EventHandler_BodyCanReferenceParams()
    {
        const string src = """
            actor LogProcessor
            {
                string lastPath = "";
                behavior Idle
                {
                    on event FileChanged(object s, System.IO.FileSystemEventArgs e) =>
                    {
                        lastPath = e.FullPath;
                    }
                }
            }
            """;
        var code = EmitCSharp(src);
        // The body's `e.FullPath` must reach the unboxed local, and
        // `lastPath` must reach the actor field with its underscore prefix.
        Assert.Contains("var e = __ev.e;", code);
        Assert.Contains("_lastPath = e.FullPath;", code);
    }

    [Fact]
    public void EventHandler_PrivateVisibility_EmitsPrivateBridge()
    {
        const string src = """
            actor LogProcessor
            {
                behavior Idle
                {
                    private on event FileChanged(object s, System.IO.FileSystemEventArgs e) => { }
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("private void FileChanged(", code);
    }

    [Fact]
    public void MultipleEventHandlers_BothEmit()
    {
        const string src = """
            actor LogProcessor
            {
                behavior Idle
                {
                    on event FileChanged(object s, System.IO.FileSystemEventArgs e) => { }
                    on event FileDeleted(object s, System.IO.FileSystemEventArgs e) => { }
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("__Event_FileChanged", code);
        Assert.Contains("__Event_FileDeleted", code);
        Assert.Contains("public void FileChanged(", code);
        Assert.Contains("public void FileDeleted(", code);
    }

    [Fact]
    public void DuplicateEventHandlerName_EmitsCE0013()
    {
        const string src = """
            actor LogProcessor
            {
                behavior Idle
                {
                    on event FileChanged(object s, System.IO.FileSystemEventArgs e) => { }
                    on event FileChanged(object s, System.IO.RenamedEventArgs e) => { }
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.Contains(parsed.Diagnostics,
            d => d.Code == "CE0013" && d.Message.Contains("on event FileChanged"));
    }

    [Fact]
    public void EventHandler_FullActorWithInitWiring_RoundTripsThroughRoslyn()
    {
        // The whole point of the design: user writes the `+=` in init
        // with a method-group reference to the bridge name; Roslyn
        // resolves the conversion against the FileSystemWatcher event
        // delegate. With implicit Default, no behavior wrapper
        // is required — bare handlers fold into `behavior Default`.
        const string src = """
            actor LogProcessor
            {
                System.IO.FileSystemWatcher? watcher;
                string lastPath = "";

                init(string path)
                {
                    watcher = new System.IO.FileSystemWatcher(path);
                    watcher.Changed += FileChanged;
                    watcher.Deleted += FileDeleted;
                }

                on event FileChanged(object s, System.IO.FileSystemEventArgs e) =>
                {
                    lastPath = e.FullPath;
                }

                on event FileDeleted(object s, System.IO.FileSystemEventArgs e) =>
                {
                    lastPath = "";
                }
            }
            """;
        var code = EmitCSharp(src);
        var (success, errors, _) = RoslynCompileHelper.TryCompile(code, "EventHandlerSmoke");
        Assert.True(success,
            "Emitted C# did not compile:\n" + errors + "\n--- emitted ---\n" + code);
    }
}
