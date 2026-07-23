using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Xunit;

namespace Spek.Tests.Emit;

/// <summary>
/// Bare `on` / `on event` handlers at actor scope fold into a
/// synthesised behavior named "Default". The name is fixed so stack
/// traces and supervision logs stay readable. Auto-become picks the
/// first behavior in source order; for bare-only actors that's
/// always Default.
/// </summary>
public sealed class ImplicitDefaultBehaviorTests
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
    public void BareOnHandler_FoldsIntoDefaultBehavior()
    {
        const string src = """
            message Tick();
            actor Ticker
            {
                on Tick => { }
            }
            """;
        var code = EmitCSharp(src);
        // Synthetic behavior name shows up in the dispatch / classifier
        // method names, which surface in stack traces.
        Assert.Contains("Default_HandleAsync", code);
        Assert.Contains("_behavior = Default_HandleAsync;", code);
    }

    [Fact]
    public void BareOnHandler_DispatchArmEmittedInDefault()
    {
        const string src = """
            message Inc();
            actor Counter
            {
                int n = 0;
                on Inc => { n = n + 1; }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("private async Task Default_HandleAsync", code);
        Assert.Contains("case Inc _:", code);
    }

    [Fact]
    public void BareOnEventHandler_FoldsIntoDefaultBehavior()
    {
        const string src = """
            actor LogProcessor
            {
                System.IO.FileSystemWatcher? watcher;
                init(string path)
                {
                    watcher = new System.IO.FileSystemWatcher(path);
                    watcher.Changed += FileChanged;
                }
                on event FileChanged(object s, System.IO.FileSystemEventArgs e) => { }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("Default_HandleAsync", code);
        Assert.Contains("__Event_FileChanged", code);
    }

    [Fact]
    public void BareOnHandler_RoundTripsThroughRoslyn()
    {
        const string src = """
            message Tick();
            message Reply(int count);
            actor Ticker
            {
                int count = 0;
                on Tick => { count = count + 1; return new Reply(count); }
            }
            """;
        var code = EmitCSharp(src);
        var (success, errors, _) = RoslynCompileHelper.TryCompile(code, "BareDefaultSmoke");
        Assert.True(success,
            "Emitted C# did not compile:\n" + errors + "\n--- emitted ---\n" + code);
    }

    [Fact]
    public void BareOnEventHandler_RoundTripsThroughRoslyn()
    {
        // The motivating example from the design conversation. With
        // implicit Default, the actor reads as "just the handlers"
        // — no behavior wrapper required.
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
        var (success, errors, _) = RoslynCompileHelper.TryCompile(code, "BareEventDefaultSmoke");
        Assert.True(success,
            "Emitted C# did not compile:\n" + errors + "\n--- emitted ---\n" + code);
    }

    [Fact]
    public void ExplicitBehavior_StillWorks_UnaffectedByImplicitDefault()
    {
        // Existing actors must keep emitting unchanged when
        // they don't use bare handlers.
        const string src = """
            message Inc();
            actor Counter
            {
                int n = 0;
                behavior Active
                {
                    on Inc => { n = n + 1; }
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("Active_HandleAsync", code);
        Assert.DoesNotContain("Default_HandleAsync", code);
        Assert.Contains("_behavior = Active_HandleAsync;", code);
    }

    [Fact]
    public void MixedMode_WithoutExplicitBecomeDefault_FlagsCE0014()
    {
        // Mixed mode (some bare handlers, some inside an explicit
        // `behavior X {}`) is allowed at parse, but the synthesized
        // Default behavior must be reached via `become Default;` —
        // otherwise CE0014 (the existing "behavior unreachable" rule)
        // fires. This is a clean signal: the user opted into multiple
        // behaviors so they need to disambiguate the entry point.
        const string src = """
            message Inc();
            message Reset();
            actor Counter
            {
                int n = 0;
                behavior Active
                {
                    on Inc => { n = n + 1; }
                }
                on Reset => { n = 0; }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.Contains(parsed.Diagnostics,
            d => d.Code == "CE0014" && d.Message.Contains("'Default'"));
    }

    [Fact]
    public void MixedMode_WithBecomeDefault_Compiles()
    {
        // The escape hatch: explicit `become Default;` inside the
        // explicit behavior makes Default reachable, both behaviors
        // emit, and the user can switch between them.
        const string src = """
            message Inc();
            message Reset();
            actor Counter
            {
                int n = 0;
                behavior Active
                {
                    on Inc    => { n = n + 1; }
                    on Reset  => { become Default; }
                }
                on Reset => { n = 0; become Active; }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("Active_HandleAsync", code);
        Assert.Contains("Default_HandleAsync", code);
        Assert.Contains("_behavior = Active_HandleAsync;", code);
    }
}
