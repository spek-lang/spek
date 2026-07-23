using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Xunit;

namespace Spek.Tests.Emit;

/// <summary>
/// `term { }` block, the disposal counterpart to `init { }`.
/// Triggers `IAsyncDisposable` emission on the generated actor or
/// region class. The runtime hooks:
///   * Actors — `ActorSlot` calls `InvokeOnTerm()` after
///     `InvokeOnPostStop()` in the stop sequence.
///   * Regions — `ActorSystem.Dispose` walks
///     `_regionConstructionOrder` LIFO and calls `OnTerm()` on each.
/// </summary>
public sealed class TermBlockTests
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
    public void Actor_WithTermBlock_ImplementsIAsyncDisposable()
    {
        const string src = """
            message Tick();
            actor A
            {
                int n = 0;
                term
                {
                    n = -1;
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("System.IAsyncDisposable", code);
        Assert.Contains("protected override void OnTerm()", code);
        Assert.Contains("public System.Threading.Tasks.ValueTask DisposeAsync()", code);
        Assert.Contains("OnTerm();", code);
    }

    [Fact]
    public void Actor_WithoutTermBlock_DoesNotImplementIAsyncDisposable()
    {
        // Sanity check: actors without `term { }` don't pay for the
        // disposable shape. Confirms the trigger is purely the
        // presence of the block.
        const string src = """
            message Tick();
            actor A
            {
                int n = 0;
                on Tick =>
                {
                    n = n + 1;
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.DoesNotContain("System.IAsyncDisposable", code);
        Assert.DoesNotContain("OnTerm()", code);
        Assert.DoesNotContain("DisposeAsync", code);
    }

    [Fact]
    public void Region_WithTermBlock_ImplementsIAsyncDisposable()
    {
        const string src = """
            shared Cache
            {
                int hits = 0;
                term
                {
                    hits = -1;
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("System.IAsyncDisposable", code);
        Assert.Contains("protected override void OnTerm()", code);
        Assert.Contains("public System.Threading.Tasks.ValueTask DisposeAsync()", code);
    }

    [Fact]
    public void Region_WithoutTermBlock_DoesNotImplementIAsyncDisposable()
    {
        const string src = """
            shared Cache
            {
                int hits = 0;
            }
            """;
        var code = EmitCSharp(src);
        Assert.DoesNotContain("System.IAsyncDisposable", code);
        Assert.DoesNotContain("OnTerm()", code);
    }

    [Fact]
    public void Actor_WithBothPostStopAndTerm_EmitsBoth()
    {
        // `on PostStop` and `term` are distinct hooks: PostStop is for
        // logging / final notifications, term is for resource cleanup.
        // Both should emit when both are declared.
        const string src = """
            message Tick();
            actor A
            {
                int n = 0;

                on PostStop =>
                {
                    n = 0;
                }

                term
                {
                    n = -1;
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("protected override void OnPostStop()", code);
        Assert.Contains("protected override void OnTerm()", code);
        Assert.Contains("System.IAsyncDisposable", code);
    }

    [Fact]
    public void Actor_TermBlock_RoundTripsThroughRoslyn()
    {
        // Note: avoid interpolated-string field references in term
        // bodies — the existing emitter doesn't rewrite identifiers
        // inside `$"..."` strings (orthogonal pre-existing limitation,
        // not specific to term blocks). Plain assignments work.
        const string src = """
            message Tick();
            actor A
            {
                int n = 0;

                on Tick =>
                {
                    n = n + 1;
                }

                term
                {
                    n = -1;
                }
            }
            """;
        var code = EmitCSharp(src);
        var (success, errors, _) = RoslynCompileHelper.TryCompile(code, "ActorTermSmoke");
        Assert.True(success,
            "Emitted C# did not compile:\n" + errors + "\n--- emitted ---\n" + code);
    }

    [Fact]
    public void Region_TermBlock_RoundTripsThroughRoslyn()
    {
        const string src = """
            shared Cache
            {
                int hits = 0;

                init
                {
                    hits = 0;
                }

                term
                {
                    hits = -1;
                }
            }
            """;
        var code = EmitCSharp(src);
        var (success, errors, _) = RoslynCompileHelper.TryCompile(code, "RegionTermSmoke");
        Assert.True(success,
            "Emitted C# did not compile:\n" + errors + "\n--- emitted ---\n" + code);
    }

    [Fact]
    public void Actor_TermBlock_ScopeRules_BlocksAsk()
    {
        // CE0042 — `ask` is only allowed inside an on-handler body.
        // Term inherits the same restriction (it's not an OnHandler scope).
        const string src = """
            message Ping();
            message Pong();
            actor A
            {
                term
                {
                    var p = self.Ask(new Ping());
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.False(parsed.Success);
        Assert.Contains(parsed.Diagnostics, d => d.Code == "CE0042");
    }

    [Fact]
    public void CE0110_ActorWithDisposableField_NoTerm_Warns()
    {
        // FileSystemWatcher is a known-disposable type. An actor that
        // holds one without a `term { }` block likely leaks the
        // watcher when the actor stops.
        const string src = """
            using System.IO;

            message Tick();
            actor A
            {
                FileSystemWatcher? watcher;
                on Tick =>
                {
                }
            }
            """;
        var parsed = Spek.Compiler.Parser.SpekCompiler.Parse(src);
        Assert.True(parsed.Success,
            "Warning shouldn't fail the build: " +
            string.Join("\n", parsed.Diagnostics.Select(d => $"{d.Code} {d.Severity}")));
        Assert.Contains(parsed.Diagnostics, d =>
            d.Code == "CE0110" &&
            d.Severity == Spek.Compiler.Semantic.DiagnosticSeverity.Warning);
    }

    [Fact]
    public void CE0110_ActorWithDisposableField_AndTerm_NoWarning()
    {
        const string src = """
            using System.IO;

            message Tick();
            actor A
            {
                FileSystemWatcher? watcher;
                on Tick =>
                {
                }
                term
                {
                    if (watcher != null) watcher.Dispose();
                }
            }
            """;
        var parsed = Spek.Compiler.Parser.SpekCompiler.Parse(src);
        Assert.DoesNotContain(parsed.Diagnostics, d => d.Code == "CE0110");
    }

    [Fact]
    public void CE0110_RegionWithDisposableField_NoTerm_Warns()
    {
        const string src = """
            using System.Net.Http;

            shared HttpPool
            {
                HttpClient? client;
            }
            """;
        var parsed = Spek.Compiler.Parser.SpekCompiler.Parse(src);
        Assert.True(parsed.Success);
        Assert.Contains(parsed.Diagnostics, d =>
            d.Code == "CE0110" &&
            d.Severity == Spek.Compiler.Semantic.DiagnosticSeverity.Warning);
    }

    [Fact]
    public void CE0110_PlainFields_NoWarning()
    {
        // Sanity check: no warning for plain primitive / record fields.
        const string src = """
            message Tick();
            actor A
            {
                int n = 0;
                string label = "";
                on Tick =>
                {
                    n = n + 1;
                }
            }
            """;
        var parsed = Spek.Compiler.Parser.SpekCompiler.Parse(src);
        Assert.DoesNotContain(parsed.Diagnostics, d => d.Code == "CE0110");
    }

    [Fact]
    public void Actor_TermBlock_ScopeRules_BlocksPersist()
    {
        // CE0050 — `persist` is only allowed inside an on-handler body.
        // Term inherits the restriction (regions persist via writer-
        // exit; actors via `persist;` in handler bodies; either way
        // term is not the place).
        const string src = """
            message Tick();
            actor A
            {
                int n = 0;

                on Tick =>
                {
                    n = n + 1;
                    persist;
                }

                on Restore(Snapshot s) =>
                {
                    n = s.Get<int>("n");
                }

                term
                {
                    persist;
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.False(parsed.Success);
        Assert.Contains(parsed.Diagnostics, d => d.Code == "CE0050");
    }
}
