using Spek.Compiler.Parser;
using Spek.Compiler.Semantic;
using Xunit;

namespace Spek.Tests.Semantic;

/// <summary>
/// CE0107. An explicit <c>Task&lt;T&gt;</c> / <c>ValueTask&lt;T&gt;</c>
/// local is the invisible-async escape hatch: it keeps the raw Task instead
/// of auto-awaiting at the binding. But a lazy <c>var</c> already defers and
/// auto-awaits at the use site (running concurrently the same way), so the
/// only remaining reason to name the Task type is to consume the value *as a
/// Task* (hand it to a Task-shaped API, forward it, capture it). When a
/// top-level explicit-Task local is never used in a task context, the
/// annotation has no effect — <c>var</c> would behave identically — so it is
/// redundant.
///
/// The check is sound-by-construction: it warns only for top-level locals
/// (where <c>var</c> provably defers the same way) and suppresses on any use
/// that could be a task context (member access, call receiver, any argument,
/// <c>ref</c>/<c>out</c>, a bare <c>return t</c>, an assignment, or rebinding
/// to another Task local). Warning severity — it never blocks a build.
/// </summary>
public sealed class RedundantTaskAnnotationTests
{
    private static IReadOnlyList<Diagnostic> Diagnose(string src)
    {
        var parsed = SpekCompiler.Parse(src);
        Assert.True(parsed.Success,
            "CE0107 is a warning and must not fail the build; diagnostics: " +
            string.Join("\n", parsed.Diagnostics.Select(d => $"{d.Code} {d.Line}:{d.Column} {d.Message}")));
        return parsed.Diagnostics;
    }

    private static void AssertWarns(string src)
    {
        var diag = Assert.Single(Diagnose(src), d => d.Code == "CE0107");
        Assert.Equal(DiagnosticSeverity.Warning, diag.Severity);
    }

    private static void AssertNoWarning(string src) =>
        Assert.DoesNotContain(Diagnose(src), d => d.Code == "CE0107");

    [Fact]
    public void ExplicitTaskLocal_NeverUsed_Warns()
    {
        // The textbook redundant hatch: the Task is bound to an explicit
        // type and then abandoned. The structured join awaits it, so the
        // annotation buys nothing a bare call / `var` wouldn't.
        const string src = """
            module Io
            {
                public void Peek(string path)
                {
                    Task<string> t = System.IO.File.ReadAllTextAsync(path);
                }
            }
            """;
        AssertWarns(src);
    }

    [Fact]
    public void ExplicitTaskLocal_UsedOnlyAsValue_Warns()
    {
        // `n` is consumed in arithmetic — a value position. `var` would
        // defer + auto-await at the use identically, so the Task type does
        // nothing.
        const string src = """
            module Math
            {
                public int Plus()
                {
                    Task<int> n = System.Threading.Tasks.Task.FromResult(5);
                    return n + 1;
                }
            }
            """;
        AssertWarns(src);
    }

    [Fact]
    public void ExplicitTaskLocals_ConcurrentValueUse_Warns()
    {
        // The classic "explicit Task for concurrency" pattern — but lazy
        // `var` is already concurrent for top-level locals, so this is
        // exactly the redundancy CE0107 teaches away from. Both locals warn.
        const string src = """
            module Math
            {
                public int Sum()
                {
                    Task<int> a = System.Threading.Tasks.Task.FromResult(1);
                    Task<int> b = System.Threading.Tasks.Task.FromResult(2);
                    return a + b;
                }
            }
            """;
        var diags = Diagnose(src).Where(d => d.Code == "CE0107").ToList();
        Assert.Equal(2, diags.Count);
    }

    [Fact]
    public void ValueTaskLocal_NeverUsed_Warns()
    {
        const string src = """
            module Io
            {
                public void Peek()
                {
                    ValueTask<int> t = System.Threading.Tasks.ValueTask.FromResult(1);
                }
            }
            """;
        AssertWarns(src);
    }

    [Fact]
    public void ExplicitTaskLocal_Forwarded_NoWarning()
    {
        // `return t` hands the raw Task back to the caller — a genuine
        // task-context use. The explicit type is doing real work.
        const string src = """
            module Io
            {
                public Task<string> Get(string path)
                {
                    Task<string> t = System.IO.File.ReadAllTextAsync(path);
                    return t;
                }
            }
            """;
        AssertNoWarning(src);
    }

    [Fact]
    public void ExplicitTaskLocal_PassedAsArgument_NoWarning()
    {
        // Handed to a Task-shaped API (`Task.WhenAll`) — keeping the Task is
        // the whole point.
        const string src = """
            module Io
            {
                public void Both(string path)
                {
                    Task<string> t = System.IO.File.ReadAllTextAsync(path);
                    System.Threading.Tasks.Task.WhenAll(t);
                }
            }
            """;
        AssertNoWarning(src);
    }

    [Fact]
    public void ExplicitTaskLocal_MemberAccess_NoWarning()
    {
        // A member access could be a Task member (`.ConfigureAwait`,
        // `.Result`, ...). We can't tell at the Spek layer, so we suppress
        // — sound over complete.
        const string src = """
            module Io
            {
                public void Tune(string path)
                {
                    Task<string> t = System.IO.File.ReadAllTextAsync(path);
                    t.ConfigureAwait(false);
                }
            }
            """;
        AssertNoWarning(src);
    }

    [Fact]
    public void VarLocal_NoWarning()
    {
        // `var` is not the hatch — it never carries a redundant annotation.
        const string src = """
            module Io
            {
                public int Len(string path)
                {
                    var content = System.IO.File.ReadAllTextAsync(path);
                    return content.Length;
                }
            }
            """;
        AssertNoWarning(src);
    }

    [Fact]
    public void ExplicitValueTypeLocal_NoWarning()
    {
        // `string content = <Task call>` awaits at the binding (value-typed
        // target). It isn't a Task local at all, so CE0107 doesn't apply.
        const string src = """
            module Io
            {
                public int Len(string path)
                {
                    string content = System.IO.File.ReadAllTextAsync(path);
                    return content.Length;
                }
            }
            """;
        AssertNoWarning(src);
    }

    [Fact]
    public void NestedTaskLocal_NotTopLevel_NoWarning()
    {
        // Scope boundary: only top-level locals are checked, because that's
        // where lazy `var` provably defers the same way. A Task local inside
        // a nested block may defer differently than `var` would, so we don't
        // second-guess it.
        const string src = """
            module Io
            {
                public void F(string path)
                {
                    if (path.Length > 0)
                    {
                        Task<string> t = System.IO.File.ReadAllTextAsync(path);
                    }
                }
            }
            """;
        AssertNoWarning(src);
    }

    [Fact]
    public void ActorHandler_ExplicitTaskLocal_NeverUsed_Warns()
    {
        // Handler bodies are callable roots too — the check is not
        // module-only.
        const string src = """
            message Go();

            actor Worker
            {
                on Go =>
                {
                    Task<int> t = System.Threading.Tasks.Task.FromResult(1);
                }
            }
            """;
        AssertWarns(src);
    }
}
