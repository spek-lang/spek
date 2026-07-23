using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Xunit;
using Xunit.Abstractions;

namespace Spek.Tests.Emit;

/// <summary>
/// Invisible async, Stage 1: lazy `var` + generalized await +
/// structured join, gated to single-exit methods (defer only when
/// provably safe; eager fallback otherwise). See
/// the async chapter of the language guide.
///
/// Written test-first: these assert the Stage 1 contract before the
/// rewriter implements it.
/// </summary>
public sealed class InvisibleAsyncConcurrencyTests(ITestOutputHelper output)
{
    private static string EmitCSharp(string source)
    {
        var parsed = SpekCompiler.Parse(source);
        Assert.True(parsed.Success,
            "Source must parse; diagnostics: " +
            string.Join("\n", parsed.Diagnostics.Select(d => $"{d.Code} {d.Line}:{d.Column} {d.Message}")));
        return new FileEmitter().Emit(parsed.Tree!);
    }

    private void AssertCompiles(string code, string label)
    {
        var (ok, summary, _) = RoslynCompileHelper.TryCompile(code, label);
        if (!ok)
        {
            output.WriteLine("─── emitted C# ───");
            output.WriteLine(code);
            output.WriteLine("─── diagnostics ───");
            output.WriteLine(summary);
        }
        Assert.True(ok, $"Emitted C# did not compile:\n{summary}");
    }

    // ── Lazy var: independent calls defer at the binding and await at use ──
    [Fact]
    public void LazyVar_DefersAtBinding_AwaitsAtUse()
    {
        const string src = """
            module M
            {
                public int Combine(int x, int y) { return x + y; }

                public int Run()
                {
                    var a = System.Threading.Tasks.Task.FromResult(1);
                    var b = System.Threading.Tasks.Task.FromResult(2);
                    return Combine(a, b);
                }
            }
            """;
        var code = EmitCSharp(src);
        // Deferred: the binding is NOT awaited.
        Assert.Contains("var a = System.Threading.Tasks.Task.FromResult(1)", code);
        Assert.DoesNotContain("var a = await", code);
        // Awaited at the value-use.
        Assert.Contains("await a", code);
        Assert.Contains("await b", code);
        AssertCompiles(code, "LazyVarConcurrency");
    }

    // ── The explicit Task<T> escape hatch becomes usable (value extracted) ──
    [Fact]
    public void ExplicitTaskLocal_ValueUse_IsAwaited()
    {
        const string src = """
            module M
            {
                public int Run()
                {
                    Task<int> t = System.Threading.Tasks.Task.FromResult(5);
                    int x = t;
                    return x + 1;
                }
            }
            """;
        var code = EmitCSharp(src);
        // Explicit Task<T> kept (deferred) at the binding...
        Assert.Contains("Task<int> t = System.Threading.Tasks.Task.FromResult(5)", code);
        Assert.DoesNotContain("Task<int> t = await", code);
        // ...and awaited where its value is used.
        Assert.Contains("await t", code);
        AssertCompiles(code, "ExplicitTaskHatch");
    }

    // ── Unused deferred Task is joined at scope exit (single-exit method) ──
    [Fact]
    public void UnusedDeferredVar_IsJoinedAtScopeExit()
    {
        const string src = """
            module M
            {
                public void Run()
                {
                    var a = System.Threading.Tasks.Task.FromResult(5);
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("var a = System.Threading.Tasks.Task.FromResult(5)", code);
        Assert.DoesNotContain("var a = await", code);
        // The join awaits it before the handler returns.
        Assert.Contains("await a", code);
        AssertCompiles(code, "UnusedDeferredJoin");
    }

    // ── Stage 3: guard-clause early returns BEFORE the work still defer ──
    [Fact]
    public void GuardClauseBeforeDeferral_StillDefers()
    {
        const string src = """
            module M
            {
                public int Combine(int x, int y) { return x + y; }

                public int Run(bool skip)
                {
                    if (skip) { return 0; }
                    var a = System.Threading.Tasks.Task.FromResult(1);
                    var b = System.Threading.Tasks.Task.FromResult(2);
                    return Combine(a, b);
                }
            }
            """;
        var code = EmitCSharp(src);
        // The early return is before a/b exist, so they're still deferred.
        Assert.Contains("var a = System.Threading.Tasks.Task.FromResult(1)", code);
        Assert.DoesNotContain("var a = await", code);
        Assert.Contains("await a", code);
        Assert.Contains("await b", code);
        AssertCompiles(code, "GuardClauseDefers");
    }

    // ── A return AFTER a deferred local → joined before that exit ──
    [Fact]
    public void ReturnAfterDeferral_JoinsBeforeEarlyExit()
    {
        const string src = """
            module M
            {
                public int Run(bool c)
                {
                    var a = System.Threading.Tasks.Task.FromResult(5);
                    if (c) { return 0; }
                    return a + 1;
                }
            }
            """;
        var code = EmitCSharp(src);
        // `a` is deferred; it's live at the nested `return 0`, so it's
        // awaited (joined) before that exit, and at its value-use otherwise.
        Assert.Contains("var a = System.Threading.Tasks.Task.FromResult(5)", code);
        Assert.DoesNotContain("var a = await", code);
        Assert.Contains("await a", code);
        AssertCompiles(code, "ReturnAfterDeferralJoin");

        // Runtime: both paths produce the right answer (and `a` completes).
        var asm = RoslynCompileHelper.CompileAndLoad(code, "ReturnAfterDeferralRun");
        var run = asm.GetType("M")!.GetMethod("Run")!;
        Assert.Equal(0, ((System.Threading.Tasks.Task<int>)run.Invoke(null, new object[] { true })!).GetAwaiter().GetResult());
        Assert.Equal(6, ((System.Threading.Tasks.Task<int>)run.Invoke(null, new object[] { false })!).GetAwaiter().GetResult());
    }

    // ── Multiple early exits after a deferred local → each gets a join ──
    [Fact]
    public void MultipleExits_EachJoinedBeforeReturn()
    {
        const string src = """
            module M
            {
                public int Run(int mode)
                {
                    var a = System.Threading.Tasks.Task.FromResult(10);
                    if (mode == 1) { return 1; }
                    if (mode == 2) { return 2; }
                    return a + 100;
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("var a = System.Threading.Tasks.Task.FromResult(10)", code);
        Assert.DoesNotContain("var a = await", code);
        AssertCompiles(code, "MultiExitJoin");

        var asm = RoslynCompileHelper.CompileAndLoad(code, "MultiExitRun");
        var run = asm.GetType("M")!.GetMethod("Run")!;
        int Run(int mode) =>
            ((System.Threading.Tasks.Task<int>)run.Invoke(null, new object[] { mode })!).GetAwaiter().GetResult();
        Assert.Equal(110, Run(0));
        Assert.Equal(1,   Run(1));
        Assert.Equal(2,   Run(2));
    }

    // ── A Task bound inside a loop → eager fallback (and no out-of-scope join) ──
    [Fact]
    public void LoopBoundTask_FallsBackToEager()
    {
        const string src = """
            module M
            {
                public int Run()
                {
                    int sum = 0;
                    for (int i = 0; i < 3; i = i + 1)
                    {
                        var a = System.Threading.Tasks.Task.FromResult(i);
                        sum = sum + a;
                    }
                    return sum;
                }
            }
            """;
        var code = EmitCSharp(src);
        // Nested in a loop → not deferred → eager; the join must NOT reference
        // `a` at method scope (it's out of scope there).
        Assert.Contains("var a = await System.Threading.Tasks.Task.FromResult(i)", code);
        AssertCompiles(code, "LoopEager");
    }

    // ── Regression guard: the simple sync-like case stays eager ──
    [Fact]
    public void SyncLikeValueBinding_StaysEager()
    {
        const string src = """
            module M
            {
                public int Run()
                {
                    string s = System.IO.File.ReadAllTextAsync("x");
                    return s.Length;
                }
            }
            """;
        var code = EmitCSharp(src);
        // Explicit unwrapped type → awaited at the binding (value context).
        Assert.Contains("string s = await System.IO.File.ReadAllTextAsync", code);
        AssertCompiles(code, "SyncLikeEager");
    }

    // ── End-to-end: deferred + joined code runs and returns the right value ──
    [Fact]
    public void EndToEnd_LazyVar_RunsAndCompletes()
    {
        const string src = """
            module Calc
            {
                public int Combine(int x, int y) { return x + y; }

                public int Run()
                {
                    var a = System.Threading.Tasks.Task.FromResult(20);
                    var b = System.Threading.Tasks.Task.FromResult(22);
                    return Combine(a, b);
                }
            }
            """;
        var code = EmitCSharp(src);
        AssertCompiles(code, "EndToEndLazyVar");

        var asm = RoslynCompileHelper.CompileAndLoad(code, "EndToEndLazyVarRun");
        var calc = asm.GetType("Calc")!;
        var run  = calc.GetMethod("Run")!;
        // Run lowered to async Task<int>; await the result and assert completion.
        var task = (System.Threading.Tasks.Task<int>)run.Invoke(null, null)!;
        Assert.Equal(42, task.GetAwaiter().GetResult());
    }
}
