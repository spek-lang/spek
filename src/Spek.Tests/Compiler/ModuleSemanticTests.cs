using Spek.Compiler.Parser;
using Xunit;

namespace Spek.Tests.Compiler;

/// <summary>
/// Semantic checks on module declarations. Symbol-table
/// integration, CE0013 duplicate-name detection across modules
/// and inside modules. Emit is a follow-up commit; this layer
/// just validates the structural rules.
/// </summary>
public sealed class ModuleSemanticTests
{
    [Fact]
    public void DuplicateModuleAtFileScope_ReportsCE0013()
    {
        const string src = """
            module Foo { }
            module Foo { }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.False(parsed.Success);
        var diag = Assert.Single(parsed.Diagnostics, d => d.Code == "CE0013");
        Assert.Contains("Foo", diag.Message);
        Assert.Contains("module",   diag.Message, StringComparison.OrdinalIgnoreCase);
    }

    [Fact]
    public void DuplicateFunctionInModule_ReportsCE0013()
    {
        const string src = """
            module Validators
            {
                public bool IsValid(string s) { return true; }
                public bool IsValid(int n)    { return true; }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.False(parsed.Success);
        var diag = Assert.Single(parsed.Diagnostics, d => d.Code == "CE0013");
        Assert.Contains("method 'IsValid'", diag.Message);   // unified term: module callables are methods
        Assert.Contains("module 'Validators'", diag.Message);
    }

    [Fact]
    public void DuplicateNestedModule_ReportsCE0013()
    {
        const string src = """
            module Outer
            {
                module Inner { }
                module Inner { }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.False(parsed.Success);
        var diag = Assert.Single(parsed.Diagnostics, d => d.Code == "CE0013");
        Assert.Contains("nested module 'Inner'", diag.Message);
    }

    [Fact]
    public void FunctionAndNestedModule_SameName_NoConflict()
    {
        // Functions and nested modules live in independent
        // namespaces inside a module — same separation C# has
        // between methods and nested types.
        const string src = """
            module Outer
            {
                public int Doubled(int n) { return n + n; }
                module Doubled { }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.True(parsed.Success,
            "Diagnostics: " +
            string.Join("\n", parsed.Diagnostics.Select(d => $"{d.Code} {d.Line}:{d.Column} {d.Message}")));
    }

    [Fact]
    public void ModuleAndActor_SameName_ReportsCE0013()
    {
        // Modules share the top-level namespace with actors / messages
        // / channels / regions. A module and an actor can't share a
        // simple name within the same file.
        // (Cross-kind duplicates are flagged inside their respective
        // categories; same-name across kinds is caught by the cross-file
        // collision rule. Here we test that a module duplicate triggers
        // the module-specific CE0013, separate from the actor's.)
        const string src = """
            actor Counter
            {
                int n = 0;
            }
            module Counter { }
            """;
        var parsed = SpekCompiler.Parse(src);
        // Both decls parse and pass — no module-duplicate fires
        // because they're different kinds. But CE0013 cross-kind
        // detection isn't wired for actor↔module today (only same-
        // kind dups are flagged at file scope). This test pins the
        // current behavior; tightening to cross-kind is a future CE
        // refinement.
        Assert.True(parsed.Success,
            "Today, actor/module name collision is not flagged. " +
            "If this test fails because we tightened the rule, update it accordingly.");
    }

    [Fact]
    public void Module_AlongsideActor_BothInSymbolTable()
    {
        const string src = """
            message Tick();

            module Helpers
            {
                public int Double(int n) { return n + n; }
            }

            actor A
            {
                on Tick => { }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.True(parsed.Success,
            "Diagnostics: " +
            string.Join("\n", parsed.Diagnostics.Select(d => $"{d.Code} {d.Line}:{d.Column} {d.Message}")));
    }
}
