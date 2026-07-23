using Spek.Compiler.AST;
using Spek.Compiler.Parser;
using Xunit;

namespace Spek.Tests.Compiler;

/// <summary>
/// Module / method declaration parsing. Covers the
/// grammar additions plus the AstBuilder visitors. Emit and
/// semantic analysis land in follow-up commits; this commit's
/// scope is: parse cleanly, produce well-shaped AST.
/// </summary>
public sealed class ModuleParsingTests
{
    private static SpekFile Parse(string source)
    {
        var parsed = SpekCompiler.Parse(source);
        Assert.True(parsed.Success,
            "Source must parse; diagnostics: " +
            string.Join("\n", parsed.Diagnostics.Select(d => $"{d.Code} {d.Line}:{d.Column} {d.Message}")));
        return parsed.Tree!;
    }

    [Fact]
    public void EmptyModule_Parses()
    {
        var tree = Parse("module Empty { }");
        var module = Assert.Single(tree.Declarations.OfType<ModuleDecl>());
        Assert.Equal("Empty",  module.Name);
        Assert.Equal(Visibility.Public, module.Visibility);   // default
        Assert.Empty(module.Methods);
        Assert.Empty(module.NestedModules);
    }

    [Fact]
    public void Module_WithSingleMethod_Parses()
    {
        const string src = """
            module Validators
            {
                public bool IsValidEmail(string s)
                {
                    return s.Contains("@");
                }
            }
            """;
        var tree = Parse(src);
        var module = Assert.Single(tree.Declarations.OfType<ModuleDecl>());
        var func   = Assert.Single(module.Methods);

        Assert.Equal("IsValidEmail",    func.Name);
        Assert.Equal(Visibility.Public, func.Visibility);
        Assert.NotNull(func.ReturnType);
        Assert.Equal("bool",            func.ReturnType!.ToString());
        var param = Assert.Single(func.Parameters);
        Assert.Equal("s",       param.Name);
        Assert.Equal("string",  param.Type.ToString());
    }

    [Fact]
    public void Module_VoidFunction_HasNullReturnType()
    {
        const string src = """
            module Sinks
            {
                public void LogIt(string s)
                {
                    return;
                }
            }
            """;
        var tree = Parse(src);
        var func = Assert.Single(tree.Declarations.OfType<ModuleDecl>().Single().Methods);

        Assert.Null(func.ReturnType);   // void
    }

    [Fact]
    public void Module_MultipleVisibilityLevels_Parse()
    {
        const string src = """
            module Format
            {
                public   string FormatName(string first, string last) { return first; }
                internal string Trim(string s)                          { return s; }
                private  string StripWhitespace(string s)               { return s; }
            }
            """;
        var tree = Parse(src);
        var module = tree.Declarations.OfType<ModuleDecl>().Single();
        Assert.Equal(3, module.Methods.Count);
        Assert.Equal(Visibility.Public,   module.Methods[0].Visibility);
        Assert.Equal(Visibility.Internal, module.Methods[1].Visibility);
        Assert.Equal(Visibility.Private,  module.Methods[2].Visibility);
    }

    [Fact]
    public void NestedModules_Parse()
    {
        // Modules nest as sub-namespacing — Erlang-style.
        const string src = """
            module Outer
            {
                public int A() { return 1; }

                module Inner
                {
                    public int B() { return 2; }
                }
            }
            """;
        var tree  = Parse(src);
        var outer = tree.Declarations.OfType<ModuleDecl>().Single();

        Assert.Single(outer.Methods);                  // A
        var inner = Assert.Single(outer.NestedModules);
        Assert.Equal("Inner", inner.Name);
        Assert.Single(inner.Methods);                  // B
    }

    [Fact]
    public void ModuleVisibility_DefaultsToPublic()
    {
        var tree = Parse("module Defaulted { }");
        var module = tree.Declarations.OfType<ModuleDecl>().Single();
        Assert.Equal(Visibility.Public, module.Visibility);
    }

    [Fact]
    public void ModuleVisibility_ExplicitInternal_IsRecorded()
    {
        var tree = Parse("internal module Hidden { }");
        var module = tree.Declarations.OfType<ModuleDecl>().Single();
        Assert.Equal(Visibility.Internal, module.Visibility);
    }

    [Fact]
    public void Module_AlongsideActorsAndMessages_Coexists()
    {
        const string src = """
            message Tick();

            module Helpers
            {
                public int Double(int n) { return n + n; }
            }

            actor Counter
            {
                int n = 0;
                on Tick => { n = n + 1; }
            }
            """;
        var tree = Parse(src);
        Assert.Single(tree.Declarations.OfType<MessageDecl>());
        Assert.Single(tree.Declarations.OfType<ModuleDecl>());
        Assert.Single(tree.Declarations.OfType<ActorDecl>());
    }
}
