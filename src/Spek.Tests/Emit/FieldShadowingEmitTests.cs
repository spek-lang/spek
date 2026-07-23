using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Xunit;

namespace Spek.Tests.Emit;

/// <summary>
/// The field-aware expression emitter prefixes actor-field references with <c>_</c>. A
/// parameter, handler binding, or local that shadows a field must NOT be <c>_</c>-prefixed
/// — it binds to the param/local (C# scoping), not the field. Before this, the emitter did
/// a flat <c>_fields.Contains(name)</c> check, so a method param named <c>value</c> on an
/// actor with a <c>value</c> field was silently rewritten to <c>_value</c> — a latent
/// correctness bug.
/// </summary>
public sealed class FieldShadowingEmitTests
{
    private static string Emit(string src)
    {
        var parse = SpekCompiler.Parse(src);
        Assert.True(parse.Success,
            string.Join("\n", parse.Diagnostics.Select(d => $"  {d.Code} {d.Message}")));
        return new FileEmitter().Emit(parse.Tree!);
    }

    [Fact]
    public void MethodParam_ShadowsField_UsesParamNotField()
    {
        const string src = """
            message Go();
            actor A
            {
                int value = 0;
                init() { become R; }
                behavior R { on Go => { value = Helper(5); } }
                int Helper(int value) { return value + 1; }
            }
            """;
        var cs = Emit(src);
        Assert.Contains("return (value + 1);", cs);        // the param
        Assert.DoesNotContain("return (_value + 1);", cs); // NOT the field
        Assert.Contains("_value = Helper(5);", cs);        // the actual field write stays prefixed
    }

    [Fact]
    public void HandlerBinding_ShadowsField_UsesBinding()
    {
        const string src = """
            message Carry(int n);
            actor A
            {
                int carry = 0;
                init() { become R; }
                behavior R { on Carry carry => { self.Tell(new Carry(carry.n)); } }
            }
            """;
        var cs = Emit(src);
        Assert.Contains("carry.n", cs);          // the message binding
        Assert.DoesNotContain("_carry.n", cs);   // not the field
    }

    [Fact]
    public void Local_ShadowsField_UsesLocalAfterDeclaration()
    {
        // The actor handles Reply too — otherwise the self-tell is provably
        // dead mail and (correctly) trips CE0126.
        const string src = """
            message Reply(int n);
            message Go();
            actor A
            {
                int total = 0;
                init() { become R; }
                behavior R
                {
                    on Go => { var total = 5; self.Tell(new Reply(total)); }
                    on Reply r => { }
                }
            }
            """;
        var cs = Emit(src);
        Assert.Contains("var total = 5;", cs);
        Assert.Contains("new Reply(total)", cs);          // the local
        Assert.DoesNotContain("new Reply(_total)", cs);   // not the field
    }

    [Fact]
    public void NonShadowedFieldAccess_StillPrefixed()
    {
        // Regression guard: the fix must not break ordinary field prefixing.
        const string src = """
            message Go();
            actor A
            {
                int count = 0;
                init() { become R; }
                behavior R { on Go => { count = count + 1; } }
            }
            """;
        var cs = Emit(src);
        Assert.Contains("(_count + 1)", cs);   // the unshadowed field reference is prefixed
    }
}
