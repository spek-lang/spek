using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Xunit;

namespace Spek.Tests.Parser;

/// <summary>
/// Contextual keywords as member names: after a `.` the position is unambiguous, so a
/// keyword there is a member name, not a keyword. Before this, `FailureDirective.Stop`
/// / `.Restart` / `.Escalate` failed to parse (CE0001) — making those directives
/// impossible to return from an OnFailure override (3 of 4 directive values were
/// unreferenceable).
/// </summary>
public sealed class MemberAccessKeywordTests
{
    [Theory]
    [InlineData("Stop")]
    [InlineData("Restart")]
    [InlineData("Escalate")]
    public void FailureDirective_MemberAccess_Parses(string directive)
    {
        var src =
            "message Go();\n" +
            "actor A\n" +
            "{\n" +
            "    init() { become R; }\n" +
            "    behavior R { on Go => { } }\n" +
            "    FailureDirective OnFailure(System.Exception ex, object msg) { return FailureDirective." + directive + "; }\n" +
            "}\n";

        var parse = SpekCompiler.Parse(src);
        Assert.True(parse.Success,
            string.Join("\n", parse.Diagnostics.Select(d => $"{d.Code} {d.Message}")));
        Assert.Contains("FailureDirective." + directive, new FileEmitter().Emit(parse.Tree!));
    }

    [Fact]
    public void KeywordMemberNames_AndKeywordNamedCall_Parse()
    {
        // Keyword member names (`x.message`, `x.event`) and a keyword-named method
        // call (`x.Stop()`) all parse after the dot.
        const string src = """
            module M
            {
                public int F(object x) { var a = x.message; var b = x.event; x.Stop(); return 0; }
            }
            """;

        var parse = SpekCompiler.Parse(src);
        Assert.True(parse.Success,
            string.Join("\n", parse.Diagnostics.Select(d => $"{d.Code} {d.Message}")));
    }
}
