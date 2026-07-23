using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Xunit;
using Xunit.Abstractions;

namespace Spek.Tests.Emit;

/// <summary>
/// The <c>on any</c> catch-all handler (docs: actors.md "handlers"): binds
/// whatever message didn't match an earlier arm. A behavior with a catch-all
/// routes unmatched messages to the user's arm instead of the dead-letter
/// <c>Unhandled</c> default; specific arms still win because they emit first.
/// </summary>
public sealed class CatchAllHandlerTests(ITestOutputHelper output)
{
    private const string Src = """
        namespace X;

        message Known();
        message Seen(string kind);

        actor Sorter
        {
            init() { become Active; }

            behavior Active
            {
                on Known    => return new Seen("known");
                on any msg  => return new Seen("other");
            }
        }
        """;

    [Fact]
    public void CatchAll_EmitsDefaultArm_NotUnhandled()
    {
        var parsed = SpekCompiler.Parse(Src);
        Assert.True(parsed.Success,
            string.Join("\n", parsed.Diagnostics.Select(d => $"{d.Code} {d.Message}")));
        var code = new FileEmitter().Emit(parsed.Tree!);

        // The catch-all replaces the dead-letter default: no Unhandled(...) call
        // is emitted for this behavior, and the specific arm still emits first.
        Assert.Contains("case Known", code);
        Assert.DoesNotContain("Unhandled(_msg);", code);

        var (ok, summary, _) = RoslynCompileHelper.TryCompile(code, "CatchAll");
        if (!ok) { output.WriteLine(code); output.WriteLine(summary); }
        Assert.True(ok, $"Emitted C# did not compile:\n{summary}");
    }

    [Fact]
    public void CatchAll_IsExemptFromDeclaredMessageRule()
    {
        // `on any` binds no specific type, so the CE0096 public-handler rule
        // (pattern must name a declared message) has nothing to check.
        var parsed = SpekCompiler.Parse(Src);
        Assert.True(parsed.Success);
        Assert.DoesNotContain(parsed.Diagnostics, d => d.Code == "CE0096");
    }
}
