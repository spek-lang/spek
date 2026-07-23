using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Xunit;

namespace Spek.Tests.Emit;

/// <summary>
/// Coverage for the handler-visibility modifiers — `public`,
/// `internal`, `private` on `on Foo => ...` arms. Default (omitted)
/// is `public`, preserving the earlier behaviour. Private handlers are
/// runtime-checked: only `self.Tell` can reach them; other senders
/// dead-letter.
/// </summary>
public sealed class HandlerVisibilityTests
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
    public void PublicOnHandler_EmitsNoSenderCheck()
    {
        const string src = """
            message Tick();
            actor A
            {
                behavior Idle
                {
                    public on Tick => { }
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.DoesNotContain("private handler 'Tick' is reachable only via self.Tell", code);
    }

    [Fact]
    public void DefaultVisibilityIsPublic_EmitsNoSenderCheck()
    {
        // Omitting the modifier preserves the earlier semantics.
        const string src = """
            message Tick();
            actor A
            {
                behavior Idle
                {
                    on Tick => { }
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.DoesNotContain("private handler", code);
    }

    [Fact]
    public void PrivateOnHandler_EmitsSenderEqualityCheckAndDeadLetterFallback()
    {
        const string src = """
            message Tick();
            actor A
            {
                behavior Idle
                {
                    private on Tick => { }
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("ReferenceEquals(_sender, _selfRef)", code);
        Assert.Contains("ToDeadLetter(_msg, \"private handler 'Tick' is reachable only via self.Tell\")", code);
    }

    [Fact]
    public void InternalOnHandler_EmitsNoSenderCheck()
    {
        // `internal` is a marker — runtime enforcement is deferred
        // to when cluster-aware sender tracking lands. For now it
        // behaves like public at runtime.
        const string src = """
            message Tick();
            actor A
            {
                behavior Idle
                {
                    internal on Tick => { }
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.DoesNotContain("private handler", code);
    }

    [Fact]
    public void MixedVisibility_PerHandlerCheckOnPrivateOnly()
    {
        const string src = """
            message Public();
            message Private();
            actor A
            {
                behavior Idle
                {
                    public on Public => { }
                    private on Private => { }
                }
            }
            """;
        var code = EmitCSharp(src);

        // The Private case has the guard.
        Assert.Contains("private handler 'Private' is reachable only via self.Tell", code);

        // Find the case for Public; verify the guard isn't there.
        var publicCaseIdx = code.IndexOf("case Public");
        Assert.True(publicCaseIdx > 0);
        var privateCaseIdx = code.IndexOf("case Private", publicCaseIdx);
        Assert.True(privateCaseIdx > publicCaseIdx);
        var publicSlice = code.Substring(publicCaseIdx, privateCaseIdx - publicCaseIdx);
        Assert.DoesNotContain("private handler", publicSlice);
    }
}
