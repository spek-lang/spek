using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Xunit;

namespace Spek.Tests.Emit;

/// <summary>
/// #line source mapping. When <see cref="FileEmitter.Emit"/> is given a
/// source file name, every user statement in the .g.cs is preceded by a
/// <c>#line N "file.spek"</c> directive (with <c>#line default</c> restored
/// after each body), so Roslyn diagnostics and debugger stepping land on the
/// .spek source. Mapping is opt-in: with no file name the output carries no
/// directives, which is why every other emit test is unaffected.
/// </summary>
public sealed class SourceMappingTests
{
    private static string Emit(string source, string? fileName = null)
    {
        var parsed = SpekCompiler.Parse(source);
        Assert.True(parsed.Success,
            string.Join("\n", parsed.Diagnostics.Select(d => $"{d.Code} {d.Message}")));
        return new FileEmitter().Emit(parsed.Tree!, sourceFileName: fileName);
    }

    private const string ModuleSrc = """
        module M
        {
            public int Add(int a, int b)
            {
                var sum = a + b;
                return sum;
            }
        }
        """;

    [Fact]
    public void NoFileName_EmitsNoDirectives()
    {
        var code = Emit(ModuleSrc);
        Assert.DoesNotContain("#line", code);
    }

    [Fact]
    public void WithFileName_EmitsDirectivesAndRestoresDefault()
    {
        var code = Emit(ModuleSrc, "m.spek");
        // `var sum = a + b;` is .spek line 5; `return sum;` is line 6.
        Assert.Contains("#line 5 \"m.spek\"", code);
        Assert.Contains("#line 6 \"m.spek\"", code);
        Assert.Contains("#line default", code);
        // Directives are preprocessor lines — never indented.
        foreach (var line in code.Split('\n'))
            if (line.Contains("#line"))
                Assert.StartsWith("#line", line.TrimEnd());
    }

    [Fact]
    public void MappedOutput_StillCompiles()
    {
        var code = Emit(ModuleSrc, "m.spek");
        var (ok, summary, _) = RoslynCompileHelper.TryCompile(code, "Mapped");
        Assert.True(ok, $"Mapped emit must still compile:\n{summary}");
    }

    [Fact]
    public void RoslynError_MapsBackToSpekLine()
    {
        // `int n = s;` (a CS0029) is on .spek line 5 of this source.
        const string broken = """
            module M
            {
                public int Bad(string s)
                {
                    int n = s;
                    return n;
                }
            }
            """;
        var code = Emit(broken, "bad.spek");
        var (ok, _, errors) = RoslynCompileHelper.TryCompile(code, "MappedBad");
        Assert.False(ok);
        var mapped = errors[0].Location.GetMappedLineSpan();
        Assert.Equal("bad.spek", mapped.Path);
        Assert.Equal(5, mapped.StartLinePosition.Line + 1);
    }

    [Fact]
    public void Directives_SurviveActiveAsyncRewrite()
    {
        // The ask forces the AsyncRewriter to actually rewrite (insert await,
        // mark methods async) — the directives must ride through the Roslyn
        // round-trip, and the result must still compile.
        const string src = """
            message Ping();
            message Pong();
            message Go();
            actor Ponger {
              behavior Idle { on Ping => return new Pong(); }
            }
            actor Pinger {
              ActorRef peer;
              init(ActorRef p) { peer = p; }
              behavior Ready {
                on Go => {
                  var pong = peer.Ask(new Ping());
                  peer.Tell(pong);
                }
              }
            }
            """;
        var code = Emit(src, "pingpong.spek");
        Assert.Contains("#line 12 \"pingpong.spek\"", code);   // the ask statement
        Assert.Contains("await", code);
        var (ok, summary, _) = RoslynCompileHelper.TryCompile(code, "MappedRewrite");
        Assert.True(ok, $"Rewritten mapped emit must compile:\n{summary}");
    }
}
