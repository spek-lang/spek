using Spek.Compiler.AST;
using Spek.Compiler.Parser;
using Spek.LanguageServer;
using Xunit;

namespace Spek.Tests.LanguageServer;

/// <summary>
/// Exercises <see cref="PositionResolver"/> — given a parsed tree and a
/// source position, it should return the innermost AST node whose span
/// contains that position.
/// </summary>
public class PositionResolverTests
{
    private static SpekFile ParseOrFail(string source)
    {
        var result = SpekCompiler.Parse(source);
        Assert.True(result.Success,
            "Parse failed: " + string.Join("\n",
                result.Diagnostics.Select(d => $"  {d.Code} {d.Line}:{d.Column} {d.Message}")));
        return result.Tree!;
    }

    [Fact]
    public void Find_InsideActorName_ReturnsActorDecl()
    {
        const string src = "actor MyActor { behavior Idle { on Ping => { } } }\nmessage Ping();";
        var tree = ParseOrFail(src);

        // Position on the letter "M" of "MyActor" (line 1, col 7 — 1-based)
        var node = PositionResolver.Find(tree, line: 1, column: 7);

        Assert.NotNull(node);
        Assert.Contains(node!.GetType().Name, new[] { "ActorDecl" });
    }

    [Fact]
    public void Find_OutsideAllDeclarations_ReturnsNull()
    {
        const string src = "actor A { behavior B { } }\n";
        var tree = ParseOrFail(src);

        // Column 1000 is past the end of every span.
        var node = PositionResolver.Find(tree, line: 1, column: 1000);

        Assert.Null(node);
    }

    [Fact]
    public void FindChain_InsideNewExpression_IncludesNewExprAncestor()
    {
        const string src = """
            message Ping();
            message Pong();
            actor A
            {
                behavior Idle
                {
                    on Ping => sender.Tell(new Pong());
                }
            }
            """;
        var tree = ParseOrFail(src);

        // Find the Pong identifier inside `new Pong()`.
        var pongLine = src.Split('\n').ToList().FindIndex(l => l.Contains("new Pong")) + 1;
        var pongCol  = src.Split('\n')[pongLine - 1].IndexOf("Pong") + 1;

        var chain = PositionResolver.FindChain(tree, pongLine, pongCol);

        Assert.NotEmpty(chain);
        Assert.Contains(chain, n => n is NewExpr);
    }

    [Fact]
    public void FindChain_OnBecomeStatement_IncludesBecomeStmt()
    {
        const string src = """
            message Start();
            message Done();
            actor A
            {
                init() { become Idle; }
                behavior Idle { on Start => { become Busy; } }
                behavior Busy { on Done  => { become Idle; } }
            }
            """;
        var tree = ParseOrFail(src);

        // Point at the "Busy" in "become Busy;" inside the Idle behavior.
        var line  = src.Split('\n').ToList().FindIndex(l => l.Contains("become Busy")) + 1;
        var col   = src.Split('\n')[line - 1].IndexOf("Busy") + 1;

        var chain = PositionResolver.FindChain(tree, line, col);

        Assert.Contains(chain, n => n is BecomeStmt bs && bs.BehaviorName == "Busy");
        Assert.Contains(chain, n => n is ActorDecl);
    }
}
