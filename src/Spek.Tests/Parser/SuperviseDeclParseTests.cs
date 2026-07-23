using Spek.Compiler.AST;
using Spek.Compiler.Parser;
using Xunit;

namespace Spek.Tests.Parser;

/// <summary>
/// Exercises both <c>supervise</c> declaration forms:
/// <list type="bullet">
/// <item>A — <c>supervise(child, strategy: OneForOne(...))</c> — per-child override.</item>
/// <item>B — <c>supervise OneForOne(...)</c> — default policy for all children.</item>
/// </list>
/// </summary>
public class SuperviseDeclParseTests
{
    private static ActorDecl ParseActor(string src)
    {
        var r = SpekCompiler.Parse(src);
        Assert.True(r.Success,
            string.Join("\n", r.Diagnostics.Select(d => $"  {d.Code} {d.Line}:{d.Column} {d.Message}")));
        return r.Tree!.Declarations.OfType<ActorDecl>().Single();
    }

    [Fact]
    public void DefaultForm_ParsesWithNullTarget()
    {
        const string src = """
            message Ping();
            actor Parent
            {
                behavior Idle { on Ping => { } }
                supervise OneForOne(on Failure: Restart, maxRetries: 5, withinTime: System.TimeSpan.FromMinutes(1));
            }
            """;
        var actor = ParseActor(src);
        var sup   = actor.Members.OfType<SuperviseDecl>().Single();

        Assert.Null(sup.Target);
        Assert.IsType<OneForOneStrategy>(sup.Strategy);
    }

    [Fact]
    public void ResumeDirective_Parses()
    {
        // Resume is a first-class supervise directive now, not only
        // reachable via an OnChildFailure override.
        const string src = """
            message Ping();
            actor Parent
            {
                behavior Idle { on Ping => { } }
                supervise OneForOne(on Failure: Resume);
            }
            """;
        var actor = ParseActor(src);
        var sup   = actor.Members.OfType<SuperviseDecl>().Single();
        var arm   = ((OneForOneStrategy)sup.Strategy).Options.OfType<OnFailureOption>().Single();

        Assert.Equal(RestartAction.Resume, arm.Action);
    }

    [Fact]
    public void PerChildForm_ParsesWithTargetExpression()
    {
        const string src = """
            message Ping();
            actor Parent
            {
                ActorRef child;
                behavior Idle { on Ping => { } }
                supervise(child, strategy: OneForOne(on Failure: Stop));
            }
            """;
        var actor = ParseActor(src);
        var sup   = actor.Members.OfType<SuperviseDecl>().Single();

        Assert.NotNull(sup.Target);
        var nameExpr = Assert.IsType<NameExpr>(sup.Target);
        Assert.Equal("child", nameExpr.Name.Simple);
    }

    [Fact]
    public void ExceptionTypedArm_ParsesExceptionType()
    {
        const string src = """
            message Ping();
            actor Parent
            {
                behavior Idle { on Ping => { } }
                supervise OneForOne(
                    on Failure(System.IO.IOException): Restart,
                    on Failure: Stop);
            }
            """;
        var actor = ParseActor(src);
        var sup   = actor.Members.OfType<SuperviseDecl>().Single();
        var strategy = Assert.IsType<OneForOneStrategy>(sup.Strategy);
        var arms = strategy.Options.OfType<OnFailureOption>().ToList();

        Assert.Equal(2, arms.Count);
        Assert.NotNull(arms[0].ExceptionType);
        Assert.Equal("System.IO.IOException", arms[0].ExceptionType!.ToString());
        Assert.Equal(RestartAction.Restart, arms[0].Action);
        Assert.Null(arms[1].ExceptionType);
        Assert.Equal(RestartAction.Stop, arms[1].Action);
    }

    [Fact]
    public void UntypedArm_ParsesWithNullExceptionType()
    {
        const string src = """
            message Ping();
            actor Parent
            {
                behavior Idle { on Ping => { } }
                supervise OneForOne(on Failure: Restart);
            }
            """;
        var actor = ParseActor(src);
        var sup   = actor.Members.OfType<SuperviseDecl>().Single();
        var strategy = Assert.IsType<OneForOneStrategy>(sup.Strategy);
        var arm = Assert.Single(strategy.Options.OfType<OnFailureOption>());

        Assert.Null(arm.ExceptionType);
        Assert.Equal(RestartAction.Restart, arm.Action);
    }

    [Fact]
    public void BothForms_CoexistInOneActor()
    {
        const string src = """
            message Ping();
            actor Parent
            {
                ActorRef hot;
                ActorRef cold;
                behavior Idle { on Ping => { } }
                supervise OneForOne(on Failure: Restart, maxRetries: 3, withinTime: System.TimeSpan.FromSeconds(30));
                supervise(hot,  strategy: OneForOne(on Failure: Escalate));
                supervise(cold, strategy: OneForOne(on Failure: Stop));
            }
            """;
        var actor = ParseActor(src);
        var supervises = actor.Members.OfType<SuperviseDecl>().ToList();

        Assert.Equal(3, supervises.Count);
        Assert.Single(supervises.Where(s => s.Target is null));
        Assert.Equal(2, supervises.Count(s => s.Target is not null));
    }
}
