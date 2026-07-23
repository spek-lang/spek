using Spek.Compiler.AST;
using Spek.Compiler.Emit;
using Xunit;

namespace Spek.Tests.Emit;

public class ActorEmitterTests
{
    private static string EmitActor(ActorDecl actor)
    {
        var w = new CSharpWriter();
        new ActorEmitter(w).Emit(actor);
        return w.ToString();
    }

    private static SourceSpan NoSpan => SourceSpan.None;
    private static TypeRef SimpleType(string name) =>
        new(NoSpan, new QualifiedName(NoSpan, [name]), []);

    // ─── Class header ─────────────────────────────────────────────────────────

    [Fact]
    public void DefaultActor_EmitsInternalSealedClass()
    {
        var actor = new ActorDecl(NoSpan, Visibility.Private, false, "Echo", [], [], null, [], []);
        var code  = EmitActor(actor);
        Assert.Contains("internal sealed class Echo", code);
        Assert.Contains(": Spek.ActorBase", code);
    }

    [Fact]
    public void PublicActor_EmitsPublicSealedClass()
    {
        var actor = new ActorDecl(NoSpan, Visibility.Public, false, "Manager", [], [], null, [], []);
        Assert.Contains("public sealed class Manager", EmitActor(actor));
    }

    [Fact]
    public void AbstractActor_EmitsPublicAbstractClass()
    {
        var actor = new ActorDecl(NoSpan, Visibility.Private, true, "Base", [], [], null, [], []);
        var code  = EmitActor(actor);
        Assert.Contains("public abstract class Base", code);
    }

    // ─── Fields ───────────────────────────────────────────────────────────────

    [Fact]
    public void Field_EmittedWithUnderscorePrefix()
    {
        var field = new FieldDecl(NoSpan, Visibility.Private, SimpleType("decimal"), "balance",
            new DecimalLiteralExpr(NoSpan, 1000m, true));
        var actor = new ActorDecl(NoSpan, Visibility.Private, false, "Wallet", [], [], null, [], [field]);
        var code  = EmitActor(actor);
        Assert.Contains("private decimal _balance = 1000m;", code);
    }

    // ─── Behavior ─────────────────────────────────────────────────────────────

    [Fact]
    public void Behavior_EmitsDelegateFieldAndHandlerMethod()
    {
        var handler = new OnHandler(
            NoSpan,
            new NoBindPattern(NoSpan, new QualifiedName(NoSpan, ["Ping"])),
            new InlineHandlerBody(NoSpan, new SenderExpr(NoSpan)));

        var behavior = new BehaviorDecl(NoSpan, false, false, "Waiting", [handler]);
        var actor    = new ActorDecl(NoSpan, Visibility.Private, false, "Echo", [], [], null, [], [behavior]);
        var code     = EmitActor(actor);

        Assert.Contains("_behavior = null!", code);
        Assert.Contains("private async Task Waiting_HandleAsync", code);
        Assert.Contains("case Ping _:", code);
    }

    // ─── Constructor from init ────────────────────────────────────────────────

    [Fact]
    public void Init_EmitsConstructorWithBecome()
    {
        var initBody = new BlockStmt(NoSpan, [new BecomeStmt(NoSpan, "Idle")]);
        var init     = new InitBlock(NoSpan, [], initBody);
        var behavior = new BehaviorDecl(NoSpan, false, false, "Idle", []);
        var actor    = new ActorDecl(NoSpan, Visibility.Private, false, "Worker", [], [], null, [],
            [init, behavior]);
        var code = EmitActor(actor);

        Assert.Contains("public Worker()", code);
        Assert.Contains("_behavior = Idle_HandleAsync;", code);
    }

    // ─── Lifecycle hooks ─────────────────────────────────────────────────────

    [Fact]
    public void PreStart_EmitsOnPreStartOverride()
    {
        var hook = new LifecycleHook(
            NoSpan, new PreStartEvent(NoSpan),
            new BlockHandlerBody(NoSpan, new BlockStmt(NoSpan, [])));
        var actor = new ActorDecl(NoSpan, Visibility.Private, false, "A", [], [], null, [], [hook]);
        Assert.Contains("protected override void OnPreStart()", EmitActor(actor));
    }

    [Fact]
    public void Restore_EmitsOnRestoreOverride()
    {
        var hook = new LifecycleHook(
            NoSpan, new RestoreEvent(NoSpan, SimpleType("Snapshot"), "s"),
            new BlockHandlerBody(NoSpan, new BlockStmt(NoSpan, [])));
        var actor = new ActorDecl(NoSpan, Visibility.Private, false, "A", [], [], null, [], [hook]);
        Assert.Contains("protected override void OnRestore(Spek.Persistence.Snapshot s)", EmitActor(actor));
    }

    // ─── Field access in expressions ─────────────────────────────────────────

    [Fact]
    public void FieldAccess_InHandlerBody_IsPrefixed()
    {
        // on GetBalance => sender.Tell(new BalanceResponse(balance));
        var tellArg   = new NewExpr(NoSpan, new QualifiedName(NoSpan, ["BalanceResponse"]), [],
            [new NameExpr(NoSpan, new QualifiedName(NoSpan, ["balance"]))]);
        var tellCall  = new MethodCallExpr(NoSpan, new SenderExpr(NoSpan), "Tell", [], [tellArg]);
        var handler   = new OnHandler(
            NoSpan, new NoBindPattern(NoSpan, new QualifiedName(NoSpan, ["GetBalance"])),
            new InlineHandlerBody(NoSpan, tellCall));
        var behavior  = new BehaviorDecl(NoSpan, false, false, "Active", [handler]);
        var field     = new FieldDecl(NoSpan, Visibility.Private, SimpleType("decimal"), "balance", null);
        var actor     = new ActorDecl(NoSpan, Visibility.Private, false, "Wallet", [], [], null, [],
            [field, behavior]);

        var code = EmitActor(actor);
        Assert.Contains("_balance", code);
        Assert.DoesNotContain("new BalanceResponse(balance)", code); // un-prefixed form absent
    }
}
