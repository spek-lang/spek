using Spek.Compiler.AST;
using Spek.Compiler.Parser;
using Spek.Tests.Fixtures;
using Xunit;

namespace Spek.Tests.Parser;

public class ParserTests
{
    // ─── Fixture helpers ─────────────────────────────────────────────────────

    private static CompilationResult Parse(string fixture) =>
        SpekCompiler.Parse(FixtureLoader.Load(fixture));

    private static CompilationResult ParseOk(string fixture)
    {
        var result = Parse(fixture);
        Assert.True(result.Success,
            $"Expected no diagnostics but got:\n" +
            string.Join("\n", result.Diagnostics.Select(d => $"  {d.Code} {d.Line}:{d.Column} {d.Message}")));
        return result;
    }

    // ─── 01 — Messages only ──────────────────────────────────────────────────

    [Fact]
    public void Messages_ParsesNamespace()
    {
        var r = ParseOk("01_messages_only.spek");
        Assert.Equal("MyBank.Messages", r.Tree!.Namespace!.Name.ToString());
    }

    [Fact]
    public void Messages_ParsesAllTenMessages()
    {
        var r = ParseOk("01_messages_only.spek");
        var messages = r.Tree!.Declarations.OfType<MessageDecl>().ToList();
        Assert.Equal(10, messages.Count);
    }

    [Fact]
    public void Messages_DepositHasTwoFields()
    {
        var r = ParseOk("01_messages_only.spek");
        var deposit = r.Tree!.Declarations.OfType<MessageDecl>()
                        .Single(m => m.Name == "Deposit");
        Assert.Equal(2, deposit.Fields.Count);
        Assert.Equal("amount",   deposit.Fields[0].Name);
        Assert.Equal("decimal",  deposit.Fields[0].Type.Name.Simple);
        Assert.Equal("fromUser", deposit.Fields[1].Name);
        Assert.Equal("string",   deposit.Fields[1].Type.Name.Simple);
    }

    [Fact]
    public void Messages_ShutdownHasDefaultValue()
    {
        var r = ParseOk("01_messages_only.spek");
        var msg = r.Tree!.Declarations.OfType<MessageDecl>()
                   .Single(m => m.Name == "Shutdown");
        Assert.NotNull(msg.Fields[0].DefaultValue);
        var lit = Assert.IsType<StringLiteralExpr>(msg.Fields[0].DefaultValue);
        Assert.Equal("normal", lit.Value);
    }

    [Fact]
    public void Messages_ResponseIsGenericWithOneTypeParam()
    {
        var r = ParseOk("01_messages_only.spek");
        var msg = r.Tree!.Declarations.OfType<MessageDecl>()
                   .Single(m => m.Name == "Response");
        Assert.Single(msg.TypeParameters);
        Assert.Equal("T", msg.TypeParameters[0].Name);
    }

    // ─── 02 — Simple actor ───────────────────────────────────────────────────

    [Fact]
    public void SimpleActor_ParsesActorDecl()
    {
        var r = ParseOk("02_simple_actor.spek");
        var actor = r.Tree!.Declarations.OfType<ActorDecl>().Single();
        Assert.Equal("Echo", actor.Name);
        Assert.Equal(Visibility.Private, actor.Visibility);
        Assert.False(actor.IsAbstract);
    }

    [Fact]
    public void SimpleActor_HasInitAndOneBehavior()
    {
        var r = ParseOk("02_simple_actor.spek");
        var actor = r.Tree!.Declarations.OfType<ActorDecl>().Single();
        Assert.Single(actor.Members.OfType<InitBlock>());
        Assert.Single(actor.Members.OfType<BehaviorDecl>());
    }

    [Fact]
    public void SimpleActor_InitEndsWithBecome()
    {
        var r = ParseOk("02_simple_actor.spek");
        var actor  = r.Tree!.Declarations.OfType<ActorDecl>().Single();
        var init   = actor.Members.OfType<InitBlock>().Single();
        var become = Assert.IsType<BecomeStmt>(init.Body.Statements.Single());
        Assert.Equal("Waiting", become.BehaviorName);
    }

    [Fact]
    public void SimpleActor_WaitingBehaviorHasPingHandler()
    {
        var r       = ParseOk("02_simple_actor.spek");
        var actor   = r.Tree!.Declarations.OfType<ActorDecl>().Single();
        var behavior = actor.Members.OfType<BehaviorDecl>().Single();
        Assert.Equal("Waiting", behavior.Name);
        var handler = Assert.Single(behavior.Handlers);
        var pattern = Assert.IsType<NoBindPattern>(handler.Pattern);
        Assert.Equal("Ping", pattern.MessageType.Simple);
    }

    // ─── 03 — Become ─────────────────────────────────────────────────────────

    [Fact]
    public void Become_ActorHasTwoBehaviors()
    {
        var r     = ParseOk("03_become.spek");
        var actor = r.Tree!.Declarations.OfType<ActorDecl>().Single();
        var behaviors = actor.Members.OfType<BehaviorDecl>().ToList();
        Assert.Equal(2, behaviors.Count);
        Assert.Contains(behaviors, b => b.Name == "Off");
        Assert.Contains(behaviors, b => b.Name == "On");
    }

    [Fact]
    public void Become_OffBehaviorHasThreeHandlers()
    {
        var r    = ParseOk("03_become.spek");
        var actor = r.Tree!.Declarations.OfType<ActorDecl>().Single();
        var off   = actor.Members.OfType<BehaviorDecl>().Single(b => b.Name == "Off");
        Assert.Equal(3, off.Handlers.Count);
    }

    // ─── 04 — Persist / passivate ────────────────────────────────────────────

    [Fact]
    public void Persist_ActorHasPassivateTimeout()
    {
        var r    = ParseOk("04_persist_passivate.spek");
        var actor = r.Tree!.Declarations.OfType<ActorDecl>().Single();
        var pass  = Assert.Single(actor.Members.OfType<PassivateDecl>());
        // Timeout is now a TimeSpan-valued expression: System.TimeSpan.FromMinutes(10).
        var call = Assert.IsType<MethodCallExpr>(pass.Timeout);
        Assert.Equal("FromMinutes", call.Method);
    }

    [Fact]
    public void Persist_DepositHandlerHasPersistStatement()
    {
        var r    = ParseOk("04_persist_passivate.spek");
        var actor = r.Tree!.Declarations.OfType<ActorDecl>().Single();
        var behavior = actor.Members.OfType<BehaviorDecl>().Single(b => b.Name == "Active");
        var deposit  = behavior.Handlers.Single(h =>
            h.Pattern is NamedBindPattern p && p.MessageType.Simple == "Deposit");
        var block = Assert.IsType<BlockHandlerBody>(deposit.Body).Block;
        Assert.Contains(block.Statements, s => s is PersistStmt);
    }

    [Fact]
    public void Persist_HasRestoreLifecycleHook()
    {
        var r    = ParseOk("04_persist_passivate.spek");
        var actor = r.Tree!.Declarations.OfType<ActorDecl>().Single();
        var restore = actor.Members.OfType<LifecycleHook>()
                       .Single(h => h.Event is RestoreEvent);
        var re = Assert.IsType<RestoreEvent>(restore.Event);
        Assert.Equal("s", re.Binding);
    }

    // ─── 05 — Full bank account ──────────────────────────────────────────────

    [Fact]
    public void Full_ParsesWithNoErrors()
    {
        var r = Parse("05_bank_account_full.spek");
        Assert.True(r.Success,
            string.Join("\n", r.Diagnostics.Select(d => $"{d.Code} {d.Line}:{d.Column} {d.Message}")));
    }

    [Fact]
    public void Full_HasElevenMessages()
    {
        // 8 core bank messages + AuditEntry + GetOrCreateAccount + AccountRef
        var r = ParseOk("05_bank_account_full.spek");
        Assert.Equal(11, r.Tree!.Declarations.OfType<MessageDecl>().Count());
    }

    [Fact]
    public void Full_HasThreeActorsAndOneProgram()
    {
        // BankAccountManager, BankAccount, AuditLogger
        var r = ParseOk("05_bank_account_full.spek");
        Assert.Equal(3, r.Tree!.Declarations.OfType<ActorDecl>().Count());
        Assert.Single(r.Tree!.Declarations.OfType<ProgramDecl>());
    }

    [Fact]
    public void Full_BankAccountHasTwoBehaviors()
    {
        var r = ParseOk("05_bank_account_full.spek");
        var account = r.Tree!.Declarations.OfType<ActorDecl>()
                       .Single(a => a.Name == "BankAccount");
        var behaviors = account.Members.OfType<BehaviorDecl>().ToList();
        Assert.Equal(2, behaviors.Count);
        Assert.Contains(behaviors, b => b.Name == "NormalOperation");
        Assert.Contains(behaviors, b => b.Name == "Frozen");
    }

    [Fact]
    public void Full_BankAccountManagerIsPublic()
    {
        var r = ParseOk("05_bank_account_full.spek");
        var mgr = r.Tree!.Declarations.OfType<ActorDecl>()
                   .Single(a => a.Name == "BankAccountManager");
        Assert.Equal(Visibility.Public, mgr.Visibility);
    }
}
