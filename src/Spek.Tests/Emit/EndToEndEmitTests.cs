using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Spek.Tests.Fixtures;
using Xunit;
using Xunit.Abstractions;

namespace Spek.Tests.Emit;

public class EndToEndEmitTests(ITestOutputHelper output)
{
    private string EmitFixture(string fileName)
    {
        var result = SpekCompiler.Parse(FixtureLoader.Load(fileName));
        Assert.True(result.Success,
            "Parse failed:\n" + string.Join("\n", result.Diagnostics.Select(d => $"  {d.Code} {d.Line}:{d.Column} {d.Message}")));
        var code = new FileEmitter().Emit(result.Tree!);
        output.WriteLine(code);
        return code;
    }

    // ─── 01 — Messages only ──────────────────────────────────────────────────

    [Fact]
    public void MessagesOnly_EmitsNamespaceAndRecords()
    {
        var code = EmitFixture("01_messages_only.spek");
        Assert.Contains("namespace MyBank.Messages;", code);
        Assert.Contains("public record Deposit(decimal amount, string fromUser);", code);
        Assert.Contains("public record GetBalance();", code);
        Assert.Contains("public record Shutdown(string reason = \"normal\");", code);
        Assert.Contains("public record Response<T>(T value);", code);
    }

    // ─── 02 — Simple actor ───────────────────────────────────────────────────

    [Fact]
    public void SimpleActor_EmitsClassAndBehavior()
    {
        var code = EmitFixture("02_simple_actor.spek");
        Assert.Contains("internal sealed class Echo : Spek.ActorBase", code);
        Assert.Contains("_behavior = Waiting_HandleAsync;", code);
        Assert.Contains("private async Task Waiting_HandleAsync", code);
        Assert.Contains("case Ping _:", code);
        // The reply carries the actor as its sender (implicit-sender convention).
        Assert.Contains("_sender.Tell(new Pong(), _selfRef)", code);
    }

    // ─── 03 — Become ─────────────────────────────────────────────────────────

    [Fact]
    public void Become_EmitsTwoBehaviors()
    {
        var code = EmitFixture("03_become.spek");
        Assert.Contains("private async Task Off_HandleAsync", code);
        Assert.Contains("private async Task On_HandleAsync", code);
        Assert.Contains("_behavior = On_HandleAsync;", code);
        Assert.Contains("_behavior = Off_HandleAsync;", code);
    }

    // ─── 04 — Persist / passivate ────────────────────────────────────────────

    [Fact]
    public void PersistPassivate_EmitsPersistAndCaptureFields()
    {
        var code = EmitFixture("04_persist_passivate.spek");
        Assert.Contains("await PersistAsync();", code);
        Assert.Contains("CaptureFields()", code);
        Assert.Contains("[\"balance\"] = _balance", code);
        Assert.Contains("protected override void OnRestore(Spek.Persistence.Snapshot s)", code);
    }

    // ─── 05 — Full bank account ──────────────────────────────────────────────

    [Fact]
    public void FullBankAccount_EmitsCompleteFile()
    {
        var code = EmitFixture("05_bank_account_full.spek");

        // Messages
        Assert.Contains("public record Deposit(decimal amount, string fromUser);", code);

        // Actors
        Assert.Contains("public sealed class BankAccountManager : Spek.ActorBase", code);
        Assert.Contains("internal sealed class BankAccount : Spek.ActorBase", code);

        // Behaviors
        Assert.Contains("private async Task NormalOperation_HandleAsync", code);
        Assert.Contains("private async Task Frozen_HandleAsync", code);
        Assert.Contains("case Withdraw w:", code);

        // Persist
        Assert.Contains("await PersistAsync();", code);

        // Lifecycle
        Assert.Contains("protected override void OnPreStart()", code);
        Assert.Contains("protected override void OnRestore(Spek.Persistence.Snapshot s)", code);

        // Program
        Assert.Contains("public static async System.Threading.Tasks.Task Main(string[] args)", code);
    }
}
