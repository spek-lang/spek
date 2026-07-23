using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Spek.Runtime;
using Spek.Testing;
using Spek.Tests.Emit;
using Spek.Tests.Fixtures;
using Xunit;

namespace Spek.Tests.Runtime;

/// <summary>
/// The ultimate integration test: compile <c>05_bank_account_full.spek</c> through
/// the full pipeline (parse → semantic → emit → Roslyn compile → load), then
/// exercise the compiled bank actors: deposit, check balance, freeze, withdraw.
/// Proves every language feature plus the runtime primitives it relies on
/// work together end-to-end.
/// </summary>
public class FullBankFixtureTest
{
    [Fact]
    public void FullBankAccount_Compiled_AndRunsTheWholeScenario()
    {
        // ─── Parse → Semantic → Emit → Roslyn compile → load ────────────────
        var source = FixtureLoader.Load("05_bank_account_full.spek");
        var parse  = SpekCompiler.Parse(source);
        Assert.True(parse.Success,
            string.Join("\n", parse.Diagnostics.Select(d => $"  {d.Code} {d.Line}:{d.Column} {d.Message}")));

        var csharp   = new FileEmitter().Emit(parse.Tree!);
        var assembly = RoslynCompileHelper.CompileAndLoad(csharp, "Fixture05");

        // ─── Grab every type we'll need at runtime ──────────────────────────
        var auditLoggerType  = assembly.GetType("MyBank.Actors.AuditLogger")!;
        var managerType      = assembly.GetType("MyBank.Actors.BankAccountManager")!;
        var accountType      = assembly.GetType("MyBank.Actors.BankAccount")!;

        var depositType            = assembly.GetType("MyBank.Actors.Deposit")!;
        var withdrawType           = assembly.GetType("MyBank.Actors.Withdraw")!;
        var getBalanceType         = assembly.GetType("MyBank.Actors.GetBalance")!;
        var balanceResponseType    = assembly.GetType("MyBank.Actors.BalanceResponse")!;
        var freezeAccountType      = assembly.GetType("MyBank.Actors.FreezeAccount")!;
        var accountLockedType      = assembly.GetType("MyBank.Actors.AccountLocked")!;
        var insufficientFundsType  = assembly.GetType("MyBank.Actors.InsufficientFunds")!;
        var getOrCreateAccountType = assembly.GetType("MyBank.Actors.GetOrCreateAccount")!;
        var accountRefType         = assembly.GetType("MyBank.Actors.AccountRef")!;

        Assert.NotNull(auditLoggerType);
        Assert.NotNull(managerType);
        Assert.NotNull(accountType);

        using var system = new TestActorSystem("bank-test");
        var probe = system.CreateProbe();

        // ─── Wire the hierarchy ─────────────────────────────────────────────
        var audit   = system.Spawn(auditLoggerType, "audit");
        var manager = system.Spawn(managerType, audit);

        // ─── Open an account ────────────────────────────────────────────────
        probe.Send(manager, Activator.CreateInstance(getOrCreateAccountType, "acct-1")!);
        var accountRefMsg = probe.ExpectMsg(accountRefType);
        var account = (ActorRef)accountRefType.GetProperty("account")!.GetValue(accountRefMsg)!;

        // ─── Deposit 250 → balance 1250 (starts at 1000.00) ─────────────────
        account.Tell(Activator.CreateInstance(depositType, 250m, "alice")!);
        probe.Send(account, Activator.CreateInstance(getBalanceType)!);
        var afterDeposit = probe.ExpectMsg(balanceResponseType);
        var afterDepositBal = (decimal)balanceResponseType.GetProperty("currentBalance")!.GetValue(afterDeposit)!;
        Assert.Equal(1250m, afterDepositBal);

        // ─── Withdraw 400 → balance 850 ─────────────────────────────────────
        account.Tell(Activator.CreateInstance(withdrawType, 400m, "alice")!);
        probe.Send(account, Activator.CreateInstance(getBalanceType)!);
        var afterWithdraw = probe.ExpectMsg(balanceResponseType);
        var afterWithdrawBal = (decimal)balanceResponseType.GetProperty("currentBalance")!.GetValue(afterWithdraw)!;
        Assert.Equal(850m, afterWithdrawBal);

        // ─── Overdraft → InsufficientFunds reply, balance unchanged ─────────
        probe.Send(account, Activator.CreateInstance(withdrawType, 5000m, "alice")!);
        var overdraftReply = probe.ExpectMsg(insufficientFundsType);
        Assert.NotNull(overdraftReply);

        // ─── Freeze → subsequent withdraw is locked ─────────────────────────
        account.Tell(Activator.CreateInstance(freezeAccountType, "suspicious activity")!);
        probe.Send(account, Activator.CreateInstance(withdrawType, 10m, "alice")!);
        var lockedReply = probe.ExpectMsg(accountLockedType);
        Assert.NotNull(lockedReply);
    }
}
