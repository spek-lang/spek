using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Spek.Tests.Fixtures;
using Xunit;
using Xunit.Abstractions;

namespace Spek.Tests.Emit;

/// <summary>
/// Compiles every fixture's emitted C# with Roslyn and asserts it produces
/// no errors. This is the first test tier that would catch a bug where the
/// emitter produces syntactically-plausible but invalid C# — the substring
/// assertions in <see cref="EndToEndEmitTests"/> can't detect that.
/// </summary>
public class EmittedCodeCompilesTests(ITestOutputHelper output)
{
    private string EmitFixture(string fixture)
    {
        var result = SpekCompiler.Parse(FixtureLoader.Load(fixture));
        Assert.True(result.Success,
            "Parse/semantic failed:\n" + string.Join("\n",
                result.Diagnostics.Select(d => $"  {d.Code} {d.Line}:{d.Column} {d.Message}")));
        return new FileEmitter().Emit(result.Tree!);
    }

    private void AssertEmittedCodeCompiles(string fixture)
    {
        var csharp = EmitFixture(fixture);
        var (success, summary, _) = RoslynCompileHelper.TryCompile(csharp, fixture);
        if (!success)
        {
            output.WriteLine("─── emitted C# ───");
            output.WriteLine(csharp);
            output.WriteLine("─── diagnostics ───");
            output.WriteLine(summary);
        }
        Assert.True(success, $"Emitted C# for {fixture} did not compile:\n{summary}");
    }

    [Fact] public void Fixture01_MessagesOnly_Compiles()     => AssertEmittedCodeCompiles("01_messages_only.spek");
    [Fact] public void Fixture02_SimpleActor_Compiles()      => AssertEmittedCodeCompiles("02_simple_actor.spek");
    [Fact] public void Fixture03_Become_Compiles()           => AssertEmittedCodeCompiles("03_become.spek");
    [Fact] public void Fixture04_PersistPassivate_Compiles() => AssertEmittedCodeCompiles("04_persist_passivate.spek");
    [Fact] public void Fixture05_FullBankAccount_Compiles()  => AssertEmittedCodeCompiles("05_bank_account_full.spek");
}
