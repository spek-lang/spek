using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using Spek.LanguageServer;
using Xunit;

namespace Spek.Tests.LanguageServer;

/// <summary>
/// Coverage for <see cref="SpekCodeLensHandler"/> — the "▶ Run test" lenses on
/// the <c>…Tests</c> convention (a test container plus each parameterless public
/// method).
/// </summary>
public class CodeLensTests
{
    private const string WalletTests = """
        message Deposit(decimal amount);
        message Balance(decimal amount);

        actor Wallet
        {
            decimal balance = 0m;
            on Deposit d  => { balance += d.amount; }
        }

        class WalletTests
        {
            TestActorSystem sys;
            init() { sys = new TestActorSystem(); }

            public void DepositIncreasesBalance() { var w = sys.Spawn<Wallet>(); }
            public void StartsAtZero()            { }
            void Helper()                         { }
        }
        """;

    private static async Task<List<CodeLens>> Lenses(string src)
    {
        var cache = new DocumentCache();
        var uri   = DocumentUri.From("file:///WalletTests.spek");
        cache.Update(uri, src.Replace("\r\n", "\n"));

        var handler = new SpekCodeLensHandler(cache);
        var result  = await handler.Handle(new CodeLensParams
        {
            TextDocument = new TextDocumentIdentifier { Uri = uri },
        }, CancellationToken.None);

        return (result ?? Enumerable.Empty<CodeLens>()).ToList();
    }

    private static string Filter(CodeLens lens) => lens.Command!.Arguments![0]!.ToString();

    [Fact]
    public async Task ContainerAndPublicMethods_GetLenses_HelpersDoNot()
    {
        var lenses = await Lenses(WalletTests);

        // One container lens + two public-method lenses; the non-public Helper is skipped.
        Assert.Equal(3, lenses.Count);

        var runAll = Assert.Single(lenses, l => l.Command!.Title == "▶ Run all tests");
        Assert.Equal("WalletTests", Filter(runAll));

        var filters = lenses
            .Where(l => l.Command!.Title == "▶ Run test")
            .Select(Filter)
            .ToList();
        Assert.Contains("WalletTests.DepositIncreasesBalance", filters);
        Assert.Contains("WalletTests.StartsAtZero", filters);
        Assert.DoesNotContain(filters, f => f.Contains("Helper"));

        // The command the extension handles, with the file path passed through.
        Assert.All(lenses, l =>
        {
            Assert.Equal("spek.runTest", l.Command!.Name);
            Assert.EndsWith("WalletTests.spek", l.Command!.Arguments![1]!.ToString());
        });
    }

    [Fact]
    public async Task NamespaceIsPrefixedIntoTheFilter()
    {
        var lenses = await Lenses("namespace Banking.Tests;\n\n" + WalletTests);

        Assert.Contains(lenses, l => Filter(l) == "Banking.Tests.WalletTests");
        Assert.Contains(lenses, l => Filter(l) == "Banking.Tests.WalletTests.StartsAtZero");
    }

    [Fact]
    public async Task NonTestType_GetsNoLenses()
    {
        const string plain = """
            class Wallet
            {
                public void Deposit() { }
            }
            """;
        Assert.Empty(await Lenses(plain));
    }
}
