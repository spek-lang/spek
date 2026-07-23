using Newtonsoft.Json.Linq;
using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using Spek.LanguageServer;
using Xunit;

namespace Spek.Tests.LanguageServer;

/// <summary>
/// Coverage for <see cref="SpekCodeLensHandler"/> — the "▶ Run test" lenses on
/// the <c>…Tests</c> convention (a test container plus each parameterless public
/// method), and the message-flow lenses ("N senders · M handlers" on message
/// declarations, "→ N senders" on handler arms).
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

    private static async Task<List<CodeLens>> LensesAsync(string src)
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

    private static List<CodeLens> RunTestLenses(IEnumerable<CodeLens> lenses) =>
        lenses.Where(l => l.Command!.Name == "spek.runTest").ToList();

    private static List<CodeLens> FlowLenses(IEnumerable<CodeLens> lenses) =>
        lenses.Where(l => l.Command!.Name == "editor.action.showReferences").ToList();

    /// <summary>The single flow lens sitting on the (0-based) line of the
    /// first source line containing <paramref name="lineContains"/>.</summary>
    private static CodeLens FlowLensAt(
        IEnumerable<CodeLens> lenses, string src, string lineContains) =>
        Assert.Single(FlowLenses(lenses),
            l => l.Range.Start.Line == LspTestSupport.LineOf(src, lineContains));

    [Fact]
    public async Task ContainerAndPublicMethods_GetLenses_HelpersDoNotAsync()
    {
        var runTest = RunTestLenses(await LensesAsync(WalletTests));

        // One container lens + two public-method lenses; the non-public Helper is skipped.
        Assert.Equal(3, runTest.Count);

        var runAll = Assert.Single(runTest, l => l.Command!.Title == "▶ Run all tests");
        Assert.Equal("WalletTests", Filter(runAll));

        var filters = runTest
            .Where(l => l.Command!.Title == "▶ Run test")
            .Select(Filter)
            .ToList();
        Assert.Contains("WalletTests.DepositIncreasesBalance", filters);
        Assert.Contains("WalletTests.StartsAtZero", filters);
        Assert.DoesNotContain(filters, f => f.Contains("Helper"));

        // The command the extension handles, with the file path passed through.
        Assert.All(runTest, l =>
        {
            Assert.Equal("spek.runTest", l.Command!.Name);
            Assert.EndsWith("WalletTests.spek", l.Command!.Arguments![1]!.ToString());
        });
    }

    [Fact]
    public async Task NamespaceIsPrefixedIntoTheFilterAsync()
    {
        var lenses = await LensesAsync("namespace Banking.Tests;\n\n" + WalletTests);

        Assert.Contains(lenses, l => Filter(l) == "Banking.Tests.WalletTests");
        Assert.Contains(lenses, l => Filter(l) == "Banking.Tests.WalletTests.StartsAtZero");
    }

    [Fact]
    public async Task NonTestType_GetsNoLensesAsync()
    {
        const string plain = """
            class Wallet
            {
                public void Deposit() { }
            }
            """;
        Assert.Empty(await LensesAsync(plain));
    }

    // ─── message-flow lenses ────────────────────────────────────────────

    /// <summary>Known flow counts: Ping has 1 sender (new Ping(1)) and 2
    /// handlers; Pong has 1 sender and 1 handler; Orphan has none of either.</summary>
    private const string FlowDoc = """
        message Ping(int count);
        message Pong(int count);
        message Orphan();

        actor Echo
        {
            behavior Idle
            {
                on Ping p =>
                {
                    sender.Tell(new Pong(p.count));
                }
            }
        }

        actor Caller
        {
            behavior Active
            {
                on Ping => { }
                on Pong =>
                {
                    var e = spawn<Echo>();
                    e.Tell(new Ping(1));
                }
            }
        }
        """;

    [Fact]
    public async Task MessageDecl_Lens_CountsSendersAndHandlersAsync()
    {
        var lenses = await LensesAsync(FlowDoc);

        Assert.Equal("1 sender · 2 handlers",
            FlowLensAt(lenses, FlowDoc, "message Ping").Command!.Title);
        Assert.Equal("1 sender · 1 handler",
            FlowLensAt(lenses, FlowDoc, "message Pong").Command!.Title);
    }

    [Fact]
    public async Task MessageDecl_ZeroCounts_StillRenderALensAsync()
    {
        // "0 senders" is a protocol smell the lens exists to surface —
        // never suppress the zero-count lens.
        var lenses = await LensesAsync(FlowDoc);

        Assert.Equal("0 senders · 0 handlers",
            FlowLensAt(lenses, FlowDoc, "message Orphan").Command!.Title);
    }

    [Fact]
    public async Task HandlerArm_Lens_CountsSendersAsync()
    {
        var lenses = await LensesAsync(FlowDoc);

        Assert.Equal("→ 1 sender",
            FlowLensAt(lenses, FlowDoc, "on Ping p").Command!.Title);
        Assert.Equal("→ 1 sender",
            FlowLensAt(lenses, FlowDoc, "on Ping =>").Command!.Title);
        Assert.Equal("→ 1 sender",
            FlowLensAt(lenses, FlowDoc, "on Pong =>").Command!.Title);

        // 3 message-decl lenses + 3 handler-arm lenses, nothing else.
        Assert.Equal(6, FlowLenses(lenses).Count);
    }

    [Fact]
    public async Task FlowLens_Command_IsShowReferences_WithLspShapedArgumentsAsync()
    {
        var lenses = await LensesAsync(FlowDoc);
        var ping   = FlowLensAt(lenses, FlowDoc, "message Ping");

        // [uri, anchor position, locations] — LSP-shaped, for the extension
        // to rehydrate into vscode.Uri / Position / Location[].
        var args = ping.Command!.Arguments!;
        Assert.Equal(3, args.Count);
        Assert.EndsWith("WalletTests.spek", args[0]!.ToString());

        var anchor = (JObject)args[1]!;
        Assert.Equal(LspTestSupport.LineOf(FlowDoc, "message Ping"),
            anchor["line"]!.Value<int>());

        // 1 sender + 2 handlers, senders first.
        var locations = (JArray)args[2]!;
        Assert.Equal(3, locations.Count);
        Assert.All(locations, loc =>
        {
            Assert.EndsWith("WalletTests.spek", loc["uri"]!.ToString());
            Assert.NotNull(loc["range"]!["start"]!["line"]);
            Assert.NotNull(loc["range"]!["end"]!["character"]);
        });
        Assert.Equal(LspTestSupport.LineOf(FlowDoc, "e.Tell(new Ping(1))"),
            locations[0]!["range"]!["start"]!["line"]!.Value<int>());
        Assert.Equal(LspTestSupport.LineOf(FlowDoc, "on Ping p"),
            locations[1]!["range"]!["start"]!["line"]!.Value<int>());
        Assert.Equal(LspTestSupport.LineOf(FlowDoc, "on Ping =>"),
            locations[2]!["range"]!["start"]!["line"]!.Value<int>());
    }

    [Fact]
    public async Task HandlerArm_Lens_TargetsItsSendersAsync()
    {
        var lenses = await LensesAsync(FlowDoc);
        var arm    = FlowLensAt(lenses, FlowDoc, "on Pong =>");

        var locations = (JArray)arm.Command!.Arguments![2]!;
        var only = Assert.Single(locations);
        Assert.Equal(LspTestSupport.LineOf(FlowDoc, "sender.Tell(new Pong(p.count))"),
            only["range"]!["start"]!["line"]!.Value<int>());
    }

    [Fact]
    public async Task UnsentButHandledMessage_ShowsTheSmellStringAsync()
    {
        // The WalletTests fixture never constructs Deposit — the lens must
        // say so rather than hide.
        var lenses = await LensesAsync(WalletTests);

        Assert.Equal("0 senders · 1 handler",
            FlowLensAt(lenses, WalletTests, "message Deposit").Command!.Title);
        Assert.Equal("→ 0 senders",
            FlowLensAt(lenses, WalletTests, "on Deposit d").Command!.Title);
    }
}
