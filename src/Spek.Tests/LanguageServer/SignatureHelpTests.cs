using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using Spek.LanguageServer;
using Xunit;

namespace Spek.Tests.LanguageServer;

/// <summary>
/// Coverage for <see cref="SpekSignatureHelpHandler"/>. Drives the handler
/// directly (skipping the JSON-RPC layer): a <c>|</c> in the source marks the
/// caret, and the helper resolves it to an LSP <see cref="Position"/> before
/// asking for signature help.
/// </summary>
public class SignatureHelpTests
{
    private const string Uri = "file:///sig.spek";

    // Resolve the '|'-marked caret to a Position and return the handler's result.
    // A non-null `seed` is applied first, so the live source can be half-typed
    // (unparseable) while the message declarations still come from a last good tree.
    private static async Task<SignatureHelp?> HelpAt(string withCaret, string? seed = null)
    {
        withCaret = withCaret.Replace("\r\n", "\n");
        int idx   = withCaret.IndexOf('|');
        Assert.True(idx >= 0, "test source must contain a '|' caret marker");
        var src   = withCaret.Remove(idx, 1);

        int line = 0, ch = 0;
        for (int i = 0; i < idx; i++)
        {
            if (src[i] == '\n') { line++; ch = 0; } else ch++;
        }

        var cache = new DocumentCache();
        var uri   = DocumentUri.From(Uri);
        if (seed is not null) cache.Update(uri, seed);   // establish a LastGoodTree
        cache.Update(uri, src);

        var handler = new SpekSignatureHelpHandler(cache);
        return await handler.Handle(new SignatureHelpParams
        {
            TextDocument = new TextDocumentIdentifier { Uri = uri },
            Position     = new Position(line, ch),
        }, CancellationToken.None);
    }

    private const string Wallet = """
        message Deposit(decimal amount, string note);

        actor Wallet
        {
            init() { become Active; }
            behavior Active
            {
                on Deposit d => { self.Tell(new Deposit(1m, "x")); }
            }
        }
        """;

    [Fact]
    public async Task ShowsMessageFields_WithFirstParamActive()
    {
        var help = await HelpAt(Wallet.Replace("new Deposit(1m", "new Deposit(|1m"));

        Assert.NotNull(help);
        var sig = Assert.Single(help!.Signatures);
        Assert.Equal("Deposit(decimal amount, string note)", sig.Label);
        Assert.Equal(0, help.ActiveParameter);
        Assert.Equal(2, sig.Parameters!.Count());
    }

    [Fact]
    public async Task SecondArgument_HighlightsSecondParam()
    {
        var help = await HelpAt(Wallet.Replace("1m, \"x\"", "1m, |\"x\""));

        Assert.NotNull(help);
        Assert.Equal(1, help!.ActiveParameter);
    }

    [Fact]
    public async Task NonConstructorCall_ReturnsNothing()
    {
        // Caret sits in the outer `.Tell( … )` call, which is not a `new Msg(`.
        var help = await HelpAt(Wallet.Replace("self.Tell(new", "self.Tell(|new"));
        Assert.Null(help);
    }

    [Fact]
    public async Task UnclosedCall_StillHelpsFromLastGoodTree()
    {
        // The live source is truncated mid-argument, so the parser rejects it and
        // the live tree is null — but a prior good parse (the seed) still carries
        // the Deposit declaration, so help must survive.
        const string half = """
            actor Driver
            {
                init() { self.Tell(new Deposit(|
            """;
        var help = await HelpAt(half, seed: Wallet);

        Assert.NotNull(help);
        Assert.Equal("Deposit(decimal amount, string note)", Assert.Single(help!.Signatures).Label);
        Assert.Equal(0, help.ActiveParameter);
    }
}
