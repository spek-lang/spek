using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using Spek.Compiler.Parser;
using Xunit;

// Disambiguate: OmniSharp's Protocol.Models has its own CompletionContext type.
using CompletionContext = Spek.LanguageServer.CompletionContext;

namespace Spek.Tests.LanguageServer;

/// <summary>
/// Covers <see cref="CompletionContext.Collect"/> — what the LSP's
/// completion handler layers on top of the static keyword list when the
/// user is typing inside an actor body.
/// </summary>
public class CompletionContextTests
{
    private static Spek.Compiler.AST.SpekFile ParseOrFail(string source)
    {
        var r = SpekCompiler.Parse(source);
        Assert.True(r.Success,
            string.Join("\n", r.Diagnostics.Select(d => $"  {d.Code} {d.Line}:{d.Column} {d.Message}")));
        return r.Tree!;
    }

    [Fact]
    public void InsideBehaviorHandler_IncludesFieldsAndBehaviors()
    {
        // All three behaviors reachable so CE0014 (unused behavior)
        // doesn't trip the parse assertion.
        const string src = """
            message Ping();
            actor A
            {
                decimal balance = 0.00m;
                string  owner   = "unknown";

                behavior Idle   { on Ping => { become Busy;   } }
                behavior Busy   { on Ping => { become Frozen; } }
                behavior Frozen { on Ping => { become Idle;   } }
            }
            """;
        var tree = ParseOrFail(src);

        // Position cursor inside the `Idle` handler body.
        var lines = src.Split('\n');
        var handlerLine = lines.ToList().FindIndex(l => l.Contains("behavior Idle")) + 1;
        var handlerCol  = lines[handlerLine - 1].IndexOf("{ on") + 3;

        var scope = CompletionContext.Collect(tree, handlerLine, handlerCol);

        Assert.Contains(scope, s => s.Name == "balance" && s.Kind == CompletionItemKind.Field);
        Assert.Contains(scope, s => s.Name == "owner"   && s.Kind == CompletionItemKind.Field);
        Assert.Contains(scope, s => s.Name == "Idle"    && s.Kind == CompletionItemKind.EnumMember);
        Assert.Contains(scope, s => s.Name == "Busy"    && s.Kind == CompletionItemKind.EnumMember);
        Assert.Contains(scope, s => s.Name == "Frozen"  && s.Kind == CompletionItemKind.EnumMember);
    }

    [Fact]
    public void InsideBoundHandler_IncludesPatternBinding()
    {
        const string src = """
            message Deposit(decimal amount);
            actor Wallet
            {
                behavior Active
                {
                    on Deposit d => { }
                }
            }
            """;
        var tree = ParseOrFail(src);

        // Position cursor inside the `d => { }` handler body.
        var lines = src.Split('\n');
        var line = lines.ToList().FindIndex(l => l.Contains("on Deposit d")) + 1;
        var col  = lines[line - 1].IndexOf("=>") + 5;

        var scope = CompletionContext.Collect(tree, line, col);

        Assert.Contains(scope, s =>
            s.Name == "d" && s.Kind == CompletionItemKind.Variable && s.Detail == "Deposit");
    }

    [Fact]
    public void FileScope_IncludesMessageAndActorTypes()
    {
        const string src = """
            message Ping();
            message Pong();
            actor Echo { behavior Idle { on Ping => return new Pong(); } }
            actor Pinger { behavior Ready { on Ping => { } } }
            """;
        var tree = ParseOrFail(src);

        // Position cursor inside Echo's handler.
        var scope = CompletionContext.Collect(tree, line: 3, column: 32);

        Assert.Contains(scope, s => s.Name == "Ping"   && s.Kind == CompletionItemKind.Struct);
        Assert.Contains(scope, s => s.Name == "Pong"   && s.Kind == CompletionItemKind.Struct);
        Assert.Contains(scope, s => s.Name == "Echo"   && s.Kind == CompletionItemKind.Class);
        Assert.Contains(scope, s => s.Name == "Pinger" && s.Kind == CompletionItemKind.Class);
    }

    [Fact]
    public void OutsideAnyActor_OnlyFileScopeSymbolsReturned()
    {
        const string src = """
            message Ping();
            actor A { behavior Idle { on Ping => { } } }
            """;
        var tree = ParseOrFail(src);

        // Column 1000 on line 1 — past every span — but message/actor
        // file-scope decls always apply.
        var scope = CompletionContext.Collect(tree, line: 1, column: 1000);

        // File-scope types are always present.
        Assert.Contains(scope, s => s.Name == "Ping");
        Assert.Contains(scope, s => s.Name == "A");

        // Nothing actor-interior should show up.
        Assert.DoesNotContain(scope, s => s.Kind == CompletionItemKind.Field);
        Assert.DoesNotContain(scope, s => s.Kind == CompletionItemKind.EnumMember);
    }
}
