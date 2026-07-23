using Spek.Compiler.AST;
using Spek.Compiler.Parser;
using Spek.LanguageServer;
using Xunit;

namespace Spek.Tests.LanguageServer;

/// <summary>
/// Coverage for <see cref="ReferenceFinder"/> — the AST walker that powers
/// <c>textDocument/rename</c> and <c>textDocument/references</c>. We assert
/// the count of occurrences (declaration + references) by feeding the
/// parser a known fixture and checking the spans returned.
/// </summary>
public class ReferenceFinderTests
{
    private static SpekFile Parse(string src)
    {
        var r = SpekCompiler.Parse(src);
        Assert.True(r.Success,
            string.Join("\n", r.Diagnostics.Select(d => $"  {d.Code} {d.Line}:{d.Column} {d.Message}")));
        return r.Tree!;
    }

    [Fact]
    public void Message_References_NewExpr_AndPattern()
    {
        // Ping is referenced 3 times (decl + new + pattern) — find all
        // 4 occurrences (decl + 3 refs) when IncludeDeclaration = true.
        const string src = """
            message Ping();
            message Pong();

            actor Echo
            {
                behavior Idle
                {
                    on Ping => { sender.Tell(new Pong()); }
                }
            }

            actor Caller
            {
                behavior Active
                {
                    on Ping p => { /* ... */ }
                }
            }
            """;
        var tree = Parse(src);

        var allOccurrences = ReferenceFinder.FindAllOccurrences(
            tree, ReferenceFinder.Kind.Message, "Ping");
        // declaration + on Ping (Echo) + on Ping p (Caller) = 3
        Assert.Equal(3, allOccurrences.Count);

        var refsOnly = ReferenceFinder.FindReferences(
            tree, ReferenceFinder.Kind.Message, "Ping");
        Assert.Equal(2, refsOnly.Count);
    }

    [Fact]
    public void Actor_References_FoundOnSpawn()
    {
        const string src = """
            message Spawn();
            actor Worker { behavior Idle { on Spawn => { } } }
            actor Boss
            {
                behavior Idle
                {
                    on Spawn => { var w = spawn<Worker>(); }
                }
            }
            """;
        var tree = Parse(src);

        var refs = ReferenceFinder.FindReferences(
            tree, ReferenceFinder.Kind.Actor, "Worker");
        // The spawn<Worker>() type-arg is the reference.
        Assert.Single(refs);
    }

    [Fact]
    public void Behavior_References_ScopedToOwningActor()
    {
        // Two actors both have a behavior named "Idle". A become Idle
        // inside actor A should ONLY count for A's Idle, not B's.
        const string src = """
            message Tick();
            actor A
            {
                behavior Idle  { on Tick => { become Busy; } }
                behavior Busy  { on Tick => { become Idle; } }
            }
            actor B
            {
                behavior Idle  { on Tick => { become Other; } }
                behavior Other { on Tick => { become Idle; } }
            }
            """;
        var tree = Parse(src);

        var actorA = tree.Declarations.OfType<ActorDecl>().Single(a => a.Name == "A");
        var actorB = tree.Declarations.OfType<ActorDecl>().Single(a => a.Name == "B");

        var aIdleRefs = ReferenceFinder.FindReferences(
            tree, ReferenceFinder.Kind.Behavior, "Idle", actorA);
        var bIdleRefs = ReferenceFinder.FindReferences(
            tree, ReferenceFinder.Kind.Behavior, "Idle", actorB);

        // Each actor's `become Idle;` references count only within its scope.
        Assert.Single(aIdleRefs);
        Assert.Single(bIdleRefs);
    }

    [Fact]
    public void Channel_References_ImplementedAndInherited()
    {
        const string src = """
            message Ping();
            channel HostBase { on Ping; }
            channel ServerHost : HostBase { }
            actor MyServer : ServerHost { behavior Idle { on Ping => { } } }
            """;
        var tree = Parse(src);

        var hostBaseRefs = ReferenceFinder.FindReferences(
            tree, ReferenceFinder.Kind.Channel, "HostBase");
        // Referenced once: as a base of ServerHost.
        Assert.Single(hostBaseRefs);

        var serverHostRefs = ReferenceFinder.FindReferences(
            tree, ReferenceFinder.Kind.Channel, "ServerHost");
        // Referenced once: in MyServer's colon list.
        Assert.Single(serverHostRefs);
    }

    [Fact]
    public void Enum_References_FoundInMessageFieldType()
    {
        const string src = """
            enum HostState { Running, Stopped }
            message StateChanged(HostState state);
            """;
        var tree = Parse(src);

        var refs = ReferenceFinder.FindReferences(
            tree, ReferenceFinder.Kind.Enum, "HostState");
        // Referenced once: as the StateChanged field type.
        Assert.Single(refs);
    }
}

/// <summary>
/// Coverage for <see cref="SymbolUnderCursor"/> — the dispatcher that
/// figures out what symbol the cursor is on. Drives both rename and
/// find-references.
/// </summary>
public class SymbolUnderCursorTests
{
    private static SpekFile Parse(string src)
    {
        var r = SpekCompiler.Parse(src);
        Assert.True(r.Success,
            string.Join("\n", r.Diagnostics.Select(d => $"  {d.Code} {d.Line}:{d.Column} {d.Message}")));
        return r.Tree!;
    }

    [Fact]
    public void CursorOnNewExprMessageType_ResolvesAsMessage()
    {
        const string src = """
            message Ping();
            actor Echo { behavior Idle { on Ping => { var x = new Ping(); } } }
            """;
        var tree = Parse(src);

        // Position cursor on "Ping" inside `new Ping()` — line 2, somewhere
        // around column ~50 ish. Find by string scan.
        var lines = src.Split('\n');
        var pingCol = lines[1].LastIndexOf("Ping") + 2; // mid-token
        var (kind, name, _) = SymbolUnderCursor.Resolve(tree, line: 2, column: pingCol);

        Assert.Equal(ReferenceFinder.Kind.Message, kind);
        Assert.Equal("Ping", name);
    }

    [Fact]
    public void CursorOnSpawnTypeArg_ResolvesAsActor()
    {
        const string src = """
            message Go();
            actor Worker { behavior Idle { on Go => { } } }
            actor Boss
            {
                behavior Active { on Go => { var w = spawn<Worker>(); } }
            }
            """;
        var tree = Parse(src);

        // Cursor on "Worker" inside `spawn<Worker>()`.
        var lines = src.Split('\n');
        var wIdx  = lines.ToList().FindIndex(l => l.Contains("spawn<Worker>"));
        var col   = lines[wIdx].IndexOf("Worker") + 2;
        var (kind, name, _) = SymbolUnderCursor.Resolve(tree, line: wIdx + 1, column: col);

        Assert.Equal(ReferenceFinder.Kind.Actor, kind);
        Assert.Equal("Worker", name);
    }

    [Fact]
    public void CursorOnBecomeTarget_ResolvesAsBehaviorScopedToActor()
    {
        const string src = """
            message Tick();
            actor Toggle
            {
                behavior On  { on Tick => { become Off; } }
                behavior Off { on Tick => { become On;  } }
            }
            """;
        var tree = Parse(src);

        // Cursor on "Off" inside `become Off;`.
        var lines = src.Split('\n');
        var lineIdx = lines.ToList().FindIndex(l => l.Contains("become Off"));
        var col     = lines[lineIdx].IndexOf("Off") + 2;
        var (kind, name, owner) = SymbolUnderCursor.Resolve(tree, line: lineIdx + 1, column: col);

        Assert.Equal(ReferenceFinder.Kind.Behavior, kind);
        Assert.Equal("Off", name);
        Assert.NotNull(owner);
        Assert.Equal("Toggle", owner!.Name);
    }
}
