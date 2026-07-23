using Spek.Compiler.Parser;
using Xunit;

namespace Spek.Tests.Semantic;

/// <summary>
/// Tests <see cref="SpekCompilation"/>'s cross-file symbol resolution.
/// The pay-off: a <c>message</c> declared in one file is visible to an
/// <c>ask</c>/<c>Tell</c> in another, without triggering false-positive
/// "undeclared message" diagnostics under single-file analysis.
/// </summary>
public class SpekCompilationTests
{
    [Fact]
    public void Messages_Declared_In_OtherFile_Resolve_At_Call_Site()
    {
        // File A: declares the message.
        const string fileA = """
            namespace MyBank.Messages;
            message Balance();
            message BalanceResponse(decimal amount);
            """;

        // File B: uses the message in an ask without re-declaring it.
        const string fileB = """
            namespace MyBank.Actors;
            using MyBank.Messages;

            message Query();
            actor BankQueryActor
            {
                int placeholder = 0;
                behavior Idle
                {
                    on Query =>
                    {
                        var r = placeholder.Ask(new Balance());
                    }
                }
            }
            """;

        var compilation = SpekCompilation.Create(new[]
        {
            ("messages.spek", fileA),
            ("actors.spek",   fileB),
        });

        Assert.True(compilation.Success,
            "Expected success; diagnostics:\n" +
            string.Join("\n", compilation.Diagnostics.Select(d => $"  {d.Code} {d.Line}:{d.Column} {d.Message}")));
        Assert.Equal(2, compilation.Files.Count);
    }

    [Fact]
    public void UnresolvedMessage_In_Multifile_Still_Reports_CE0020()
    {
        // Reference a message that exists in neither file.
        const string fileA = """
            namespace A;
            message Known();
            """;
        const string fileB = """
            namespace B;
            message Query();
            actor B_Actor
            {
                int placeholder = 0;
                behavior Idle
                {
                    on Query =>
                    {
                        var r = placeholder.Ask(new DoesNotExist());
                    }
                }
            }
            """;

        var compilation = SpekCompilation.Create(new[]
        {
            ("a.spek", fileA),
            ("b.spek", fileB),
        });

        var ce0020 = compilation.Diagnostics.Single(d => d.Code == "CE0020");
        Assert.Contains("DoesNotExist", ce0020.Message);
    }

    [Fact]
    public void Single_File_Via_Compilation_Still_Works()
    {
        const string src = """
            message Ping();
            actor A
            {
                behavior Idle { on Ping => { } }
            }
            """;

        var compilation = SpekCompilation.Create(new[] { ("one.spek", src) });

        Assert.True(compilation.Success);
        Assert.Single(compilation.Files);
    }

    [Fact]
    public void CE0013_Fires_When_Message_Name_Collides_In_Same_Namespace()
    {
        // Both files declare `namespace MyApp;` — same namespace. The
        // simple name `Transfer` collides; CE0013 must fire.
        const string fileA = """
            namespace MyApp;
            message Transfer(decimal amount);
            """;
        const string fileB = """
            namespace MyApp;
            message Transfer(string target);
            actor A { behavior Idle { on Transfer t => { } } }
            """;

        var compilation = SpekCompilation.Create(new[]
        {
            ("a.spek", fileA),
            ("b.spek", fileB),
        });

        var ce0013 = compilation.Diagnostics.Single(d => d.Code == "CE0013");
        Assert.Contains("Transfer", ce0013.Message);
        Assert.Contains("across files", ce0013.Message);
    }

    [Fact]
    public void CE0013_Fires_When_Actor_Name_Collides_In_Same_Namespace()
    {
        const string fileA = """
            namespace MyApp;
            message Ping();
            actor Worker { behavior Idle { on Ping => { } } }
            """;
        const string fileB = """
            namespace MyApp;
            message Ping();
            actor Worker { behavior Idle { on Ping => { } } }
            """;

        var compilation = SpekCompilation.Create(new[]
        {
            ("a.spek", fileA),
            ("b.spek", fileB),
        });

        // Both `Ping` and `Worker` collide — expect two CE0013s.
        var crossFileDuplicates = compilation.Diagnostics
            .Where(d => d.Code == "CE0013" && d.Message.Contains("across files"))
            .ToList();
        Assert.Equal(2, crossFileDuplicates.Count);
    }

    /// <summary>
    /// Namespace-scoped resolution: two files declaring the same
    /// simple name under DIFFERENT namespaces emit two distinct C# types
    /// and must NOT trip CE0013.
    /// </summary>
    [Fact]
    public void Namespace_Scoped_Resolution_AllowsSameNameInDifferentNamespaces()
    {
        const string fileA = """
            namespace Bank.A;
            message Transfer(decimal amount);
            """;
        const string fileB = """
            namespace Bank.B;
            message Transfer(string target);
            actor B { behavior Idle { on Transfer t => { } } }
            """;

        var compilation = SpekCompilation.Create(new[]
        {
            ("a.spek", fileA),
            ("b.spek", fileB),
        });

        Assert.True(compilation.Success,
            "Cross-namespace names should not flag CE0013: " +
            string.Join("\n", compilation.Diagnostics.Select(d => $"  {d.Code} {d.Message}")));
        Assert.DoesNotContain(compilation.Diagnostics, d => d.Code == "CE0013");
    }

    /// <summary>
    /// Sibling case to the test above: two files in DIFFERENT namespaces
    /// each declaring the same simple `actor` name should also be
    /// permitted (the emitted C# classes live in different namespaces).
    /// </summary>
    [Fact]
    public void Namespace_Scoped_Resolution_AllowsSameActorNameInDifferentNamespaces()
    {
        const string fileA = """
            namespace MyApp.Workers;
            message Run();
            actor Worker { behavior Idle { on Run => { } } }
            """;
        const string fileB = """
            namespace MyApp.Coordinators;
            message Run();
            actor Worker { behavior Idle { on Run => { } } }
            """;

        var compilation = SpekCompilation.Create(new[]
        {
            ("a.spek", fileA),
            ("b.spek", fileB),
        });

        Assert.True(compilation.Success,
            "Cross-namespace actors should not flag CE0013: " +
            string.Join("\n", compilation.Diagnostics.Select(d => $"  {d.Code} {d.Message}")));
        Assert.DoesNotContain(compilation.Diagnostics, d => d.Code == "CE0013");
    }

    /// <summary>
    /// Files without explicit namespaces share an implicit "" bucket —
    /// declaring the same simple name in two namespace-less files
    /// SHOULD still flag CE0013.
    /// </summary>
    [Fact]
    public void NoNamespace_DuplicateAcrossFiles_StillFlagsCe0013()
    {
        const string fileA = """
            message Ping();
            """;
        const string fileB = """
            message Ping();
            """;

        var compilation = SpekCompilation.Create(new[]
        {
            ("a.spek", fileA),
            ("b.spek", fileB),
        });

        Assert.Contains(compilation.Diagnostics,
            d => d.Code == "CE0013" && d.Message.Contains("across files"));
    }

    [Fact]
    public void CE0013_SingleFile_Duplicate_StillReported_NotDoubled()
    {
        // Same-file dup should be reported once (by per-file check),
        // not twice (by per-file AND cross-file).
        const string fileA = """
            message Ping();
            message Ping();
            actor A { behavior Idle { on Ping => { } } }
            """;

        var compilation = SpekCompilation.Create(new[] { ("a.spek", fileA) });

        var ce0013s = compilation.Diagnostics.Where(d => d.Code == "CE0013").ToList();
        Assert.Single(ce0013s);
        Assert.DoesNotContain("across files", ce0013s[0].Message);
    }

    [Fact]
    public void ParseError_In_OneFile_Does_Not_Block_Others_From_Analysis()
    {
        const string bad = "actor A { this is nonsense";  // parse error
        const string good = """
            message Ping();
            actor B { behavior Idle { on Ping => { } } }
            """;

        var compilation = SpekCompilation.Create(new[]
        {
            ("bad.spek", bad),
            ("good.spek", good),
        });

        Assert.False(compilation.Success);
        // Parse errors from the bad file surface, but the good file is still
        // analyzed and gets no spurious errors.
        Assert.Contains(compilation.Diagnostics, d => d.Code == "CE0001");
        Assert.Single(compilation.Files);  // only the parseable one
    }
}
