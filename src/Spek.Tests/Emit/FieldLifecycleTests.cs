using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Spek.Compiler.Semantic;
using Xunit;

namespace Spek.Tests.Emit;

/// <summary>
/// Field lifecycle markers (`deprecated`, `retired`) on shared
/// regions and actors. The full migration mechanism is parked to a
/// later release; for now we ship just these informational markers so
/// data providers know which keys to drop and so authors get
/// compile-time guidance about which fields not to reach for in new
/// code.
/// </summary>
public sealed class FieldLifecycleTests
{
    private static string EmitCSharp(string source)
    {
        var parsed = SpekCompiler.Parse(source);
        Assert.True(parsed.Success,
            "Source must parse; diagnostics: " +
            string.Join("\n", parsed.Diagnostics.Select(d => $"{d.Code} {d.Line}:{d.Column} {d.Message}")));
        return new FileEmitter().Emit(parsed.Tree!);
    }

    // ─── deprecated — captured/restored, warns on use ──────────────────

    [Fact]
    public void DeprecatedField_OnPersistedRegion_StillCapturedAndRestored()
    {
        // The data still roundtrips so existing snapshots survive the
        // deprecation period — only the *referencing* code gets a
        // warning.
        const string src = """
            shared MarketCache : Persisted
            {
                long lastPrice = 0;
                deprecated string oldSymbol = "";
            }
            program Main
            {
                var system = new ActorSystem("test");
                system.RegisterPersistenceProvider<MarketCache>(new InMemorySnapshotStore());
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("[\"oldSymbol\"] = oldSymbol,", code);
        Assert.Contains("if (snapshot.Fields.ContainsKey(\"oldSymbol\"))", code);
    }

    [Fact]
    public void CE0101_DeprecatedFieldReference_ReportsWarning()
    {
        const string src = """
            message Refresh();
            shared MarketCache
            {
                deprecated string oldSymbol = "";
            }
            actor Trader
            {
                use MarketCache cache;

                on Refresh =>
                {
                    var s = cache.oldSymbol;
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        // Warnings don't fail the parse — Success stays true.
        Assert.True(parsed.Success,
            "Deprecated field references should warn, not error: " +
            string.Join("\n", parsed.Diagnostics.Select(d => $"{d.Code} {d.Severity}: {d.Message}")));
        Assert.Contains(parsed.Diagnostics, d =>
            d.Code == "CE0101" && d.Severity == DiagnosticSeverity.Warning);
    }

    // ─── retired — skipped from capture, error on use ──────────────────

    [Fact]
    public void RetiredField_OnPersistedRegion_SkippedFromCaptureAndRestore()
    {
        // The provider sees the key disappear from new snapshots and
        // cleans its storage on the next save.
        const string src = """
            shared MarketCache : Persisted
            {
                long lastPrice = 0;
                retired string oldSymbol = "";
            }
            program Main
            {
                var system = new ActorSystem("test");
                system.RegisterPersistenceProvider<MarketCache>(new InMemorySnapshotStore());
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("[\"lastPrice\"] = lastPrice,", code);
        Assert.DoesNotContain("[\"oldSymbol\"]", code);
        Assert.DoesNotContain("snapshot.Fields.ContainsKey(\"oldSymbol\")", code);
    }

    [Fact]
    public void CE0102_RetiredFieldReference_ReportsError()
    {
        const string src = """
            message Refresh();
            shared MarketCache
            {
                retired string oldSymbol = "";
            }
            actor Trader
            {
                use MarketCache cache;

                on Refresh =>
                {
                    var s = cache.oldSymbol;
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.False(parsed.Success);
        Assert.Contains(parsed.Diagnostics, d =>
            d.Code == "CE0102" && d.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    public void RetiredField_OnNonPersistedRegion_ParsesButIsNoop()
    {
        // No persistence shape, so capture/restore aren't emitted at all.
        // The field still occupies the region's class for name-reservation.
        const string src = """
            shared Counters
            {
                int active = 0;
                retired int legacyCount = 0;
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("public int active = 0;", code);
        Assert.Contains("public int legacyCount = 0;", code);
        Assert.DoesNotContain("CaptureFields", code);
    }

    // ─── Mutual exclusion + interaction with transient ─────────────────

    [Fact]
    public void TransientAndDeprecated_AreMutuallyExclusiveAtTheGrammarLevel()
    {
        // The grammar accepts at most one of TRANSIENT, DEPRECATED, RETIRED.
        // Stacking them is a parse error (CE0001).
        const string src = """
            shared X
            {
                transient deprecated int n = 0;
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.False(parsed.Success);
    }

    [Fact]
    public void DeprecatedField_RoundTripsThroughRoslyn()
    {
        const string src = """
            shared MarketCache : Persisted
            {
                long lastPrice = 0;
                deprecated string oldSymbol = "";
            }
            program Main
            {
                var system = new ActorSystem("test");
                system.RegisterPersistenceProvider<MarketCache>(new InMemorySnapshotStore());
            }
            """;
        var code = EmitCSharp(src);
        var (success, errors, _) = RoslynCompileHelper.TryCompile(code, "DeprecatedFieldSmoke");
        Assert.True(success,
            "Emitted C# did not compile:\n" + errors + "\n--- emitted ---\n" + code);
    }

    [Fact]
    public void RetiredField_RoundTripsThroughRoslyn()
    {
        const string src = """
            shared MarketCache : Persisted
            {
                long lastPrice = 0;
                retired string oldSymbol = "";
            }
            program Main
            {
                var system = new ActorSystem("test");
                system.RegisterPersistenceProvider<MarketCache>(new InMemorySnapshotStore());
            }
            """;
        var code = EmitCSharp(src);
        var (success, errors, _) = RoslynCompileHelper.TryCompile(code, "RetiredFieldSmoke");
        Assert.True(success,
            "Emitted C# did not compile:\n" + errors + "\n--- emitted ---\n" + code);
    }
}
