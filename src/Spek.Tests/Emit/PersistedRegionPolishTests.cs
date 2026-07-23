using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Xunit;

namespace Spek.Tests.Emit;

/// <summary>
/// Persisted-region polish. Two refinements:
///   1. <c>transient</c> field keyword — opts a field out of
///      <c>CaptureFields()</c> / <c>RestoreFields()</c>. Behaves
///      identically on non-persisted regions and on actor fields
///      (the keyword parses but the emitter ignores it where there
///      is no persistence shape to opt out of).
///   2. CE0100 — semantic check that catches direct assignments of a
///      shared-region read into an actor field. Forces an explicit
///      borrow through a local or a deep-copy call so the actor
///      doesn't hold a reference past the region's lock.
/// </summary>
public sealed class PersistedRegionPolishTests
{
    private static string EmitCSharp(string source)
    {
        var parsed = SpekCompiler.Parse(source);
        Assert.True(parsed.Success,
            "Source must parse; diagnostics: " +
            string.Join("\n", parsed.Diagnostics.Select(d => $"{d.Code} {d.Line}:{d.Column} {d.Message}")));
        return new FileEmitter().Emit(parsed.Tree!);
    }

    // ─── transient ───────────────────────────────────────────────────────

    [Fact]
    public void TransientField_OnPersistedRegion_SkippedFromCapture()
    {
        const string src = """
            shared MarketCache : Persisted
            {
                long lastPrice = 0;
                transient int sessionRequests = 0;
            }
            program Main
            {
                var system = new ActorSystem("test");
                system.RegisterPersistenceProvider<MarketCache>(new InMemorySnapshotStore());
            }
            """;
        var code = EmitCSharp(src);

        // The persisted field appears in capture/restore.
        Assert.Contains("[\"lastPrice\"] = lastPrice,", code);
        Assert.Contains("if (snapshot.Fields.ContainsKey(\"lastPrice\"))", code);

        // The transient field does NOT appear in capture/restore.
        Assert.DoesNotContain("[\"sessionRequests\"]", code);
        Assert.DoesNotContain("snapshot.Fields.ContainsKey(\"sessionRequests\")", code);
        Assert.DoesNotContain("sessionRequests = snapshot.Get<", code);
    }

    [Fact]
    public void TransientField_OnPersistedRegion_KeepsInitialiser()
    {
        // The transient field is still emitted as a normal C# field —
        // it just doesn't participate in persistence. Initialiser holds.
        const string src = """
            shared MarketCache : Persisted
            {
                long lastPrice = 0;
                transient int sessionRequests = 0;
            }
            program Main
            {
                var system = new ActorSystem("test");
                system.RegisterPersistenceProvider<MarketCache>(new InMemorySnapshotStore());
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("public int sessionRequests = 0;", code);
    }

    [Fact]
    public void TransientField_OnNonPersistedRegion_ParsesButHasNoEffect()
    {
        // Without ': Persisted' there is no capture/restore; the
        // keyword just parses cleanly and the field is a normal C#
        // field on the SharedRegion base.
        const string src = """
            shared Counters
            {
                transient int inflightRequests = 0;
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("public int inflightRequests = 0;", code);
        Assert.DoesNotContain("CaptureFields", code);
        Assert.DoesNotContain("RestoreFields", code);
    }

    [Fact]
    public void TransientField_OnActor_SkippedFromCapture()
    {
        // Actor-level capture (driven by `persist;` or `passivate after`)
        // also honours the transient keyword.
        const string src = """
            message Tick();
            actor Counter
            {
                int total = 0;
                transient int sinceRestart = 0;

                passivate after System.TimeSpan.FromMinutes(10);

                on Tick =>
                {
                    total = total + 1;
                    sinceRestart = sinceRestart + 1;
                    persist;
                }

                on Restore(Snapshot s) =>
                {
                    total = s.Get<int>("total");
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("[\"total\"] = _total,", code);
        Assert.DoesNotContain("[\"sinceRestart\"]", code);
    }

    [Fact]
    public void TransientField_RoundTripsThroughRoslyn()
    {
        const string src = """
            shared MarketCache : Persisted
            {
                long lastPrice = 0;
                transient int sessionRequests = 0;
            }
            program Main
            {
                var system = new ActorSystem("test");
                system.RegisterPersistenceProvider<MarketCache>(new InMemorySnapshotStore());
            }
            """;
        var code = EmitCSharp(src);
        var (success, errors, _) = RoslynCompileHelper.TryCompile(code, "TransientFieldSmoke");
        Assert.True(success,
            "Emitted C# did not compile:\n" + errors + "\n--- emitted ---\n" + code);
    }

    // ─── CE0100 deep-copy ───────────────────────────────────────────────

    [Fact]
    public void CE0100_DirectAssignment_FromRegionFieldToActorField_Reports()
    {
        const string src = """
            message Refresh();
            shared MarketCache
            {
                string lastSymbol = "";
            }
            actor Trader
            {
                use MarketCache cache;
                string mySymbol = "";

                on Refresh =>
                {
                    mySymbol = cache.lastSymbol;
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.False(parsed.Success);
        Assert.Contains(parsed.Diagnostics, d => d.Code == "CE0100");
    }

    [Fact]
    public void CE0100_RegionFieldThroughLocal_NoReport()
    {
        // Routing the region read through a local sanitises the borrow —
        // the local is a snapshot the developer chose to copy out.
        const string src = """
            message Refresh();
            shared MarketCache
            {
                string lastSymbol = "";
            }
            actor Trader
            {
                use MarketCache cache;
                string mySymbol = "";

                on Refresh =>
                {
                    var snap = cache.lastSymbol;
                    mySymbol = snap;
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.True(parsed.Success,
            "Diagnostics: " +
            string.Join("\n", parsed.Diagnostics.Select(d => $"{d.Code} {d.Line}:{d.Column} {d.Message}")));
    }

    [Fact]
    public void CE0100_RegionFieldThroughFunction_NoReport()
    {
        // Wrapping the read in a function/method call also signals
        // intent — the developer is taking a deep copy or projecting.
        const string src = """
            message Refresh();
            shared MarketCache
            {
                string lastSymbol = "";
            }
            actor Trader
            {
                use MarketCache cache;
                string mySymbol = "";

                on Refresh =>
                {
                    mySymbol = string.Copy(cache.lastSymbol);
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.True(parsed.Success,
            "Diagnostics: " +
            string.Join("\n", parsed.Diagnostics.Select(d => $"{d.Code} {d.Line}:{d.Column} {d.Message}")));
    }

    [Fact]
    public void CE0100_AssignmentWithinRegion_NoReport()
    {
        // Writing a region field into another region field is a region-
        // internal concern; CE0100 is about the actor/region edge only.
        const string src = """
            shared MarketCache
            {
                string a = "";
                string b = "";
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.True(parsed.Success);
    }
}
