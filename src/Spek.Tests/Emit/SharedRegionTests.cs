using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Xunit;

namespace Spek.Tests.Emit;

/// <summary>
/// Phase 1 — `shared X { fields... }` regions plus `use X foo;`
/// attachment in actor bodies. Reader/writer handlers acquire the
/// region's RW lock around their body. Init blocks, persistence
/// clauses, and CE0087 extensions are out of scope here — covered by
/// follow-up commits.
/// </summary>
public sealed class SharedRegionTests
{
    private static string EmitCSharp(string source)
    {
        var parsed = SpekCompiler.Parse(source);
        Assert.True(parsed.Success,
            "Source must parse; diagnostics: " +
            string.Join("\n", parsed.Diagnostics.Select(d => $"{d.Code} {d.Line}:{d.Column} {d.Message}")));
        return new FileEmitter().Emit(parsed.Tree!);
    }

    [Fact]
    public void SharedRegion_Parses_AsTopLevelDeclaration()
    {
        const string src = """
            shared MarketCache
            {
                int lastUpdated = 0;
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.True(parsed.Success,
            "Source must parse; diagnostics: " +
            string.Join("\n", parsed.Diagnostics.Select(d => $"{d.Code} {d.Line}:{d.Column} {d.Message}")));
    }

    [Fact]
    public void SharedRegion_EmitsClassDerivingFromSharedRegion()
    {
        const string src = """
            shared MarketCache
            {
                int lastUpdated = 0;
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("internal sealed class MarketCache : Spek.SharedRegion", code);
        Assert.Contains("public int lastUpdated = 0;", code);
    }

    [Fact]
    public void UseDecl_EmitsLazyPropertyAccessor()
    {
        const string src = """
            shared MarketCache
            {
                int lastUpdated = 0;
            }
            actor PriceWriter
            {
                use MarketCache cache;
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("private MarketCache? __region_cache;", code);
        Assert.Contains("private MarketCache cache", code);
        Assert.Contains("=> __region_cache ??= GetSharedRegion<MarketCache>();", code);
    }

    [Fact]
    public void WriterHandler_WrapsBodyInWriterLock()
    {
        const string src = """
            message Update(int symbol, long price);
            shared MarketCache
            {
                long lastPrice = 0;
            }
            actor PriceWriter
            {
                use MarketCache cache;
                writer on Update u => { cache.lastPrice = u.price; }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("await cache.EnterWriterAsync();", code);
        Assert.Contains("cache.ExitWriter();", code);
        Assert.Contains("cache.lastPrice = u.price;", code);
    }

    [Fact]
    public void ReaderHandler_WrapsBodyInReaderLock()
    {
        const string src = """
            message GetLast();
            message LastPrice(long price);
            shared MarketCache
            {
                long lastPrice = 0;
            }
            actor PriceReader
            {
                use MarketCache cache;
                reader on GetLast g => { return new LastPrice(cache.lastPrice); }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("await cache.EnterReaderAsync();", code);
        Assert.Contains("cache.ExitReader();", code);
    }

    [Fact]
    public void UseDecl_UnresolvedRegion_TriggersCE0097()
    {
        const string src = """
            actor Bad
            {
                use NotARegion cache;
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.Contains(parsed.Diagnostics,
            d => d.Code == "CE0097" && d.Message.Contains("NotARegion"));
    }

    // ─── Phase 2 — init block ───────────────────────────────────────────

    [Fact]
    public void SharedRegion_InitBlock_EmitsInitializeOverride()
    {
        const string src = """
            shared MarketCache
            {
                long lastUpdated = 0;
                init { lastUpdated = 42; }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("protected override void Initialize()", code);
        Assert.Contains("lastUpdated = 42;", code);
    }

    [Fact]
    public void SharedRegion_NoInitBlock_DoesNotEmitInitializeOverride()
    {
        const string src = """
            shared MarketCache
            {
                long lastUpdated = 0;
            }
            """;
        var code = EmitCSharp(src);
        Assert.DoesNotContain("protected override void Initialize()", code);
    }

    [Fact]
    public void SharedRegion_InitBlockWithReaderActor_RoundTripsThroughRoslyn()
    {
        const string src = """
            message GetLast();
            message Reply(long price);
            shared MarketCache
            {
                long lastPrice = 0;
                init { lastPrice = 100; }
            }
            actor PriceReader
            {
                use MarketCache cache;
                reader on GetLast g => { return new Reply(cache.lastPrice); }
            }
            """;
        var code = EmitCSharp(src);
        var (success, errors, _) = RoslynCompileHelper.TryCompile(code, "SharedRegionInitSmoke");
        Assert.True(success,
            "Emitted C# did not compile:\n" + errors + "\n--- emitted ---\n" + code);
    }

    // ─── Phase 2 — CE0087 extension ─────────────────────────────────────

    [Fact]
    public void ReaderHandler_MutatingRegionField_TriggersCE0087()
    {
        const string src = """
            message Tick();
            shared MarketCache
            {
                long lastPrice = 0;
            }
            actor Bad
            {
                use MarketCache cache;
                reader on Tick t => { cache.lastPrice = 99; }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.Contains(parsed.Diagnostics,
            d => d.Code == "CE0087" && d.Message.Contains("'cache'"));
    }

    [Fact]
    public void WriterHandler_MutatingRegionField_OK()
    {
        // Same shape as the CE0087 case but writer mode — must NOT
        // trigger the diagnostic.
        const string src = """
            message Tick();
            shared MarketCache
            {
                long lastPrice = 0;
            }
            actor Good
            {
                use MarketCache cache;
                writer on Tick t => { cache.lastPrice = 99; }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.DoesNotContain(parsed.Diagnostics, d => d.Code == "CE0087");
    }

    [Fact]
    public void ReaderHandler_ReadingRegionField_OK()
    {
        // Reading a region field from a reader is the canonical
        // intended pattern — no diagnostic.
        const string src = """
            message Tick();
            message Reply(long v);
            shared MarketCache
            {
                long lastPrice = 0;
            }
            actor Reader
            {
                use MarketCache cache;
                reader on Tick t => { return new Reply(cache.lastPrice); }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.DoesNotContain(parsed.Diagnostics, d => d.Code == "CE0087");
    }

    // ─── Phase 3 — persistence (capability inheritance) ─────────────────

    [Fact]
    public void PersistedRegion_Parses()
    {
        const string src = """
            shared MarketCache : Persisted
            {
                long lastPrice = 0;
            }
            program Main
            {
                var system = new ActorSystem("test");
                system.RegisterPersistenceProvider<MarketCache>(new InMemorySnapshotStore());
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.True(parsed.Success,
            "Source must parse; diagnostics: " +
            string.Join("\n", parsed.Diagnostics.Select(d => $"{d.Code} {d.Line}:{d.Column} {d.Message}")));
    }

    [Fact]
    public void PersistedRegion_EmitsPersistedBaseClass()
    {
        const string src = """
            shared MarketCache : Persisted
            {
                long lastPrice = 0;
            }
            program Main
            {
                var system = new ActorSystem("test");
                system.RegisterPersistenceProvider<MarketCache>(new InMemorySnapshotStore());
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("internal sealed class MarketCache : Spek.PersistedRegion", code);
    }

    [Fact]
    public void PersistedRegion_EmitsCaptureAndRestoreOverrides()
    {
        const string src = """
            shared MarketCache : Persisted
            {
                long lastPrice = 0;
                int slowestMs   = 0;
            }
            program Main
            {
                var system = new ActorSystem("test");
                system.RegisterPersistenceProvider<MarketCache>(new InMemorySnapshotStore());
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("protected override System.Collections.Generic.IReadOnlyDictionary<string, object?> CaptureFields()", code);
        Assert.Contains("[\"lastPrice\"] = lastPrice,", code);
        Assert.Contains("[\"slowestMs\"] = slowestMs,", code);
        Assert.Contains("protected override void RestoreFields(Spek.Persistence.Snapshot snapshot)", code);
        Assert.Contains("if (snapshot.Fields.ContainsKey(\"lastPrice\"))", code);
        Assert.Contains("lastPrice = snapshot.Get<long>(\"lastPrice\");", code);
    }

    [Fact]
    public void PersistedRegion_InitCanSetSelfName()
    {
        const string src = """
            shared MarketCache : Persisted
            {
                long lastPrice = 0;

                init
                {
                    self.Name = "metrics-v2";
                }
            }
            program Main
            {
                var system = new ActorSystem("test");
                system.RegisterPersistenceProvider<MarketCache>(new InMemorySnapshotStore());
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("this.Name = \"metrics-v2\";", code);
    }

    [Fact]
    public void TransientRegion_EmitsSharedRegionBase_NoPersistedOverrides()
    {
        // No `: Persisted` -> base class stays SharedRegion, no
        // CaptureFields/RestoreFields emitted.
        const string src = """
            shared MarketCache
            {
                long lastPrice = 0;
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("internal sealed class MarketCache : Spek.SharedRegion", code);
        Assert.DoesNotContain("CaptureFields", code);
        Assert.DoesNotContain("RestoreFields", code);
    }

    [Fact]
    public void PersistedRegion_WithoutProvider_TriggersCE0098()
    {
        // : Persisted but no RegisterPersistenceProvider call anywhere
        // in any program block -> CE0098.
        const string src = """
            shared MarketCache : Persisted
            {
                long lastPrice = 0;
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.Contains(parsed.Diagnostics,
            d => d.Code == "CE0098" && d.Message.Contains("MarketCache"));
    }

    [Fact]
    public void PersistedRegion_WithProviderInProgram_NoCE0098()
    {
        // Same shape as the failing case above but with a registration.
        const string src = """
            shared MarketCache : Persisted
            {
                long lastPrice = 0;
            }
            program Main
            {
                var system = new ActorSystem("test");
                var store = new InMemorySnapshotStore();
                system.RegisterPersistenceProvider<MarketCache>(store);
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.DoesNotContain(parsed.Diagnostics, d => d.Code == "CE0098");
    }

    [Fact]
    public void PersistedRegion_FullProgram_RoundTripsThroughRoslyn()
    {
        const string src = """
            message Update(long price);
            shared MarketCache : Persisted
            {
                long lastPrice = 0;
                init { self.Name = "market-v1"; }
            }
            actor PriceWriter
            {
                use MarketCache cache;
                writer on Update u => { cache.lastPrice = u.price; }
            }
            program Main
            {
                var system = new ActorSystem("smoke");
                system.RegisterPersistenceProvider<MarketCache>(new InMemorySnapshotStore());
                var w = system.Spawn<PriceWriter>();
                w.Tell(new Update(42));
            }
            """;
        var code = EmitCSharp(src);
        var (success, errors, _) = RoslynCompileHelper.TryCompile(code, "PersistedRegionSmoke");
        Assert.True(success,
            "Emitted C# did not compile:\n" + errors + "\n--- emitted ---\n" + code);
    }

    [Fact]
    public void SharedRegion_FullActorPair_RoundTripsThroughRoslyn()
    {
        // The bank-of-quotes example from the design conversation:
        // a writer actor that updates the region, a reader actor
        // that snapshots it, plus the region itself. If this
        // compiles, the locking shape and accessor emit are valid.
        const string src = """
            message Update(int symbol, long price);
            message GetLast(int symbol);
            message Reply(long price);

            shared MarketCache
            {
                long lastPrice = 0;
                int lastSymbol = 0;
            }

            actor PriceWriter
            {
                use MarketCache cache;
                writer on Update u =>
                {
                    cache.lastSymbol = u.symbol;
                    cache.lastPrice = u.price;
                }
            }

            actor PriceReader
            {
                use MarketCache cache;
                reader on GetLast g => { return new Reply(cache.lastPrice); }
            }
            """;
        var code = EmitCSharp(src);
        var (success, errors, _) = RoslynCompileHelper.TryCompile(code, "SharedRegionSmoke");
        Assert.True(success,
            "Emitted C# did not compile:\n" + errors + "\n--- emitted ---\n" + code);
    }
}
