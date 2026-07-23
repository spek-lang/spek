using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Xunit;

namespace Spek.Tests.Emit;

/// <summary>
/// Guards the native-test emit by convention: with <c>--tests</c> (emitTests), a
/// <c>*Tests</c> class's public methods lower to <c>[Spek.Testing.SpekTest]</c>
/// methods; non-public methods stay helpers; fields + <c>init</c> (→ a per-test
/// constructor) carry state. Without <c>--tests</c> the same type is an ordinary
/// class — no test emission (so a <c>*Tests</c> type never breaks a non-test build).
/// The end-to-end run lives in samples/NativeTesting (not in the solution); this
/// keeps the emitter honest in CI.
/// </summary>
public class TestConventionEmitTests
{
    private const string Src = """
        message Query();
        message Reply(int n);
        actor Echo { init() { become Active; } behavior Active { on Query => return new Reply(1); } }

        class EchoTests
        {
            TestActorSystem sys;
            init() { sys = new TestActorSystem(); }

            public void ReplyArrives()
            {
                var echo = sys.Spawn<Echo>();
                Reply r = echo.Ask(new Query());
                Xunit.Assert.Equal(1, r.n);
            }

            int seed() { return 1; }

            public int Probes { get; set; }

            T Identity<T>(T x) { return x; }
        }
        """;

    [Fact]
    public void TestContainer_emits_SpekTest_only_under_emitTests()
    {
        var parsed = SpekCompiler.Parse(Src);
        Assert.True(parsed.Success,
            string.Join("; ", parsed.Diagnostics.Select(d => $"{d.Code} {d.Message}")));

        // Gating: without --tests, `EchoTests` is an ordinary class — no tests emitted,
        // and the test-only usings (Spek.Testing / System.Linq) are absent.
        var normal = new FileEmitter().Emit(parsed.Tree!, emitTests: false);
        Assert.DoesNotContain("Spek.Testing.SpekTest", normal);
        Assert.DoesNotContain("using System.Linq;", normal);

        // With --tests: public methods → [SpekTest]; init → ctor; non-public stays a helper.
        var tests = new FileEmitter().Emit(parsed.Tree!, emitTests: true);
        Assert.Contains("using Spek.Testing;", tests);
        Assert.Contains("using System.Linq;", tests);   // LINQ assertions (Count/Any/Select) in tests
        Assert.Contains("public class EchoTests : System.IDisposable", tests);  // TestActorSystem field
        Assert.Contains("[Spek.Testing.SpekTest]", tests);
        Assert.Contains("public async System.Threading.Tasks.Task ReplyArrives()", tests);
        Assert.Contains("public EchoTests()", tests);    // init → per-test constructor
        Assert.Contains("private int seed()", tests);    // non-public method = helper, not a test
        Assert.Contains("public int Probes { get", tests);   // property — was silently dropped
        Assert.Contains("Identity<T>(T x)", tests);          // generic helper — type params were dropped
        Assert.Contains("public void Dispose()", tests); // per-test cleanup
        Assert.Contains("sys?.Dispose();", tests);       // the TestActorSystem field is disposed
    }
}
