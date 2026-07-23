using Spek.Compiler.Parser;
using Spek.Compiler.Semantic;
using Xunit;

namespace Spek.Tests.Semantic;

/// <summary>
/// CE0136 (warning). The read-side companion to CE0135: a lambda that
/// captures actor state read-only and is passed as a call argument to a
/// callee the compiler cannot see into. If that callee stores or
/// parallelizes the callback, its reads race the actor's writes — a torn
/// read instead of a torn write, so a warning instead of an error. Trusted
/// callees (the LINQ operator name set, the actor's own and sibling Spek
/// types, the Spek.Streams factories) stay silent.
/// </summary>
public sealed class LambdaReadCaptureTests
{
    private static bool HasWarning(CompilationResult r, string code) =>
        r.Diagnostics.Any(d => d.Code == code && d.Severity == DiagnosticSeverity.Warning);

    [Fact]
    public void CE0136_ReadCaptureToUnknownCallee_Warns()
    {
        // The canonical shape: a read-only capture handed to a foreign
        // registration API. The build still succeeds — this is a warning.
        const string src = """
            message Rename(string NewName);
            actor Profile
            {
                string name = "";
                ExternalHooks hooks = null;

                on Rename r =>
                {
                    name = r.NewName;
                    hooks.Register(() => System.Console.WriteLine(name));
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.True(parsed.Success, "CE0136 is a warning — the build must still succeed.");
        Assert.True(HasWarning(parsed, "CE0136"));

        // The message teaches both fixes, spelled with the user's own names.
        var d = parsed.Diagnostics.First(x => x.Code == "CE0136");
        Assert.Equal(
            "Lambda captures actor state ('name') by reference and is passed to " +
            "'Register', which Spek cannot see into — if it stores or parallelizes the " +
            "callback, its reads race this actor's writes. Capture a copy instead " +
            "('var n = name;'), or have the callback Tell the actor.",
            d.Message);
    }

    [Fact]
    public void CE0136_ReadCaptureThroughLinq_NotFlagged()
    {
        // The overwhelmingly common case must stay silent: LINQ operators
        // invoke their callback immediately, synchronously, and don't retain
        // it. This is the same read-capture CE0135 already leaves alone.
        const string src = """
            using System.Linq;
            using System.Collections.Generic;

            message Sweep();
            actor Filter
            {
                int filterId = 3;
                List<int> items = new List<int>();

                on Sweep =>
                {
                    var hits = items.Where(x => x == filterId).Select(x => x + filterId).ToList();
                    var n = items.Count(x => x > filterId);
                    var best = items.OrderByDescending(x => x - filterId).FirstOrDefault();
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.DoesNotContain(parsed.Diagnostics, d => d.Code == "CE0136");
    }

    [Fact]
    public void CE0136_CaptureByValueRewrite_Silences()
    {
        // The fix the diagnostic names must itself compile clean: copy the
        // field into a local and let the lambda close over the local. A local
        // is not actor state, so nothing the callee does with it can race.
        const string src = """
            message Rename(string NewName);
            actor Profile
            {
                string name = "";
                ExternalHooks hooks = null;

                on Rename r =>
                {
                    name = r.NewName;
                    var n = name;
                    hooks.Register(() => System.Console.WriteLine(n));
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.True(parsed.Success);
        Assert.DoesNotContain(parsed.Diagnostics, d => d.Code == "CE0136");
    }

    [Fact]
    public void CE0136_TaintedLocalReadCapture_Warns()
    {
        // Same taint tracking as CE0135: a local bound to a read-capturing
        // lambda carries the capture to the untrusted call site.
        const string src = """
            message Up();
            actor Auditor
            {
                int seen = 0;
                ExternalHooks hooks = null;

                on Up u =>
                {
                    var report = () => System.Console.WriteLine(seen);
                    hooks.Register(report);
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.True(HasWarning(parsed, "CE0136"));

        var d = parsed.Diagnostics.First(x => x.Code == "CE0136");
        Assert.Contains("'seen'", d.Message);
        Assert.Contains("var s = seen;", d.Message);
    }

    [Fact]
    public void CE0136_SpekClassMethod_NotFlagged()
    {
        // A confined-class actor field is Spek source, compiled under these
        // same rules — its methods are trusted with a read capture.
        const string src = """
            class Registry
            {
                public void Register(System.Action cb) { }
            }

            message Up();
            actor Auditor
            {
                int seen = 0;
                Registry registry = new Registry();

                on Up u =>
                {
                    registry.Register(() => System.Console.WriteLine(seen));
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.DoesNotContain(parsed.Diagnostics, d => d.Code == "CE0136");
    }

    [Fact]
    public void CE0136_SpawnArgument_Warns()
    {
        // A spawned child runs on another thread by definition — handing it a
        // by-reference read of this actor's state is the race in miniature.
        const string src = """
            message Up();
            actor Worker
            {
                init(System.Func<int> readTotal) { }
                on Up u => { }
            }

            actor Auditor
            {
                int total = 0;

                on Up u =>
                {
                    var child = spawn<Worker>(() => total);
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.True(HasWarning(parsed, "CE0136"));
    }

    [Fact]
    public void CE0136_MutatingLambda_GetsCE0135_NotBoth()
    {
        // Mutation subsumes reading: a state-writing lambda at an escape site
        // is the CE0135 error, never a second CE0136 on top.
        const string src = """
            message Up();
            actor Auditor
            {
                int seen = 0;
                ExternalHooks hooks = null;

                on Up u =>
                {
                    hooks.Register(() => seen = seen + 1);
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.Contains(parsed.Diagnostics, d => d.Code == "CE0135");
        Assert.DoesNotContain(parsed.Diagnostics, d => d.Code == "CE0136");
    }

    [Fact]
    public void CE0136_RegionHandleReadCapture_Warns()
    {
        // A `use` handle read is shared state by another name; carried into
        // unknown code it bypasses the region's reader discipline.
        const string src = """
            shared Tally
            {
                int hits = 0;
            }

            message Up();
            actor Auditor
            {
                use Tally tally;
                ExternalHooks hooks = null;

                on Up u =>
                {
                    hooks.Register(() => System.Console.WriteLine(tally.hits));
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.True(HasWarning(parsed, "CE0136"));
    }

    [Fact]
    public void CE0136_LocalAndParameterCapture_NotFlagged()
    {
        // Locals, parameters, and message fields are not actor state; a
        // lambda over them may travel anywhere.
        const string src = """
            message Up(int Delta);
            actor Auditor
            {
                ExternalHooks hooks = null;

                on Up u =>
                {
                    var floor = 10;
                    hooks.Register(() => System.Console.WriteLine(u.Delta + floor));
                }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.DoesNotContain(parsed.Diagnostics, d => d.Code == "CE0136");
    }
}
