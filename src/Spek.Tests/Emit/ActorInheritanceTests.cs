using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Xunit;
using Xunit.Abstractions;

namespace Spek.Tests.Emit;

/// <summary>
/// Actor inheritance — the same reuse + abstract-only model as classes. An
/// <c>abstract actor</c> is a base that shares fields and methods and can
/// declare <c>abstract</c> methods a derived actor must implement; a concrete
/// actor is sealed. No <c>virtual</c>/<c>override</c> keyword — the emitter
/// infers <c>override</c> for a method implementing a base actor's abstract
/// method. CE0122 guards abstract-method placement; CE0123 guards the base
/// (only an abstract actor is extendable). Roslyn does the conformance check
/// (a missing implementation is C# CS0534).
/// </summary>
public sealed class ActorInheritanceTests(ITestOutputHelper output)
{
    private static string EmitCSharp(string source)
    {
        var parsed = SpekCompiler.Parse(source);
        Assert.True(parsed.Success,
            string.Join("\n", parsed.Diagnostics.Select(d => $"{d.Code} {d.Message}")));
        return new FileEmitter().Emit(parsed.Tree!);
    }

    private void AssertCompiles(string code, string label)
    {
        var (ok, summary, _) = RoslynCompileHelper.TryCompile(code, label);
        if (!ok) { output.WriteLine(code); output.WriteLine(summary); }
        Assert.True(ok, $"Emitted C# did not compile:\n{summary}");
    }

    private const string Workers = """
        namespace X;

        message Job(int value);
        message Done(int result, int handled);

        abstract actor Worker
        {
            protected int handled = 0;
            public abstract int Transform(int x);
            protected void Bump() { handled = handled + 1; }
        }

        actor Doubler : Worker
        {
            init() { become Active; }
            behavior Active
            {
                on Job j => { Bump(); return new Done(Transform(j.value), handled); }
            }
            public int Transform(int x) { return x * 2; }
        }
        """;

    // ─── emit ───────────────────────────────────────────────────────────

    [Fact]
    public void AbstractActor_EmitsAbstractClass_WithAbstractMethod()
    {
        var code = EmitCSharp(Workers);
        Assert.Contains("public abstract class Worker : Spek.ActorBase", code);
        Assert.Contains("public abstract int Transform(int x);", code);
        // A protected base field is shared with derived actors.
        Assert.Contains("protected int _handled", code);
    }

    [Fact]
    public void DerivedActor_ExtendsBase_InfersOverride_AndCompiles()
    {
        var code = EmitCSharp(Workers);
        Assert.Contains("class Doubler : Worker", code);
        Assert.Contains("public override int Transform(int x)", code);   // override inferred
        AssertCompiles(code, "ActorInheritance");
    }

    // ─── CE0123 / CE0122 ────────────────────────────────────────────────

    [Fact]
    public void ExtendConcreteActor_ReportsCE0123()
    {
        const string src = """
            namespace X;
            message Ping();
            actor Base  { behavior Idle { on Ping => { } } }
            actor Child : Base { behavior Idle { on Ping => { } } }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.False(parsed.Success);
        var diag = Assert.Single(parsed.Diagnostics, d => d.Code == "CE0123");
        Assert.Contains("not abstract", diag.Message);
    }

    [Fact]
    public void AbstractMethodInNonAbstractActor_ReportsCE0122()
    {
        const string src = """
            namespace X;
            actor A { public abstract int F(); behavior Idle { } }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.False(parsed.Success);
        var diag = Assert.Single(parsed.Diagnostics, d => d.Code == "CE0122");
        Assert.Contains("not an 'abstract actor'", diag.Message);
    }

    // ─── abstract / override behaviors ──────────────────────────────────
    // The behavior-level side of actor inheritance (docs: actors.md "Abstract
    // actors"): an `abstract behavior` on the base declares a contract; the
    // derived actor fills it in with `override behavior`.

    private const string Behaviors = """
        namespace X;

        message Run();
        message Count(int n);

        abstract actor WorkerBase
        {
            abstract behavior Running { }
        }

        actor PrintWorker : WorkerBase
        {
            int jobsDone = 0;

            init() { become Running; }

            override behavior Running
            {
                on Run => { jobsDone = jobsDone + 1; return new Count(jobsDone); }
            }
        }
        """;

    [Fact]
    public void OverrideBehavior_EmitsHandlerOnDerived_AndCompiles()
    {
        var code = EmitCSharp(Behaviors);
        // The abstract base emits an abstract class; the derived actor owns the
        // Running dispatch arm and wires it in its constructor.
        Assert.Contains("public abstract class WorkerBase : Spek.ActorBase", code);
        Assert.Contains("sealed class PrintWorker : WorkerBase", code);
        Assert.Contains("Running_HandleAsync", code);
        AssertCompiles(code, "OverrideBehavior");
    }

    [Fact]
    public void MissingOverrideBehavior_BecomeTarget_ReportsCE0011()
    {
        // The docs claim the subclass "must fill in" an abstract behavior. The
        // enforcement is indirect but real: a subclass that omits the override
        // has no behavior of that name, so its `become` fails with CE0011.
        const string src = """
            namespace X;
            message Run();
            abstract actor WorkerBase
            {
                abstract behavior Running { }
            }
            actor Lazy : WorkerBase
            {
                init() { become Running; }
            }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.False(parsed.Success);
        var diag = Assert.Single(parsed.Diagnostics, d => d.Code == "CE0011");
        Assert.Contains("Running", diag.Message);
    }

    // ─── passthrough type-check ─────────────────────────────────────────

    [Fact]
    public void DerivedOmitsAbstractImpl_SurfacesAsCS0534_ViaRoslyn()
    {
        const string src = """
            namespace X;
            message Ping();
            abstract actor A { public abstract int F(); }
            actor B : A { behavior Idle { on Ping => { } } }
            """;
        var code = EmitCSharp(src);
        var (ok, summary, _) = RoslynCompileHelper.TryCompile(code, "MissingActorAbstract");
        Assert.False(ok, "a derived actor omitting an abstract method must not compile");
        Assert.Contains("CS0534", summary);
    }
}
