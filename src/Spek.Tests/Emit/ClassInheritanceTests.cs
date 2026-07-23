using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Xunit;
using Xunit.Abstractions;

namespace Spek.Tests.Emit;

/// <summary>
/// Class inheritance — reuse + abstract-only. An <c>abstract class</c> carries
/// abstract methods a subclass must implement, plus inherited fields/methods for
/// reuse; only an abstract class is a legal base (concrete classes stay sealed).
/// There is no <c>virtual</c>/<c>override</c> keyword: the emitter infers
/// <c>override</c> when a subclass method matches a base abstract method, and
/// a constructor chains with <c>init(...) : base(...)</c>. CE0122 guards abstract
/// methods; CE0123 guards the base list. Roslyn does the conformance check
/// (a missing implementation is C#'s CS0534).
/// </summary>
public sealed class ClassInheritanceTests(ITestOutputHelper output)
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

    private const string Shapes = """
        namespace X;

        abstract class Shape
        {
            string name;
            init(string n) { name = n; }
            public abstract double Area();
            public string Describe() { return name; }
        }

        class Circle : Shape
        {
            double r;
            init(double radius) : base("circle") { r = radius; }
            public double Area() { return 3.14159 * r * r; }
        }
        """;

    // ─── emit ───────────────────────────────────────────────────────────

    [Fact]
    public void AbstractClass_EmitsAbstractNotSealed()
    {
        var code = EmitCSharp(Shapes);
        Assert.Contains("abstract class Shape", code);
        Assert.DoesNotContain("sealed class Shape", code);
        // The abstract method emits as a bodyless signature.
        Assert.Contains("public abstract double Area();", code);
    }

    [Fact]
    public void Subclass_InfersOverride_AndChainsBaseCtor_AndCompiles()
    {
        var code = EmitCSharp(Shapes);
        Assert.Contains("sealed class Circle : Shape", code);
        Assert.Contains("public override double Area()", code);   // override inferred
        Assert.Contains("""public Circle(double radius) : base("circle")""", code);
        AssertCompiles(code, "ShapeInheritance");
    }

    [Fact]
    public void AbstractBaseAndInterface_OrdersBaseClassFirst()
    {
        // Source lists the interface first; C# requires the base class first,
        // so the emitter reorders regardless of how they were written.
        const string src = """
            namespace X;

            interface Named { string Name { get; } }

            abstract class Base
            {
                public abstract int Kind();
            }

            class Impl : Named, Base
            {
                public string Name { get => "impl"; }
                public int Kind()  { return 1; }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("class Impl : Base, Named", code);   // base first, interface after
        AssertCompiles(code, "BaseFirstOrdering");
    }

    [Fact]
    public void AbstractSubclassOfAbstract_NeednotImplement_AndCompiles()
    {
        // An abstract subclass may leave the base's abstract method unimplemented.
        const string src = """
            namespace X;

            abstract class A { public abstract int F(); }
            abstract class B : A { public int G() { return 2; } }
            class C : B { public int F() { return 1; } }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("abstract class B : A", code);
        Assert.Contains("public override int F()", code);
        AssertCompiles(code, "AbstractChain");
    }

    // ─── CE0123: base list ──────────────────────────────────────────────

    [Fact]
    public void ExtendConcreteClass_ReportsCE0123()
    {
        const string src = """
            namespace X;
            class A { }
            class B : A { }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.False(parsed.Success);
        var diag = Assert.Single(parsed.Diagnostics, d => d.Code == "CE0123");
        Assert.Contains("not abstract", diag.Message);
    }

    [Fact]
    public void TwoBaseClasses_ReportsCE0123()
    {
        const string src = """
            namespace X;
            abstract class A { }
            abstract class B { }
            class C : A, B { }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.False(parsed.Success);
        var diag = Assert.Single(parsed.Diagnostics, d => d.Code == "CE0123");
        Assert.Contains("more than one base class", diag.Message);
    }

    [Fact]
    public void UnknownBase_ReportsCE0123()
    {
        const string src = """
            namespace X;
            class C : Nope { }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.False(parsed.Success);
        var diag = Assert.Single(parsed.Diagnostics, d => d.Code == "CE0123");
        Assert.Contains("Nope", diag.Message);
    }

    // ─── CE0122: abstract-method well-formedness ────────────────────────

    [Fact]
    public void AbstractMethodInNonAbstractClass_ReportsCE0122()
    {
        const string src = """
            namespace X;
            class A { public abstract int F(); }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.False(parsed.Success);
        var diag = Assert.Single(parsed.Diagnostics, d => d.Code == "CE0122");
        Assert.Contains("not an 'abstract class'", diag.Message);
    }

    [Fact]
    public void PrivateAbstractMethod_ReportsCE0122()
    {
        const string src = """
            namespace X;
            abstract class A { private abstract int F(); }
            """;
        var parsed = SpekCompiler.Parse(src);
        Assert.False(parsed.Success);
        Assert.Contains(parsed.Diagnostics, d => d.Code == "CE0122" && d.Message.Contains("private"));
    }

    // ─── passthrough type-check ─────────────────────────────────────────

    [Fact]
    public void SubclassOmitsAbstractImpl_SurfacesAsCS0534_ViaRoslyn()
    {
        const string src = """
            namespace X;
            abstract class A { public abstract int F(); }
            class B : A { }
            """;
        var code = EmitCSharp(src);
        var (ok, summary, _) = RoslynCompileHelper.TryCompile(code, "MissingAbstractImpl");
        Assert.False(ok, "a subclass that omits an abstract method must not compile");
        Assert.Contains("CS0534", summary);
    }
}
