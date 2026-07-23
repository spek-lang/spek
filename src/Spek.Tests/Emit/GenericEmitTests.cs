using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Xunit;
using Xunit.Abstractions;

namespace Spek.Tests.Emit;

/// <summary>
/// Generics on user types. Spek lowers type parameters and type
/// arguments straight to C# generics, so Roslyn does the type-checking,
/// constraint-solving, and inference. These tests prove the emitted generic
/// C# actually compiles (not just that it contains the right substrings).
///
/// Stage 1 covers the kinds that were already wired earlier — generic
/// <c>message</c>, <c>class</c>, and <c>actor</c> — locking them with Roslyn
/// round-trips. Generic methods/functions and `where` constraints are added in
/// later stages (and tested there).
/// </summary>
public sealed class GenericEmitTests(ITestOutputHelper output)
{
    private static string EmitCSharp(string source)
    {
        var parsed = SpekCompiler.Parse(source);
        Assert.True(parsed.Success,
            "Source must parse + analyze; diagnostics: " +
            string.Join("\n", parsed.Diagnostics.Select(d => $"{d.Code} {d.Line}:{d.Column} {d.Message}")));
        return new FileEmitter().Emit(parsed.Tree!);
    }

    private void AssertCompiles(string source, string name)
    {
        var code = EmitCSharp(source);
        var (ok, summary, _) = RoslynCompileHelper.TryCompile(code, name);
        if (!ok)
        {
            output.WriteLine("─── emitted C# ───");
            output.WriteLine(code);
            output.WriteLine("─── diagnostics ───");
            output.WriteLine(summary);
        }
        Assert.True(ok, $"Emitted generic C# did not compile:\n{summary}");
    }

    [Fact]
    public void GenericMessage_RoundTripsThroughRoslyn()
    {
        const string src = "message Cell<T>(T value);";
        var code = EmitCSharp(src);
        Assert.Contains("public record Cell<T>(T value);", code);
        AssertCompiles(src, "GenericMessage");
    }

    [Fact]
    public void GenericClass_UsesTypeParam_AsField_Init_Method()
    {
        const string src = """
            class Box<T>
            {
                T item;
                init(T initial) { item = initial; }
                public T    Get()      { return item; }
                public void Set(T v)   { item = v; }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("sealed class Box<T>", code);
        AssertCompiles(src, "GenericClass");
    }

    [Fact]
    public void GenericClass_MultipleTypeParams()
    {
        const string src = """
            class Pair<TFirst, TSecond>
            {
                TFirst  first;
                TSecond second;
                init(TFirst f, TSecond s) { first = f; second = s; }
                public TFirst  First()  { return first; }
                public TSecond Second() { return second; }
            }
            """;
        AssertCompiles(src, "GenericPair");
    }

    [Fact]
    public void GenericActor_RoundTripsThroughRoslyn()
    {
        const string src = """
            message Put(int n);
            actor Cache<TKey, TValue>
            {
                int count = 0;
                writer on Put p => { count = p.n; }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("class Cache<TKey, TValue>", code);
        AssertCompiles(src, "GenericActor");
    }

    [Fact]
    public void GenericClass_AsActorField_WithInstantiation()
    {
        // `new Box<int>(0)` and member calls through a generic field.
        const string src = """
            class Box<T>
            {
                T item;
                init(T initial) { item = initial; }
                public T    Get()    { return item; }
                public void Set(T v) { item = v; }
            }

            message Put(int n);
            actor Counter
            {
                Box<int> box = new Box<int>(0);
                writer on Put p => { box.Set(p.n); }
            }
            """;
        AssertCompiles(src, "GenericFieldUse");
    }

    [Fact]
    public void GenericFunction_RoundTripsThroughRoslyn()
    {
        const string src = """
            module Util
            {
                public T Identity<T>(T x)      { return x; }
                public U Second<T, U>(T a, U b) { return b; }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("static T Identity<T>(T x)", code);
        Assert.Contains("static U Second<T, U>(T a, U b)", code);
        AssertCompiles(src, "GenericFunction");
    }

    [Fact]
    public void GenericMethod_OnGenericClass_RoundTripsThroughRoslyn()
    {
        // A generic method (U) on a generic class (T).
        const string src = """
            class Box<T>
            {
                T item;
                init(T initial) { item = initial; }
                public T Get()              { return item; }
                public U OrElse<U>(U other) { return other; }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("public U OrElse<U>(U other)", code);
        AssertCompiles(src, "GenericMethod");
    }

    // ── where-clause constraints ──

    [Fact]
    public void GenericFunction_WithComparableConstraint_RoundTrips()
    {
        // The constraint makes `a.CompareTo(b)` legal — Roslyn enforces it.
        const string src = """
            module Algo
            {
                public T Larger<T>(T a, T b) where T : System.IComparable<T>
                {
                    if (a.CompareTo(b) > 0) { return a; } else { return b; }
                }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("Larger<T>(T a, T b) where T : System.IComparable<T>", code);
        AssertCompiles(src, "ConstrainedFunction");
    }

    [Fact]
    public void GenericClass_WithNewConstraint_RoundTrips()
    {
        const string src = """
            class Factory<T> where T : new()
            {
                public T Create() { return new T(); }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("sealed class Factory<T> where T : new()", code);
        AssertCompiles(src, "ConstrainedClass");
    }

    [Fact]
    public void GenericMethod_MultipleConstraints_RoundTrips()
    {
        const string src = """
            module Make
            {
                public T Fresh<T>() where T : class, new() { return new T(); }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("Fresh<T>() where T : class, new()", code);
        AssertCompiles(src, "MultiConstraint");
    }

    [Fact]
    public void GenericActor_WithConstraint_RoundTrips()
    {
        const string src = """
            message Go();
            actor Holder<T> where T : class
            {
                writer on Go => { }
            }
            """;
        var code = EmitCSharp(src);
        Assert.Contains("class Holder<T> : Spek.ActorBase where T : class", code);
        AssertCompiles(src, "ConstrainedActor");
    }

    [Fact]
    public void NestedGenericTypeArguments_RoundTrip()
    {
        const string src = """
            message Inner<T>(T v);
            message Outer(Inner<int> a);
            """;
        var code = EmitCSharp(src);
        Assert.Contains("Inner<int>", code);
        AssertCompiles(src, "NestedGenericArgs");
    }
}
