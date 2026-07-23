using System.Collections;
using System.Reflection;
using Spek.Compiler.AST;
using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Xunit;

namespace Spek.Tests.Emit;

/// <summary>
/// A structural completeness guard comparing the parsed AST to the emitted C#:
/// every NAMED declaration in the tree (message / enum / module / class / actor and
/// their fields, methods, properties, behaviors) must show up by name in the output.
///
/// This catches the "parsed but silently dropped" class of bug generically — the one
/// that hid for a long time when actor methods were never emitted (taking the
/// documented OnFailure hook with them). If a member kind stops being emitted, its
/// name goes missing here and this fails, naming exactly what vanished.
/// </summary>
public sealed class AstEmitCompletenessTests
{
    // Curated to exercise the member-bearing declaration kinds. Worker carries a
    // `supervise` (which itself emits OnChildFailure), so the drop detectors here are
    // the plain helper methods (Square / Used / Tally) and Sensitive.OnFailure, which
    // have no other source in the output.
    private const string Src = """
        namespace Demo;

        message Ping(int seq);
        message Pong(string label);

        enum Color { Red, Green, Blue }

        module MathUtil
        {
            public int Square(int x) { return x * x; }
        }

        class Box
        {
            public int Size { get; set; }
            int capacity;
            int Used() { return capacity; }
        }

        actor Worker
        {
            int counter = 0;

            init() { become Running; }

            behavior Running
            {
                on Ping p => { counter = counter + p.seq; }
                on Pong q => { become Paused; }
            }
            behavior Paused { on Ping p => { become Running; } }

            int Tally() { return counter; }

            supervise OneForOne(on Failure: Restart);
        }

        actor Sensitive
        {
            init() { become Idle; }

            behavior Idle { on Ping p => { } }

            // NB: `.Resume` (not `.Stop`/`.Restart`/`.Escalate`) — those are keywords
            // and won't parse in member-access position (see language proposals).
            FailureDirective OnFailure(Exception ex, object msg) { return FailureDirective.Resume; }
        }
        """;

    [Fact]
    public void EveryDeclaredName_AppearsInTheEmittedCSharp()
    {
        var parse = SpekCompiler.Parse(Src);
        Assert.True(parse.Success,
            string.Join("\n", parse.Diagnostics.Select(d => $"  {d.Code} {d.Message}")));

        var csharp = new FileEmitter().Emit(parse.Tree!);

        var declared = CollectDeclaredNames(parse.Tree!).ToList();
        Assert.NotEmpty(declared);   // sanity — the walk actually found declarations

        var missing = declared
            .Where(d => !csharp.Contains(d.Name, StringComparison.Ordinal))
            .ToList();

        Assert.True(missing.Count == 0,
            "Declared in the AST but not emitted (silently dropped):\n" +
            string.Join("\n", missing.Select(d => $"  {d.Kind} '{d.Name}'")));
    }

    /// <summary>
    /// Walks the AST, yielding the Name of every <c>*Decl</c> node that carries a
    /// string Name. (ProgramDecl is excluded — its name doesn't surface verbatim.)
    /// </summary>
    private static IEnumerable<(string Kind, string Name)> CollectDeclaredNames(object node)
    {
        var type = node.GetType();

        if (type.Name.EndsWith("Decl", StringComparison.Ordinal)
            && type.Name != "ProgramDecl"
            && type.GetProperty("Name") is { } nameProp
            && nameProp.PropertyType == typeof(string)
            && nameProp.GetValue(node) is string name
            && name.Length > 0)
        {
            yield return (type.Name, name);
        }

        foreach (var prop in type.GetProperties(BindingFlags.Public | BindingFlags.Instance))
        {
            var value = prop.GetValue(node);
            switch (value)
            {
                case AstNode child:
                    foreach (var r in CollectDeclaredNames(child)) yield return r;
                    break;
                case IEnumerable seq when value is not string:
                    foreach (var item in seq)
                        if (item is AstNode itemNode)
                            foreach (var r in CollectDeclaredNames(itemNode)) yield return r;
                    break;
            }
        }
    }
}
