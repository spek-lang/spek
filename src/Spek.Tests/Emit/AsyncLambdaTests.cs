using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Spek.Compiler.Emit;
using Xunit;

namespace Spek.Tests.Emit;

/// <summary>
/// Async lambdas, delegate-type-contextual. The invisible-async pass
/// descends into a lambda body only when the lambda's converted delegate type
/// returns Task/ValueTask (so making it <c>async</c> never changes a
/// value-returning delegate's signature), and only awaits calls in
/// bare-statement position (the "after-next" middleware pattern). Calls in
/// return / expression-body position stay forwarded as Tasks.
///
/// Tests use a throwaway assembly (a Task-returning delegate + a pipe that
/// takes it) so they don't depend on AspNetCore being installed at a path —
/// the same scenario, hermetically.
/// </summary>
public sealed class AsyncLambdaTests
{
    private static string CompilePipeDll()
    {
        const string ext = """
            namespace Ext
            {
                public delegate System.Threading.Tasks.Task AsyncStep(int ctx);
                public static class Pipe
                {
                    public static void Use(AsyncStep step) { }
                    public static void UseFunc(System.Func<int, int> f) { }
                    public static System.Threading.Tasks.Task NextAsync(int ctx)
                        => System.Threading.Tasks.Task.CompletedTask;
                }
            }
            """;
        var trusted = ((string)AppContext.GetData("TRUSTED_PLATFORM_ASSEMBLIES")!)
            .Split(Path.PathSeparator)
            .Where(p => !string.IsNullOrEmpty(p))
            .Select(p => (MetadataReference)MetadataReference.CreateFromFile(p));
        var comp = CSharpCompilation.Create(
            "ExtPipe_" + Guid.NewGuid().ToString("N"),
            [CSharpSyntaxTree.ParseText(ext)],
            trusted,
            new CSharpCompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var path = Path.Combine(Path.GetTempPath(), comp.AssemblyName + ".dll");
        Assert.True(comp.Emit(path).Success, "pipe assembly must compile");
        return path;
    }

    private static string Rewrite(string caller, string dll) =>
        AsyncRewriter.Rewrite(caller, extraReferencePaths: [dll]);

    [Fact]
    public void AfterNext_LambdaMadeAsync_StatementPositionCallIsAwaited()
    {
        var dll = CompilePipeDll();
        try
        {
            const string caller = """
                namespace Demo {
                  public class C {
                    public void M() {
                      Ext.Pipe.Use((ctx) => {
                        Ext.Pipe.NextAsync(ctx);
                        System.Console.WriteLine("after");
                      });
                    }
                  }
                }
                """;
            var outp = Rewrite(caller, dll);
            Assert.Contains("async (ctx) =>", outp);
            Assert.Contains("await Ext.Pipe.NextAsync(ctx)", outp);
        }
        finally { File.Delete(dll); }
    }

    [Fact]
    public void BeforeNext_NotMadeAsync_ReturnPositionCallIsForwarded()
    {
        var dll = CompilePipeDll();
        try
        {
            const string caller = """
                namespace Demo {
                  public class C {
                    public void M() {
                      Ext.Pipe.Use((ctx) => {
                        return Ext.Pipe.NextAsync(ctx);
                      });
                    }
                  }
                }
                """;
            var outp = Rewrite(caller, dll);
            Assert.DoesNotContain("async", outp);
            Assert.DoesNotContain("await", outp);
            Assert.Contains("return Ext.Pipe.NextAsync(ctx)", outp);
        }
        finally { File.Delete(dll); }
    }

    [Fact]
    public void ValueReturningDelegateLambda_IsUntouched()
    {
        var dll = CompilePipeDll();
        try
        {
            // Func<int,int> — not Task-returning, so even if it called a Task
            // method the lambda must stay synchronous. (Here it doesn't, but
            // the point is the delegate-type gate.)
            const string caller = """
                namespace Demo {
                  public class C {
                    public void M() {
                      Ext.Pipe.UseFunc((x) => x + 1);
                    }
                  }
                }
                """;
            var outp = Rewrite(caller, dll);
            Assert.DoesNotContain("async", outp);
            Assert.DoesNotContain("await", outp);
        }
        finally { File.Delete(dll); }
    }

    [Fact]
    public void SimpleLambda_AfterNext_AlsoHandled()
    {
        // `ctx => { ... }` (no parens) exercises VisitSimpleLambdaExpression.
        var dll = CompilePipeDll();
        try
        {
            const string caller = """
                namespace Demo {
                  public class C {
                    public void M() {
                      Ext.Pipe.Use(ctx => {
                        Ext.Pipe.NextAsync(ctx);
                        System.Console.WriteLine("x");
                      });
                    }
                  }
                }
                """;
            var outp = Rewrite(caller, dll);
            Assert.Contains("async ctx =>", outp);
            Assert.Contains("await Ext.Pipe.NextAsync(ctx)", outp);
        }
        finally { File.Delete(dll); }
    }
}
