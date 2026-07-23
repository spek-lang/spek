using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Spek.Compiler.Emit;
using Xunit;

namespace Spek.Tests.Emit;

/// <summary>
/// Injectable async reference set. The invisible-async pass is
/// BCL-seeded; <c>extraReferencePaths</c> (the CLI's <c>--ref</c>) lets it
/// also see Task-returning APIs from assemblies outside the BCL — the
/// mechanism that makes AspNetCore's <c>app.RunAsync()</c> auto-await. These
/// tests prove the contrast with a throwaway assembly the BCL can't possibly
/// contain: the same call is left alone without the ref and awaited with it.
/// </summary>
public sealed class AsyncRefInjectionTests
{
    // Compile a tiny assembly exposing a Task-returning static method to a
    // temp .dll, returning its path.
    private static string CompileTaskHelperDll()
    {
        const string ext = """
            namespace Ext
            {
                public static class Helper
                {
                    public static System.Threading.Tasks.Task DoAsync()
                        => System.Threading.Tasks.Task.CompletedTask;
                }
            }
            """;
        var trusted = ((string)AppContext.GetData("TRUSTED_PLATFORM_ASSEMBLIES")!)
            .Split(Path.PathSeparator)
            .Where(p => !string.IsNullOrEmpty(p))
            .Select(p => (MetadataReference)MetadataReference.CreateFromFile(p));
        var comp = CSharpCompilation.Create(
            "ExtHelper_" + Guid.NewGuid().ToString("N"),
            [CSharpSyntaxTree.ParseText(ext)],
            trusted,
            new CSharpCompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var path = Path.Combine(Path.GetTempPath(), comp.AssemblyName + ".dll");
        var result = comp.Emit(path);
        Assert.True(result.Success, "helper assembly must compile");
        return path;
    }

    private const string CallerCode = """
        namespace Demo
        {
            public class C
            {
                public void Run()
                {
                    Ext.Helper.DoAsync();
                }
            }
        }
        """;

    [Fact]
    public void WithoutRef_UnresolvedTaskCall_IsNotAwaited()
    {
        // Ext.Helper isn't in the BCL, so its return type is unresolved and
        // the pass can't classify it as awaitable — call left untouched.
        var rewritten = AsyncRewriter.Rewrite(CallerCode);
        Assert.DoesNotContain("await", rewritten);
    }

    [Fact]
    public void WithRef_TaskCall_IsAwaited_AndMethodMadeAsync()
    {
        var dll = CompileTaskHelperDll();
        try
        {
            var rewritten = AsyncRewriter.Rewrite(CallerCode, extraReferencePaths: [dll]);
            Assert.Contains("await Ext.Helper.DoAsync()", rewritten);
            Assert.Contains("async", rewritten);   // Run() promoted to async Task
        }
        finally
        {
            File.Delete(dll);
        }
    }

    [Fact]
    public void NonexistentRefPath_IsIgnored()
    {
        // The File.Exists guard means a bad --ref path is harmless, not a crash.
        var rewritten = AsyncRewriter.Rewrite(
            CallerCode, extraReferencePaths: ["/no/such/assembly.dll"]);
        Assert.DoesNotContain("await", rewritten);
    }

    [Fact]
    public void DuplicateFrameworkRef_IsSkipped_SoBclStaysResolvable()
    {
        // Regression: forwarding a project's references can include an assembly that
        // duplicates the framework seed — a ref-pack System.*.dll, or a test host's
        // netcoreapp3.1 dll. Two copies of a core assembly made `Task` ambiguous and
        // silently dropped the auto-await (caught when a Spek handler's `Task.Delay`
        // stopped being awaited under a test project's full reference set). The
        // rewriter now skips extra refs whose filename the seed already provides.
        const string code = """
            namespace Demo
            {
                public class C
                {
                    public void Run()
                    {
                        System.Threading.Tasks.Task.Delay(1);
                    }
                }
            }
            """;
        var duplicateFrameworkDll = typeof(object).Assembly.Location;  // already in the seed

        var rewritten = AsyncRewriter.Rewrite(code, extraReferencePaths: [duplicateFrameworkDll]);

        Assert.Contains("await System.Threading.Tasks.Task.Delay", rewritten);
    }
}
