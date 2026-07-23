using System.Reflection;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Spek.Runtime;

namespace Spek.Tests.Emit;

/// <summary>
/// Compiles emitted Spek-to-C# source with Roslyn against the runtime's
/// metadata. Used by end-to-end tests to prove that what the emitter
/// produces is actually valid C#, not just syntactically-plausible text
/// that happens to contain the expected substrings.
/// </summary>
internal static class RoslynCompileHelper
{
    // The reference set is identical for every compile and expensive to load, so build
    // it ONCE and share it across all calls. Rebuilding it per call — ~200× across the
    // doc-snippet suite — re-reads every assembly's metadata each time, which pushes the
    // test host into the memory pressure that aborts it when DocSnippetTests runs
    // alongside the rest of the suite. Roslyn happily shares a MetadataReference across
    // compilations.
    private static readonly Lazy<MetadataReference[]> SharedRefs = new(() =>
    {
        var trusted = ((string)AppContext.GetData("TRUSTED_PLATFORM_ASSEMBLIES")!)
            .Split(Path.PathSeparator);
        var refs = trusted
            .Where(p => !string.IsNullOrEmpty(p))
            .Select(p => MetadataReference.CreateFromFile(p))
            .Cast<MetadataReference>()
            .ToList();

        // Spek.Runtime (ActorBase) is the engine; Spek.Persistence (Snapshot /
        // ISnapshotStore) is referenced by emitted `shared X : Persisted` region programs.
        refs.Add(MetadataReference.CreateFromFile(typeof(ActorBase).Assembly.Location));
        refs.Add(MetadataReference.CreateFromFile(typeof(Spek.Persistence.Snapshot).Assembly.Location));
        // testing.md snippets use TestActorSystem / TestProbe from Spek.Testing, and a
        // `*Tests` container emits [Spek.Testing.SpekTest] (a Xunit.FactAttribute subclass
        // in Spek.Testing.Xunit) — so reference both that and xUnit's FactAttribute base.
        refs.Add(MetadataReference.CreateFromFile(typeof(Spek.Testing.TestActorSystem).Assembly.Location));
        refs.Add(MetadataReference.CreateFromFile(typeof(Spek.Testing.SpekTestAttribute).Assembly.Location));
        refs.Add(MetadataReference.CreateFromFile(typeof(Xunit.FactAttribute).Assembly.Location));
        // Stream-shaped handlers (`on X => debounce(..) => { }`) emit against
        // the operator library.
        refs.Add(MetadataReference.CreateFromFile(typeof(Spek.Streams.StreamOperator<>).Assembly.Location));

        return refs.ToArray();
    });

    /// <summary>
    /// Compiles <paramref name="source"/> to an in-memory assembly. Returns the
    /// success flag plus a readable diagnostic summary.
    /// </summary>
    public static (bool Success, string Output, IReadOnlyList<Diagnostic> Errors) TryCompile(
        string source, string assemblyName = "SpekEmitTest")
    {
        var tree = CSharpSyntaxTree.ParseText(source);

        var compilation = CSharpCompilation.Create(
            assemblyName,
            syntaxTrees: new[] { tree },
            references: SharedRefs.Value,
            options: new CSharpCompilationOptions(
                OutputKind.DynamicallyLinkedLibrary,
                // Matches Spek.Runtime.csproj — nullable + implicit usings on.
                nullableContextOptions: NullableContextOptions.Enable));

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);

        var errors = result.Diagnostics
            .Where(d => d.Severity == DiagnosticSeverity.Error)
            .ToList();

        var summary = errors.Count == 0
            ? "(no errors)"
            : string.Join("\n", errors.Select(e =>
                $"  {e.Id} {e.Location.GetLineSpan().StartLinePosition}: {e.GetMessage()}"));

        return (result.Success, summary, errors);
    }

    /// <summary>
    /// Compiles <paramref name="source"/> and loads the resulting assembly
    /// into the default load context. Throws if compilation fails.
    /// </summary>
    public static Assembly CompileAndLoad(string source, string assemblyName = "SpekEmitTest")
        => CompileAndLoad(new[] { source }, assemblyName);

    /// <summary>
    /// Same as the single-source overload, but accepts multiple source
    /// files so a test can compile emitted Spek-to-C# alongside a hand-
    /// written C# "shim" file (e.g. to provide helper classes the Spek
    /// source calls into). Both files become part of the same assembly.
    /// </summary>
    public static Assembly CompileAndLoad(IEnumerable<string> sources, string assemblyName = "SpekEmitTest")
    {
        var trees = sources.Select(s => CSharpSyntaxTree.ParseText(s)).ToArray();

        var compilation = CSharpCompilation.Create(
            assemblyName,
            syntaxTrees: trees,
            references: SharedRefs.Value,
            options: new CSharpCompilationOptions(
                OutputKind.DynamicallyLinkedLibrary,
                nullableContextOptions: NullableContextOptions.Enable));

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);

        if (!result.Success)
        {
            var errors = string.Join("\n", result.Diagnostics
                .Where(d => d.Severity == DiagnosticSeverity.Error)
                .Select(e => $"  {e.Id} {e.Location.GetLineSpan().StartLinePosition}: {e.GetMessage()}"));
            throw new InvalidOperationException($"Compilation failed:\n{errors}");
        }

        peStream.Position = 0;
        return Assembly.Load(peStream.ToArray());
    }
}
