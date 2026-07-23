using System.Reflection;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Spek.Compiler.Emit;
using Spek.Compiler.Parser;

namespace Spek.Benchmarks.Language;

/// <summary>
/// Compiles Spek source text to a loaded in-memory assembly: parse via
/// <see cref="SpekCompiler.Parse"/>, emit C# via <see cref="FileEmitter"/>,
/// Roslyn-compile against the runtime's metadata, load. A trimmed-down
/// sibling of Spek.Tests' RoslynCompileHelper, with one deliberate
/// difference: emitted code compiles at <see cref="OptimizationLevel.Release"/>,
/// because these assemblies are measured, not just loaded.
///
/// Benchmark GlobalSetup methods call this once, then cache ActorRefs and
/// pre-created message instances so the measured operation is a plain
/// Tell/AskAsync with no reflection on the hot path.
/// </summary>
internal static class SpekSourceCompiler
{
    // The reference set is identical for every compile and expensive to
    // load, so build it ONCE and share it across all suites. Roslyn happily
    // shares a MetadataReference across compilations.
    private static readonly Lazy<MetadataReference[]> SharedRefs = new(() =>
    {
        var paths = new HashSet<string>(
            ((string)AppContext.GetData("TRUSTED_PLATFORM_ASSEMBLIES")!)
                .Split(Path.PathSeparator)
                .Where(p => !string.IsNullOrEmpty(p)),
            StringComparer.OrdinalIgnoreCase)
        {
            // Emitted code references ActorBase / SharedRegion (Spek.Runtime)
            // and StreamOperator<T> (Spek.Streams). Both normally ride in on
            // the trusted-assembly list already; adding them into the same
            // de-duplicating set keeps the helper independent of host
            // probing details without risking a duplicate reference.
            typeof(Spek.ActorBase).Assembly.Location,
            typeof(Spek.Streams.StreamOperator<>).Assembly.Location,
        };

        return paths
            .Select(p => (MetadataReference)MetadataReference.CreateFromFile(p))
            .ToArray();
    });

    /// <summary>
    /// Runs the full language pipeline over <paramref name="spekSource"/>
    /// and returns the loaded assembly. Throws with readable diagnostics if
    /// either the Spek parse or the Roslyn compile of the emitted C# fails —
    /// a failure here means the benchmark is measuring nothing, so it must
    /// be loud.
    /// </summary>
    public static Assembly Compile(string spekSource, string assemblyName)
    {
        var parse = SpekCompiler.Parse(spekSource);
        if (!parse.Success)
            throw new InvalidOperationException(
                $"Spek parse failed for {assemblyName}:\n" + string.Join("\n",
                    parse.Diagnostics.Select(d => $"  {d.Code} {d.Line}:{d.Column} {d.Message}")));

        var csharp = new FileEmitter().Emit(parse.Tree!);
        var tree   = CSharpSyntaxTree.ParseText(csharp);

        var compilation = CSharpCompilation.Create(
            assemblyName,
            syntaxTrees: new[] { tree },
            references: SharedRefs.Value,
            options: new CSharpCompilationOptions(
                OutputKind.DynamicallyLinkedLibrary,
                optimizationLevel: OptimizationLevel.Release,
                nullableContextOptions: NullableContextOptions.Enable));

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        if (!result.Success)
        {
            var errors = string.Join("\n", result.Diagnostics
                .Where(d => d.Severity == DiagnosticSeverity.Error)
                .Select(e => $"  {e.Id} {e.Location.GetLineSpan().StartLinePosition}: {e.GetMessage()}"));
            throw new InvalidOperationException(
                $"Roslyn compile of emitted C# failed for {assemblyName}:\n{errors}");
        }

        peStream.Position = 0;
        return Assembly.Load(peStream.ToArray());
    }

    /// <summary>Resolves a type by simple name in the compiled assembly.
    /// Setup-only — never call this from a measured method.</summary>
    public static Type TypeNamed(Assembly assembly, string name)
        => assembly.GetType(name) ?? assembly.GetTypes().Single(t => t.Name == name);

    /// <summary>Creates a message instance by simple type name. Setup-only —
    /// measured methods reuse the returned instance (messages are immutable
    /// records, so a single instance is safe to Tell any number of times).</summary>
    public static object NewMessage(Assembly assembly, string name, params object?[] args)
        => Activator.CreateInstance(TypeNamed(assembly, name), args)!;
}
