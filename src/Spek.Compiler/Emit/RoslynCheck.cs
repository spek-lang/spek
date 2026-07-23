using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;

namespace Spek.Compiler.Emit;

/// <summary>
/// Post-emit validation: compiles the generated C# in memory against the
/// BCL (plus any extra references) and reports the errors with their
/// <em>mapped</em> locations — when the emitter produced <c>#line</c>
/// directives, errors point at the .spek source line, not the .g.cs.
/// Backs <c>spekc compile --check</c>.
/// </summary>
public static class RoslynCheck
{
    /// <summary>One post-emit C# error, location already #line-mapped.</summary>
    public sealed record CheckError(string Id, string File, int Line, int Column, string Message)
    {
        public override string ToString() => $"{File}({Line},{Column}): error {Id}: {Message}";
    }

    /// <summary>
    /// Compiles <paramref name="csharp"/> against the trusted platform
    /// assemblies plus <paramref name="extraReferencePaths"/> (e.g. the
    /// Spek.Runtime dll). Returns the C# errors, empty when clean. Warnings
    /// are not reported — the Spek-side CE diagnostics own style concerns;
    /// this pass exists to catch code that will not build.
    /// </summary>
    public static IReadOnlyList<CheckError> Check(
        string csharp, IEnumerable<string>? extraReferencePaths = null)
        => Check([csharp], extraReferencePaths);

    /// <summary>
    /// Compiles a whole project's emitted C# together — one compilation over
    /// every <paramref name="sources"/> file — so cross-file references (a type
    /// declared in one <c>.g.cs</c>, used in another) resolve. Same reference
    /// set and #line-mapped errors as the single-file overload.
    /// </summary>
    public static IReadOnlyList<CheckError> Check(
        IEnumerable<string> sources, IEnumerable<string>? extraReferencePaths = null)
    {
        var trees = sources.Select(s => CSharpSyntaxTree.ParseText(s)).ToArray();

        var trusted = ((string)AppContext.GetData("TRUSTED_PLATFORM_ASSEMBLIES")!)
            .Split(Path.PathSeparator);
        var refs = trusted
            .Where(p => !string.IsNullOrEmpty(p))
            .Select(p => MetadataReference.CreateFromFile(p))
            .Cast<MetadataReference>()
            .ToList();
        foreach (var p in extraReferencePaths ?? [])
            if (File.Exists(p))
                refs.Add(MetadataReference.CreateFromFile(p));

        var compilation = CSharpCompilation.Create(
            "SpekCheck",
            syntaxTrees: trees,
            references: refs,
            options: new CSharpCompilationOptions(
                OutputKind.DynamicallyLinkedLibrary,
                nullableContextOptions: NullableContextOptions.Enable));

        using var pe = new MemoryStream();
        var result = compilation.Emit(pe);

        return result.Diagnostics
            .Where(d => d.Severity == DiagnosticSeverity.Error)
            .Select(d =>
            {
                // GetMappedLineSpan honors #line directives: with source
                // mapping on, this is the .spek file and line.
                var span = d.Location.GetMappedLineSpan();
                return new CheckError(
                    d.Id,
                    string.IsNullOrEmpty(span.Path) ? "<generated>" : span.Path,
                    span.StartLinePosition.Line + 1,
                    span.StartLinePosition.Character + 1,
                    d.GetMessage());
            })
            .ToList();
    }
}
