using Spek.Compiler.Parser;
using Xunit;

namespace Spek.Tests.Samples;

/// <summary>
/// Guards against sample source rot: every sample must parse + analyze cleanly
/// against the CURRENT grammar/semantics. GrpcUserApi silently rotted to a
/// CE0001 parse error when the grammar evolved (a braceless `if`), and its
/// stale committed <c>.g.cs</c> hid it from the build — this is the cheap CI
/// gate that catches that whole class of rot.
///
/// Each sample DIRECTORY compiles as one unit (<see cref="SpekCompilation"/>),
/// mirroring the build: a type declared in one file resolves from any other,
/// so multi-file samples like AlertHub are checked the way they actually
/// compile, and cross-file duplicate names still flag (CE0013).
/// </summary>
public sealed class SamplesParseTests
{
    private static string RepoRoot()
    {
        var dir = AppContext.BaseDirectory;
        while (dir is not null && !File.Exists(Path.Combine(dir, "src", "Spek.slnx")))
            dir = Path.GetDirectoryName(dir);
        return dir ?? throw new InvalidOperationException("Could not find src/Spek.slnx above the test assembly.");
    }

    public static IEnumerable<object[]> SampleDirectories()
    {
        var samples = Path.Combine(RepoRoot(), "samples");
        if (!Directory.Exists(samples)) yield break;
        foreach (var dir in Directory.EnumerateDirectories(samples).OrderBy(p => p, StringComparer.Ordinal))
        {
            if (Directory.EnumerateFiles(dir, "*.spek", SearchOption.AllDirectories)
                    .Any(f => !f.Contains($"{Path.DirectorySeparatorChar}obj{Path.DirectorySeparatorChar}")
                           && !f.Contains($"{Path.DirectorySeparatorChar}bin{Path.DirectorySeparatorChar}")))
                yield return new object[] { Path.GetFileName(dir), dir };
        }
    }

    [Theory]
    [MemberData(nameof(SampleDirectories))]
    public void Sample_CompilesAsOneUnit(string name, string dir)
    {
        var sources = Directory.EnumerateFiles(dir, "*.spek", SearchOption.AllDirectories)
            .Where(f => !f.Contains($"{Path.DirectorySeparatorChar}obj{Path.DirectorySeparatorChar}")
                     && !f.Contains($"{Path.DirectorySeparatorChar}bin{Path.DirectorySeparatorChar}"))
            .OrderBy(f => f, StringComparer.Ordinal)
            .Select(f => (Name: Path.GetFileName(f), Source: File.ReadAllText(f)))
            .ToList();

        var compilation = SpekCompilation.Create(sources);
        Assert.True(compilation.Success,
            $"samples/{name} did not parse/analyze cleanly:\n" +
            string.Join("\n", compilation.Diagnostics.Select(d => $"  {d.Code} ({d.Line}:{d.Column}) {d.Message}")));
    }

    [Fact]
    public void Samples_AreDiscovered()
    {
        // Guard the glob: if it matched nothing, Sample_CompilesAsOneUnit
        // would "pass" by running zero cases.
        Assert.NotEmpty(SampleDirectories());
    }
}
