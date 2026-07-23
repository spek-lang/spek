using Xunit;

namespace Spek.Tests.Emit;

/// <summary>
/// Guards the MSBuild integration that makes invisible-async work in a real build.
/// The in-repo <c>build/Spek.targets</c> and the consumable
/// <c>Spek.Build/build/Spek.Build.targets</c> must forward the project's resolved
/// references to spekc via <c>--ref</c>, so the async rewriter can see Task-returning
/// APIs of referenced assemblies (e.g. <c>TestActorSystem.ExpectStop</c>) and
/// auto-await them. Without this wiring those calls are silently fire-and-forget —
/// the regression these tests exist to prevent.
/// </summary>
public sealed class SpekTargetsReferenceWiringTests
{
    private static string RepoRoot()
    {
        var dir = AppContext.BaseDirectory;
        while (dir is not null && !File.Exists(Path.Combine(dir, "src", "Spek.slnx")))
            dir = Path.GetDirectoryName(dir);
        return dir ?? throw new InvalidOperationException("Could not find src/Spek.slnx above the test assembly.");
    }

    [Theory]
    [InlineData("build/Spek.targets")]
    [InlineData("src/Spek.Build/build/Spek.Build.targets")]
    public void Targets_ForwardResolvedReferencesToSpekc(string relativePath)
    {
        var content = File.ReadAllText(Path.Combine(RepoRoot(), relativePath));

        // The reference set CoreCompile uses, forwarded to spekc so invisible-async
        // can resolve referenced Task-returning APIs.
        Assert.Contains("ReferencePathWithRefAssemblies", content);
        Assert.Contains("--ref", content);
    }

    [Fact]
    public void SpekTargets_ResolvesCompilerByConfiguration_NotHardcodedDebug()
    {
        var content = File.ReadAllText(Path.Combine(RepoRoot(), "build/Spek.targets"));

        // SpekcDll must track $(Configuration) so a Release solution build finds the
        // Release spekc.dll; a hardcoded \Debug\ path silently breaks Release builds.
        Assert.Contains("$(Configuration)", content);
        Assert.DoesNotContain(@"\Debug\net", content);
    }
}
