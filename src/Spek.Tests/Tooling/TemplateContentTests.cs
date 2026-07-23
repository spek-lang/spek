using Xunit;

namespace Spek.Tests.Tooling;

/// <summary>
/// Guards the dotnet-new template content against version-scheme drift. The
/// public scheme is <c>0.&lt;era&gt;.&lt;build&gt;</c> with no prerelease
/// suffix (Directory.Build.props / release.yml), so a template pinning a
/// suffixed or absent version would fail restore for every user the moment
/// templates ship — a break no compile step catches.
/// </summary>
public sealed class TemplateContentTests
{
    private static string RepoRoot()
    {
        var dir = AppContext.BaseDirectory;
        while (dir is not null && !File.Exists(Path.Combine(dir, "src", "Spek.slnx")))
            dir = Path.GetDirectoryName(dir);
        return dir ?? throw new InvalidOperationException("Could not find src/Spek.slnx above the test assembly.");
    }

    [Fact]
    public void TemplateContent_PackageReferences_MatchTheVersionScheme()
    {
        var contentRoot = Path.Combine(RepoRoot(), "src", "Spek.Templates", "content");
        var csprojs = Directory.GetFiles(contentRoot, "*.csproj", SearchOption.AllDirectories);
        Assert.NotEmpty(csprojs);

        foreach (var proj in csprojs)
        {
            var text = File.ReadAllText(proj);
            foreach (System.Text.RegularExpressions.Match m in
                System.Text.RegularExpressions.Regex.Matches(
                    text,
                    "<PackageReference\\s+Include=\"(Spek[^\"]*)\"\\s+Version=\"([^\"]+)\""))
            {
                var version = m.Groups[2].Value;
                Assert.Matches(@"^0\.\d+\.\*$", version);
            }
        }
    }
}
