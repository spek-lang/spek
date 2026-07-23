using System.Reflection;

namespace Spek.Tests.Fixtures;

public static class FixtureLoader
{
    private static readonly Assembly Asm = typeof(FixtureLoader).Assembly;

    /// <summary>Loads a .spek fixture by filename (e.g. "01_messages_only.spek").</summary>
    public static string Load(string fileName)
    {
        var resourceName = Asm.GetManifestResourceNames()
            .FirstOrDefault(n => n.EndsWith(fileName, StringComparison.OrdinalIgnoreCase))
            ?? throw new FileNotFoundException($"Fixture '{fileName}' not found in embedded resources.");

        using var stream = Asm.GetManifestResourceStream(resourceName)!;
        using var reader = new StreamReader(stream);
        return reader.ReadToEnd();
    }
}
