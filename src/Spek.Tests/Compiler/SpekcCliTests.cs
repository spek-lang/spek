using Spek.Cli;
using Xunit;

namespace Spek.Tests.Compiler;

/// <summary>
/// In-process integration coverage for the `spekc` <c>compile</c> and
/// <c>format</c> subcommands, driven through <see cref="CliRunner.Run"/>
/// with captured <see cref="StringWriter"/>s (no `dotnet run` subprocess).
/// Exercises end-to-end argv parsing, exit codes, file I/O, and the
/// out/err stream split.
/// </summary>
public sealed class SpekcCliTests
{
    private static (int ExitCode, string StdOut, string StdErr) RunSpekc(params string[] args)
    {
        var stdout = new StringWriter();
        var stderr = new StringWriter();
        var exit   = CliRunner.Run(args, stdout, stderr);
        return (exit, stdout.ToString(), stderr.ToString());
    }

    private static string NewTempDir()
    {
        var temp = Path.Combine(Path.GetTempPath(), "spekc-cli-" + Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(temp);
        return temp;
    }

    // ─── compile ────────────────────────────────────────────────────────

    [Fact]
    public void Compile_ValidSpek_ToOutDir_EmitsCsFile()
    {
        var temp = NewTempDir();
        try
        {
            var spekFile = Path.Combine(temp, "Echo.spek");
            File.WriteAllText(spekFile, """
                namespace Samples;

                message Ping();
                message Pong();

                actor Echo
                {
                    init() { become Waiting; }

                    behavior Waiting
                    {
                        on Ping => sender.Tell(new Pong());
                    }
                }
                """);

            var outDir = Path.Combine(temp, "gen");
            var (exit, stdout, stderr) = RunSpekc("compile", spekFile, "--out", outDir);

            Assert.Equal(0, exit);
            var csFile = Path.Combine(outDir, "Echo.g.cs");
            Assert.True(File.Exists(csFile), $"expected emitted C# at {csFile}; stderr was: {stderr}");
            Assert.Contains($"-> {csFile}", stdout);
            Assert.Contains("class Echo", File.ReadAllText(csFile));
        }
        finally { Directory.Delete(temp, recursive: true); }
    }

    [Fact]
    public void Compile_LineMapMode_ControlsHashLinePathStyle()
    {
        var temp = NewTempDir();
        try
        {
            var spekFile = Path.Combine(temp, "Echo.spek");
            File.WriteAllText(spekFile, """
                message Ping();
                message Pong();

                actor Echo
                {
                    on Ping => sender.Tell(new Pong());
                }
                """);
            var absolute = Path.GetFullPath(spekFile).Replace('\\', '/');

            // Default: relative #line paths (machine-neutral for checked-in output).
            var relDir = Path.Combine(temp, "rel");
            Assert.Equal(0, RunSpekc("compile", spekFile, "--out", relDir).ExitCode);
            var relCs = File.ReadAllText(Path.Combine(relDir, "Echo.g.cs"));
            Assert.Contains("#line", relCs);
            Assert.DoesNotContain(absolute, relCs);

            // --abs-line-map: absolute #line paths so a debugger's PDB resolves them.
            var absDir = Path.Combine(temp, "abs");
            Assert.Equal(0, RunSpekc("compile", spekFile, "--out", absDir, "--abs-line-map").ExitCode);
            var absCs = File.ReadAllText(Path.Combine(absDir, "Echo.g.cs"));
            Assert.Contains(absolute, absCs);
        }
        finally { Directory.Delete(temp, recursive: true); }
    }

    [Fact]
    public void Compile_InvalidSpek_NonZeroExit_WithDiagnosticOnErr()
    {
        var temp = NewTempDir();
        try
        {
            // Unterminated actor body — a hard parse error.
            var spekFile = Path.Combine(temp, "Broken.spek");
            File.WriteAllText(spekFile, """
                namespace Samples;

                actor Broken
                {
                    behavior Idle
                    {
                        on Ping =>
                """);

            var outDir = Path.Combine(temp, "gen");
            var (exit, _, stderr) = RunSpekc("compile", spekFile, "--out", outDir);

            Assert.NotEqual(0, exit);
            Assert.False(string.IsNullOrWhiteSpace(stderr), "expected a diagnostic on the err writer");
            Assert.Contains("Build FAILED", stderr);
            Assert.False(File.Exists(Path.Combine(outDir, "Broken.g.cs")),
                "no C# should be emitted for a failed compile");
        }
        finally { Directory.Delete(temp, recursive: true); }
    }

    // ─── format ─────────────────────────────────────────────────────────

    [Fact]
    public void Format_Write_RewritesFileInPlace()
    {
        var temp = NewTempDir();
        try
        {
            // Mangled whitespace the formatter is guaranteed to normalise.
            var spekFile = Path.Combine(temp, "Mangled.spek");
            const string mangled = """
                actor   A
                {
                behavior   Idle
                {
                on Ping  =>  {  }
                }
                }
                """;
            File.WriteAllText(spekFile, mangled);

            var (exit, stdout, stderr) = RunSpekc("format", spekFile, "--write");

            Assert.Equal(0, exit);
            var rewritten = File.ReadAllText(spekFile);
            Assert.NotEqual(mangled, rewritten);
            Assert.Contains("\n    behavior", rewritten);
            Assert.Contains("\n        on Ping", rewritten);
            Assert.Contains($"formatted {spekFile}", stdout);
            Assert.True(string.IsNullOrEmpty(stderr), $"unexpected stderr: {stderr}");
        }
        finally { Directory.Delete(temp, recursive: true); }
    }

    [Fact]
    public void Format_NoWrite_WritesFormattedSourceToOut()
    {
        var temp = NewTempDir();
        try
        {
            var spekFile = Path.Combine(temp, "Mangled.spek");
            const string mangled = """
                actor   A
                {
                behavior   Idle
                {
                on Ping  =>  {  }
                }
                }
                """;
            File.WriteAllText(spekFile, mangled);

            var (exit, stdout, stderr) = RunSpekc("format", spekFile);

            Assert.Equal(0, exit);
            // The formatted source goes to stdout; the file is left untouched.
            Assert.Contains("\n    behavior", stdout);
            Assert.Contains("\n        on Ping", stdout);
            Assert.Equal(mangled, File.ReadAllText(spekFile));
            Assert.True(string.IsNullOrEmpty(stderr), $"unexpected stderr: {stderr}");
        }
        finally { Directory.Delete(temp, recursive: true); }
    }

    // ─── usage / dispatch ─────────────────────────────────────────────────

    [Fact]
    public void NoArgs_ExitsOne_WithUsageOnErr()
    {
        var (exit, stdout, stderr) = RunSpekc();

        Assert.Equal(1, exit);
        Assert.Contains("Usage: spekc compile", stderr);
        Assert.True(string.IsNullOrEmpty(stdout), $"usage must not go to stdout: {stdout}");
    }

    [Fact]
    public void UnknownCommand_ExitsOne_WithUsageOnErr()
    {
        var (exit, stdout, stderr) = RunSpekc("frobnicate");

        Assert.Equal(1, exit);
        Assert.Contains("Usage: spekc compile", stderr);
        Assert.True(string.IsNullOrEmpty(stdout), $"usage must not go to stdout: {stdout}");
    }

    // ─── cross-file compilation ─────────────────────────────────────────

    [Fact]
    public void Compile_CrossFileTypeReference_Resolves()
    {
        // The enum and the message that carries it live in separate files.
        // Compiling them as one unit must resolve the reference (no false CE0010),
        // and --check proves the emitted C# cross-compiles. This is the coverage
        // that was missing when per-file compilation silently broke this.
        var temp = NewTempDir();
        try
        {
            File.WriteAllText(Path.Combine(temp, "State.spek"),
                "namespace X;\nenum HostState { Up, Down }\n");
            File.WriteAllText(Path.Combine(temp, "Messages.spek"),
                "namespace X;\nmessage StateChanged(HostState State);\n");

            var gen = Path.Combine(temp, "gen");
            var (exit, _, stderr) = RunSpekc("compile", temp, "--out", gen, "--check");

            Assert.True(exit == 0, $"cross-file compile should succeed; stderr:\n{stderr}");
            Assert.True(File.Exists(Path.Combine(gen, "State.g.cs")));
            Assert.True(File.Exists(Path.Combine(gen, "Messages.g.cs")));
            Assert.Contains("record StateChanged(HostState State)",
                File.ReadAllText(Path.Combine(gen, "Messages.g.cs")));
        }
        finally { Directory.Delete(temp, recursive: true); }
    }

    [Fact]
    public void Compile_SingleFileMissingType_StillFails()
    {
        // Compiling only the message file — the enum's file is absent — can't see
        // HostState, so it trips CE0010. Confirms resolution needs the whole set,
        // which the multi-file path supplies (and that we didn't over-loosen).
        var temp = NewTempDir();
        try
        {
            var msg = Path.Combine(temp, "Messages.spek");
            File.WriteAllText(msg, "namespace X;\nmessage StateChanged(HostState State);\n");

            var (exit, _, stderr) = RunSpekc("compile", msg, "--out", Path.Combine(temp, "gen"));

            Assert.NotEqual(0, exit);
            Assert.Contains("CE0010", stderr);
        }
        finally { Directory.Delete(temp, recursive: true); }
    }

    [Fact]
    public void Compile_CrossFileDuplicateType_IsReported()
    {
        // The same type declared in two files must be caught (CE0013), not
        // silently double-emitted.
        var temp = NewTempDir();
        try
        {
            File.WriteAllText(Path.Combine(temp, "A.spek"), "namespace X;\nenum Dup { A, B }\n");
            File.WriteAllText(Path.Combine(temp, "B.spek"), "namespace X;\nenum Dup { C, D }\n");

            var (exit, _, stderr) = RunSpekc("compile", temp, "--out", Path.Combine(temp, "gen"));

            Assert.NotEqual(0, exit);
            Assert.Contains("CE0013", stderr);
        }
        finally { Directory.Delete(temp, recursive: true); }
    }

    // ─── coverage gaps closed: exit-code contract completeness ──────────

    [Fact]
    public void Compile_NonexistentInput_FailsCleanly_ExitsOne()
    {
        var (exit, _, stderr) = RunSpekc("compile", "/nope/definitely-missing.spek");
        Assert.Equal(1, exit);
        Assert.Contains("missing.spek", stderr);
    }

    [Fact]
    public void Compile_Warning_StillEmits_ExitsZero()
    {
        var temp = NewTempDir();
        try
        {
            var spekFile = Path.Combine(temp, "Warny.spek");
            // CE0134 (direct time read in an actor) is a warning: the build
            // must emit and exit 0, with the warning on stderr.
            File.WriteAllText(spekFile, """
                namespace W;
                message T();
                actor A
                {
                    behavior Default
                    {
                        on T t => { var n = DateTime.UtcNow; }
                    }
                }
                """);
            var outDir = Path.Combine(temp, "gen");
            var (exit, _, stderr) = RunSpekc("compile", spekFile, "--out", outDir);

            Assert.Equal(0, exit);
            Assert.True(File.Exists(Path.Combine(outDir, "Warny.g.cs")));
            Assert.Contains("CE0134", stderr);
        }
        finally { Directory.Delete(temp, recursive: true); }
    }

    [Fact]
    public void Compile_Check_CleanSource_ExitsZero()
    {
        var temp = NewTempDir();
        try
        {
            var spekFile = Path.Combine(temp, "Clean.spek");
            File.WriteAllText(spekFile, """
                namespace C;
                message Ping();
                actor A
                {
                    behavior Default { on Ping p => { } }
                }
                """);
            var (exit, _, _) = RunSpekc("compile", spekFile, "--out",
                Path.Combine(temp, "gen"), "--check");
            Assert.Equal(0, exit);
        }
        finally { Directory.Delete(temp, recursive: true); }
    }

    [Fact]
    public void Compile_Directory_EmitsForEverySpekFile()
    {
        var temp = NewTempDir();
        try
        {
            File.WriteAllText(Path.Combine(temp, "One.spek"),
                "namespace D;\nmessage A();\n");
            File.WriteAllText(Path.Combine(temp, "Two.spek"),
                "namespace D;\nmessage B();\n");

            var outDir = Path.Combine(temp, "gen");
            var (exit, _, _) = RunSpekc("compile", temp, "--out", outDir);

            Assert.Equal(0, exit);
            Assert.True(File.Exists(Path.Combine(outDir, "One.g.cs")));
            Assert.True(File.Exists(Path.Combine(outDir, "Two.g.cs")));
        }
        finally { Directory.Delete(temp, recursive: true); }
    }
}
