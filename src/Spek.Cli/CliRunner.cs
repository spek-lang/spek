using Spek.Compiler.AST;
using Spek.Compiler.Emit;
using Spek.Compiler.Format;
using Spek.Compiler.Parser;
using Spek.Compiler.Proto;
using Spek.Compiler.Semantic;

namespace Spek.Cli;

/// <summary>
/// In-process entry point for the <c>spekc</c> command-line tool. All
/// command logic lives here so it can be driven directly from tests with
/// captured <see cref="System.IO.TextWriter"/>s instead of spawning a
/// <c>dotnet run</c> subprocess. <c>Program.cs</c> is a thin shim that
/// forwards the process argv and the real <c>Console</c> streams.
/// </summary>
public static class CliRunner
{
    /// <summary>
    /// Runs the CLI with the given <paramref name="args"/>, writing normal
    /// output to <paramref name="out"/> and diagnostics/usage to
    /// <paramref name="error"/>. Returns the process exit code.
    /// </summary>
    public static int Run(string[] args, System.IO.TextWriter @out, System.IO.TextWriter error)
    {
        if (args.Length < 1 || (args[0] != "compile" && args[0] != "format"
                                && args[0] != "proto-import" && args[0] != "proto-export"))
        {
            error.WriteLine("Usage: spekc compile      <file.spek> [--out <dir>] [--check] [--ref <dll>] [--no-line-map]");
            error.WriteLine("       spekc compile      <dir>       [--out <dir>]   (compiles *.spek in directory)");
            error.WriteLine("         --check        compile the emitted C# with Roslyn and report errors");
            error.WriteLine("                        on the .spek lines (BCL + Spek.Runtime referenced;");
            error.WriteLine("                        add more with --ref)");
            error.WriteLine("         --ref <dll>    extra metadata reference for --check (repeatable)");
            error.WriteLine("         --no-line-map  emit without #line directives (no .spek source mapping)");
            error.WriteLine("         --abs-line-map #line directives use absolute .spek paths (for debugging:");
            error.WriteLine("                        the PDB then resolves sources without a working-dir guess)");
            error.WriteLine("       spekc format       <file.spek> [--write]       (rewrite file in place with --write)");
            error.WriteLine("       spekc format       <dir>       [--write]       (formats *.spek in directory)");
            error.WriteLine("       spekc proto-import <descriptor.bin> <ChannelName> [--out <file.g.spek>]");
            error.WriteLine("                                              (synthesise channel from a proto descriptor)");
            error.WriteLine("       spekc proto-export <file.spek> <ChannelName> [--out <file.proto>] [--package <name>]");
            error.WriteLine("                                              (emit .proto from a channel decl)");
            return 1;
        }

        switch (args[0])
        {
            case "format":
                return RunFormat(args[1..], @out, error);
            case "proto-import":
                return RunProtoImport(args[1..], @out, error);
            case "proto-export":
                return RunProtoExport(args[1..], @out, error);
        }

        // Parse compile options
        string? outDir = null;
        string? baseDir = null;
        bool check = false;
        bool lineMap = true;
        bool absoluteLineMap = false;
        bool emitTests = false;
        var extraRefs = new List<string>();
        var inputs = new List<string>();
        for (int i = 1; i < args.Length; i++)
        {
            switch (args[i])
            {
                case "--out" when i + 1 < args.Length:
                    outDir = args[++i];
                    break;
                case "--base" when i + 1 < args.Length:
                    // Mirror the input tree under --out relative to this dir, so
                    // sub/Foo.spek emits to <out>/sub/Foo.g.cs (used by the MSBuild
                    // integration to keep obj/spek/ laid out like the source).
                    baseDir = args[++i];
                    break;
                case "--check":
                    check = true;
                    break;
                case "--no-line-map":
                    lineMap = false;
                    break;
                case "--abs-line-map":
                    absoluteLineMap = true;
                    break;
                case "--tests":
                    emitTests = true;   // `*Tests` modules/classes emit as native tests (test projects)
                    break;
                case "--ref" when i + 1 < args.Length:
                    extraRefs.Add(args[++i]);
                    break;
                default:
                    inputs.Add(args[i]);
                    break;
            }
        }

        if (inputs.Count == 0)
        {
            error.WriteLine("error: no input files specified");
            return 1;
        }

        // Expand directories to *.spek files
        var files = new List<string>();
        foreach (var input in inputs)
        {
            if (Directory.Exists(input))
                files.AddRange(Directory.GetFiles(input, "*.spek", SearchOption.TopDirectoryOnly));
            else if (File.Exists(input))
                files.Add(input);
            else
            {
                error.WriteLine($"error: '{input}' not found");
                return 1;
            }
        }

        int errorCount = 0;
        var emitter = new FileEmitter();

        // Parse every input first, then compile them as ONE unit: a combined
        // symbol table across all files, and per-file semantic analysis against
        // it. That's what makes cross-file type references resolve — a message
        // field typed as an enum declared in a sibling file no longer trips a
        // false "undeclared type". Each file still emits to its own .g.cs.
        var parsed = files.Select(f =>
        {
            var source = File.ReadAllText(f);
            var (tree, diags) = SpekCompiler.ParseToTree(source);
            return (Path: f, Source: source, Tree: tree, Diags: new List<Diagnostic>(diags));
        }).ToList();

        var trees = parsed.Where(p => p.Tree is not null).Select(p => p.Tree!).ToList();
        var symbols = SymbolTable.BuildCombined(trees);
        foreach (var p in parsed)
            if (p.Tree is not null)
                p.Diags.AddRange(SemanticAnalyzer.Analyze(p.Tree, symbols));

        // Surface every diagnostic Rust-style (header + source line + caret),
        // rendered against its own file. Warnings show even on success; only
        // errors fail the build and skip emit.
        foreach (var p in parsed)
        {
            foreach (var d in p.Diags)
            {
                error.WriteLine(DiagnosticRenderer.Render(p.Source, p.Path, d));
                error.WriteLine();
            }
            errorCount += p.Diags.Count(d => d.Severity == DiagnosticSeverity.Error);
        }

        // Duplicate declarations that collide across files (CE0013) — rendered
        // plainly, since they inherently point into more than one file.
        foreach (var d in SemanticAnalyzer.CheckCrossFileDuplicates(trees))
        {
            error.WriteLine($"error[{d.Code}]: {d.Message}");
            error.WriteLine();
            if (d.Severity == DiagnosticSeverity.Error) errorCount++;
        }

        var emitted = new List<string>();   // for a project-wide --check
        foreach (var p in parsed)
        {
            if (p.Tree is null || p.Diags.Any(d => d.Severity == DiagnosticSeverity.Error))
                continue;

            // With --base, mirror the input's path under --out (so sub/Foo.spek ->
            // <out>/sub/Foo.g.cs); otherwise emit flat, in --out or next to source.
            string outputFile;
            if (outDir is not null && baseDir is not null)
            {
                var rel = Path.GetRelativePath(Path.GetFullPath(baseDir), Path.GetFullPath(p.Path));
                outputFile = Path.Combine(outDir, Path.ChangeExtension(rel, null) + ".g.cs");
            }
            else
            {
                var od = outDir ?? Path.GetDirectoryName(p.Path)!;
                if (od.Length == 0) od = ".";
                outputFile = Path.Combine(od, Path.GetFileNameWithoutExtension(p.Path) + ".g.cs");
            }
            var outputDir = Path.GetDirectoryName(Path.GetFullPath(outputFile))!;

            // Relative #line paths keep checked-in generated files machine-neutral;
            // absolute paths (--abs-line-map) let a debugger's PDB resolve the
            // .spek without guessing a working directory.
            var mapPath = lineMap
                ? (absoluteLineMap
                    ? Path.GetFullPath(p.Path).Replace('\\', '/')
                    : Path.GetRelativePath(outputDir, p.Path).Replace('\\', '/'))
                : null;
            // Emit with the COMBINED symbol table so cross-file references and
            // ask reply-type inference resolve across files. --ref assemblies feed
            // both the invisible-async pass and --check.
            var csharp = emitter.Emit(p.Tree, symbols, sourceFileName: mapPath,
                asyncReferencePaths: extraRefs.Count > 0 ? extraRefs : null,
                emitTests: emitTests);

            Directory.CreateDirectory(outputDir);
            File.WriteAllText(outputFile, csharp);
            @out.WriteLine($"  {p.Path} -> {outputFile}");
            emitted.Add(csharp);
        }

        if (check && emitted.Count > 0)
        {
            // Compile the whole project's emitted C# together so cross-file
            // references resolve. Errors carry their #line-mapped .spek location.
            var refs = new List<string> { typeof(Spek.ActorBase).Assembly.Location };
            refs.AddRange(extraRefs);
            var checkErrors = RoslynCheck.Check(emitted, refs);
            foreach (var e in checkErrors)
                error.WriteLine(e.ToString());
            if (checkErrors.Count > 0)
                error.WriteLine(
                    $"  --check: the emitted C# has {checkErrors.Count} error(s). " +
                    "If they name missing types from a NuGet/framework package, pass --ref <dll>.");
            errorCount += checkErrors.Count;
        }

        if (errorCount > 0)
        {
            error.WriteLine($"\nBuild FAILED — {errorCount} error(s).");
            return 1;
        }

        return 0;
    }

    // ─── format subcommand ──────────────────────────────────────────────────
    //
    // `spekc format <input> [--write | --check]` re-prints Spek source with
    // canonical indentation and whitespace, preserving every comment verbatim.
    // A directory input is walked recursively for `*.spek`. Without a flag the
    // formatted output goes to stdout (pipe to a diff tool); `--write` rewrites
    // files in place; `--check` changes nothing and exits non-zero, listing any
    // file that isn't already canonically formatted (for CI).
    private static int RunFormat(string[] formatArgs, System.IO.TextWriter @out, System.IO.TextWriter error)
    {
        bool write = false;
        bool check = false;
        var inputs = new List<string>();
        foreach (var a in formatArgs)
        {
            switch (a)
            {
                case "--write":
                    write = true;
                    break;
                case "--check":
                    check = true;
                    break;
                default:
                    inputs.Add(a);
                    break;
            }
        }

        if (inputs.Count == 0)
        {
            error.WriteLine("error: no input files specified");
            return 1;
        }

        var files = new List<string>();
        foreach (var input in inputs)
        {
            if (Directory.Exists(input))
                files.AddRange(Directory.GetFiles(input, "*.spek", SearchOption.AllDirectories));
            else if (File.Exists(input))
                files.Add(input);
            else
            {
                error.WriteLine($"error: '{input}' not found");
                return 1;
            }
        }

        int unformatted = 0;
        foreach (var spekFile in files)
        {
            var source    = File.ReadAllText(spekFile);
            var formatted = SpekFormatter.Format(source);

            if (check)
            {
                // Report but change nothing; exit non-zero if any file isn't
                // already canonically formatted (the `gofmt -l` / CI idiom).
                if (formatted != source)
                {
                    unformatted++;
                    @out.WriteLine(spekFile);
                }
            }
            else if (write)
            {
                if (formatted != source)
                {
                    File.WriteAllText(spekFile, formatted);
                    @out.WriteLine($"  formatted {spekFile}");
                }
            }
            else
            {
                @out.Write(formatted);
            }
        }

        if (check && unformatted > 0)
        {
            error.WriteLine($"{unformatted} file(s) need formatting (run 'spekc format --write').");
            return 1;
        }

        return 0;
    }

    // ─── proto-import subcommand ────────────────────────────────────────────
    //
    // `spekc proto-import <descriptor.bin> <ChannelName> [--out <path>]`
    // Reads a binary FileDescriptorSet (typically produced by
    // `protoc --descriptor_set_out`) and synthesises a Spek channel
    // declaration plus all referenced message decls. Writes the
    // synthesised source to <path> when --out is given; otherwise to
    // `<ChannelName>.g.spek` next to the descriptor file. The intended
    // caller is the `Spek.Hosting.AspNetCore.Grpc` MSBuild target,
    // which orchestrates the protoc invocation that produces the
    // descriptor.
    private static int RunProtoImport(string[] importArgs, System.IO.TextWriter @out, System.IO.TextWriter error)
    {
        if (importArgs.Length < 2)
        {
            error.WriteLine("error: usage — spekc proto-import <descriptor.bin> <ChannelName> [--out <path>]");
            return 1;
        }

        var descriptorPath = importArgs[0];
        var channelName    = importArgs[1];
        string? outPath    = null;
        string protoLabel  = importArgs[0];   // default to descriptor path

        for (int i = 2; i < importArgs.Length; i++)
        {
            switch (importArgs[i])
            {
                case "--out" when i + 1 < importArgs.Length:
                    outPath = importArgs[++i];
                    break;
                case "--proto-label" when i + 1 < importArgs.Length:
                    protoLabel = importArgs[++i];   // override the "Source: ..." label
                    break;
                default:
                    error.WriteLine($"error: unknown argument '{importArgs[i]}'");
                    return 1;
            }
        }

        if (!File.Exists(descriptorPath))
        {
            error.WriteLine($"error: descriptor file '{descriptorPath}' not found");
            return 1;
        }

        var synth = new ProtoChannelSynthesizer();
        var src   = synth.Synthesize(descriptorPath, channelName, protoLabel);

        foreach (var diag in synth.Diagnostics)
            error.WriteLine($"{diag.Severity.ToLowerInvariant()}: {diag.Message}");

        if (synth.Diagnostics.Any(d => d.Severity == "Error"))
            return 1;

        outPath ??= Path.Combine(
            Path.GetDirectoryName(descriptorPath) ?? ".",
            $"{channelName}.g.spek");

        Directory.CreateDirectory(Path.GetDirectoryName(Path.GetFullPath(outPath))!);
        File.WriteAllText(outPath, src);
        @out.WriteLine($"  {descriptorPath} -> {outPath}");
        return 0;
    }

    // ─── proto-export subcommand ────────────────────────────────────────────
    //
    // `spekc proto-export <file.spek> <ChannelName> [--out <path>] [--package <name>]`
    // Reads <file.spek>, locates the named channel + its referenced
    // messages, and emits a proto3 file. Output goes to <path> with
    // --out, or `<ChannelName>.proto` next to the source file by
    // default. <name> overrides the auto-derived proto package name.
    private static int RunProtoExport(string[] exportArgs, System.IO.TextWriter @out, System.IO.TextWriter error)
    {
        if (exportArgs.Length < 2)
        {
            error.WriteLine("error: usage — spekc proto-export <file.spek> <ChannelName> [--out <path>] [--package <name>]");
            return 1;
        }

        var spekPath    = exportArgs[0];
        var channelName = exportArgs[1];
        string? outPath = null;
        string? package = null;

        for (int i = 2; i < exportArgs.Length; i++)
        {
            if (exportArgs[i] == "--out" && i + 1 < exportArgs.Length)
                outPath = exportArgs[++i];
            else if (exportArgs[i] == "--package" && i + 1 < exportArgs.Length)
                package = exportArgs[++i];
            else
            {
                error.WriteLine($"error: unknown argument '{exportArgs[i]}'");
                return 1;
            }
        }

        if (!File.Exists(spekPath))
        {
            error.WriteLine($"error: source file '{spekPath}' not found");
            return 1;
        }

        var source = File.ReadAllText(spekPath);
        var parsed = SpekCompiler.Parse(source);

        if (!parsed.Success || parsed.Tree is null)
        {
            foreach (var d in parsed.Diagnostics)
                error.WriteLine($"{spekPath}({d.Line},{d.Column}): error {d.Code}: {d.Message}");
            return 1;
        }

        var channel = parsed.Tree.Declarations.OfType<ChannelDecl>()
            .FirstOrDefault(c => c.Name == channelName);
        if (channel is null)
        {
            error.WriteLine($"error: channel '{channelName}' not found in '{spekPath}'");
            return 1;
        }

        var messages = parsed.Tree.Declarations.OfType<MessageDecl>()
            .ToDictionary(m => m.Name);

        var emitter = new ChannelToProtoEmitter();
        var proto   = emitter.Emit(channel, messages, spekPath, package);

        foreach (var diag in emitter.Diagnostics)
            error.WriteLine($"{diag.Severity.ToLowerInvariant()}: {diag.Message}");

        if (emitter.Diagnostics.Any(d => d.Severity == "Error"))
            return 1;

        outPath ??= Path.Combine(
            Path.GetDirectoryName(spekPath) ?? ".",
            $"{channelName}.proto");

        Directory.CreateDirectory(Path.GetDirectoryName(Path.GetFullPath(outPath))!);
        File.WriteAllText(outPath, proto);
        @out.WriteLine($"  {spekPath} -> {outPath}");
        return 0;
    }
}
