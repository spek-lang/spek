using Google.Protobuf;
using Google.Protobuf.Reflection;
using Spek.Cli;
using Xunit;

namespace Spek.Tests.Compiler;

/// <summary>
/// Exercises the `spekc proto-import` and
/// `spekc proto-export` subcommands in-process via
/// <see cref="CliRunner.Run"/>. The underlying synthesizer/emitter
/// are tested directly elsewhere; this suite catches CLI-level
/// wiring bugs (argv parsing, output-file writing, exit codes) that
/// the in-process tests can't surface.
///
/// <para>
/// These used to spawn the CLI as a `dotnet run` subprocess,
/// which was slow enough to balloon the suite; they now drive the
/// CLI directly with captured <see cref="StringWriter"/>s.
/// </para>
/// </summary>
public sealed class SpekcProtoCliTests
{
    private static (int ExitCode, string StdOut, string StdErr) RunSpekc(params string[] args)
    {
        var stdout = new StringWriter();
        var stderr = new StringWriter();
        var exit   = CliRunner.Run(args, stdout, stderr);
        return (exit, stdout.ToString(), stderr.ToString());
    }

    private static string WriteSampleDescriptor(string tempDir)
    {
        var file = new FileDescriptorProto
        {
            Name    = "user_api.proto",
            Syntax  = "proto3",
            Package = "userapi",
        };
        var getUser = new DescriptorProto { Name = "GetUser" };
        getUser.Field.Add(new FieldDescriptorProto
        {
            Name = "id", Number = 1, JsonName = "id",
            Type = FieldDescriptorProto.Types.Type.String,
            Label = FieldDescriptorProto.Types.Label.Optional,
        });
        var user = new DescriptorProto { Name = "User" };
        user.Field.Add(new FieldDescriptorProto
        {
            Name = "id", Number = 1, JsonName = "id",
            Type = FieldDescriptorProto.Types.Type.String,
            Label = FieldDescriptorProto.Types.Label.Optional,
        });
        file.MessageType.Add(getUser);
        file.MessageType.Add(user);

        var service = new ServiceDescriptorProto { Name = "UserApi" };
        service.Method.Add(new MethodDescriptorProto
        {
            Name = "GetUser", InputType = ".userapi.GetUser", OutputType = ".userapi.User",
        });
        file.Service.Add(service);

        var fileSet = new FileDescriptorSet();
        fileSet.File.Add(file);
        var descriptorPath = Path.Combine(tempDir, "user_api.descriptor.bin");
        File.WriteAllBytes(descriptorPath, fileSet.ToByteArray());
        return descriptorPath;
    }

    // ─── proto-import ──────────────────────────────────────────────────

    [Fact]
    public void ProtoImport_WritesGSpekFile_WithSynthesizedChannel()
    {
        var temp = Path.Combine(Path.GetTempPath(), "spekc-cli-" + Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(temp);
        try
        {
            var descriptor = WriteSampleDescriptor(temp);
            var outFile    = Path.Combine(temp, "UserApi.g.spek");

            var (exit, _, stderr) = RunSpekc(
                "proto-import", descriptor, "UserApi",
                "--out", outFile, "--proto-label", "user_api.proto");

            Assert.Equal(0, exit);
            Assert.True(File.Exists(outFile), $"expected output file at {outFile}; stderr was: {stderr}");

            var generated = File.ReadAllText(outFile);
            Assert.Contains("channel UserApi", generated);
            Assert.Contains("on GetUser;",     generated);
            Assert.Contains("emits User;",     generated);
            Assert.Contains("Source: user_api.proto", generated);
        }
        finally { Directory.Delete(temp, recursive: true); }
    }

    [Fact]
    public void ProtoExport_WritesProtoFile_FromHandWrittenChannel()
    {
        var temp = Path.Combine(Path.GetTempPath(), "spekc-cli-" + Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(temp);
        try
        {
            var spekFile = Path.Combine(temp, "UserApi.spek");
            File.WriteAllText(spekFile, """
                message GetUser(string id);
                message User(string id, string name);
                channel UserApi { on GetUser; emits User; }
                """);
            var outProto = Path.Combine(temp, "user_api.proto");

            var (exit, _, stderr) = RunSpekc(
                "proto-export", spekFile, "UserApi",
                "--out", outProto, "--package", "myorg.user.v1");

            Assert.Equal(0, exit);
            Assert.True(File.Exists(outProto), $"expected output file at {outProto}; stderr was: {stderr}");

            var generated = File.ReadAllText(outProto);
            Assert.Contains("syntax = \"proto3\";",    generated);
            Assert.Contains("package myorg.user.v1;", generated);
            Assert.Contains("service UserApi {",      generated);
            Assert.Contains("rpc GetUser(GetUser)",   generated);
        }
        finally { Directory.Delete(temp, recursive: true); }
    }
}
