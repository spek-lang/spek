using Spek.Compiler.AST;
using Spek.Compiler.Emit;
using Xunit;

namespace Spek.Tests.Emit;

public class MessageEmitterTests
{
    private static string EmitMessage(MessageDecl msg)
    {
        var w = new CSharpWriter();
        new MessageEmitter(w).Emit(msg);
        return w.ToString().Trim();
    }

    private static SourceSpan NoSpan => SourceSpan.None;

    private static TypeRef SimpleType(string name) =>
        new(NoSpan, new QualifiedName(NoSpan, [name]), []);

    private static QualifiedName SimpleName(string name) =>
        new(NoSpan, [name]);

    [Fact]
    public void SimpleMessage_EmitsRecord()
    {
        var msg = new MessageDecl(
            NoSpan, "Deposit", [],
            [new MessageField(NoSpan, SimpleType("decimal"), "amount", null),
             new MessageField(NoSpan, SimpleType("string"),  "fromUser", null)]);

        Assert.Equal("public record Deposit(decimal amount, string fromUser);", EmitMessage(msg));
    }

    [Fact]
    public void GenericMessage_EmitsRecordWithTypeParam()
    {
        var msg = new MessageDecl(
            NoSpan, "Response",
            [new TypeParameter(NoSpan, "T")],
            [new MessageField(NoSpan, SimpleType("T"), "value", null)]);

        Assert.Equal("public record Response<T>(T value);", EmitMessage(msg));
    }

    [Fact]
    public void MessageWithDefault_EmitsDefaultValue()
    {
        var msg = new MessageDecl(
            NoSpan, "Shutdown", [],
            [new MessageField(NoSpan, SimpleType("string"), "reason",
                new StringLiteralExpr(NoSpan, "normal"))]);

        Assert.Equal("public record Shutdown(string reason = \"normal\");", EmitMessage(msg));
    }

    [Fact]
    public void EmptyMessage_EmitsEmptyRecord()
    {
        var msg = new MessageDecl(NoSpan, "GetBalance", [], []);
        Assert.Equal("public record GetBalance();", EmitMessage(msg));
    }
}
