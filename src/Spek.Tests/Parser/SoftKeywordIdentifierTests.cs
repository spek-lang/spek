using Spek.Compiler.Parser;
using Xunit;

namespace Spek.Tests.Parser;

/// <summary>
/// Soft keywords: the collision-prone keywords
/// <c>actor</c> / <c>message</c> / <c>channel</c> / <c>after</c> / <c>strategy</c> are
/// usable as ordinary identifiers in every name-binding position — variable, parameter,
/// field, message field, foreach variable, handler binding — and as references. They stay
/// reserved only as declaration starters (<c>actor X { }</c>) and fixed contextual
/// sequences (<c>passivate after</c>, <c>strategy:</c>). A C#/Java developer should never
/// hit "that's reserved?!" for these. Handler-mode keywords (event/reader/writer) are
/// deliberately excluded to avoid ambiguity with <c>on event</c> / <c>reader on</c>.
/// </summary>
public sealed class SoftKeywordIdentifierTests
{
    public static IEnumerable<object[]> SoftKeywords()
    {
        yield return ["actor"];
        yield return ["message"];
        yield return ["channel"];
        yield return ["after"];
        yield return ["strategy"];
    }

    [Theory]
    [MemberData(nameof(SoftKeywords))]
    public void AsLocalVariableAndReference(string kw) =>
        Ok($"module M {{ public int F() {{ var {kw} = 5; return {kw}; }} }}");

    [Theory]
    [MemberData(nameof(SoftKeywords))]
    public void AsParameter(string kw) =>
        Ok($"module M {{ public int F(int {kw}) {{ return {kw}; }} }}");

    [Theory]
    [MemberData(nameof(SoftKeywords))]
    public void AsActorField(string kw) =>
        Ok($"message Go();\nactor A {{ int {kw} = 0; init() {{ become R; }} " +
           $"behavior R {{ on Go => {{ {kw} = {kw} + 1; }} }} }}");

    [Theory]
    [MemberData(nameof(SoftKeywords))]
    public void AsMessageField(string kw) =>
        Ok($"message Carrier(int {kw});");

    [Theory]
    [MemberData(nameof(SoftKeywords))]
    public void AsForeachVariable(string kw) =>
        Ok($"module M {{ public int F(System.Collections.Generic.List<int> xs) {{ " +
           $"var total = 0; foreach (var {kw} in xs) {{ total = total + {kw}; }} return total; }} }}");

    [Theory]
    [MemberData(nameof(SoftKeywords))]
    public void AsHandlerBinding(string kw) =>
        Ok($"message Go(int n);\nactor A {{ int sum = 0; init() {{ become R; }} " +
           $"behavior R {{ on Go {kw} => {{ sum = sum + {kw}.n; }} }} }}");

    [Fact]
    public void DeclarationFormsStillReserved()
    {
        // `actor`/`message` remain declaration starters — the softening must not break the
        // hard forms. This is a real actor + message decl, not an identifier use.
        Ok("message Ping();\nactor Worker { init() { become R; } behavior R { on Ping => { } } }");
    }

    private static void Ok(string src)
    {
        var parse = SpekCompiler.Parse(src);
        Assert.True(parse.Success,
            string.Join("\n", parse.Diagnostics.Select(d => $"{d.Code} {d.Message}")));
    }
}
