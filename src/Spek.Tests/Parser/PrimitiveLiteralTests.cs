using Spek.Compiler.Emit;
using Spek.Compiler.Parser;
using Spek.Tests.Emit;   // RoslynCompileHelper
using Xunit;

namespace Spek.Tests.Parser;

/// <summary>
/// The literal-forms safety net. Spek follows C#'s lead on primitive
/// literals, and every form a C# developer can write should:
///   1. parse,
///   2. emit **verbatim** (no silent re-formatting — that would drop digit
///      separators and type suffixes like <c>L</c>/<c>UL</c>/<c>f</c> that
///      change the C# type), and
///   3. produce C# that actually compiles.
/// Malformed forms must be rejected, not silently mis-lexed.
///
/// <see cref="Forms"/> is the single canonical list of supported forms,
/// driving both the per-form checks and the compile-the-lot check, so a
/// deviation from C# is caught here rather than discovered ad hoc later.
/// </summary>
public sealed class PrimitiveLiteralTests
{
    public static readonly string[] Forms =
    {
        // ── integers ──
        "0", "42", "100_000_000",            // decimal + digit separators
        "0xFF", "0Xff", "0x1_000",           // hex (+ separators)
        "0b1010", "0B1010_1010",             // binary (+ separators)
        "100L", "100u", "100U", "100ul", "100UL",   // integer suffixes
        // ── reals (double / float / decimal) ──
        "1.5", "1_000.000_5",                // fractional + separators
        "1.5e3", "1.5E-3", "2e10",           // exponents
        "1.5f", "1.5F", "1.5d", "1.5D",      // float / double suffixes
        "1.5m", "1.5M", "0m", "100m",        // decimal suffix (+ integer-valued)
        "5f", "5d",                          // integer-valued float/double
        // ── char ──
        "'a'", "'0'", "'\\n'", "'\\''", "'\\u0041'",
        // ── bool ──
        "true", "false",
        // ── string ──
        "\"hello\"", "\"a\\\"b\"", "\"line1\\nline2\\t\"",
    };

    public static IEnumerable<object[]> AllForms() => Forms.Select(f => new object[] { f });

    private static (bool Ok, string Code, string Diags) EmitProbe(string body)
    {
        var parsed = SpekCompiler.Parse($"program Main {{ {body} }}");
        if (!parsed.Success)
            return (false, "", string.Join("; ", parsed.Diagnostics.Select(d => $"{d.Code} {d.Message}")));
        return (true, new FileEmitter().Emit(parsed.Tree!), "");
    }

    // ── 1. Each form parses and emits verbatim ──
    [Theory]
    [MemberData(nameof(AllForms))]
    public void Literal_ParsesAndEmitsVerbatim(string literal)
    {
        var (ok, code, diags) = EmitProbe($"var probe = {literal}; System.Console.WriteLine(probe);");
        Assert.True(ok, $"`{literal}` failed to parse: {diags}");
        Assert.True(code.Contains($"= {literal};"),
            $"`{literal}` did not emit verbatim — re-formatted or re-interpreted.\nEmitted: " +
            (code.Split('\n').FirstOrDefault(l => l.Contains("probe")) ?? "(probe line not found)"));
    }

    // ── 2. The whole set produces valid C# (substring-contains isn't proof) ──
    [Fact]
    public void AllForms_EmitCompilableCSharp()
    {
        var body = string.Join("\n",
            Forms.Select((f, i) => $"var p{i} = {f}; System.Console.WriteLine(p{i});"));
        var (ok, code, diags) = EmitProbe(body);
        Assert.True(ok, $"failed to parse: {diags}");

        var (compiled, summary, _) = RoslynCompileHelper.TryCompile(code, "AllLiterals");
        Assert.True(compiled, $"emitted literals did not compile:\n{summary}");
    }

    // ── 3. The message-field default path emits verbatim and compiles ──
    [Fact]
    public void MessageFieldDefaults_EmitVerbatimAndCompile()
    {
        var parsed = SpekCompiler.Parse("message M(char c = 'a', int h = 0xFF, decimal d = 0m);");
        Assert.True(parsed.Success,
            string.Join("; ", parsed.Diagnostics.Select(d => $"{d.Code} {d.Message}")));
        var code = new FileEmitter().Emit(parsed.Tree!);

        Assert.Contains("'a'",  code);
        Assert.Contains("0xFF", code);
        Assert.Contains("0m",   code);

        var (compiled, summary, _) = RoslynCompileHelper.TryCompile(code, "MsgDefaults");
        Assert.True(compiled, $"message with literal defaults did not compile:\n{summary}");
    }

    // ── 4. Malformed literals are rejected, not silently mis-lexed ──
    [Theory]
    [InlineData("0x")]      // hex prefix, no digits
    [InlineData("0b")]      // binary prefix, no digits
    [InlineData("''")]      // empty char
    [InlineData("'ab'")]    // multi-character char
    public void MalformedLiteral_IsRejected(string literal)
    {
        var (ok, _, _) = EmitProbe($"var probe = {literal};");
        Assert.False(ok, $"`{literal}` should have been rejected but parsed");
    }
}
