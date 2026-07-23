using System.Text;

namespace Spek.Compiler.Semantic;

/// <summary>
/// Renders a <see cref="Diagnostic"/> in the Rust-style framed format —
/// a header (<c>error[CE0020]: …</c>), a <c>--&gt;</c> location line, and the
/// offending source line with a caret underline pointing at the exact span:
///
/// <code>
/// error[CE0020]: '.Ask(...)' message type 'Foo' is not a declared 'message'.
///   --> bank.spek:12:33
///    |
/// 12 |     Balance b = account.Ask(new Foo());
///    |                                 ^^^
///    |
/// </code>
///
/// The underline width comes from <see cref="Diagnostic.Span"/> when present
/// (EndColumn is 1-based exclusive, so width = End - Start); diagnostics
/// that only know a point (e.g. ANTLR syntax errors) get a single caret,
/// widened to the identifier under it.
/// </summary>
public static class DiagnosticRenderer
{
    /// <summary>Renders one diagnostic against <paramref name="source"/> (the
    /// full text of <paramref name="fileName"/>). Returns a multi-line block
    /// with no trailing newline.</summary>
    public static string Render(string source, string fileName, Diagnostic d)
    {
        var severity = d.Severity == DiagnosticSeverity.Error ? "error" : "warning";
        var lines    = source.Replace("\r\n", "\n").Replace('\r', '\n').Split('\n');

        var gutter = new string(' ', d.Line.ToString().Length);
        var sb = new StringBuilder();

        sb.Append(severity).Append('[').Append(d.Code).Append("]: ").Append(d.Message).Append('\n');
        sb.Append(gutter).Append("--> ").Append(fileName).Append(':').Append(d.Line).Append(':').Append(d.Column);

        if (d.Line >= 1 && d.Line <= lines.Length)
        {
            var src        = lines[d.Line - 1];
            var caretStart = Math.Max(0, d.Column - 1);

            // Width from the span if it's on a single line; else a single caret.
            var width = 1;
            if (d.Span is { } s && s.EndLine == s.StartLine)
                width = Math.Max(1, s.EndColumn - s.StartColumn);

            // A single-token span (Start == Stop) gives width 1; widen it to
            // cover the whole identifier under the caret so the underline hugs
            // the offending name (`Tick`, `GetBalance`) rather than one char.
            if (width == 1 && caretStart < src.Length && IsWordChar(src[caretStart]))
            {
                var end = caretStart;
                while (end < src.Length && IsWordChar(src[end])) end++;
                width = end - caretStart;
            }

            // Don't run past the end of the source line.
            width = Math.Min(width, Math.Max(1, src.Length - caretStart));

            sb.Append('\n').Append(gutter).Append(" |");
            sb.Append('\n').Append(d.Line).Append(" | ").Append(src);
            sb.Append('\n').Append(gutter).Append(" | ")
              .Append(new string(' ', caretStart)).Append(new string('^', width));
            sb.Append('\n').Append(gutter).Append(" |");
        }

        return sb.ToString();
    }

    private static bool IsWordChar(char c) => char.IsLetterOrDigit(c) || c == '_';
}
