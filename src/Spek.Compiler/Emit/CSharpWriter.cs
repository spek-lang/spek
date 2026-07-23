namespace Spek.Compiler.Emit;

/// <summary>Thin wrapper around StringBuilder that handles indentation and,
/// when enabled, <c>#line</c> source mapping back to the .spek file.</summary>
public sealed class CSharpWriter
{
    private readonly System.Text.StringBuilder _sb = new();
    private int _indent;

    // ─── #line source mapping ────────────────────────────────────────────
    // When a .spek file name is supplied, MapLine emits `#line N "file"`
    // before user statements so Roslyn diagnostics (and debugger stepping)
    // land on the .spek source instead of the generated .g.cs. MapDefault
    // restores generated-code attribution when emit leaves a user body.
    // Mapping is opt-in: with no file name both calls are no-ops, so
    // callers that never asked for mapping get byte-identical output.
    private string? _mapFile;
    private bool _mapped;

    public void EnableSourceMapping(string spekFileName) => _mapFile = spekFileName;

    /// <summary>Attribute the next emitted line(s) to .spek line
    /// <paramref name="spekLine"/>. No-op when mapping is disabled.</summary>
    public void MapLine(int spekLine)
    {
        if (_mapFile is null || spekLine <= 0) return;
        // `#line` is a preprocessor directive — always column 0, never indented.
        _sb.Append("#line ").Append(spekLine).Append(" \"").Append(_mapFile).Append('"').AppendLine();
        _mapped = true;
    }

    /// <summary>Back to generated-code attribution. No-op when mapping is
    /// disabled or no directive is currently open.</summary>
    public void MapDefault()
    {
        if (_mapFile is null || !_mapped) return;
        _sb.AppendLine("#line default");
        _mapped = false;
    }

    public void Indent() => _indent++;
    public void Dedent() => _indent--;

    public void Line(string text = "")
    {
        if (text.Length == 0)
            _sb.AppendLine();
        else
            _sb.Append(' ', _indent * 4).AppendLine(text);
    }

    /// <summary>
    /// Emit a captured <c>///</c> doc-comment block (from a declaration's
    /// <see cref="Spek.Compiler.AST.AstNode.DocComment"/>) verbatim, one line
    /// each, at the current indent. No-op when the declaration had no docs.
    /// </summary>
    public void DocComment(IReadOnlyList<string>? lines)
    {
        if (lines is null) return;
        foreach (var line in lines)
            Line(line);
    }

    public void Append(string text) => _sb.Append(text);

    public override string ToString() => _sb.ToString();
}
