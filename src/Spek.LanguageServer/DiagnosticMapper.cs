using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using SpekDiag = Spek.Compiler.Semantic.Diagnostic;
using SpekSeverity = Spek.Compiler.Semantic.DiagnosticSeverity;

namespace Spek.LanguageServer;

/// <summary>
/// Converts <see cref="SpekDiag"/> instances produced by the Spek compiler
/// into LSP <see cref="Diagnostic"/> values for transmission to the client.
/// Exposed publicly so tests and future adapters can exercise the mapping
/// without spinning up the full LSP server.
/// </summary>
public static class DiagnosticMapper
{
    public static Diagnostic ToLsp(SpekDiag d)
    {
        // Spek lines/columns are 1-based; LSP is 0-based.
        var startLine = Math.Max(0, d.Line - 1);
        var startCol  = Math.Max(0, d.Column - 1);

        // Prefer the real source span for the squiggle range. A single-token
        // span has Start == Stop (zero width), so fall back to a one-character
        // range there rather than emitting an empty squiggle.
        Position end;
        if (d.Span is { } s && (s.EndLine > s.StartLine || s.EndColumn > s.StartColumn))
            end = new Position(Math.Max(0, s.EndLine - 1), Math.Max(0, s.EndColumn - 1));
        else
            end = new Position(startLine, startCol + 1);

        var range = new OmniSharp.Extensions.LanguageServer.Protocol.Models.Range(
            new Position(startLine, startCol), end);

        return new Diagnostic
        {
            Code     = d.Code,
            Message  = d.Message,
            Range    = range,
            Severity = SeverityFor(d),
            Source   = "spek",
        };
    }

    /// <summary>
    /// Maps the Spek diagnostic's severity to the LSP one. Warnings (CE0101,
    /// CE0107, CE0109, CE0110, …) surface as warning squiggles;
    /// everything else — including ANTLR syntax errors (CE0001) — is an error.
    /// </summary>
    private static DiagnosticSeverity SeverityFor(SpekDiag d) => d.Severity switch
    {
        SpekSeverity.Warning     => DiagnosticSeverity.Warning,
        SpekSeverity.Information  => DiagnosticSeverity.Information,
        SpekSeverity.Hint         => DiagnosticSeverity.Hint,
        _                         => DiagnosticSeverity.Error,
    };
}
