using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using Spek.LanguageServer;
using Xunit;
using LspRange = OmniSharp.Extensions.LanguageServer.Protocol.Models.Range;

namespace Spek.Tests.LanguageServer;

/// <summary>
/// Shared harness for the per-handler LSP tests. Every feature handler in
/// <c>Spek.LanguageServer</c> is constructible with just a
/// <see cref="DocumentCache"/> (the same object the server's DI container
/// hands them), so the tests drive each handler in-process: seed the cache
/// with a document, build the handler, call <c>Handle</c> with real LSP
/// request params, and assert on the response — no server, no transport.
/// </summary>
internal static class LspTestSupport
{
    /// <summary>Representative document shared by the definition and hover
    /// tests: two messages, an actor with a field and two behaviors (one
    /// multi-statement handler using new/become, one with single-line
    /// handlers), and a second actor that spawns the first.</summary>
    public const string EchoDoc = """
        message Ping(int count);
        message Pong(int count);

        actor Echo
        {
            int hits = 0;

            behavior Idle
            {
                on Ping p =>
                {
                    hits = hits + 1;
                    sender.Tell(new Pong(p.count));
                    become Busy;
                }
            }

            behavior Busy
            {
                on Ping => { become Idle; }
                on Pong => { }
            } // end-busy
        } // end-echo

        actor Spawner
        {
            behavior Boot
            {
                on Ping => { var e = spawn<Echo>(); }
            }
        }
        """;

    /// <summary>Document shared by the references and rename tests: Ping is
    /// declared once and referenced twice (a no-bind pattern in Echo, a
    /// named-bind pattern in Caller).</summary>
    public const string PingPongDoc = """
        message Ping();
        message Pong();

        actor Echo
        {
            behavior Idle
            {
                on Ping => { sender.Tell(new Pong()); }
            }
        }

        actor Caller
        {
            behavior Active
            {
                on Ping p => { }
            }
        }
        """;

    public static string Normalize(string source) => source.Replace("\r\n", "\n");

    /// <summary>Parses <paramref name="source"/> into a fresh cache under
    /// <paramref name="uriString"/> — the same path the server's didOpen
    /// takes before any feature request.</summary>
    public static (DocumentCache Cache, DocumentUri Uri) Open(string uriString, string source)
    {
        var cache = new DocumentCache();
        var uri   = DocumentUri.From(uriString);
        cache.Update(uri, Normalize(source));
        return (cache, uri);
    }

    /// <summary>0-based line index of the first line containing
    /// <paramref name="lineContains"/>.</summary>
    public static int LineOf(string source, string lineContains)
    {
        var lines = Normalize(source).Split('\n');
        var idx = Array.FindIndex(lines, l => l.Contains(lineContains, StringComparison.Ordinal));
        Assert.True(idx >= 0, $"no line contains '{lineContains}'");
        return idx;
    }

    /// <summary>An LSP (0-based) position INSIDE <paramref name="token"/> —
    /// one character past its start — on the first line containing
    /// <paramref name="lineContains"/>. <paramref name="token"/> may carry
    /// trailing context (e.g. <c>"hits + 1"</c>) to disambiguate which
    /// occurrence on the line the cursor lands in.</summary>
    public static Position CursorOn(string source, string lineContains, string token)
    {
        var start = CursorAtStartOf(source, lineContains, token);
        return new Position(start.Line, start.Character + 1);
    }

    /// <summary>An LSP (0-based) position on the FIRST character of
    /// <paramref name="token"/>. Single-token AST spans record only the
    /// token's start column (see <c>AstBuilder.Span</c>), so features that
    /// resolve bare identifiers (e.g. field hover) only hit when the cursor
    /// sits exactly on the first character.</summary>
    public static Position CursorAtStartOf(string source, string lineContains, string token)
    {
        var lineIdx = LineOf(source, lineContains);
        var line = Normalize(source).Split('\n')[lineIdx];
        var col = line.IndexOf(token, StringComparison.Ordinal);
        Assert.True(col >= 0, $"'{token}' not found on line '{line}'");
        return new Position(lineIdx, col);
    }

    /// <summary>The source text covered by a single-line LSP range.</summary>
    public static string TextAt(string source, LspRange range)
    {
        Assert.Equal(range.Start.Line, range.End.Line);
        var line = Normalize(source).Split('\n')[range.Start.Line];
        return line.Substring(range.Start.Character, range.End.Character - range.Start.Character);
    }
}
