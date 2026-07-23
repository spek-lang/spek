using System.Text;
using Antlr4.Runtime;
using Spek.Compiler.Grammar;

namespace Spek.Compiler.Format;

/// <summary>
/// Re-prints Spek source with canonical whitespace and indentation
/// while preserving every comment verbatim. Works at the token-stream
/// level (not the AST) so comments stay anchored to the code they
/// originally lived next to — block comments before a declaration
/// remain before that declaration, end-of-line comments stay
/// end-of-line, etc.
///
/// Conservative by design: indentation is re-derived from
/// brace nesting, runs of horizontal whitespace collapse to a single
/// space, but the user's choice of where to break lines is otherwise
/// preserved. Aggressive structural reformatting (e.g. forcing
/// one-statement-per-line, re-laying-out parameter lists) is deferred
/// until a later release.
/// </summary>
public static class SpekFormatter
{
    private const string IndentUnit = "    ";  // 4 spaces

    public static string Format(string source)
    {
        ArgumentNullException.ThrowIfNull(source);

        var input = CharStreams.fromString(source);
        var lexer = new SpekLexer(input);
        var tokens = new CommonTokenStream(lexer);
        tokens.Fill();

        var tokenList = tokens.GetTokens();
        if (tokenList is null || tokenList.Count == 0)
            return string.Empty;

        // Which `<` / `>` tokens are generic type-argument delimiters (they hug,
        // like `Foo<T>`) rather than comparison operators (`a < b`, which stay spaced).
        var typeArgs = FindTypeArgAngles(tokenList);

        var output = new StringBuilder(source.Length);
        int indentLevel = 0;
        bool atLineStart = true;
        // True when the most recent emitted token shares its line with
        // the next non-whitespace token (used to format `} else {` etc.).
        IToken? prev = null;
        int prevIndex = -1;

        for (int i = 0; i < tokenList.Count; i++)
        {
            var token = tokenList[i];
            if (token.Type == TokenConstants.EOF) break;
            var type = token.Type;
            var text = token.Text ?? string.Empty;
            // One-token lookahead: needed to tell null-conditional `?.` / `?[`
            // (tight) apart from the ternary `cond ? a : b` (spaced).
            int nextType = i + 1 < tokenList.Count ? tokenList[i + 1].Type : TokenConstants.EOF;

            // Whitespace tokens drive newline detection but contribute
            // nothing to the output themselves — we synthesize spacing.
            if (type == SpekLexer.LINE_COMMENT || type == SpekLexer.BLOCK_COMMENT)
            {
                EmitComment(output, ref atLineStart, ref indentLevel, prev, token);
                prev = token;
                prevIndex = i;
                continue;
            }

            // The lexer's whitespace rule is `WS -> skip`, so newlines
            // never reach us as tokens. We instead derive vertical
            // whitespace from the source line numbers carried on each
            // real token. If the gap between prev and this token spans
            // ≥1 newline, emit at least one newline.
            if (prev is not null && token.Line > prev.Line)
            {
                int newlines = Math.Min(2, token.Line - prev.Line);
                for (int n = 0; n < newlines; n++) output.Append('\n');
                atLineStart = true;
            }

            // Closing brace de-dents *before* it lands on the line.
            if (type == SpekLexer.RBRACE && indentLevel > 0)
                indentLevel--;

            bool needSpace = NeedSpace(tokenList, typeArgs, prevIndex, i, prev, type, nextType);
            EmitWithSpacing(output, ref atLineStart, indentLevel, needSpace, text);

            // Opening brace bumps the indent for everything that follows.
            if (type == SpekLexer.LBRACE)
                indentLevel++;

            prev = token;
            prevIndex = i;
        }

        // Always end with a single trailing newline.
        if (output.Length == 0 || output[^1] != '\n')
            output.Append('\n');

        return output.ToString();
    }

    private static void EmitComment(
        StringBuilder output, ref bool atLineStart, ref int indentLevel,
        IToken? prev, IToken comment)
    {
        var text = comment.Text ?? string.Empty;

        // If the comment starts on a fresh line, place it at the
        // current indent. Otherwise leave it trailing the prior token
        // with a single space.
        if (prev is null || comment.Line > prev.Line)
        {
            // Newlines between previous output and this comment.
            if (prev is not null)
            {
                int newlines = Math.Min(2, comment.Line - prev.Line);
                for (int i = 0; i < newlines; i++) output.Append('\n');
            }
            output.Append(string.Concat(Enumerable.Repeat(IndentUnit, indentLevel)));
            output.Append(text.TrimEnd('\r', '\n'));
            output.Append('\n');
            atLineStart = true;
        }
        else
        {
            output.Append("  ");
            output.Append(text.TrimEnd('\r', '\n'));
            // A line comment includes its trailing newline in the
            // token text, so treat us as at-line-start afterwards.
            if (comment.Type == SpekLexer.LINE_COMMENT)
            {
                if (output.Length == 0 || output[^1] != '\n') output.Append('\n');
                atLineStart = true;
            }
        }
    }

    private static void EmitWithSpacing(
        StringBuilder output, ref bool atLineStart, int indentLevel,
        bool needSpace, string text)
    {
        if (atLineStart)
        {
            output.Append(string.Concat(Enumerable.Repeat(IndentUnit, indentLevel)));
            atLineStart = false;
        }
        else if (needSpace)
        {
            output.Append(' ');
        }
        output.Append(text);
    }

    // Spacing between the previous real token and the current one. Generic
    // type-argument angles override the default comparison-operator spacing so
    // `Foo<T>(x)` hugs instead of rendering as `Foo < T > (x)`.
    private static bool NeedSpace(
        IList<IToken> tokens, HashSet<int> typeArgs, int prevIndex, int i,
        IToken? prev, int type, int nextType)
    {
        if (prev is null) return false;

        // No space *before* a type-argument angle: `Foo``<`, content`>`, `>``>`.
        if (typeArgs.Contains(i)) return false;

        // No space *after* a type-argument angle in its interior / call positions.
        if (prevIndex >= 0 && typeArgs.Contains(prevIndex))
        {
            int pType = tokens[prevIndex].Type;
            if (pType == SpekLexer.LT) return false;                              // `<``T`
            if (pType == SpekLexer.GT && type == SpekLexer.LPAREN) return false;  // `>``(`  generic call
            // A `>` before an identifier / `{` keeps normal spacing (`List<int> x`).
        }

        return NeedsSpaceBetween(prev.Type, type, nextType);
    }

    // ─── Generic type-argument detection ─────────────────────────────────────
    //
    // Token-level heuristic (the formatter has no AST): a run `IDENT < … >` (or
    // `spawn < … >`) is a type-argument list when the brackets balance and every
    // token inside is type-shaped (identifiers, dots, commas, `?`, `[` `]`, or
    // nested angles), and the closing `>` is followed by a type-context token.
    // Everything else stays a comparison. The one false positive is an
    // unparenthesised chained comparison `a < b > c`, which is vanishingly rare
    // and still compiles (whitespace is insignificant to the parser).
    private static HashSet<int> FindTypeArgAngles(IList<IToken> tokens)
    {
        var result = new HashSet<int>();
        for (int i = 0; i < tokens.Count; i++)
        {
            if (tokens[i].Type != SpekLexer.LT) continue;
            int p = PrevSignificant(tokens, i);
            if (p < 0) continue;
            int pt = tokens[p].Type;
            if (pt != SpekLexer.IDENTIFIER && pt != SpekLexer.SPAWN) continue;

            var span = new List<int>();
            int depth = 0;
            int close = -1;
            bool ok = true;
            for (int j = i; j < tokens.Count; j++)
            {
                int t = tokens[j].Type;
                if (t == SpekLexer.LINE_COMMENT || t == SpekLexer.BLOCK_COMMENT) continue;
                if (t == SpekLexer.LT) { depth++; span.Add(j); }
                else if (t == SpekLexer.GT) { depth--; span.Add(j); if (depth == 0) { close = j; break; } }
                else if (IsTypeContent(t)) { /* keep scanning */ }
                else { ok = false; break; }
            }
            if (!ok || close < 0) continue;

            int f = NextSignificant(tokens, close);
            if (f < 0 || IsTypeFollower(tokens[f].Type))
                foreach (var idx in span) result.Add(idx);
        }
        return result;
    }

    private static bool IsTypeContent(int t) =>
        t == SpekLexer.IDENTIFIER || t == SpekLexer.DOT || t == SpekLexer.COMMA
        || t == SpekLexer.QUESTION || t == SpekLexer.LBRACKET || t == SpekLexer.RBRACKET;

    private static bool IsTypeFollower(int t) =>
        t == SpekLexer.LPAREN || t == SpekLexer.DOT || t == SpekLexer.RPAREN
        || t == SpekLexer.COMMA || t == SpekLexer.GT || t == SpekLexer.LBRACKET
        || t == SpekLexer.RBRACKET || t == SpekLexer.LBRACE || t == SpekLexer.SEMICOLON
        || t == SpekLexer.IDENTIFIER;

    private static int PrevSignificant(IList<IToken> tokens, int i)
    {
        for (int k = i - 1; k >= 0; k--)
        {
            int t = tokens[k].Type;
            if (t != SpekLexer.LINE_COMMENT && t != SpekLexer.BLOCK_COMMENT) return k;
        }
        return -1;
    }

    private static int NextSignificant(IList<IToken> tokens, int i)
    {
        for (int k = i + 1; k < tokens.Count; k++)
        {
            int t = tokens[k].Type;
            if (t == TokenConstants.EOF) return -1;
            if (t != SpekLexer.LINE_COMMENT && t != SpekLexer.BLOCK_COMMENT) return k;
        }
        return -1;
    }

    /// <summary>
    /// Conservative spacing rules: insert a space between two tokens
    /// on the same line unless one side is one of the "tight"
    /// punctuators that should hug the neighbour.
    /// </summary>
    private static bool NeedsSpaceBetween(int prev, int next, int nextNext)
    {
        // Null-conditional access `?.` / `?[`: the QUESTION hugs its
        // receiver. Lookahead at nextNext disambiguates from the ternary
        // `cond ? a : b`, whose QUESTION keeps a space on both sides.
        if (next == SpekLexer.QUESTION
            && (nextNext == SpekLexer.DOT || nextNext == SpekLexer.LBRACKET))
            return false;

        // Closing punctuation hugs whatever came before.
        if (next == SpekLexer.SEMICOLON || next == SpekLexer.COMMA
            || next == SpekLexer.RPAREN  || next == SpekLexer.RBRACKET
            || next == SpekLexer.DOT)
            return false;

        // Opening punctuation hugs whatever comes after.
        if (prev == SpekLexer.LPAREN || prev == SpekLexer.LBRACKET
            || prev == SpekLexer.DOT)
            return false;

        // `name(` and `name[` and `name<` — call/index/generic-args.
        if (next == SpekLexer.LPAREN
            && (prev == SpekLexer.IDENTIFIER || prev == SpekLexer.RPAREN
                || prev == SpekLexer.RBRACKET))
            return false;
        // `?[` — the index bracket hugs a null-conditional QUESTION too
        // (a `?` directly before `[` is always null-conditional; no
        // expression form puts a ternary `?` against `[`).
        if (next == SpekLexer.LBRACKET
            && (prev == SpekLexer.IDENTIFIER || prev == SpekLexer.QUESTION))
            return false;

        return true;
    }
}
