using System.Collections;
using Spek.Compiler.AST;

namespace Spek.LanguageServer;

/// <summary>
/// Given a parsed <see cref="SpekFile"/> and a source position (1-based
/// line and column, matching Spek AST span conventions), finds the chain
/// of AST nodes whose <see cref="SourceSpan"/> contains that position,
/// innermost first.
///
/// LSP handlers use this to answer "what did the user click on?" and then
/// walk up the chain to find what that token refers to — for example, an
/// inner <see cref="QualifiedName"/> plus an outer <see cref="NewExpr"/>
/// tells us the user clicked a message name inside a <c>new Foo(...)</c>
/// expression.
/// </summary>
public static class PositionResolver
{
    /// <summary>
    /// Returns the chain of AST nodes whose span contains (line, column),
    /// from the outermost down to the innermost. Empty if the position is
    /// outside every declaration.
    /// </summary>
    public static IReadOnlyList<AstNode> FindChain(SpekFile file, int line, int column)
    {
        var chain = new List<AstNode>();
        Visit(file, line, column, chain);
        return chain;
    }

    /// <summary>
    /// Convenience wrapper — returns the innermost node whose span contains
    /// (line, column), or <c>null</c> if the position is outside every
    /// declaration.
    /// </summary>
    public static AstNode? Find(SpekFile file, int line, int column)
    {
        var chain = FindChain(file, line, column);
        return chain.Count == 0 ? null : chain[^1];
    }

    private static void Visit(AstNode node, int line, int column, List<AstNode> chain)
    {
        if (!Contains(node.Span, line, column)) return;
        chain.Add(node);

        foreach (var child in EnumerateChildren(node))
            Visit(child, line, column, chain);
    }

    private static bool Contains(SourceSpan span, int line, int column)
    {
        if (line < span.StartLine || line > span.EndLine) return false;
        if (line == span.StartLine && column < span.StartColumn) return false;
        if (line == span.EndLine   && column > span.EndColumn)   return false;
        return true;
    }

    /// <summary>
    /// Generic enumeration of child AST nodes via reflection. Skips strings
    /// (which are <see cref="IEnumerable"/> but not collections of AstNode).
    /// </summary>
    private static IEnumerable<AstNode> EnumerateChildren(AstNode node)
    {
        foreach (var prop in node.GetType().GetProperties())
        {
            var value = prop.GetValue(node);
            switch (value)
            {
                case AstNode child:
                    yield return child;
                    break;

                case string:
                    break; // string is IEnumerable<char>, skip.

                case IEnumerable enumerable:
                    foreach (var item in enumerable)
                        if (item is AstNode c) yield return c;
                    break;
            }
        }
    }
}
