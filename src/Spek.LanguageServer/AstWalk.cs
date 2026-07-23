using System.Collections;
using Spek.Compiler.AST;

namespace Spek.LanguageServer;

/// <summary>
/// Reflection-based AST traversal shared by feature handlers. It walks every
/// public property of each <see cref="AstNode"/>, descending into child nodes and
/// collections of nodes, so callers can filter for the node kinds they care about
/// without hand-writing a visitor per node type. Order is a pre-order walk (a node
/// before its children).
/// </summary>
internal static class AstWalk
{
    /// <summary>Enumerate <paramref name="root"/> and every node beneath it.</summary>
    public static IEnumerable<AstNode> EnumerateAll(AstNode root)
    {
        yield return root;
        foreach (var child in EnumerateChildren(root))
            foreach (var grand in EnumerateAll(child))
                yield return grand;
    }

    private static IEnumerable<AstNode> EnumerateChildren(AstNode node)
    {
        foreach (var prop in node.GetType().GetProperties())
        {
            var value = prop.GetValue(node);
            switch (value)
            {
                case AstNode child: yield return child; break;
                case string: break;   // string is IEnumerable<char>, skip
                case IEnumerable enumerable:
                    foreach (var item in enumerable)
                        if (item is AstNode c) yield return c;
                    break;
            }
        }
    }
}
