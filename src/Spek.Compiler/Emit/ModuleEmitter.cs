using Spek.Compiler.AST;
using Spek.Compiler.Semantic;

namespace Spek.Compiler.Emit;

/// <summary>
/// Emits a Spek <c>module</c> declaration as a C# static class
/// with each method emitted static.
///
/// Mapping summary:
///   module Foo { … }                  → public static class Foo { … }
///   internal module Foo { … }         → internal static class Foo { … }
///   public bool IsX(string s) { … }   → public static bool IsX(string s) { … }
///   void LogIt(string s) { … }        → ??? static void LogIt(string s) { … }
///   module Bar { … } (nested)         → public static class Bar { … } (nested)
///
/// Modules are stateless: there are no instance fields, no <c>self</c>,
/// no <c>sender</c>. The emitter therefore creates an
/// <see cref="ExpressionEmitter"/> with an empty actor-field set so bare
/// identifiers in function bodies emit as-is (parameters and locals
/// resolve through normal C# scoping in the generated code).
/// </summary>
public sealed class ModuleEmitter
{
    private readonly CSharpWriter _w;
    private readonly SymbolTable? _symbols;

    public ModuleEmitter(CSharpWriter writer, SymbolTable? symbols = null)
    {
        _w       = writer;
        _symbols = symbols;
    }

    public void Emit(ModuleDecl module)
    {
        var vis = VisibilityKeyword(module.Visibility);

        _w.Line($"{vis} static class {module.Name}");
        _w.Line("{");
        _w.Indent();

        // Methods and nested modules share the static-class body; we
        // emit methods first then nested modules. The two namespaces
        // are independent — semantic analysis already verified there's
        // no same-kind duplicate within either pile.
        var first = true;
        foreach (var m in module.Methods)
        {
            if (!first) _w.Line();
            EmitMethod(m);
            first = false;
        }

        foreach (var nested in module.NestedModules)
        {
            if (!first) _w.Line();
            Emit(nested);
            first = false;
        }

        _w.Dedent();
        _w.Line("}");
        _w.Line();
    }

    // A module method emits as a C# `static` method — the only thing that
    // distinguishes it from an actor/class method (the dev never writes `static`).
    private void EmitMethod(MethodDecl m)
    {
        var vis = VisibilityKeyword(m.Visibility);
        var ret = m.ReturnType?.ToString() ?? "void";
        var tps = m.TypeParameters.Count > 0        // Generic method
            ? $"<{string.Join(", ", m.TypeParameters.Select(p => p.Name))}>"
            : "";

        var parms = string.Join(", ",
            m.Parameters.Select(FormatParam));

        _w.Line($"{vis} static {ret} {m.Name}{tps}({parms}){ExpressionEmitter.FormatWhereClauses(m.WhereClauses)}");

        // Empty actor-field set: module methods are stateless, so there's no
        // `_field` rewrite to do. Pass the symbol table through so cross-module
        // calls and other resolutions still work as the language grows.
        var exprEmitter = new ExpressionEmitter(new HashSet<string>(), _symbols);
        var stmtEmitter = new StatementEmitter(_w, exprEmitter);
        stmtEmitter.EmitBlock(m.Body);
    }

    // Formats a parameter for the emitted C# signature, prefixing
    // the `in` / `ref` / `out` modifier when present.
    private static string FormatParam(Param p)
    {
        var mod = ExpressionEmitter.ParamModifierKeyword(p.Modifier);
        return mod.Length == 0
            ? $"{p.Type} {p.Name}"
            : $"{mod} {p.Type} {p.Name}";
    }

    private static string VisibilityKeyword(Visibility v) => v switch
    {
        Visibility.Public    => "public",
        Visibility.Internal  => "internal",
        Visibility.Protected => "protected",
        Visibility.Private   => "private",
        _                    => "public",
    };
}
