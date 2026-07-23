using Spek.Compiler.AST;
using Spek.Compiler.Semantic;

namespace Spek.Compiler.Emit;

/// <summary>
/// Emits a Spek <c>class</c> declaration as a plain C# instance class:
/// fields, an optional <c>init(params)</c> constructor, and methods.
///
/// Mapping summary:
///   class Builder { … }               → internal sealed class Builder { … }
///   public class Account { … }        → public sealed class Account { … }
///   string buffer = "";               → private string buffer = "";   (field)
///   public void Append(string s){…}   → public void Append(string s){…}
///   init(string id) { … }             → public Account(string id) { … }
///
/// Unlike an actor, a class has no generated dispatch infrastructure
/// (<c>_behavior</c>, <c>_selfRef</c>, …), so fields keep their plain names —
/// a bare field reference in a method body resolves to the instance field by
/// normal C# scoping, and a <c>public</c> field stays reachable as
/// <c>obj.field</c>. <c>self</c> emits as <c>this</c> (the expression emitter is
/// created with <c>selfIsThis: true</c> and an empty actor-field set).
/// </summary>
public sealed class ClassEmitter
{
    private readonly CSharpWriter _w;
    private readonly SymbolTable? _symbols;

    public ClassEmitter(CSharpWriter writer, SymbolTable? symbols = null)
    {
        _w       = writer;
        _symbols = symbols;
    }

    public void Emit(ClassDecl cls)
    {
        // Top-level types can't be `private`/`protected` in C#; anything that
        // isn't explicitly public collapses to internal (same as actors).
        var vis = cls.Visibility == Visibility.Public ? "public" : "internal";
        var typeParams = cls.TypeParameters.Count > 0
            ? $"<{string.Join(", ", cls.TypeParameters.Select(p => p.Name))}>"
            : "";

        // An abstract class is extendable (never sealed); a concrete class is
        // sealed (only abstract classes are valid bases, so a concrete class is
        // always a leaf). `abstract` also lets `abstract` members exist (C# rule).
        var kind = cls.IsAbstract ? "abstract" : "sealed";

        // Order the base list base-class-first (C# requires it), then the
        // implemented interfaces — regardless of the order the user wrote them.
        var baseClass  = cls.Bases.FirstOrDefault(b => _symbols?.ResolveClass(b.Simple) is not null);
        var interfaces = cls.Bases.Where(b => !ReferenceEquals(b, baseClass)).Select(b => b.ToString());
        var ordered    = (baseClass is not null ? new[] { baseClass.ToString() } : [])
            .Concat(interfaces).ToList();
        var bases = ordered.Count > 0 ? " : " + string.Join(", ", ordered) : "";

        _w.Line($"{vis} {kind} class {cls.Name}{typeParams}{bases}{ExpressionEmitter.FormatWhereClauses(cls.WhereClauses)}");
        _w.Line("{");
        _w.Indent();

        // No actor-field rewrite (classes have no dispatch infrastructure);
        // `self` → `this`.
        var exprEmitter = new ExpressionEmitter(new HashSet<string>(), _symbols, selfIsThis: true);
        var stmtEmitter = new StatementEmitter(_w, exprEmitter);

        // ── fields ──
        foreach (var f in cls.Fields)
        {
            var fvis = MemberVisibility(f.Visibility);
            var init = f.Initializer != null
                ? $" = {exprEmitter.Emit(f.Initializer)}"
                : "";
            _w.Line($"{fvis} {f.Type} {f.Name}{init};");
        }
        if (cls.Fields.Count > 0) _w.Line();

        // ── constructor (from the optional init block) ──
        if (cls.Init is { } init2)
        {
            var parms = string.Join(", ", init2.Parameters.Select(FormatParam));
            // `: base(args)` chains to an abstract base's constructor.
            var baseCall = init2.BaseArgs is { } ba
                ? $" : base({string.Join(", ", ba.Select(exprEmitter.Emit))})"
                : "";
            _w.Line($"public {cls.Name}({parms}){baseCall}");
            _w.Line("{");
            _w.Indent();
            foreach (var s in init2.Body.Statements) stmtEmitter.EmitStatement(s);
            _w.Dedent();
            _w.Line("}");
            _w.Line();
        }

        // ── methods (instance methods) ──
        var first = true;
        foreach (var m in cls.Methods)
        {
            if (!first) _w.Line();
            var mvis = MemberVisibility(m.Visibility);
            var ret  = m.ReturnType?.ToString() ?? "void";
            var mtp  = m.TypeParameters.Count > 0       // Generic method
                ? $"<{string.Join(", ", m.TypeParameters.Select(p => p.Name))}>"
                : "";
            var parms = string.Join(", ", m.Parameters.Select(FormatParam));

            if (m.IsAbstract)
            {
                // `abstract T M(params);` — no body. The subclass implements it.
                _w.Line($"{mvis} abstract {ret} {m.Name}{mtp}({parms}){ExpressionEmitter.FormatWhereClauses(m.WhereClauses)};");
            }
            else
            {
                // Infer `override` when this method implements an abstract method
                // inherited from an ancestor abstract class — the user never
                // writes `override` (there is no `virtual` in Spek).
                var overrideKw = OverridesBaseAbstract(cls, m) ? "override " : "";
                _w.Line($"{mvis} {overrideKw}{ret} {m.Name}{mtp}({parms}){ExpressionEmitter.FormatWhereClauses(m.WhereClauses)}");
                stmtEmitter.EmitBlock(m.Body);
            }
            first = false;
        }

        // ── properties ──
        if (cls.Properties.Count > 0 && (cls.Methods.Count > 0 || cls.Init is not null || cls.Fields.Count > 0))
            _w.Line();
        foreach (var p in cls.Properties)
        {
            var pvis = MemberVisibility(p.Visibility);
            var accessors = string.Join(" ", p.Accessors.Select(a =>
            {
                var av = a.Visibility is { } v ? MemberVisibility(v) + " " : "";
                return a.Body != null
                    ? $"{av}{a.Kind} => {exprEmitter.Emit(a.Body)};"
                    : $"{av}{a.Kind};";
            }));
            var initTail = p.Initializer != null ? $" = {exprEmitter.Emit(p.Initializer)};" : "";
            _w.Line($"{pvis} {p.Type} {p.Name} {{ {accessors} }}{initTail}");
        }

        _w.Dedent();
        _w.Line("}");
        _w.Line();
    }

    /// <summary>
    /// True when <paramref name="method"/> implements an <c>abstract</c> method
    /// declared somewhere up <paramref name="cls"/>'s base-class chain — matched
    /// by name and parameter count. Used to infer the C# <c>override</c> keyword
    /// so Spek source never spells it. Walks only the base *class* link (the
    /// first base that resolves to a class); interfaces don't carry abstracts.
    /// </summary>
    private bool OverridesBaseAbstract(ClassDecl cls, MethodDecl method)
    {
        if (_symbols is null) return false;

        var seen = new HashSet<string>(StringComparer.Ordinal);
        var current = cls;
        while (current is not null)
        {
            var baseName = current.Bases.FirstOrDefault(b => _symbols.ResolveClass(b.Simple) is not null);
            if (baseName is null) return false;
            if (!seen.Add(baseName.Simple)) return false;   // cycle guard

            var baseClass = _symbols.ResolveClass(baseName.Simple);
            if (baseClass is null) return false;

            if (baseClass.Methods.Any(bm =>
                    bm.IsAbstract &&
                    bm.Name == method.Name &&
                    bm.Parameters.Count == method.Parameters.Count))
                return true;

            current = baseClass;
        }
        return false;
    }

    private static string FormatParam(Param p)
    {
        var mod = ExpressionEmitter.ParamModifierKeyword(p.Modifier);
        return mod.Length == 0 ? $"{p.Type} {p.Name}" : $"{mod} {p.Type} {p.Name}";
    }

    private static string MemberVisibility(Visibility v) => v switch
    {
        Visibility.Public    => "public",
        Visibility.Internal  => "internal",
        Visibility.Protected => "protected",
        Visibility.Private   => "private",
        _                    => "private",
    };
}
