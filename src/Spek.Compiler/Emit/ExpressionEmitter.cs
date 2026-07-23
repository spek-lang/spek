using Spek.Compiler.AST;
using Spek.Compiler.Semantic;

namespace Spek.Compiler.Emit;

/// <summary>
/// Translates Spek AST expressions to C# expression strings.
/// Needs the set of actor field names so bare identifiers that are
/// fields get the generated <c>_</c> prefix.
/// Optionally holds a <see cref="SymbolTable"/> so <see cref="AskExpr"/>
/// can infer the reply type from handler returns (Option D).
/// </summary>
public sealed class ExpressionEmitter
{
    private readonly IReadOnlySet<string> _fields;
    private readonly SymbolTable? _symbols;
    // When emitting a `shared X { ... init { ... } }`
    // body, `self` refers to the region instance (emit as `this`),
    // not an actor's `_selfRef`. Default false preserves actor-handler
    // semantics everywhere else.
    private readonly bool _selfIsThis;
    // True when emitting inside an actor body (handlers, init, term,
    // methods), where `_selfRef` exists. Gates the implicit-sender rewrite
    // on Tell: `x.Tell(msg)` → `x.Tell(msg, _selfRef)` (Akka convention),
    // so the receiver's `sender.Tell(reply)` routes back to this actor.
    // Programs, modules, and classes have no _selfRef and never rewrite.
    private readonly bool _inActor;

    public ExpressionEmitter(IReadOnlySet<string> actorFields, SymbolTable? symbols = null,
        bool selfIsThis = false, bool inActor = false)
    {
        _fields = actorFields;
        _symbols = symbols;
        _selfIsThis = selfIsThis;
        _inActor = inActor;
    }

    // Names of parameters / locals currently in scope. A name here shadows an actor
    // field of the same name, so it must NOT get the `_` field prefix. Set per body
    // (params/handler binding) and extended as the StatementEmitter emits locals.
    private readonly HashSet<string> _shadowed = new();

    /// <summary>Replaces the shadowed-name set (the params/binding for a body).</summary>
    public void SetShadowed(IEnumerable<string> names)
    {
        _shadowed.Clear();
        foreach (var n in names) _shadowed.Add(n);
    }

    /// <summary>Adds a local name as the StatementEmitter declares it.</summary>
    public void AddShadowed(string name) => _shadowed.Add(name);

    public string Emit(Expr expr) => expr switch
    {
        AskExpr ask                 => EmitAsk(ask),
        AssignExpr assign           => EmitAssign(assign),
        ConditionalExpr cond        => $"({Emit(cond.Condition)} ? {Emit(cond.Then)} : {Emit(cond.Else)})",
        BinaryExpr bin              => $"({Emit(bin.Left)} {Op(bin.Op)} {Emit(bin.Right)})",
        UnaryExpr u                 => $"({UnaryOpStr(u.Op)}{Emit(u.Operand)})",
        TypeOpExpr t                => EmitTypeOp(t),
        MethodCallExpr mc           => EmitMethodCall(mc),
        // `self.TraceContext` / `self.Metrics` / `self.Log` / `self.System`
        // resolve to actor-instance properties on `ActorBase`, not to
        // members of the `_selfRef` ActorRef. Special-case here so the
        // shape mirrors how `self.Tell` is rewritten further down.
        MemberAccessExpr ma when ma.Target is SelfExpr && IsSelfAccessor(ma.Member)
                                    => $"this.{SelfAccessorProperty(ma.Member)}",
        InvocationExpr inv          => $"{inv.Callee}({string.Join(", ", inv.Args.Select(Emit))})",
        MemberAccessExpr ma         => $"{Emit(ma.Target)}{(ma.NullConditional ? "?." : ".")}{ma.Member}",
        IndexExpr ix                => $"{Emit(ix.Target)}{(ix.NullConditional ? "?[" : "[")}{Emit(ix.Index)}]",
        SwitchExpr sw               => EmitSwitch(sw),
        NewExpr n                   => EmitNew(n),
        SpawnExpr s                 => EmitSpawn(s),
        SelfExpr                    => _selfIsThis ? "this" : "_selfRef",
        SenderExpr                  => "_currentSender",
        LambdaExpr l                => EmitLambda(l),
        NameExpr n                  => EmitName(n),
        ParenExpr p                 => $"({Emit(p.Inner)})",
        IntLiteralExpr i            => i.Raw ?? i.Value.ToString(),
        DecimalLiteralExpr d        => d.Raw ?? (d.HasSuffix ? $"{d.Value}m" : d.Value.ToString()),
        StringLiteralExpr s         => s.Raw ?? $"\"{Escape(s.Value)}\"",
        CharLiteralExpr c           => c.Raw,
        InterpolatedStringExpr s    => EmitInterpolated(s),
        BoolLiteralExpr b           => b.Value ? "true" : "false",
        NullLiteralExpr             => "null",
        // Call-site `in` / `ref` / `out` argument: restate the C#
        // keyword and emit the inner expression. C# enforces that the
        // modifier matches the callee's parameter, so we lean on the
        // emitted code's own compile check rather than a Spek-side CE.
        RefArgExpr r                => $"{ParamModifierKeyword(r.Modifier)} {Emit(r.Inner)}",
        NamedArgExpr na             => $"{na.Name}: {Emit(na.Value)}",
        ArrayExpr arr               => EmitArray(arr),
        TupleExpr tup               => $"({string.Join(", ", tup.Elements.Select(Emit))})",
        ThrowExpr th                => $"throw {Emit(th.Value)}",
        DefaultExpr d               => d.Type is null ? "default" : $"default({d.Type})",
        // Inline out-var declaration: `out var x`.
        OutVarExpr o                => $"out var {o.Name}",
        _                           => throw new NotSupportedException(
                                           $"Cannot emit expression type {expr.GetType().Name}")
    };

    internal static string ParamModifierKeyword(ParamModifier m) => m switch
    {
        ParamModifier.In  => "in",
        ParamModifier.Ref => "ref",
        ParamModifier.Out => "out",
        _                 => "",
    };

    // Renders generic `where T : C1, C2` constraint clauses (with a
    // leading space) for a declaration's C# signature, or "" when there are
    // none. Constraints are already verbatim C# fragments.
    internal static string FormatWhereClauses(IReadOnlyList<WhereClause> clauses) =>
        clauses.Count == 0
            ? ""
            : " " + string.Join(" ",
                clauses.Select(w => $"where {w.TypeParam} : {string.Join(", ", w.Constraints)}"));

    // ─── Helpers ─────────────────────────────────────────────────────────────

    // Reconstruct an interpolated string, emitting each hole's expression
    // through this same emitter (so fields become `_field`, `self`/`sender`
    // are rewritten, etc.); literal text and the `:format`/`,align` suffix
    // pass through verbatim.
    private string EmitInterpolated(InterpolatedStringExpr s)
    {
        var sb = new System.Text.StringBuilder(s.Prefix);
        foreach (var part in s.Parts)
        {
            switch (part)
            {
                case InterpolationText t: sb.Append(t.Text); break;
                case InterpolationHole h: sb.Append('{').Append(Emit(h.Expr)).Append(h.Suffix).Append('}'); break;
            }
        }
        return sb.Append('"').ToString();
    }

    private string EmitName(NameExpr n)
    {
        // Single-part names that match a field get the _ prefix — unless an in-scope
        // parameter or local shadows the field, in which case the name binds to the
        // param/local (C# scoping), not the field.
        if (n.Name.Parts.Count == 1 && _fields.Contains(n.Name.Simple) && !_shadowed.Contains(n.Name.Simple))
            return $"_{n.Name.Simple}";
        // Multi-part names whose first segment is a field need the _
        // prefix on that segment (e.g., `watcher.Changed` → `_watcher.Changed`)
        // so member access on a Spek field reaches the underlying C# field — again,
        // unless a param/local shadows that first segment.
        if (n.Name.Parts.Count > 1 && _fields.Contains(n.Name.Parts[0]) && !_shadowed.Contains(n.Name.Parts[0]))
            return $"_{n.Name.Parts[0]}.{string.Join(".", n.Name.Parts.Skip(1))}";
        return n.Name.ToString();
    }

    private string EmitAssign(AssignExpr a)
    {
        var op = a.Op switch
        {
            AssignOp.Assign      => "=",
            AssignOp.PlusAssign  => "+=",
            AssignOp.MinusAssign => "-=",
            AssignOp.StarAssign  => "*=",
            AssignOp.SlashAssign => "/=",
            AssignOp.ModAssign      => "%=",
            AssignOp.AndAssign      => "&=",
            AssignOp.OrAssign       => "|=",
            AssignOp.XorAssign      => "^=",
            AssignOp.CoalesceAssign => "??=",
            _                    => "="
        };
        return $"{Emit(a.Left)} {op} {Emit(a.Right)}";
    }

    private string EmitSwitch(SwitchExpr s)
    {
        var subject = Emit(s.Subject);
        var arms = string.Join(",\n    ",
            s.Arms.Select(EmitSwitchArm));
        return $"({subject} switch {{\n    {arms}\n}})";
    }

    private string EmitSwitchArm(SwitchArm arm)
    {
        var pattern = EmitPattern(arm.Pattern);
        var guard   = arm.When is null ? "" : $" when {Emit(arm.When)}";
        var result  = Emit(arm.Result);
        return $"{pattern}{guard} => {result}";
    }

    // internal so StatementEmitter can render switch-statement case labels.
    internal string EmitPattern(SwitchPattern pattern) => pattern switch
    {
        DiscardPattern         => "_",
        // A binding-less type pattern that's really a bare enum variant
        // (`Active =>`) is parsed as a TypePattern; qualify it to `Status.Active`
        // (a C# constant pattern). Real type-tests and non-enum names pass through.
        TypePattern tp         => tp.Binding is null
                                    ? (TryQualifyEnumVariant(tp.Type.ToString()) ?? $"{tp.Type}")
                                    : $"{tp.Type} {tp.Binding}",
        ConstPattern cp        => EmitConstPattern(cp),
        RelationalPattern rp   => $"{RelationalOpToCSharp(rp.Op)} {Emit(rp.Value)}",
        PropertyPattern pp     => EmitPropertyPattern(pp),
        NotPattern n           => $"not {ParenIfBinary(n.Inner)}",
        AndPattern a           => $"{ParenIfOr(a.Left)} and {ParenIfOr(a.Right)}",
        OrPattern o            => $"{EmitPattern(o.Left)} or {EmitPattern(o.Right)}",
        _                      => throw new InvalidOperationException(
                                    $"Unknown switch pattern: {pattern.GetType().Name}")
    };

    /// <summary>
    /// Spek allows a bare enum-variant name as a switch pattern (<c>Active =></c>),
    /// but C# requires the qualified member (<c>Status.Active</c>) in a constant
    /// pattern. Lower a bare name that resolves to a unique enum variant to its
    /// qualified form; already-qualified names and non-enum constants pass through.
    /// </summary>
    private string EmitConstPattern(ConstPattern cp)
    {
        if (cp.Value is NameExpr { Name.Parts: { Count: 1 } parts }
            && TryQualifyEnumVariant(parts[0]) is { } qualified)
            return qualified;
        return Emit(cp.Value);
    }

    /// <summary>Returns <c>"{Enum}.{name}"</c> when <paramref name="bareName"/> is a
    /// member of exactly one declared enum; null if it's no enum's member or is
    /// ambiguous across enums (where the bare form can't be safely qualified).</summary>
    private string? TryQualifyEnumVariant(string bareName)
    {
        if (_symbols is null) return null;
        string? match = null;
        foreach (var e in _symbols.Enums)
        {
            if (!e.Members.Any(m => m.Name == bareName)) continue;
            if (match is not null) return null;   // ambiguous — leave it bare
            match = e.Name;
        }
        return match is null ? null : $"{match}.{bareName}";
    }

    /// <summary>Wrap <paramref name="p"/> in parens if it's a binary
    /// pattern (and/or). Required for `not (a or b)` and `not (a and
    /// b)` — without parens C# would parse as `(not a) or b`.</summary>
    private string ParenIfBinary(SwitchPattern p) =>
        p is AndPattern or OrPattern ? $"({EmitPattern(p)})" : EmitPattern(p);

    /// <summary>Wrap <paramref name="p"/> in parens if it's an `or`
    /// pattern. Required for `(a or b) and c` to keep its grouping —
    /// without parens C# would parse as `a or (b and c)`.</summary>
    private string ParenIfOr(SwitchPattern p) =>
        p is OrPattern ? $"({EmitPattern(p)})" : EmitPattern(p);

    private string EmitPropertyPattern(PropertyPattern pp)
    {
        if (pp.Properties.Count == 0) return "{ }";
        var subs = string.Join(", ",
            pp.Properties.Select(s =>
                $"{string.Join(".", s.Path.Parts)}: {EmitPattern(s.Inner)}"));
        return $"{{ {subs} }}";
    }

    private static string RelationalOpToCSharp(RelationalPatternOp op) => op switch
    {
        RelationalPatternOp.Lt  => "<",
        RelationalPatternOp.Lte => "<=",
        RelationalPatternOp.Gt  => ">",
        RelationalPatternOp.Gte => ">=",
        RelationalPatternOp.Eq  => "==",
        RelationalPatternOp.Neq => "!=",
        _ => throw new InvalidOperationException($"Unknown relational op: {op}")
    };

    /// <summary>
    /// Recognise the special <c>self.X</c> accessors that
    /// resolve to <see cref="ActorBase"/> properties instead of
    /// members on the <c>ActorRef</c>. Centralised so adding a new
    /// accessor (e.g. <c>self.Tracer</c>) is a one-line change here.
    /// Must stay in sync with the CE0012 carve-out in
    /// <c>SemanticAnalyzer.IsSelfAccessor</c>.
    /// </summary>
    private static bool IsSelfAccessor(string memberName) =>
        memberName is "TraceContext" or "Metrics" or "Log" or "System";

    /// <summary>
    /// Maps a Spek <c>self.X</c> accessor to its <c>ActorBase</c> property
    /// name. Identity for most; <c>self.System</c> maps to <c>SpekSystem</c>
    /// so the emitted member never shadows the <c>System</c> namespace inside
    /// generated actor code.
    /// </summary>
    private static string SelfAccessorProperty(string member) =>
        member == "System" ? "SpekSystem" : member;

    private string EmitMethodCall(MethodCallExpr mc)
    {
        var target   = Emit(mc.Target);
        var typeArgs  = mc.TypeArgs.Count > 0
            ? $"<{string.Join(", ", mc.TypeArgs.Select(t => t.ToString()))}>"
            : "";
        var args = string.Join(", ", mc.Args.Select(Emit));

        // `self.Tell(msg)` from inside a handler should preserve
        // the actor as the apparent sender so private-on visibility
        // checks recognise the message as intra-actor. Without this
        // rewrite, ActorRef.Tell(object) defaults to NoSender and the
        // private handler would dead-letter its own actor's messages.
        if (mc.Target is SelfExpr && mc.Method == "Tell" && mc.Args.Count == 1)
            return $"_selfRef.Tell({args}, _selfRef)";

        var accessOp = mc.NullConditional ? "?." : ".";

        // Implicit sender (Akka convention): inside an actor, a
        // fire-and-forget Tell carries this actor as the sender, so the
        // receiver's `sender.Tell(reply)` / `return reply` routes back here.
        // Without this, replies to a plain Tell address NoSender and vanish.
        if (_inActor && mc.Method == "Tell" && mc.Args.Count == 1 && typeArgs.Length == 0)
            return $"{target}{accessOp}Tell({args}, _selfRef)";

        return $"{target}{accessOp}{mc.Method}{typeArgs}({args})";
    }

    // Array creation: implicit `new[] { ... }`, sized `new T[n]`, or typed
    // `new T[] { ... }` / `new T[n] { ... }`.
    private string EmitArray(ArrayExpr arr)
    {
        var elems = string.Join(", ", arr.Elements.Select(Emit));
        if (arr.ElementType is null)
            return $"new[] {{ {elems} }}";
        var size = arr.Size != null ? Emit(arr.Size) : "";
        var init = arr.Elements.Count > 0 ? $" {{ {elems} }}" : "";
        return $"new {arr.ElementType}[{size}]{init}";
    }

    private string EmitNew(NewExpr n)
    {
        var typeArgs = n.TypeArgs.Count > 0
            ? $"<{string.Join(", ", n.TypeArgs.Select(t => t.ToString()))}>"
            : "";
        var args = string.Join(", ", n.Args.Select(Emit));
        var init = n.Initializer is null
            ? ""
            : $" {{ {string.Join(", ", n.Initializer.Select(Emit))} }}";
        return $"new {n.Type}{typeArgs}({args}){init}";
    }

    private string EmitSpawn(SpawnExpr s)
    {
        var typeArg = s.TypeArgs.Count > 0 ? s.TypeArgs[0].ToString() : "object";
        var args    = string.Join(", ", s.Args.Select(Emit));
        return $"SpawnChildAsync<{typeArg}>({args})";
    }

    private string EmitLambda(LambdaExpr l)
    {
        var paramText = EmitLambdaParams(l.Parameters);
        string bodyText;
        if (l.Body is Expr expr)
        {
            bodyText = Emit(expr);
        }
        else if (l.Body is BlockStmt block)
        {
            // Block-bodied lambdas need a statement emitter. Spawn a
            // fresh CSharpWriter and StatementEmitter so we can render
            // the block in isolation, then splice the result in. The
            // statement emitter shares this expression emitter so any
            // expressions inside the block render with the same field
            // set and symbol table.
            var writer = new CSharpWriter();
            var stmtEmitter = new StatementEmitter(writer, this);
            stmtEmitter.EmitBlock(block);
            bodyText = writer.ToString().TrimEnd();
        }
        else
        {
            throw new NotSupportedException(
                $"Lambda body must be Expr or BlockStmt; got {l.Body.GetType().Name}");
        }
        return $"{paramText} => {bodyText}";
    }

    private static string EmitLambdaParams(IReadOnlyList<LambdaParam> parameters)
    {
        if (parameters.Count == 0) return "()";
        if (parameters.Count == 1 && parameters[0].Type is null)
            return parameters[0].Name;   // bare-identifier shorthand
        var rendered = parameters.Select(p =>
            p.Type is null ? p.Name : $"{p.Type} {p.Name}");
        return $"({string.Join(", ", rendered)})";
    }

    private string EmitAsk(AskExpr ask)
    {
        // target.Ask(new Foo(args))
        //   → await target.AskAsync<ReplyType>(new Foo(args))
        //
        // Option D: reply type is inferred from matching `on Foo` handlers
        // in the compilation. If we find `on Foo => return new Pong();`
        // somewhere, ReplyType = Pong. If the reply type can't be inferred
        // (no `return`, a pre-built message value, or no SymbolTable for
        // lookup), fall back to object — the caller pattern-matches the result.
        var target  = Emit(ask.Target);
        var message = Emit(ask.Message);

        // Explicit `.Ask<T>(..)` wins when present — the user's override for
        // when inference is ambiguous, or just to document the reply type at
        // the call site. Otherwise infer from the `new Msg(..)` being sent.
        var replyType = ask.ExplicitReplyType?.ToString()
                        ?? (ask.Message is NewExpr ne ? _symbols?.InferReplyType(ne.Type)?.ToString() : null)
                        ?? "object";

        return $"await {target}.AskAsync<{replyType}>({message})";
    }

    private static string Op(BinaryOp op) => op switch
    {
        BinaryOp.Coalesce => "??",
        BinaryOp.Or  => "||", BinaryOp.And => "&&",
        BinaryOp.BitOr => "|", BinaryOp.BitXor => "^", BinaryOp.BitAnd => "&",
        BinaryOp.Eq  => "==", BinaryOp.Neq => "!=",
        BinaryOp.Lt  => "<",  BinaryOp.Lte => "<=",
        BinaryOp.Gt  => ">",  BinaryOp.Gte => ">=",
        BinaryOp.Shl => "<<", BinaryOp.Shr => ">>",
        BinaryOp.Add => "+",  BinaryOp.Sub => "-",
        BinaryOp.Mul => "*",  BinaryOp.Div => "/",
        BinaryOp.Mod => "%",
        _            => throw new ArgumentOutOfRangeException(nameof(op))
    };

    private string EmitTypeOp(TypeOpExpr t) => t.Kind switch
    {
        TypeOpKind.Cast => $"(({t.Type}){Emit(t.Operand)})",
        TypeOpKind.As   => $"({Emit(t.Operand)} as {t.Type})",
        TypeOpKind.Is   => t.Binding is null
                            ? $"({Emit(t.Operand)} is {t.Type})"
                            : $"({Emit(t.Operand)} is {t.Type} {t.Binding})",
        _ => throw new ArgumentOutOfRangeException(nameof(t))
    };

    private static string UnaryOpStr(UnaryOp op) => op switch
    {
        UnaryOp.Not    => "!",
        UnaryOp.Negate => "-",
        UnaryOp.BitNot => "~",
        _              => throw new ArgumentOutOfRangeException(nameof(op))
    };

    private static string Escape(string s) =>
        s.Replace("\\", "\\\\").Replace("\"", "\\\"");
}
