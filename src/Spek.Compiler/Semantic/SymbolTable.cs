using Spek.Compiler.AST;

namespace Spek.Compiler.Semantic;

/// <summary>
/// Per-file index of named declarations. Resolves simple names (e.g. "Deposit")
/// and qualified names (e.g. "MyBank.Messages.Deposit") against messages and
/// actors declared in this compilation unit. Designed to extend to multi-file
/// <c>SpekCompilation</c> later without callers needing to change.
/// </summary>
public sealed class SymbolTable
{
    private readonly Dictionary<string, MessageDecl> _messagesBySimpleName = new();
    private readonly Dictionary<string, ActorDecl> _actorsBySimpleName = new();
    private readonly Dictionary<string, ChannelDecl> _channelsBySimpleName = new();
    private readonly Dictionary<string, InterfaceDecl> _interfacesBySimpleName = new();
    private readonly Dictionary<string, EnumDecl> _enumsBySimpleName = new();
    private readonly Dictionary<string, SharedRegionDecl> _sharedRegionsBySimpleName = new();
    private readonly Dictionary<string, ModuleDecl> _modulesBySimpleName = new();
    private readonly Dictionary<string, ClassDecl> _classesBySimpleName = new();
    private readonly Dictionary<string, ActorSymbols> _actorSymbols = new();
    private readonly Dictionary<string, ClassSymbols> _classSymbols = new();

    public string? NamespaceName { get; }

    public IReadOnlyCollection<MessageDecl> Messages => _messagesBySimpleName.Values;
    public IReadOnlyCollection<ActorDecl> Actors => _actorsBySimpleName.Values;
    public IReadOnlyCollection<ChannelDecl> Channels => _channelsBySimpleName.Values;
    public IReadOnlyCollection<InterfaceDecl> Interfaces => _interfacesBySimpleName.Values;
    public IReadOnlyCollection<EnumDecl> Enums => _enumsBySimpleName.Values;
    public IReadOnlyCollection<SharedRegionDecl> SharedRegions => _sharedRegionsBySimpleName.Values;
    /// <summary>Top-level modules in the compilation unit.</summary>
    public IReadOnlyCollection<ModuleDecl> Modules => _modulesBySimpleName.Values;
    /// <summary>Top-level classes in the compilation unit.</summary>
    public IReadOnlyCollection<ClassDecl> Classes => _classesBySimpleName.Values;

    private SymbolTable(string? namespaceName) => NamespaceName = namespaceName;

    // ─── Construction ────────────────────────────────────────────────────────

    public static SymbolTable Build(SpekFile file) => BuildCombined([file]);

    /// <summary>
    /// Builds a symbol table spanning multiple parsed files — used by
    /// <see cref="SpekCompilation"/> so a <c>message</c> declared in one
    /// file is visible to an <c>ask</c> or <c>Tell</c> in another. Per-actor
    /// maps remain per-actor (no cross-file actor members).
    /// </summary>
    public static SymbolTable BuildCombined(IEnumerable<SpekFile> files)
    {
        // The "primary" namespace is just whichever file declares one first.
        // Multi-file Spek doesn't currently require a single namespace; this
        // field is mostly informational and doesn't affect resolution.
        var filesList = files.ToList();
        var primaryNamespace = filesList.FirstOrDefault(f => f.Namespace is not null)?.Namespace?.Name.ToString();
        var table = new SymbolTable(primaryNamespace);

        foreach (var file in filesList)
        {
            foreach (var decl in file.Declarations)
            {
                switch (decl)
                {
                    case MessageDecl m:
                        // Last writer wins for lookup here; duplicates are
                        // flagged by CE0013 in SemanticAnalyzer.CheckDuplicateDeclarations.
                        table._messagesBySimpleName[m.Name] = m;
                        break;

                    case ActorDecl a:
                        table._actorsBySimpleName[a.Name] = a;
                        table._actorSymbols[a.Name] = ActorSymbols.Build(a);
                        break;

                    case ChannelDecl c:
                        // Last writer wins; duplicates are caught by CE0013.
                        table._channelsBySimpleName[c.Name] = c;
                        break;

                    case InterfaceDecl i:
                        // Class-side contract. Shares the top-level namespace;
                        // CE0013 catches collisions across kinds.
                        table._interfacesBySimpleName[i.Name] = i;
                        break;

                    case EnumDecl e:
                        table._enumsBySimpleName[e.Name] = e;
                        break;

                    case SharedRegionDecl sr:
                        // Shared regions live in the same name-space
                        // as actors/messages/channels. CE0013 catches duplicates.
                        table._sharedRegionsBySimpleName[sr.Name] = sr;
                        break;

                    case ModuleDecl m:
                        // Modules share the top-level namespace with
                        // actors / messages / channels / regions. CE0013
                        // catches name collisions across kinds.
                        table._modulesBySimpleName[m.Name] = m;
                        break;

                    case ClassDecl cl:
                        // Classes share the top-level namespace too.
                        // ClassSymbols pre-computes which methods mutate (the
                        // confinement analysis + emit both consult it).
                        table._classesBySimpleName[cl.Name] = cl;
                        table._classSymbols[cl.Name] = ClassSymbols.Build(cl);
                        break;
                }
            }
        }
        return table;
    }

    // ─── Lookup ──────────────────────────────────────────────────────────────

    /// <summary>Resolves a simple name to a message declaration.</summary>
    public MessageDecl? ResolveMessage(string simpleName)
        => _messagesBySimpleName.GetValueOrDefault(simpleName);

    /// <summary>Resolves a possibly-qualified name to a message declaration.</summary>
    public MessageDecl? ResolveMessage(QualifiedName name)
    {
        // For now, if the name is fully qualified, we accept only the trailing
        // simple part and require it to match an in-file declaration. Cross-file /
        // cross-namespace resolution is a future SpekCompilation concern.
        return _messagesBySimpleName.GetValueOrDefault(name.Simple);
    }

    public ActorDecl? ResolveActor(QualifiedName name)
        => _actorsBySimpleName.GetValueOrDefault(name.Simple);

    public ActorDecl? ResolveActor(string simpleName)
        => _actorsBySimpleName.GetValueOrDefault(simpleName);

    public ChannelDecl? ResolveChannel(QualifiedName name)
        => _channelsBySimpleName.GetValueOrDefault(name.Simple);

    public ChannelDecl? ResolveChannel(string simpleName)
        => _channelsBySimpleName.GetValueOrDefault(simpleName);

    public InterfaceDecl? ResolveInterface(QualifiedName name)
        => _interfacesBySimpleName.GetValueOrDefault(name.Simple);

    public InterfaceDecl? ResolveInterface(string simpleName)
        => _interfacesBySimpleName.GetValueOrDefault(simpleName);

    public SharedRegionDecl? ResolveSharedRegion(string simpleName)
        => _sharedRegionsBySimpleName.GetValueOrDefault(simpleName);

    /// <summary>Resolves a possibly-qualified module name.</summary>
    public ModuleDecl? ResolveModule(string simpleName)
        => _modulesBySimpleName.GetValueOrDefault(simpleName);

    /// <summary>Resolves a class name to its declaration.</summary>
    public ClassDecl? ResolveClass(string simpleName)
        => _classesBySimpleName.GetValueOrDefault(simpleName);

    /// <summary>Per-class symbols (fields, methods, which methods mutate).</summary>
    public ClassSymbols? GetClassSymbols(string simpleName)
        => _classSymbols.GetValueOrDefault(simpleName);

    /// <summary>
    /// Walks <paramref name="channel"/>'s inheritance graph and returns the
    /// linearized set of <c>on Input;</c> message-type simple names a
    /// channel ultimately requires. Each name appears once in the result
    /// regardless of how many ancestors contributed it (diamond-safe).
    /// Bases that don't resolve are skipped — CE0093 reports them
    /// separately. Cycles are detected and broken (CE0094 surfaces them);
    /// this method is robust against cycles via a visited-set guard.
    /// </summary>
    public HashSet<string> FlattenChannelInputs(ChannelDecl channel)
    {
        var result = new HashSet<string>(StringComparer.Ordinal);
        var visited = new HashSet<string>(StringComparer.Ordinal);
        FlattenInputsImpl(channel, result, visited);
        return result;
    }

    private void FlattenInputsImpl(
        ChannelDecl channel, HashSet<string> result, HashSet<string> visited)
    {
        if (!visited.Add(channel.Name)) return;     // cycle break

        foreach (var baseName in channel.BaseChannels)
        {
            if (ResolveChannel(baseName) is { } baseChannel)
                FlattenInputsImpl(baseChannel, result, visited);
        }

        foreach (var input in channel.Members.OfType<ChannelInput>())
            result.Add(input.MessageType.Simple);
    }

    /// <summary>
    /// Mirror of <see cref="FlattenChannelInputs"/> for <c>emits</c> — walks
    /// inheritance and returns the union of declared output messages.
    /// Returns null if any channel in the graph declares <c>emits any;</c>,
    /// which acts as the strict-enforcement opt-out.
    /// </summary>
    public HashSet<string>? FlattenChannelEmits(ChannelDecl channel)
    {
        var result = new HashSet<string>(StringComparer.Ordinal);
        var visited = new HashSet<string>(StringComparer.Ordinal);
        var hasAny = false;
        FlattenEmitsImpl(channel, result, visited, ref hasAny);
        return hasAny ? null : result;
    }

    private void FlattenEmitsImpl(
        ChannelDecl channel, HashSet<string> result, HashSet<string> visited, ref bool hasAny)
    {
        if (!visited.Add(channel.Name)) return;

        foreach (var baseName in channel.BaseChannels)
        {
            if (ResolveChannel(baseName) is { } baseChannel)
                FlattenEmitsImpl(baseChannel, result, visited, ref hasAny);
        }

        foreach (var emit in channel.Members.OfType<ChannelEmits>())
        {
            if (emit.IsAny) hasAny = true;
            else if (emit.MessageType is not null) result.Add(emit.MessageType.Simple);
        }
    }

    /// <summary>
    /// Returns true if there's a path from <paramref name="channel"/> back
    /// to itself through <c>BaseChannels</c>. Used to fire CE0094 once per
    /// affected channel.
    /// </summary>
    public bool HasCircularInheritance(ChannelDecl channel)
    {
        var visited = new HashSet<string>(StringComparer.Ordinal);
        return HasCycleImpl(channel, channel.Name, visited);
    }

    private bool HasCycleImpl(ChannelDecl current, string startName, HashSet<string> visited)
    {
        if (!visited.Add(current.Name)) return false;   // already explored, no cycle through here

        foreach (var baseName in current.BaseChannels)
        {
            if (baseName.Simple == startName) return true;
            if (ResolveChannel(baseName) is { } baseChannel
                && HasCycleImpl(baseChannel, startName, visited))
                return true;
        }
        return false;
    }

    public EnumDecl? ResolveEnum(QualifiedName name)
        => _enumsBySimpleName.GetValueOrDefault(name.Simple);

    public EnumDecl? ResolveEnum(string simpleName)
        => _enumsBySimpleName.GetValueOrDefault(simpleName);

    public ActorSymbols? GetActorSymbols(string actorName)
        => _actorSymbols.GetValueOrDefault(actorName);

    /// <summary>
    /// Option D — infer the reply type of handlers that match
    /// <paramref name="messageName"/> across every actor in this
    /// compilation. Returns the first concrete type found in a
    /// <c>return new T(...);</c> statement inside an <c>on</c> handler.
    /// Returns <c>null</c> when no handler with a return expression exists
    /// (in which case <c>ask</c> at the call site can't be typed and falls
    /// back to <c>object</c>).
    /// </summary>
    public TypeRef? InferReplyType(QualifiedName messageName)
    {
        var simpleName = messageName.Simple;
        var collected = new List<TypeRef>();

        // Collect every matching handler's return type across the compilation.
        foreach (var actor in _actorsBySimpleName.Values)
        {
            foreach (var behavior in actor.Members.OfType<BehaviorDecl>())
            {
                foreach (var handler in behavior.Handlers)
                {
                    if (!HandlerMatches(handler.Pattern, simpleName)) continue;
                    var replyType = FindFirstReturnType(handler.Body);
                    if (replyType is not null) collected.Add(replyType);
                }
            }
        }

        if (collected.Count == 0) return null;

        // If every matching handler returns the same type, we can infer it
        // confidently. If different handlers return different types, the
        // call site is ambiguous — return null so the emitter falls back
        // to `object` (the caller has to cast). A dedicated CE0044 for
        // this case is planned.
        var canonical = collected[0].ToString();
        return collected.All(t => t.ToString() == canonical) ? collected[0] : null;
    }

    private static bool HandlerMatches(MessagePattern pattern, string messageName)
        => pattern switch
        {
            NamedBindPattern n => n.MessageType.Simple == messageName,
            NoBindPattern n    => n.MessageType.Simple == messageName,
            _                  => false,
        };

    private static TypeRef? FindFirstReturnType(HandlerBody body)
        => body switch
        {
            BlockHandlerBody b  => FindFirstReturnType(b.Block),
            _                   => null,   // inline expressions aren't returns
        };

    private static TypeRef? FindFirstReturnType(Stmt stmt)
    {
        switch (stmt)
        {
            case ReturnStmt r when r.Value is NewExpr newExpr:
                return new TypeRef(newExpr.Type.Span, newExpr.Type, newExpr.TypeArgs);

            case BlockStmt b:
                foreach (var s in b.Statements)
                {
                    var t = FindFirstReturnType(s);
                    if (t is not null) return t;
                }
                return null;

            case IfStmt i:
                var thenType = FindFirstReturnType(i.Then);
                if (thenType is not null) return thenType;
                if (i.Else is not null) return FindFirstReturnType(i.Else);
                return null;

            case ForStmt f:     return FindFirstReturnType(f.Body);
            case ForeachStmt f: return FindFirstReturnType(f.Body);
            case SwitchStmt sw:
                foreach (var sec in sw.Sections)
                {
                    var r = FindFirstReturnType(new BlockStmt(sec.Span, sec.Body));
                    if (r is not null) return r;
                }
                return null;
            case WhileStmt w:   return FindFirstReturnType(w.Body);
            case DoWhileStmt d: return FindFirstReturnType(d.Body);

            default: return null;
        }
    }
}

/// <summary>Per-actor symbol index — fields, behaviors, methods, init params.</summary>
public sealed class ActorSymbols
{
    public ActorDecl Declaration { get; }
    public IReadOnlySet<string> BehaviorNames { get; }
    public IReadOnlyDictionary<string, FieldDecl> Fields { get; }
    public IReadOnlyDictionary<string, MethodDecl> Methods { get; }
    public IReadOnlyList<InitBlock> InitBlocks { get; }

    private ActorSymbols(
        ActorDecl declaration,
        HashSet<string> behaviorNames,
        Dictionary<string, FieldDecl> fields,
        Dictionary<string, MethodDecl> methods,
        List<InitBlock> initBlocks)
    {
        Declaration = declaration;
        BehaviorNames = behaviorNames;
        Fields = fields;
        Methods = methods;
        InitBlocks = initBlocks;
    }

    public static ActorSymbols Build(ActorDecl actor)
    {
        var behaviors = new HashSet<string>();
        var fields = new Dictionary<string, FieldDecl>();
        var methods = new Dictionary<string, MethodDecl>();
        var inits = new List<InitBlock>();

        foreach (var member in actor.Members)
        {
            switch (member)
            {
                case BehaviorDecl b: behaviors.Add(b.Name); break;
                case FieldDecl f:    fields[f.Name] = f; break;
                case MethodDecl m:   methods[m.Name] = m; break;
                case InitBlock init: inits.Add(init); break;
            }
        }

        return new ActorSymbols(actor, behaviors, fields, methods, inits);
    }
}

/// <summary>
/// Per-class symbols. Beyond the field/method index it pre-computes
/// which methods *mutate* a field — directly (an assignment whose target roots
/// at a field, e.g. <c>buffer = …</c> or <c>self.buffer = …</c>) or transitively
/// (a call to a sibling method that mutates). The confinement check — CE0087
/// extended to class methods — consults <see cref="MutatingMethods"/> to decide
/// whether a concurrent reader handler calling <c>classField.M()</c> is a write.
/// </summary>
public sealed class ClassSymbols
{
    public ClassDecl Declaration { get; }
    public IReadOnlyDictionary<string, FieldDecl> Fields { get; }
    public IReadOnlyDictionary<string, MethodDecl> Methods { get; }
    public IReadOnlySet<string> MutatingMethods { get; }

    private ClassSymbols(
        ClassDecl declaration,
        Dictionary<string, FieldDecl> fields,
        Dictionary<string, MethodDecl> methods,
        HashSet<string> mutating)
    {
        Declaration = declaration;
        Fields = fields;
        Methods = methods;
        MutatingMethods = mutating;
    }

    public static ClassSymbols Build(ClassDecl cls)
    {
        var fields = new Dictionary<string, FieldDecl>(StringComparer.Ordinal);
        foreach (var f in cls.Fields) fields[f.Name] = f;          // last wins; CE0013 flags dups
        var methods = new Dictionary<string, MethodDecl>(StringComparer.Ordinal);
        foreach (var m in cls.Methods) methods[m.Name] = m;

        var fieldNames  = fields.Keys.ToHashSet(StringComparer.Ordinal);
        var methodNames = methods.Keys.ToHashSet(StringComparer.Ordinal);

        // Mutable state is fields AND properties — a method that writes
        // `Version = …` (a property) mutates the object exactly like a
        // field write, and must classify as mutating for CE0087.
        var stateNames = fieldNames.ToHashSet(StringComparer.Ordinal);
        foreach (var p in cls.Properties) stateNames.Add(p.Name);

        // Per method: does it write state directly, and which siblings does it call?
        var directWrite = new Dictionary<string, bool>(StringComparer.Ordinal);
        var calls       = new Dictionary<string, HashSet<string>>(StringComparer.Ordinal);
        foreach (var (name, m) in methods)
        {
            var all = ExprsIn(m.Body).ToList();
            directWrite[name] = all.OfType<AssignExpr>().Any(a => RootsAtField(a.Left, stateNames));
            var sib = new HashSet<string>(StringComparer.Ordinal);
            foreach (var e in all)
            {
                // `self.M(...)` or a bare `M(...)` resolving to a sibling method.
                if (e is MethodCallExpr mc && mc.Target is SelfExpr && methodNames.Contains(mc.Method))
                    sib.Add(mc.Method);
                else if (e is InvocationExpr inv && methodNames.Contains(inv.Callee))
                    sib.Add(inv.Callee);
            }
            calls[name] = sib;
        }

        // Fixpoint: a method mutates if it writes directly or calls a mutating sibling.
        var mutating = new HashSet<string>(
            directWrite.Where(kv => kv.Value).Select(kv => kv.Key), StringComparer.Ordinal);
        var changed = true;
        while (changed)
        {
            changed = false;
            foreach (var name in methodNames)
            {
                if (mutating.Contains(name)) continue;
                if (calls[name].Any(mutating.Contains)) { mutating.Add(name); changed = true; }
            }
        }

        return new ClassSymbols(cls, fields, methods, mutating);
    }

    /// <summary>True when an assignment target roots at one of the class's own
    /// fields: a bare <c>field</c>, <c>self.field</c>, or a member/index access
    /// rooted at either (<c>field.x = …</c>, <c>self.field[i] = …</c>).</summary>
    private static bool RootsAtField(Expr lhs, HashSet<string> fields) => Root(lhs, fields) is not null;

    private static string? Root(Expr e, HashSet<string> fields) => e switch
    {
        NameExpr n when n.Name.Parts.Count == 1 && fields.Contains(n.Name.Simple) => n.Name.Simple,
        MemberAccessExpr ma when ma.Target is SelfExpr && fields.Contains(ma.Member) => ma.Member,
        MemberAccessExpr ma => Root(ma.Target, fields),
        IndexExpr ix        => Root(ix.Target, fields),
        ParenExpr p         => Root(p.Inner, fields),
        _                   => null,
    };

    // Every Expr in a statement subtree (self + descendants), so callers can
    // filter by node type. Descends into nested blocks and lambda bodies.
    // Internal: SemanticAnalyzer reuses this walker (is-capture collection,
    // CE0113) rather than maintaining a parallel one.
    internal static IEnumerable<Expr> ExprsIn(Stmt s) => s switch
    {
        BlockStmt b      => b.Statements.SelectMany(ExprsIn),
        VarDeclStmt v    => ExprsIn(v.Initializer),
        ReturnStmt r     => r.Value is null ? [] : ExprsIn(r.Value),
        ThrowStmt t      => t.Value is null ? [] : ExprsIn(t.Value),
        ExpressionStmt e => ExprsIn(e.Expr),
        IfStmt i         => ExprsIn(i.Condition).Concat(ExprsIn(i.Then)).Concat(i.Else is null ? [] : ExprsIn(i.Else)),
        WhileStmt w      => ExprsIn(w.Condition).Concat(ExprsIn(w.Body)),
        DoWhileStmt d    => ExprsIn(d.Condition).Concat(ExprsIn(d.Body)),
        ForStmt f        => ExprsIn(f.Init).Concat(ExprsIn(f.Condition)).Concat(ExprsIn(f.Increment)).Concat(ExprsIn(f.Body)),
        ForeachStmt f    => ExprsIn(f.Collection).Concat(ExprsIn(f.Body)),
        SwitchStmt sw    => ExprsIn(sw.Subject).Concat(sw.Sections.SelectMany(sec =>
                                sec.Body.SelectMany(ExprsIn).Concat(
                                    sec.Labels.Where(l => l.Guard is not null).SelectMany(l => ExprsIn(l.Guard!))))),
        TryStmt ts       => ExprsIn(ts.Try)
                              .Concat(ts.Catches.SelectMany(c =>
                                  (c.When is null ? Enumerable.Empty<Expr>() : ExprsIn(c.When)).Concat(ExprsIn(c.Body))))
                              .Concat(ts.Finally is null ? [] : ExprsIn(ts.Finally)),
        _                => [],   // become, persist, …
    };

    internal static IEnumerable<Expr> ExprsIn(Expr e)
    {
        yield return e;
        var kids = e switch
        {
            AskExpr a          => ExprsIn(a.Target).Concat(ExprsIn(a.Message)),
            AssignExpr a       => ExprsIn(a.Left).Concat(ExprsIn(a.Right)),
            ConditionalExpr c  => ExprsIn(c.Condition).Concat(ExprsIn(c.Then)).Concat(ExprsIn(c.Else)),
            BinaryExpr b       => ExprsIn(b.Left).Concat(ExprsIn(b.Right)),
            UnaryExpr u        => ExprsIn(u.Operand),
            TypeOpExpr t       => ExprsIn(t.Operand),
            MemberAccessExpr m => ExprsIn(m.Target),
            MethodCallExpr mc  => ExprsIn(mc.Target).Concat(mc.Args.SelectMany(ExprsIn)),
            RefArgExpr r       => ExprsIn(r.Inner),
            NamedArgExpr na    => ExprsIn(na.Value),
            ArrayExpr arr      => arr.Elements.Concat(arr.Size is null ? [] : [arr.Size]).SelectMany(ExprsIn),
            TupleExpr tup      => tup.Elements.SelectMany(ExprsIn),
            ThrowExpr th       => ExprsIn(th.Value),
            InvocationExpr inv => inv.Args.SelectMany(ExprsIn),
            IndexExpr ix       => ExprsIn(ix.Target).Concat(ExprsIn(ix.Index)),
            NewExpr n          => n.Args.Concat(n.Initializer ?? []).SelectMany(ExprsIn),
            SpawnExpr sp       => sp.Args.SelectMany(ExprsIn),
            ParenExpr p        => ExprsIn(p.Inner),
            SwitchExpr sw      => ExprsIn(sw.Subject).Concat(sw.Arms.SelectMany(arm =>
                                    (arm.When is null ? Enumerable.Empty<Expr>() : ExprsIn(arm.When)).Concat(ExprsIn(arm.Result)))),
            LambdaExpr lam     => lam.Body switch { Expr ex => ExprsIn(ex), BlockStmt bs => ExprsIn(bs), _ => [] },
            InterpolatedStringExpr istr => istr.Parts.OfType<InterpolationHole>().SelectMany(h => ExprsIn(h.Expr)),
            _                  => [],   // NameExpr, literals, self, sender, out-var
        };
        foreach (var k in kids) yield return k;
    }
}
