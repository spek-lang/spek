using Spek.Compiler.AST;

namespace Spek.Compiler.Semantic;

/// <summary>
/// Runs semantic checks over a parsed <see cref="SpekFile"/> and returns
/// <see cref="Diagnostic"/>s for any violations. Covers location-based rules
/// and intra-actor consistency; type-level rules (CE0010, CE0012, CE0020) are
/// deferred until a symbol table / type resolver exists.
/// </summary>
public static class SemanticAnalyzer
{
    public static IReadOnlyList<Diagnostic> Analyze(SpekFile file)
        => Analyze(file, SymbolTable.Build(file));

    /// <summary>
    /// Analyzes <paramref name="file"/> against the given symbol table.
    /// <see cref="Parser.SpekCompilation"/> uses this overload to pass a combined
    /// table so cross-file message references resolve correctly.
    /// </summary>
    public static IReadOnlyList<Diagnostic> Analyze(SpekFile file, SymbolTable symbols)
    {
        var diagnostics = new List<Diagnostic>();

        // CE0013 — duplicate declarations at file scope and within actors.
        CheckDuplicateDeclarations(file, diagnostics);

        // CE0080 — hostile namespace imports (reflection, unsafe, etc.).
        CheckHostileImports(file, diagnostics);

        foreach (var decl in file.Declarations)
        {
            switch (decl)
            {
                case MessageDecl message:
                    AnalyzeMessage(message, symbols, diagnostics);
                    break;

                case ActorDecl actor:
                    AnalyzeActor(actor, symbols, diagnostics);
                    CheckActorInheritance(actor, symbols, diagnostics);
                    break;

                case ChannelDecl channel:
                    AnalyzeChannel(channel, symbols, diagnostics);
                    break;

                case InterfaceDecl iface:
                    AnalyzeInterface(iface, symbols, diagnostics);
                    break;

                case ClassDecl cls:
                    AnalyzeClass(cls, symbols, diagnostics);
                    break;
            }
        }

        // CE0098 — every `: Persisted` region must have at least one
        // `system.RegisterPersistenceProvider<T>(...)` call somewhere
        // in a `program` block within this compilation. Spek-only host
        // assumption. When polyglot host support is added,
        // a runtime ValidateRegistrations() becomes the fallback.
        CheckPersistedRegionsHaveProviders(file, diagnostics);

        // CE0110 — actors / regions holding fields whose type names
        // match a disposable-looking heuristic should declare a
        // `term { }` block to release them at lifecycle end.
        CheckDisposableFieldsHaveTermBlock(file, diagnostics);

        // CE0109 — non-nullable reference-typed fields without an
        // initializer, on actors / regions that don't have an init
        // block to set them at construction time.
        CheckNonNullableFieldsAreInitialized(file, diagnostics);

        // CE0107 — a top-level explicit `Task<T>` / `ValueTask<T>` local
        // (the invisible-async escape hatch) that is never used as a Task.
        // The annotation has no effect there — `var` would behave the same.
        CheckRedundantTaskAnnotations(file, diagnostics);

        // CE0112 — a mutable `class` may not be a shared-region field (it would
        // let multiple actors reach mutable state). The confined-class model
        // keeps classes single-owner; sharing is for `message` / region-native
        // fields. Closes the region escape route.
        CheckRegionFieldsArentMutableClasses(file, symbols, diagnostics);

        // CE0113 — `?.` / `?[` on the left of an assignment. C# has no
        // null-conditional assignment; without this check the emitted C#
        // fails with an opaque CS error pointing at the .g.cs.
        CheckNullConditionalAssignments(file, diagnostics);

        // CE0126 — sends whose statically-known target has no handler for the
        // message in ANY behavior (provably dead mail). Flow-based typed-send
        // checking within the compilation; the cross-boundary story is typed
        // ActorRef<Channel> (CE0030, reserved).
        CheckNeverHandledSends(file, symbols, diagnostics);
        CheckConversionTargets(file, symbols, diagnostics);
        CheckEnums(file, diagnostics);
        CheckFlagsUsage(file, symbols, diagnostics);
        CheckTimeApis(file, diagnostics);

        return diagnostics;
    }

    // CE0112. A `class` is mutable and single-owner; placing one in a
    // shared region would expose mutable state to every actor that attaches
    // the region (shared-and-mutable, which the model forbids). Message fields
    // are already blocked by CE0010 (a class isn't immutable); this closes the
    // region route. Cross-actor *transfer* of a class (returning/forwarding it)
    // is the separately-parked "moved" case.
    private static void CheckRegionFieldsArentMutableClasses(
        SpekFile file, SymbolTable symbols, List<Diagnostic> diagnostics)
    {
        foreach (var region in file.Declarations.OfType<SharedRegionDecl>())
        {
            foreach (var field in region.Fields)
            {
                // A mutable class anywhere in the field's type — the type
                // itself, or nested inside a generic container's type args
                // (`ImmutableArray<Registry>` hands every attaching actor the
                // SAME mutable Registry element). Recurse into type args the
                // way CE0010 does for message fields, so an immutable container
                // wrapping a mutable class is caught, not waved through.
                if (FindMutableClassInType(field.Type, symbols) is not { } cls) continue;

                var where = cls == field.Type.Name.Simple
                    ? $"mutable class type '{cls}'"
                    : $"type '{field.Type}', which nests the mutable class '{cls}'";
                diagnostics.Add(Diagnostic.At("CE0112", field.Span,
                    $"Shared-region field '{region.Name}.{field.Name}' has {where}. " +
                    $"A class is mutable and single-owner — placing it " +
                    $"in a shared region would let multiple actors reach mutable state. Use an " +
                    $"immutable 'message' type or a primitive for shared state, or keep the class " +
                    $"confined to a single actor."));
            }
        }
    }

    /// <summary>The first Spek <c>class</c> reachable from <paramref name="type"/>
    /// — the type itself or any type argument, recursively — or <c>null</c> if
    /// the type nests no confined class. Mirrors CE0010's recursive element
    /// walk, but looks only for classes (region-native mutable collections such
    /// as <c>List&lt;int&gt;</c> stay legal — the region provides their
    /// reader/writer discipline; a shared mutable <em>class</em> would not obey
    /// it).</summary>
    private static string? FindMutableClassInType(TypeRef type, SymbolTable symbols)
    {
        if (symbols.ResolveClass(type.Name.Simple) is not null)
            return type.Name.Simple;
        foreach (var arg in type.TypeArgs)
            if (FindMutableClassInType(arg, symbols) is { } nested)
                return nested;
        return null;
    }

    /// <summary>
    /// CE0109. A non-nullable reference-typed field without an
    /// initializer leaves the underlying C# field as <c>null</c> at
    /// runtime, which surfaces as a <c>NullReferenceException</c> on
    /// first read. Roslyn's nullability analysis catches this at the
    /// C# layer; CE0109 surfaces it earlier, at the Spek layer, where
    /// the diagnostic message can point at the Spek source line.
    ///
    /// The check is conservative:
    /// <list type="bullet">
    ///   <item>Value types (primitives, enums, BCL value types) are
    ///         skipped — they have natural defaults.</item>
    ///   <item>Nullable types (`string?`, `T?`) are skipped — null is
    ///         a valid value.</item>
    ///   <item>Owners that have an `init { ... }` (or `init() { ... }`)
    ///         block are skipped — the field is presumed to be
    ///         assigned in init.</item>
    /// </list>
    /// Severity is <see cref="DiagnosticSeverity.Warning"/>.
    /// </summary>
    private static void CheckNonNullableFieldsAreInitialized(
        SpekFile file, List<Diagnostic> diagnostics)
    {
        foreach (var decl in file.Declarations)
        {
            switch (decl)
            {
                case ActorDecl actor:
                {
                    // Actors with an init block are assumed to set
                    // fields there. False negatives are acceptable
                    // (we don't walk init bodies).
                    if (actor.Members.OfType<InitBlock>().Any()) break;
                    foreach (var f in actor.Members.OfType<FieldDecl>())
                        ReportIfUninitializedReferenceField(f, diagnostics);
                    break;
                }
                case SharedRegionDecl region:
                {
                    if (region.Init is not null) break;
                    foreach (var f in region.Fields)
                        ReportIfUninitializedReferenceField(f, diagnostics);
                    break;
                }
            }
        }
    }

    private static void ReportIfUninitializedReferenceField(
        FieldDecl field, List<Diagnostic> diagnostics)
    {
        if (field.Initializer is not null) return;
        if (field.Type.IsNullable)         return;
        if (IsValueLikeType(field.Type.Name.Simple)) return;

        diagnostics.Add(Diagnostic.At("CE0109", field.Span,
            $"Non-nullable field '{field.Name}' of type '{field.Type}' has no " +
            $"initializer. Add `= …` to set a default, mark the type " +
            $"nullable (`{field.Type}?`), or move construction into an " +
            $"`init` block.",
            DiagnosticSeverity.Warning));
    }

    private static readonly HashSet<string> ValueLikeTypes = new(StringComparer.Ordinal)
    {
        // C# primitives
        "int", "long", "short", "byte", "uint", "ulong", "ushort", "sbyte",
        "nint", "nuint", "char", "bool",
        "float", "double", "decimal",
        "void",
        // BCL value types commonly used as Spek field types
        "DateTime", "DateTimeOffset", "TimeSpan", "Guid",
    };

    private static bool IsValueLikeType(string simpleName)
        => ValueLikeTypes.Contains(simpleName);

    // ─── CE0107: redundant Task<T> annotation ──────────────────────────────
    //
    // An explicit `Task<T>` / `ValueTask<T>` local is the invisible-async
    // escape hatch (Emit/AsyncRewriter.cs): it keeps the raw Task instead of
    // auto-awaiting at the binding. But a lazy `var` already defers and
    // auto-awaits at the use site — running concurrently the same way — so
    // the ONLY remaining reason to name the Task type is to consume the value
    // *as a Task*: hand it to a Task-shaped API, forward it out, capture it.
    // When a top-level explicit-Task local is never used in such a context,
    // the annotation has no effect and `var` would behave identically, so it
    // is redundant.
    //
    // Sound-by-construction (no false "drop the Task" suggestions):
    //   * only TOP-LEVEL locals of a callable body are checked — that's where
    //     `var` provably defers the same way (the rewriter's SafeToDeferAt
    //     gates `var` deferral on top-level position). A nested Task local may
    //     defer differently than `var` would, so we leave it alone.
    //   * ANY use that could be a task context suppresses the warning: a
    //     member access (could be `.Result`/`.ConfigureAwait`), a call
    //     receiver, any call/new/ask/spawn argument, a `ref`/`out`/`in` arg, a
    //     bare `return t` (forwarding), an assignment involving the local, or
    //     rebinding it into another explicit Task local.
    //
    // Detection is purely syntactic on the Spek AST — Roslyn isn't needed to
    // tell that a NAMED Task type is or isn't consumed as a Task, and keeping
    // it here means the diagnostic points at the real Spek source line.
    // Severity is Warning; it never blocks a build.
    private static void CheckRedundantTaskAnnotations(SpekFile file, List<Diagnostic> diagnostics)
    {
        foreach (var body in CallableBodies(file))
            foreach (var stmt in body.Statements)
            {
                // Only top-level explicit `Task`/`ValueTask` locals qualify.
                if (stmt is not VarDeclStmt v || v.Type is null || !IsTaskTypeRef(v.Type))
                    continue;
                if (UsesAsTask(body, v.Name))
                    continue;
                diagnostics.Add(Diagnostic.At("CE0107", v.Span,
                    $"redundant `{v.Type}` annotation: `{v.Name}` is never used as a Task, " +
                    $"so the explicit Task type has no effect — use `var` (which also runs " +
                    $"concurrently) or drop the binding.",
                    DiagnosticSeverity.Warning));
            }
    }

    /// <summary>True when a type reference names <c>Task</c> or
    /// <c>ValueTask</c> (generic or not, regardless of namespace
    /// qualification). A user type that shadows those BCL names is the only
    /// false-positive source — acceptable for a warning.</summary>
    private static bool IsTaskTypeRef(TypeRef t) => t.Name.Simple is "Task" or "ValueTask";

    // CE0113 — assignment through a null-conditional access. `x?.y = v` /
    // `x?[i] = v` is not a thing in C# (there is no null-conditional
    // assignment), so the emitted C# would fail with an opaque error in the
    // generated file. Catch it at the Spek line instead.
    private static void CheckNullConditionalAssignments(SpekFile file, List<Diagnostic> diagnostics)
    {
        foreach (var body in CallableBodies(file))
            foreach (var a in ClassSymbols.ExprsIn(body).OfType<AssignExpr>())
            {
                if (HasNullConditionalAccess(a.Left))
                    diagnostics.Add(Diagnostic.At("CE0113", a.Span,
                        "the null-conditional operator ('?.' / '?[') cannot appear on the " +
                        "left-hand side of an assignment — there is no null-conditional " +
                        "assignment in C#. Test for null explicitly, e.g. " +
                        "`if (x != null) { x.member = value; }`."));
            }
    }

    /// <summary>True when an assignment target reaches through a
    /// null-conditional access anywhere in its chain (<c>a?.b.c</c>,
    /// <c>a.b?[i]</c>, …).</summary>
    private static bool HasNullConditionalAccess(Expr e) => e switch
    {
        MemberAccessExpr ma => ma.NullConditional || HasNullConditionalAccess(ma.Target),
        MethodCallExpr mc   => mc.NullConditional || HasNullConditionalAccess(mc.Target),
        IndexExpr ix        => ix.NullConditional || HasNullConditionalAccess(ix.Target),
        ParenExpr p         => HasNullConditionalAccess(p.Inner),
        _                   => false,
    };

    /// <summary>All top-level callable bodies in the file: program bodies,
    /// module methods (incl. nested modules), actor methods / handlers /
    /// init / term / lifecycle hooks, and region init / term blocks.</summary>
    private static IEnumerable<BlockStmt> CallableBodies(SpekFile file)
    {
        foreach (var decl in file.Declarations)
        {
            switch (decl)
            {
                case ProgramDecl p:
                    yield return p.Body;
                    break;
                case ModuleDecl m:
                    foreach (var b in ModuleBodies(m)) yield return b;
                    break;
                case ActorDecl a:
                    foreach (var b in ActorBodies(a)) yield return b;
                    break;
                case SharedRegionDecl sr:
                    if (sr.Init is not null) yield return sr.Init;
                    if (sr.Term is not null) yield return sr.Term;
                    break;
            }
        }
    }

    private static IEnumerable<BlockStmt> ModuleBodies(ModuleDecl m)
    {
        foreach (var f in m.Methods) yield return f.Body;
        foreach (var nested in m.NestedModules)
            foreach (var b in ModuleBodies(nested)) yield return b;
    }

    private static IEnumerable<BlockStmt> ActorBodies(ActorDecl a)
    {
        foreach (var member in a.Members)
        {
            switch (member)
            {
                case MethodDecl md: yield return md.Body; break;
                case InitBlock ib:  yield return ib.Body; break;
                case TermBlock tb:  yield return tb.Body; break;
                case LifecycleHook { Body: BlockHandlerBody lhb }: yield return lhb.Block; break;
                case BehaviorDecl bd:
                    foreach (var h in bd.Handlers)
                        if (h.Body is BlockHandlerBody hb) yield return hb.Block;
                    break;
            }
        }
    }

    /// <summary>Every expression in an actor's stream-operator chains, fully
    /// descended (operator args + selector lambda bodies). These run off the
    /// actor's turn but are still actor code, so the expr-level actor rules
    /// (e.g. CE0134's clock check) must see them — a blind spot ActorBodies,
    /// which yields only statement bodies, doesn't reach (red-team H2).</summary>
    private static IEnumerable<Expr> StreamOperatorExprs(ActorDecl a)
        => a.Members.OfType<BehaviorDecl>()
            .SelectMany(b => b.Handlers)
            .Where(h => h.StreamOperators is { Count: > 0 })
            .SelectMany(h => h.StreamOperators!)
            .SelectMany(ClassSymbols.ExprsIn);

    /// <summary>Every stream-operator expression across every actor in the
    /// file — the file-level companion to <see cref="StreamOperatorExprs"/>,
    /// for the whole-file conversion/cast sweep (CE0127 / CE0129).</summary>
    private static IEnumerable<Expr> AllStreamOperatorExprs(SpekFile file)
        => file.Declarations.OfType<ActorDecl>().SelectMany(StreamOperatorExprs);

    /// <summary>True when <paramref name="name"/> is used in a task context
    /// (or any position we can't prove is a plain value-use) anywhere under
    /// the statement. A bare reference in a value position (operand, condition,
    /// index, interpolation hole, plain binding) does NOT count.</summary>
    private static bool UsesAsTask(Stmt stmt, string name)
    {
        switch (stmt)
        {
            case BlockStmt b:
                return b.Statements.Any(s => UsesAsTask(s, name));
            case VarDeclStmt v:
                // Rebinding into another explicit Task slot forwards the Task.
                if (v.Type is not null && IsTaskTypeRef(v.Type) && IsName(v.Initializer, name))
                    return true;
                return UsesAsTask(v.Initializer, name);
            case ReturnStmt r:
                if (r.Value is null) return false;
                if (IsName(r.Value, name)) return true;        // `return t;` — forwarding
                return UsesAsTask(r.Value, name);
            case ThrowStmt t:
                return t.Value is not null && UsesAsTask(t.Value, name);
            case IfStmt i:
                return UsesAsTask(i.Condition, name) || UsesAsTask(i.Then, name)
                    || (i.Else is not null && UsesAsTask(i.Else, name));
            case WhileStmt w:
                return UsesAsTask(w.Condition, name) || UsesAsTask(w.Body, name);
            case DoWhileStmt dw:
                return UsesAsTask(dw.Condition, name) || UsesAsTask(dw.Body, name);
            case ForStmt f:
                return UsesAsTask(f.Init, name) || UsesAsTask(f.Condition, name)
                    || UsesAsTask(f.Increment, name) || UsesAsTask(f.Body, name);
            case ForeachStmt fe:
                return UsesAsTask(fe.Collection, name) || UsesAsTask(fe.Body, name);
            case SwitchStmt sw:
                return UsesAsTask(sw.Subject, name)
                    || sw.Sections.Any(sec => sec.Body.Any(st => UsesAsTask(st, name))
                        || sec.Labels.Any(l => l.Guard is not null && UsesAsTask(l.Guard, name)));
            case ExpressionStmt e:
                return UsesAsTask(e.Expr, name);
            case TryStmt ts:
                if (UsesAsTask(ts.Try, name)) return true;
                foreach (var c in ts.Catches)
                {
                    if (c.When is not null && UsesAsTask(c.When, name)) return true;
                    if (UsesAsTask(c.Body, name)) return true;
                }
                return ts.Finally is not null && UsesAsTask(ts.Finally, name);
            default:
                return false;   // become, persist, …
        }
    }

    private static bool UsesAsTask(Expr expr, string name)
    {
        switch (expr)
        {
            // ── task-context positions: the local appears AS a Task ──
            case MemberAccessExpr ma:
                return IsName(ma.Target, name) || UsesAsTask(ma.Target, name);
            case MethodCallExpr mc:
                return IsName(mc.Target, name) || mc.Args.Any(a => IsName(a, name))
                    || UsesAsTask(mc.Target, name) || mc.Args.Any(a => UsesAsTask(a, name));
            case InvocationExpr inv:
                return inv.Args.Any(a => IsName(a, name)) || inv.Args.Any(a => UsesAsTask(a, name));
            case NewExpr ne:
            {
                var newParts = ne.Args.Concat(ne.Initializer ?? []);
                return newParts.Any(a => IsName(a, name)) || newParts.Any(a => UsesAsTask(a, name));
            }
            case AskExpr ak:
                return IsName(ak.Target, name) || IsName(ak.Message, name)
                    || UsesAsTask(ak.Target, name) || UsesAsTask(ak.Message, name);
            case SpawnExpr sp:
                return sp.Args.Any(a => IsName(a, name)) || sp.Args.Any(a => UsesAsTask(a, name));
            case RefArgExpr ra:
                return IsName(ra.Inner, name) || UsesAsTask(ra.Inner, name);
            case NamedArgExpr na:
                return IsName(na.Value, name) || UsesAsTask(na.Value, name);
            case ArrayExpr arr:
                return arr.Elements.Any(el => IsName(el, name))
                    || arr.Elements.Any(el => UsesAsTask(el, name))
                    || (arr.Size is not null && (IsName(arr.Size, name) || UsesAsTask(arr.Size, name)));
            case TupleExpr tup:
                return tup.Elements.Any(el => IsName(el, name))
                    || tup.Elements.Any(el => UsesAsTask(el, name));
            case ThrowExpr th:
                return IsName(th.Value, name) || UsesAsTask(th.Value, name);
            case AssignExpr asn:
                return IsName(asn.Left, name) || IsName(asn.Right, name)
                    || UsesAsTask(asn.Left, name) || UsesAsTask(asn.Right, name);

            // ── value positions: recurse; a bare reference here is value-use ──
            case BinaryExpr b:      return UsesAsTask(b.Left, name) || UsesAsTask(b.Right, name);
            case UnaryExpr u:       return UsesAsTask(u.Operand, name);
            case TypeOpExpr t:      return UsesAsTask(t.Operand, name);
            case ConditionalExpr c: return UsesAsTask(c.Condition, name) || UsesAsTask(c.Then, name) || UsesAsTask(c.Else, name);
            case IndexExpr ix:      return UsesAsTask(ix.Target, name) || UsesAsTask(ix.Index, name);
            case ParenExpr p:       return UsesAsTask(p.Inner, name);
            case SwitchExpr sw:
                if (UsesAsTask(sw.Subject, name)) return true;
                foreach (var arm in sw.Arms)
                {
                    if (arm.When is not null && UsesAsTask(arm.When, name)) return true;
                    if (UsesAsTask(arm.Result, name)) return true;
                }
                return false;
            case LambdaExpr lam:
                return lam.Body switch
                {
                    Expr e       => UsesAsTask(e, name),
                    BlockStmt bs => UsesAsTask(bs, name),
                    _            => false,
                };
            case InterpolatedStringExpr istr:
                return istr.Parts.OfType<InterpolationHole>().Any(h => UsesAsTask(h.Expr, name));

            default:
                return false;   // NameExpr (bare = value-use), literals, self/sender, out-var
        }
    }

    private static bool IsName(Expr e, string name) =>
        e is NameExpr ne && ne.Name.Parts.Count == 1 && ne.Name.Simple == name;

    /// <summary>
    /// CE0110. Actors and shared regions that hold fields of
    /// likely-disposable types should declare a `term { }` block so
    /// the resource is released at lifecycle end (actor stop or
    /// `ActorSystem` shutdown). The check uses a heuristic on type
    /// simple-names — exact membership in a small known list, plus
    /// suffix matching for common BCL patterns. False negatives are
    /// fine (the user just doesn't get the warning); false positives
    /// would be annoying, so the heuristic is conservative.
    ///
    /// Severity is <see cref="DiagnosticSeverity.Warning"/> — a
    /// missing term block isn't always wrong (e.g. the user knows
    /// the resource lives forever), but it's almost always worth a
    /// second look.
    /// </summary>
    private static void CheckDisposableFieldsHaveTermBlock(
        SpekFile file, List<Diagnostic> diagnostics)
    {
        foreach (var decl in file.Declarations)
        {
            switch (decl)
            {
                case ActorDecl actor:
                {
                    var hasTerm = actor.Members.OfType<TermBlock>().Any();
                    if (hasTerm) break;
                    foreach (var f in actor.Members.OfType<FieldDecl>())
                        ReportIfDisposable(f, actor.Name, "actor", diagnostics);
                    break;
                }
                case SharedRegionDecl region:
                {
                    if (region.Term is not null) break;
                    foreach (var f in region.Fields)
                        ReportIfDisposable(f, region.Name, "shared region", diagnostics);
                    break;
                }
            }
        }
    }

    private static void ReportIfDisposable(
        FieldDecl field, string ownerName, string ownerKind, List<Diagnostic> diagnostics)
    {
        var typeName = field.Type.Name.Simple;
        if (!LooksDisposable(typeName)) return;

        diagnostics.Add(Diagnostic.At("CE0110", field.Span,
            $"Field '{field.Name}' has likely-disposable type '{typeName}' but " +
            $"{ownerKind} '{ownerName}' has no `term {{ }}` block. Add a term " +
            $"block to release the resource at lifecycle end, or move the field " +
            $"to a non-{ownerKind} owner if its lifetime is intentionally " +
            $"unmanaged.",
            DiagnosticSeverity.Warning));
    }

    private static readonly HashSet<string> KnownDisposableTypes = new(StringComparer.Ordinal)
    {
        "HttpClient", "HttpListener", "HttpResponseMessage", "HttpRequestMessage",
        "TcpClient", "TcpListener", "UdpClient", "Socket",
        "Timer", "PeriodicTimer",
        "CancellationTokenSource",
        "SemaphoreSlim", "ManualResetEvent", "ManualResetEventSlim",
        "AutoResetEvent", "Mutex",
        "Process", "FileSystemWatcher",
        "WebClient",
    };

    private static readonly string[] DisposableSuffixes =
    {
        "Stream", "Reader", "Writer", "Connection", "Pipe", "Channel",
    };

    private static bool LooksDisposable(string simpleName)
    {
        if (KnownDisposableTypes.Contains(simpleName)) return true;
        foreach (var suffix in DisposableSuffixes)
        {
            if (simpleName.Length > suffix.Length &&
                simpleName.EndsWith(suffix, StringComparison.Ordinal))
                return true;
        }
        return false;
    }

    private static void CheckPersistedRegionsHaveProviders(
        SpekFile file, List<Diagnostic> diagnostics)
    {
        var persistedRegions = file.Declarations
            .OfType<SharedRegionDecl>()
            .Where(sr => sr.BaseCapability == "Persisted")
            .ToList();
        if (persistedRegions.Count == 0) return;

        // Collect every method name + first type-arg simple-name pair
        // referenced as `*.RegisterPersistenceProvider<T>(...)` across
        // every program block.
        var registeredTypes = new HashSet<string>(StringComparer.Ordinal);
        foreach (var program in file.Declarations.OfType<ProgramDecl>())
            CollectRegistrations(program.Body, registeredTypes);

        foreach (var sr in persistedRegions)
        {
            if (registeredTypes.Contains(sr.Name)) continue;
            diagnostics.Add(Diagnostic.At("CE0098", sr.Span,
                $"Shared region '{sr.Name} : Persisted' has no registered " +
                $"snapshot-store provider. Add 'system.RegisterPersistenceProvider<{sr.Name}>(store);' " +
                $"to a 'program' block before any actor that uses the region " +
                $"is spawned, or change the region to ': Transient' (the default) " +
                $"if persistence isn't needed."));
        }
    }

    private static void CollectRegistrations(BlockStmt block, HashSet<string> registeredTypes)
    {
        foreach (var stmt in block.Statements)
            CollectRegistrationsInStmt(stmt, registeredTypes);
    }

    private static void CollectRegistrationsInStmt(Stmt stmt, HashSet<string> registeredTypes)
    {
        switch (stmt)
        {
            case ExpressionStmt es:
                CollectRegistrationsInExpr(es.Expr, registeredTypes);
                break;
            case BlockStmt b:
                CollectRegistrations(b, registeredTypes);
                break;
            case VarDeclStmt v:
                CollectRegistrationsInExpr(v.Initializer, registeredTypes);
                break;
            case IfStmt ifs:
                CollectRegistrations(ifs.Then, registeredTypes);
                if (ifs.Else is not null) CollectRegistrationsInStmt(ifs.Else, registeredTypes);
                break;
            case ForStmt f:
                CollectRegistrations(f.Body, registeredTypes);
                break;
            case ForeachStmt fe:
                CollectRegistrations(fe.Body, registeredTypes);
                break;
            case SwitchStmt sw:
                foreach (var sec in sw.Sections)
                    CollectRegistrations(new BlockStmt(sec.Span, sec.Body), registeredTypes);
                break;
            case WhileStmt ws:
                CollectRegistrations(ws.Body, registeredTypes);
                break;
            case DoWhileStmt dw:
                CollectRegistrations(dw.Body, registeredTypes);
                break;
            // Other statement shapes (return, become, etc.) can't
            // contain a registration call worth tracking; skip.
        }
    }

    private static void CollectRegistrationsInExpr(Expr expr, HashSet<string> registeredTypes)
    {
        if (expr is MethodCallExpr mc &&
            mc.Method == "RegisterPersistenceProvider" &&
            mc.TypeArgs.Count == 1)
        {
            var typeName = mc.TypeArgs[0].Name.Simple;
            registeredTypes.Add(typeName);
        }
        // Don't recurse — registrations live as top-level expression
        // statements in program bodies. Nested cases (e.g. a builder
        // pattern that wraps the call) are out of scope for now.
    }

    /// <summary>
    /// Channel-level checks. Channels support inheritance, so we validate:
    /// <list type="bullet">
    ///   <item><b>CE0093</b> — every name in <c>BaseChannels</c> resolves
    ///         to a declared channel (not a message, not an actor, not an
    ///         enum, not unknown).</item>
    ///   <item><b>CE0094</b> — circular inheritance (<c>A : B</c>,
    ///         <c>B : A</c> or longer cycles). Reported once per channel
    ///         that participates in the cycle.</item>
    /// </list>
    /// </summary>
    private static void AnalyzeChannel(
        ChannelDecl channel, SymbolTable symbols, List<Diagnostic> diagnostics)
    {
        // CE0093 — unknown base names. Bases that resolve to a non-channel
        // (e.g. a `message` with the same simple name) are also flagged
        // — the user almost certainly meant a channel.
        foreach (var baseName in channel.BaseChannels)
        {
            if (symbols.ResolveChannel(baseName) is null)
            {
                diagnostics.Add(Diagnostic.At("CE0093", baseName.Span,
                    $"Channel '{channel.Name}' inherits from unknown channel " +
                    $"'{baseName}'. Declare it with 'channel {baseName} {{ ... }}' " +
                    $"first, or remove it from the inheritance list."));
            }
        }

        // CE0094 — circular inheritance.
        if (symbols.HasCircularInheritance(channel))
        {
            diagnostics.Add(Diagnostic.At("CE0094", channel.Span,
                $"Channel '{channel.Name}' has a circular inheritance " +
                $"chain — at least one of its bases (transitively) inherits " +
                $"from '{channel.Name}'. Break the cycle by removing one of " +
                $"the inheritance edges."));
        }
    }

    /// <summary>
    /// Validates a class-side <c>interface</c> contract. The no-behavior rule
    /// (CE0120) is the deliberate divergence from modern C#: a Spek contract
    /// declares shape, never behavior or state, so it forbids default-method
    /// bodies, property-accessor bodies, and fields — none of which a
    /// pre-C#-8 interface had. Base interfaces that don't resolve to another
    /// <c>interface</c> are CE0093 (mirrors the channel check).
    /// </summary>
    private static void AnalyzeInterface(
        InterfaceDecl iface, SymbolTable symbols, List<Diagnostic> diagnostics)
    {
        // CE0120 — a method may not carry a body (no default methods).
        foreach (var m in iface.Methods)
        {
            if (m.Body is not null)
            {
                diagnostics.Add(Diagnostic.At("CE0120", m.Span,
                    $"Interface method '{iface.Name}.{m.Name}' has a body. A Spek " +
                    $"'interface' declares shape, never behavior — write the " +
                    $"signature only ('{m.Name}(...);') and put the body in the " +
                    $"implementing class. (Unlike C# 8+, interfaces have no " +
                    $"default methods.)"));
            }
        }

        // CE0120 — a property accessor may not carry a body, and the property
        // may not have an initializer (both are behavior/state).
        foreach (var p in iface.Properties)
        {
            if (p.Initializer is not null)
            {
                diagnostics.Add(Diagnostic.At("CE0120", p.Span,
                    $"Interface property '{iface.Name}.{p.Name}' has an initializer. " +
                    $"A Spek 'interface' declares shape, never state — write the " +
                    $"accessor signature only ('{p.Name} {{ get; set; }}')."));
            }
            foreach (var acc in p.Accessors)
            {
                if (acc.Body is not null)
                {
                    diagnostics.Add(Diagnostic.At("CE0120", acc.Span,
                        $"Interface property '{iface.Name}.{p.Name}' has an accessor " +
                        $"body. A Spek 'interface' declares shape, never behavior — " +
                        $"write '{acc.Kind};' and implement it in the class."));
                }
            }
        }

        // CE0120 — a field is state, which a contract never declares.
        foreach (var f in iface.Fields)
        {
            diagnostics.Add(Diagnostic.At("CE0120", f.Span,
                $"Interface '{iface.Name}' declares a field '{f.Name}'. A Spek " +
                $"'interface' declares shape, never state — move the field to the " +
                $"implementing class, or expose it as a property signature " +
                $"('{f.Type} {f.Name} {{ get; }}')."));
        }

        // CE0093 — base names must resolve to another interface.
        foreach (var baseName in iface.BaseInterfaces)
        {
            if (symbols.ResolveInterface(baseName) is null)
            {
                diagnostics.Add(Diagnostic.At("CE0093", baseName.Span,
                    $"Interface '{iface.Name}' extends unknown interface " +
                    $"'{baseName}'. Declare it with 'interface {baseName} {{ ... }}' " +
                    $"first, or remove it from the base list."));
            }
        }
    }

    /// <summary>
    /// Validates a <c>class</c>: its base list (CE0123) and its abstract-method
    /// well-formedness (CE0122). Inheritance is reuse + abstract-only — only an
    /// <c>abstract class</c> may be a base, and there is no <c>virtual</c> /
    /// <c>override</c> keyword (the emitter infers <c>override</c>). Roslyn does
    /// the rest of the type-checking in the passthrough style (a class that fails
    /// to implement an inherited abstract method surfaces as C# CS0534).
    /// </summary>
    private static void AnalyzeClass(
        ClassDecl cls, SymbolTable symbols, List<Diagnostic> diagnostics)
    {
        // ── base list (CE0123) ──
        ClassDecl? baseClass = null;
        foreach (var b in cls.Bases)
        {
            var asClass     = symbols.ResolveClass(b.Simple);
            var asInterface = symbols.ResolveInterface(b);
            if (asClass is null && asInterface is null)
            {
                diagnostics.Add(Diagnostic.At("CE0123", b.Span,
                    $"Class '{cls.Name}' lists unknown base '{b}'. A base must be an " +
                    $"'abstract class' or a declared 'interface'."));
                continue;
            }
            if (asClass is not null)
            {
                if (baseClass is not null)
                {
                    diagnostics.Add(Diagnostic.At("CE0123", b.Span,
                        $"Class '{cls.Name}' extends more than one base class " +
                        $"('{baseClass.Name}' and '{b}'). A class has a single base " +
                        $"class; the rest of the list must be interfaces."));
                    continue;
                }
                baseClass = asClass;
                if (!asClass.IsAbstract)
                {
                    diagnostics.Add(Diagnostic.At("CE0123", b.Span,
                        $"Class '{cls.Name}' extends '{b}', which is not abstract. " +
                        $"Only an 'abstract class' can be a base (concrete classes " +
                        $"are sealed). Mark '{b}' abstract, or compose it as a field " +
                        $"instead of extending it."));
                }
            }
        }

        // ── abstract-method well-formedness (CE0122) ──
        foreach (var m in cls.Methods)
        {
            if (m.Name is "To" or "TryTo")
            {
                diagnostics.Add(Diagnostic.At("CE0128", m.Span,
                    $"'{m.Name}' is reserved for the Spek conversion family " +
                    $"(x.To<T>() / x.TryTo<T>()), whose calls the compiler " +
                    $"rewrites. Rename the method."));
            }
            if (m.IsAbstract && !cls.IsAbstract)
            {
                diagnostics.Add(Diagnostic.At("CE0122", m.Span,
                    $"Method '{cls.Name}.{m.Name}' is abstract, but '{cls.Name}' is " +
                    $"not an 'abstract class'. Only an abstract class may declare " +
                    $"abstract methods — mark the class 'abstract class {cls.Name}', " +
                    $"or give the method a body."));
            }
            if (m.IsAbstract && m.Visibility == Visibility.Private)
            {
                diagnostics.Add(Diagnostic.At("CE0122", m.Span,
                    $"Abstract method '{cls.Name}.{m.Name}' is private, so no subclass " +
                    $"could implement it. Make it 'public' or 'protected'."));
            }
        }
    }

    /// <summary>
    /// Validates actor inheritance — the same reuse + abstract-only model as
    /// classes. Only an <c>abstract actor</c> may be a base (CE0123); abstract
    /// methods are only allowed on an abstract actor and can't be private
    /// (CE0122). A derived actor implements an inherited abstract method with a
    /// plain method — the emitter infers <c>override</c>.
    /// </summary>
    private static void CheckActorInheritance(
        ActorDecl actor, SymbolTable symbols, List<Diagnostic> diagnostics)
    {
        // The base *actor* is whichever colon name resolves to an actor (the
        // rest are channels; CE0091 handles unknown/duplicate names).
        var colonNames = new List<QualifiedName>();
        if (actor.BaseActor is not null) colonNames.Add(actor.BaseActor);
        colonNames.AddRange(actor.ImplementedChannels);
        foreach (var name in colonNames)
        {
            if (symbols.ResolveActor(name) is { } baseActor && !baseActor.IsAbstract)
            {
                diagnostics.Add(Diagnostic.At("CE0123", name.Span,
                    $"Actor '{actor.Name}' extends '{name}', which is not abstract. " +
                    $"Only an 'abstract actor' can be a base (concrete actors are " +
                    $"sealed). Mark '{name}' abstract, or share the behavior through " +
                    $"a 'channel' instead."));
            }
        }

        foreach (var m in actor.Members.OfType<MethodDecl>())
        {
            if (m.Name is "To" or "TryTo")
            {
                diagnostics.Add(Diagnostic.At("CE0128", m.Span,
                    $"'{m.Name}' is reserved for the Spek conversion family " +
                    $"(x.To<T>() / x.TryTo<T>()), whose calls the compiler " +
                    $"rewrites. Rename the method."));
            }

            if (m.IsAbstract && !actor.IsAbstract)
            {
                diagnostics.Add(Diagnostic.At("CE0122", m.Span,
                    $"Method '{actor.Name}.{m.Name}' is abstract, but '{actor.Name}' " +
                    $"is not an 'abstract actor'. Only an abstract actor may declare " +
                    $"abstract methods — mark it 'abstract actor {actor.Name}', or give " +
                    $"the method a body."));
            }
            if (m.IsAbstract && m.Visibility == Visibility.Private)
            {
                diagnostics.Add(Diagnostic.At("CE0122", m.Span,
                    $"Abstract method '{actor.Name}.{m.Name}' is private, so no " +
                    $"subclass could implement it. Make it 'public' or 'protected'."));
            }
        }
    }

    // ─── CE0126 — never-handled sends (typed sends within a compilation) ────
    //
    // When a Tell/Ask target's concrete actor type is statically known — a
    // local or field whose only origin is `spawn<T>(...)` / `Spawn<T>(...)`,
    // or `self` — the message type is checked against the actor's FULL handled
    // surface: the union of every behavior's handlers across the base-actor
    // chain, with `on any` counting as handling everything and an `abstract
    // message` base handler covering its variants. A message handled in NO
    // behavior is provably dead mail (it can only dead-letter), reported as a
    // CE0126 error. "Handled in another behavior than the current one" is
    // deliberately NOT flagged — that's the `become` state machine at work.
    //
    // Conservative by design: refs of unknown origin (parameters, `sender`,
    // message fields, collections, cross-boundary refs) are silent. Private
    // handlers count only for `self.Tell` (they're not the external surface —
    // same split as CE0096). The cross-assembly story is typed
    // `ActorRef<Channel>` (reserved CE0030); this check is its
    // same-compilation forerunner.

    private sealed record HandledSurface(HashSet<string> Public, HashSet<string> Private, bool HandlesAny);

    // Recognize the syntactic shape `EnumName.Member` for a declared enum —
    // the only shape the flags usage checks reason about (general
    // enum-typed expressions would need a type checker).
    private static (EnumDecl Decl, string Member)? TryEnumLiteral(Expr e, SymbolTable symbols)
    {
        return e switch
        {
            MemberAccessExpr { Target: NameExpr { Name.Parts.Count: 1 } n } ma
                when symbols.ResolveEnum(n.Name.Simple) is { } d
                => (d, ma.Member),
            NameExpr { Name.Parts.Count: 2 } qn
                when symbols.ResolveEnum(qn.Name.Parts[0]) is { } d
                => (d, qn.Name.Parts[1]),
            _ => null,
        };
    }

    // CE0131/CE0132/CE0133 — flags usage checks over every callable body:
    // bitwise operators are gated to flags enums, provably-empty literal
    // intersections are rejected, and the HasFlag family refuses non-flags
    // enums and degenerate None arguments. All checks fire only on literal
    // `EnumName.Member` operands, the shape the footguns actually take.
    private static void CheckFlagsUsage(
        SpekFile file, SymbolTable symbols, List<Diagnostic> diagnostics)
    {
        foreach (var body in CallableBodies(file))
        foreach (var e in ClassSymbols.ExprsIn(body))
        {
            switch (e)
            {
                case BinaryExpr { Op: BinaryOp.BitOr or BinaryOp.BitAnd or BinaryOp.BitXor } bin:
                {
                    var l = TryEnumLiteral(bin.Left, symbols);
                    var r = TryEnumLiteral(bin.Right, symbols);
                    if (l is null || r is null || l.Value.Decl.Name != r.Value.Decl.Name)
                        break;

                    var decl = l.Value.Decl;
                    if (!decl.IsFlags)
                    {
                        var allPow = decl.Members.Count > 0 && decl.Members.All(m =>
                            m.UnionOf is not null
                            || (m.Value is { } v && v > 0 && (v & (v - 1)) == 0));
                        var hint = allPow
                            ? $" Every member is a distinct power of two — did you " +
                              $"mean 'flags enum {decl.Name}'?"
                            : $" Bitwise combination is only meaningful for a " +
                              $"'flags enum'.";
                        diagnostics.Add(Diagnostic.At("CE0131", bin.Span,
                            $"Bitwise operation on enum '{decl.Name}', which is not " +
                            $"a flags enum.{hint}"));
                        break;
                    }

                    if (bin.Op == BinaryOp.BitAnd
                        && l.Value.Member != r.Value.Member
                        && IsSingleBitMember(decl, l.Value.Member)
                        && IsSingleBitMember(decl, r.Value.Member))
                    {
                        diagnostics.Add(Diagnostic.At("CE0132", bin.Span,
                            $"'{decl.Name}.{l.Value.Member} & {decl.Name}." +
                            $"{r.Value.Member}' is always empty — flags members are " +
                            $"disjoint bits. Did you mean '|' to combine them?"));
                    }
                    break;
                }

                case MethodCallExpr { Method: "HasFlag" or "HasAnyFlags" or "HasOnlyFlags" } call:
                {
                    foreach (var arg in call.Args)
                    {
                        if (TryEnumLiteral(arg, symbols) is not { } lit) continue;
                        if (!lit.Decl.IsFlags)
                        {
                            diagnostics.Add(Diagnostic.At("CE0133", call.Span,
                                $"'{call.Method}' with '{lit.Decl.Name}.{lit.Member}' — " +
                                $"'{lit.Decl.Name}' is not a flags enum, so flag tests " +
                                $"are meaningless on it. Compare with '==' instead, or " +
                                $"declare it 'flags enum {lit.Decl.Name}'."));
                        }
                        else if (lit.Member == "None")
                        {
                            // Three different degeneracies: HasFlag(None) is a
                            // tautology, HasAnyFlags(None) a contradiction, and
                            // HasOnlyFlags(None) — "nothing outside the empty
                            // set" — a disguised equality, not a constant.
                            var lesson = call.Method switch
                            {
                                "HasAnyFlags"  => "is always false",
                                "HasOnlyFlags" => $"is a disguised equality — " +
                                                  $"it reduces to '== {lit.Decl.Name}.None'",
                                _              => "is always true",
                            };
                            diagnostics.Add(Diagnostic.At("CE0133", call.Span,
                                $"'{call.Method}({lit.Decl.Name}.None)' {lesson}. " +
                                $"Test emptiness with 'x == {lit.Decl.Name}.None'."));
                        }
                    }
                    break;
                }
            }
        }
    }

    // CE0134 (warning) — direct wall/monotonic time reads inside an ACTOR
    // bypass the system clock, so under the test kit's virtual time they
    // silently diverge from every timer and self.Clock read. Virtual time is
    // only honest if the actor's reads go through the clock; same posture as
    // CE0119, one layer up. Scoped to actor bodies deliberately: program
    // blocks, modules, and classes are host-side code where reading real
    // time is legitimate (they have no self.Clock and no virtual-time
    // guarantee to uphold).
    private static void CheckTimeApis(SpekFile file, List<Diagnostic> diagnostics)
    {
        foreach (var actor in file.Declarations.OfType<ActorDecl>())
        // Handler/method/init/term bodies PLUS stream-operator chains — an
        // operator arg or selector (`throttle(window)`, `distinct(by: x =>
        // self.Clock … )`) is actor code that runs off the turn, and a direct
        // time read there bypasses the clock just the same (red-team H2 — this
        // separate walker had the same operator blind spot as WalkExpr).
        foreach (var e in ActorBodies(actor).SelectMany(ClassSymbols.ExprsIn)
                              .Concat(StreamOperatorExprs(actor)))
        {
            // Dotted statics may parse as a MemberAccessExpr chain OR a
            // multi-part NameExpr (`System.DateTime.UtcNow`), so both shapes
            // are matched; the type name is checked by its last segment.
            var (span, api) = e switch
            {
                MemberAccessExpr { Target: NameExpr t, Member: "Now" or "UtcNow" or "Today" } ma
                    when t.Name.Simple is "DateTime" or "DateTimeOffset"
                    => (ma.Span, $"{t.Name.Simple}.{ma.Member}"),
                MemberAccessExpr { Target: NameExpr t, Member: "TickCount" or "TickCount64" } ma
                    when t.Name.Simple == "Environment"
                    => (ma.Span, $"Environment.{ma.Member}"),
                NameExpr { Name.Parts.Count: >= 2 } n
                    when n.Name.Simple is "Now" or "UtcNow" or "Today"
                         && n.Name.Parts[^2] is "DateTime" or "DateTimeOffset"
                    => (n.Span, $"{n.Name.Parts[^2]}.{n.Name.Simple}"),
                NameExpr { Name.Parts.Count: >= 2 } n
                    when n.Name.Simple is "TickCount" or "TickCount64"
                         && n.Name.Parts[^2] == "Environment"
                    => (n.Span, $"Environment.{n.Name.Simple}"),
                MethodCallExpr { Method: "StartNew" or "GetTimestamp" } mc
                    when mc.Target is NameExpr { Name.Simple: "Stopwatch" }
                    => (mc.Span, $"Stopwatch.{mc.Method}"),
                _ => (default(SourceSpan?), null as string),
            };
            if (api is null || span is null) continue;

            diagnostics.Add(Diagnostic.At("CE0134", span,
                $"'{api}' reads time directly, bypassing the system clock — " +
                $"under virtual time (tests) it silently diverges from timers " +
                $"and other clock reads. Use 'self.Clock.GetUtcNow()' / " +
                $"'self.Clock.GetTimestamp()' instead.",
                DiagnosticSeverity.Warning));
        }
    }

    private static bool IsSingleBitMember(EnumDecl decl, string member)
        => decl.Members.Any(m => m.Name == member && m.UnionOf is null);

    // CE0130 — flags-enum declaration validation. Values are correct by
    // construction or the declaration does not compile: explicit values must
    // be powers of two (so members are provably disjoint bits), unions may
    // only name earlier members, zero members are provided (None) rather than
    // declared (the HasFlag(None) always-true trap), and duplicate bit values
    // are rejected. Plain enums accept any explicit integer, C#-style.
    private static void CheckEnums(SpekFile file, List<Diagnostic> diagnostics)
    {
        foreach (var e in file.Declarations.OfType<EnumDecl>())
        {
            if (!e.IsFlags)
            {
                foreach (var m in e.Members.Where(m => m.UnionOf is not null))
                {
                    diagnostics.Add(Diagnostic.At("CE0130", m.Span,
                        $"Enum member '{e.Name}.{m.Name}' unions other members, " +
                        $"which only a 'flags enum' supports. Declare it " +
                        $"'flags enum {e.Name}'."));
                }
                continue;
            }

            var valueToName = new Dictionary<long, string>();
            var resolved = new Dictionary<string, long>();
            var explicitUsed = new HashSet<long>(
                e.Members.Where(m => m.Value is not null).Select(m => m.Value!.Value));
            long nextPow = 1;

            foreach (var m in e.Members)
            {
                if (m.Name == "None")
                {
                    diagnostics.Add(Diagnostic.At("CE0130", m.Span,
                        $"'{e.Name}.None' is provided automatically as the empty " +
                        $"set (= 0). Remove the declaration; test emptiness with " +
                        $"'x == {e.Name}.None'."));
                    continue;
                }

                if (m.UnionOf is not null)
                {
                    long union = 0;
                    var ok = true;
                    foreach (var part in m.UnionOf)
                    {
                        if (!resolved.TryGetValue(part, out var pv))
                        {
                            diagnostics.Add(Diagnostic.At("CE0130", m.Span,
                                $"'{e.Name}.{m.Name}' unions '{part}', which is not " +
                                $"an earlier member of '{e.Name}'. Unions may only " +
                                $"combine members declared above them."));
                            ok = false;
                            break;
                        }
                        union |= pv;
                    }
                    if (ok) resolved[m.Name] = union;
                    continue;
                }

                long val;
                if (m.Value is not null)
                {
                    val = m.Value.Value;
                    if (val <= 0 || (val & (val - 1)) != 0)
                    {
                        diagnostics.Add(Diagnostic.At("CE0130", m.Span,
                            $"'{e.Name}.{m.Name} = {val}' — a flags member must be " +
                            $"a power of two so members are disjoint bits. Use a " +
                            $"union ('{m.Name} = A | B') for combinations, or drop " +
                            $"the value to auto-assign the next free bit."));
                        continue;
                    }
                }
                else
                {
                    while (explicitUsed.Contains(nextPow)) nextPow <<= 1;
                    val = nextPow;
                    explicitUsed.Add(nextPow);
                }

                if (valueToName.TryGetValue(val, out var other))
                {
                    diagnostics.Add(Diagnostic.At("CE0130", m.Span,
                        $"'{e.Name}.{m.Name}' has the same bit value ({val}) as " +
                        $"'{e.Name}.{other}'. Flags members must be distinct bits; " +
                        $"declare an alias as a union instead."));
                    continue;
                }
                valueToName[val] = m.Name;
                resolved[m.Name] = val;
            }
        }
    }

    private static readonly HashSet<string> ConversionNumericTargets =
    [
        "sbyte", "byte", "short", "ushort", "int", "uint",
        "long", "ulong", "float", "double", "decimal", "char",
    ];

    // CE0138 — the (source → target) numeric widenings that C# performs
    // IMPLICITLY yet cannot do without losing precision: the target's mantissa
    // is narrower than the source's integer range, so large values round.
    // `16777217.To<float>()` == 16777216. `To` promises lossless, so these
    // must route to `TryTo` (null when not exactly representable). Keyword
    // spellings only; NormalizePrimitive folds the System.* aliases in first.
    private static readonly HashSet<(string Source, string Target)> LossyImplicitWidenings =
    [
        ("int", "float"), ("uint", "float"), ("long", "float"), ("ulong", "float"),
        ("nint", "float"), ("nuint", "float"),
        ("long", "double"), ("ulong", "double"), ("nint", "double"), ("nuint", "double"),
    ];

    // BCL type names → the C# keyword spelling used in LossyImplicitWidenings.
    // Pass-through for anything already a keyword or unmapped.
    private static string NormalizePrimitive(string name) => name switch
    {
        "System.Single" or "Single" => "float",
        "System.Double" or "Double" => "double",
        "System.Int32"  or "Int32"  => "int",
        "System.UInt32" or "UInt32" => "uint",
        "System.Int64"  or "Int64"  => "long",
        "System.UInt64" or "UInt64" => "ulong",
        "System.IntPtr" or "IntPtr" => "nint",
        "System.UIntPtr" or "UIntPtr" => "nuint",
        _ => name,
    };

    /// <summary>The numeric primitive a conversion receiver resolves to — for
    /// CE0138. Handles integer literals (by suffix/magnitude), single-part
    /// locals/fields (via the typer), and one level of message-field access
    /// (<c>msg.value</c>). Returns null when the source type can't be pinned
    /// down (a method result, a deep expression) — CE0138 then stays silent
    /// rather than guess.</summary>
    private static string? TryNumericSourceType(Expr receiver, ExpressionTyper typer, SymbolTable symbols)
    {
        while (receiver is ParenExpr p) receiver = p.Inner;
        switch (receiver)
        {
            case IntLiteralExpr lit:
                return IntLiteralType(lit);
            case NameExpr { Name.Parts.Count: 1 } nm:
                return SimpleTypeName(typer.TypeOfName(nm.Name.Simple));
            // `msg.field` parses as a 2-part name (not a MemberAccessExpr):
            // resolve the head binding's message type, then the field.
            case NameExpr { Name.Parts.Count: 2 } nm2
                when typer.TypeOfName(nm2.Name.Parts[0]) is { } recv
                     && symbols.ResolveMessage(recv.Name) is { } m2:
                return SimpleTypeName(
                    m2.Fields.FirstOrDefault(f => f.Name == nm2.Name.Parts[1])?.Type);
            // …and the MemberAccessExpr shape, for parses that produce it.
            case MemberAccessExpr { NullConditional: false } ma
                when typer.TryGetType(ma.Target) is { } recvType
                     && symbols.ResolveMessage(recvType.Name) is { } msg:
                return SimpleTypeName(
                    msg.Fields.FirstOrDefault(f => f.Name == ma.Member)?.Type);
            default:
                return null;
        }
    }

    // A scalar (non-nullable, non-array) primitive's keyword name, or null.
    private static string? SimpleTypeName(TypeRef? t) =>
        t is { IsNullable: false, ArrayRank: 0, TypeArgs.Count: 0 }
            ? NormalizePrimitive(t.Name.Simple)
            : null;

    // The C# type of an integer literal: suffix wins (L/U/UL); otherwise the
    // smallest of int/long that holds the value — C#'s rule for decimal
    // integer literals, which is all the parser produces here.
    private static string IntLiteralType(IntLiteralExpr lit)
    {
        var raw = (lit.Raw ?? "").ToUpperInvariant();
        var u = raw.Contains('U');
        var l = raw.Contains('L');
        if (u && l) return "ulong";
        if (l) return "long";
        if (u) return "uint";
        return lit.Value is >= int.MinValue and <= int.MaxValue ? "int" : "long";
    }

    // CE0127 — the conversion family's target must be something the routing
    // can reason about: a numeric primitive or a declared
    // enum/class/message/interface. Anything else (an external type, a
    // generic, an array) has no checked lowering; the C# interop file is the
    // escape hatch. Swept over every callable body (actors, modules, classes,
    // programs) rather than in the handler walker so module code is covered.
    private static void CheckConversionTargets(
        SpekFile file, SymbolTable symbols, List<Diagnostic> diagnostics)
    {
        // Callable bodies PLUS stream-operator chains: a conversion or cast in
        // an operator arg or selector (`throttle(n.To<int>())`, `distinct(by:
        // x => (long)x.n)`) is actor code too, and was a walker blind spot —
        // CallableBodies yields only statement bodies (red-team H2, same root
        // as the CE0119/CE0134 leaks).
        var exprs = CallableBodies(file).SelectMany(ClassSymbols.ExprsIn)
                        .Concat(AllStreamOperatorExprs(file))
                        .ToList();

        foreach (var e in exprs)
        {
            if (e is not MethodCallExpr { Method: "To" or "TryTo" } call
                || call.TypeArgs.Count != 1)
            {
                continue;
            }

            var t = call.TypeArgs[0].ToString();
            var bare = !t.Contains('.') && !t.Contains('<')
                       && !t.Contains('[') && !t.EndsWith('?');
            var known = ConversionNumericTargets.Contains(t)
                || (bare && (symbols.ResolveEnum(t) is not null
                             || symbols.ResolveClass(t) is not null
                             || symbols.ResolveMessage(t) is not null
                             || symbols.ResolveInterface(t) is not null));
            if (!known)
            {
                diagnostics.Add(Diagnostic.At("CE0127", call.Span,
                    $"'{call.Method}<{t}>' — conversion target '{t}' must be a " +
                    $"numeric primitive or a declared enum, class, message, or " +
                    $"interface. Convert external types in a C# interop file."));
            }
        }

        // CE0129 — Spek has no cast operator. The parser recognizes the
        // C# shape purely so this diagnostic can teach the replacement:
        // casts hide three risk profiles (silent wraparound, runtime
        // downcast failure, intentional truncation) that the conversion
        // family splits into checked spellings.
        foreach (var e in exprs)
        {
            if (e is not TypeOpExpr { Kind: TypeOpKind.Cast } cast) continue;

            var t = cast.Type.ToString();
            string help;
            if (ConversionNumericTargets.Contains(t))
            {
                help = $"Use x.To<{t}>() when the conversion is lossless, or " +
                       $"x.TryTo<{t}>() ({t}?) when it can lose information.";
            }
            else if (symbols.ResolveEnum(t) is not null)
            {
                help = $"Use x.TryTo<{t}>() ({t}?) — null when the enum does " +
                       $"not define the value.";
            }
            else if (symbols.ResolveClass(t) is not null
                     || symbols.ResolveMessage(t) is not null
                     || symbols.ResolveInterface(t) is not null)
            {
                help = $"Test the type instead: 'x is {t} v' or 'x as {t}'.";
            }
            else
            {
                help = $"lossless: x.To<T>() — fallible: x.TryTo<T>() — " +
                       $"type test: x is T v / x as T.";
            }

            diagnostics.Add(Diagnostic.At("CE0129", cast.Span,
                $"'({t})' — Spek has no cast operator. {help}"));
        }
    }

    private sealed class SendScope
    {
        // local -> actor simple name (origin is a spawn of that actor type)
        public Dictionary<string, string> RefLocals { get; } = new(StringComparer.Ordinal);
        // local -> message simple name (origin is `new M(...)`)
        public Dictionary<string, string> MsgLocals { get; } = new(StringComparer.Ordinal);
        // locals of unknown value (params, bindings, reassigned) — these also
        // shadow any same-named field, so lookup must stop here.
        public HashSet<string> Masked { get; } = new(StringComparer.Ordinal);

        public SendScope Clone()
        {
            var c = new SendScope();
            foreach (var (k, v) in RefLocals) c.RefLocals[k] = v;
            foreach (var (k, v) in MsgLocals) c.MsgLocals[k] = v;
            foreach (var m in Masked) c.Masked.Add(m);
            return c;
        }

        public void SetRef(string name, string actorName)
        { Masked.Remove(name); MsgLocals.Remove(name); RefLocals[name] = actorName; }

        public void SetMsg(string name, string messageName)
        { Masked.Remove(name); RefLocals.Remove(name); MsgLocals[name] = messageName; }

        public void SetUnknown(string name)
        { RefLocals.Remove(name); MsgLocals.Remove(name); Masked.Add(name); }

        public bool IsLocal(string name)
            => RefLocals.ContainsKey(name) || MsgLocals.ContainsKey(name) || Masked.Contains(name);
    }

    private static void CheckNeverHandledSends(
        SpekFile file, SymbolTable symbols, List<Diagnostic> diagnostics)
    {
        var cache = new Dictionary<string, HandledSurface>(StringComparer.Ordinal);
        var noFields = new Dictionary<string, string>(StringComparer.Ordinal);

        foreach (var actor in file.Declarations.OfType<ActorDecl>())
        {
            var fields = TrackSpawnedRefFields(actor, symbols);
            foreach (var member in actor.Members)
            {
                switch (member)
                {
                    case InitBlock init:
                        WalkSendBlock(init.Body, MaskedScope(init.Parameters.Select(p => p.Name)),
                            fields, actor, symbols, cache, diagnostics);
                        break;
                    case TermBlock term:
                        WalkSendBlock(term.Body, new SendScope(), fields, actor, symbols, cache, diagnostics);
                        break;
                    case MethodDecl method:
                        WalkSendBlock(method.Body, MaskedScope(method.Parameters.Select(p => p.Name)),
                            fields, actor, symbols, cache, diagnostics);
                        break;
                    case LifecycleHook hook:
                        WalkSendHandlerBody(hook.Body, new SendScope(), fields, actor, symbols, cache, diagnostics);
                        break;
                    case BehaviorDecl beh:
                        foreach (var h in beh.Handlers)
                        {
                            var scope = new SendScope();
                            switch (h.Pattern)
                            {
                                case NamedBindPattern nb: scope.Masked.Add(nb.Binding); break;
                                case CatchAllPattern ca:  scope.Masked.Add(ca.Binding); break;
                                case EventPattern ep:
                                    foreach (var p in ep.Parameters) scope.Masked.Add(p.Name);
                                    break;
                            }
                            WalkSendHandlerBody(h.Body, scope, fields, actor, symbols, cache, diagnostics);
                        }
                        break;
                }
            }
        }

        foreach (var prog in file.Declarations.OfType<ProgramDecl>())
            WalkSendBlock(prog.Body, new SendScope(), noFields, null, symbols, cache, diagnostics);
    }

    private static SendScope MaskedScope(IEnumerable<string> names)
    {
        var s = new SendScope();
        foreach (var n in names) s.Masked.Add(n);
        return s;
    }

    /// <summary>
    /// Fields of an actor whose <em>every</em> assignment (initializer plus all
    /// writes anywhere in the actor) is a spawn of one consistent actor type.
    /// Anything else — assigned from a parameter, a message field, mixed spawn
    /// types — is untracked and stays silent.
    /// </summary>
    private static Dictionary<string, string> TrackSpawnedRefFields(ActorDecl actor, SymbolTable symbols)
    {
        var candidate = new Dictionary<string, string>(StringComparer.Ordinal);
        var poisoned  = new HashSet<string>(StringComparer.Ordinal);
        var fieldNames = actor.Members.OfType<FieldDecl>().Select(f => f.Name)
            .ToHashSet(StringComparer.Ordinal);

        void Consider(string name, Expr rhs)
        {
            if (poisoned.Contains(name)) return;
            var spawned = SpawnedActorName(rhs, symbols);
            if (spawned is null) { poisoned.Add(name); candidate.Remove(name); return; }
            if (candidate.TryGetValue(name, out var prior) && prior != spawned)
            { poisoned.Add(name); candidate.Remove(name); return; }
            candidate[name] = spawned;
        }

        foreach (var f in actor.Members.OfType<FieldDecl>())
            if (f.Initializer is not null && fieldNames.Contains(f.Name))
                Consider(f.Name, f.Initializer);

        foreach (var e in AllActorExprs(actor))
            if (e is AssignExpr { Op: AssignOp.Assign } a)
            {
                var target = a.Left switch
                {
                    NameExpr n when n.Name.Parts.Count == 1 => n.Name.Simple,
                    MemberAccessExpr { Target: SelfExpr, Member: var m } => m,
                    _ => null,
                };
                if (target is not null && fieldNames.Contains(target))
                    Consider(target, a.Right);
            }

        return candidate;
    }

    /// <summary>
    /// Every expression anywhere in the actor — block bodies (via
    /// <see cref="ActorBodies"/>) plus inline (`=> expr`) handler and
    /// lifecycle bodies, which ActorBodies skips.
    /// </summary>
    private static IEnumerable<Expr> AllActorExprs(ActorDecl actor)
    {
        foreach (var body in ActorBodies(actor))
            foreach (var e in ClassSymbols.ExprsIn(body))
                yield return e;

        foreach (var member in actor.Members)
            switch (member)
            {
                case LifecycleHook { Body: InlineHandlerBody il }:
                    foreach (var e in ClassSymbols.ExprsIn(il.Expr)) yield return e;
                    break;
                case BehaviorDecl beh:
                    foreach (var h in beh.Handlers)
                        if (h.Body is InlineHandlerBody ih)
                            foreach (var e in ClassSymbols.ExprsIn(ih.Expr)) yield return e;
                    break;
            }
    }

    private static void WalkSendHandlerBody(
        HandlerBody body, SendScope scope, IReadOnlyDictionary<string, string> fields,
        ActorDecl? actor, SymbolTable symbols,
        Dictionary<string, HandledSurface> cache, List<Diagnostic> diagnostics)
    {
        switch (body)
        {
            case BlockHandlerBody b:
                WalkSendBlock(b.Block, scope, fields, actor, symbols, cache, diagnostics);
                break;
            case InlineHandlerBody i:
                ScanForBadSends(i.Expr, scope, fields, actor, symbols, cache, diagnostics);
                break;
        }
    }

    private static void WalkSendBlock(
        BlockStmt block, SendScope scope, IReadOnlyDictionary<string, string> fields,
        ActorDecl? actor, SymbolTable symbols,
        Dictionary<string, HandledSurface> cache, List<Diagnostic> diagnostics)
    {
        foreach (var stmt in block.Statements)
            StepSendStmt(stmt, scope, fields, actor, symbols, cache, diagnostics);
    }

    private static void StepSendStmt(
        Stmt stmt, SendScope scope, IReadOnlyDictionary<string, string> fields,
        ActorDecl? actor, SymbolTable symbols,
        Dictionary<string, HandledSurface> cache, List<Diagnostic> diagnostics)
    {
        void Scan(Expr e) => ScanForBadSends(e, scope, fields, actor, symbols, cache, diagnostics);
        void Recurse(BlockStmt b, params string[] extraMasked)
        {
            var child = scope.Clone();
            foreach (var m in extraMasked) child.Masked.Add(m);
            WalkSendBlock(b, child, fields, actor, symbols, cache, diagnostics);
            MaskAssignedLocals(scope, b);
        }

        switch (stmt)
        {
            case VarDeclStmt v:
                Scan(v.Initializer);
                ClassifyBinding(scope, v.Name, v.Initializer, symbols);
                break;

            case ExpressionStmt es:
                Scan(es.Expr);
                if (es.Expr is AssignExpr { Op: AssignOp.Assign, Left: NameExpr ln } a
                    && ln.Name.Parts.Count == 1 && scope.IsLocal(ln.Name.Simple))
                    ClassifyBinding(scope, ln.Name.Simple, a.Right, symbols);
                break;

            case BlockStmt b:   Recurse(b); break;
            case IfStmt i:
                Scan(i.Condition);
                Recurse(i.Then);
                if (i.Else is not null)
                {
                    var child = scope.Clone();
                    StepSendStmt(i.Else, child, fields, actor, symbols, cache, diagnostics);
                    MaskAssignedLocals(scope, i.Else);
                }
                break;
            case WhileStmt w:   Scan(w.Condition); Recurse(w.Body); break;
            case DoWhileStmt d: Scan(d.Condition); Recurse(d.Body); break;
            case ForStmt f:
                Scan(f.Init.Initializer); Scan(f.Condition); Scan(f.Increment);
                Recurse(f.Body, f.Init.Name);
                break;
            case ForeachStmt fe:
                Scan(fe.Collection);
                Recurse(fe.Body, fe.Name);
                break;
            case TryStmt ts:
                Recurse(ts.Try);
                foreach (var c in ts.Catches)
                {
                    if (c.When is not null) Scan(c.When);
                    Recurse(c.Body, c.Binding is null ? [] : [c.Binding]);
                }
                if (ts.Finally is not null) Recurse(ts.Finally);
                break;
            case SwitchStmt sw:
                Scan(sw.Subject);
                foreach (var sec in sw.Sections)
                {
                    foreach (var l in sec.Labels)
                        if (l.Guard is not null) Scan(l.Guard);
                    var child = scope.Clone();
                    foreach (var s in sec.Body)
                        StepSendStmt(s, child, fields, actor, symbols, cache, diagnostics);
                    foreach (var s in sec.Body) MaskAssignedLocals(scope, s);
                }
                break;

            default:
                // return / throw / become / persist / break / continue —
                // scan any contained expressions for sends.
                foreach (var e in TopLevelExprsOf(stmt)) Scan(e);
                break;
        }
    }

    private static IEnumerable<Expr> TopLevelExprsOf(Stmt stmt) => stmt switch
    {
        ReturnStmt r when r.Value is not null => [r.Value],
        ThrowStmt t when t.Value is not null  => [t.Value],
        _ => [],
    };

    private static void ClassifyBinding(SendScope scope, string name, Expr rhs, SymbolTable symbols)
    {
        var spawned = SpawnedActorName(rhs, symbols);
        if (spawned is not null) { scope.SetRef(name, spawned); return; }
        var msg = StaticMessageName(rhs, scope, symbols);
        if (msg is not null) { scope.SetMsg(name, msg); return; }
        scope.SetUnknown(name);
    }

    /// <summary>The actor type a spawn-shaped expression produces, else null.</summary>
    private static string? SpawnedActorName(Expr e, SymbolTable symbols)
    {
        while (e is ParenExpr p) e = p.Inner;
        return e switch
        {
            SpawnExpr sp when sp.TypeArgs.Count == 1
                && symbols.ResolveActor(sp.TypeArgs[0].Name.Simple) is not null
                => sp.TypeArgs[0].Name.Simple,
            MethodCallExpr mc when mc.TypeArgs.Count == 1
                && mc.Method is "Spawn" or "SpawnPersistent" or "SpawnNamed"
                             or "SpawnAsync" or "SpawnPersistentAsync"
                && symbols.ResolveActor(mc.TypeArgs[0].Name.Simple) is not null
                => mc.TypeArgs[0].Name.Simple,
            _ => null,
        };
    }

    /// <summary>The declared-message type of a payload expression, else null.</summary>
    private static string? StaticMessageName(Expr e, SendScope scope, SymbolTable symbols)
    {
        while (e is ParenExpr p) e = p.Inner;
        return e switch
        {
            NewExpr n when symbols.ResolveMessage(n.Type) is not null => n.Type.Simple,
            NameExpr nm when nm.Name.Parts.Count == 1
                && scope.MsgLocals.TryGetValue(nm.Name.Simple, out var m) => m,
            _ => null,
        };
    }

    /// <summary>
    /// Recursive send scan. Mirrors <see cref="ClassSymbols.ExprsIn(Expr)"/> but
    /// stays scope-aware: lambda parameters mask same-named tracked locals
    /// inside the lambda body.
    /// </summary>
    private static void ScanForBadSends(
        Expr expr, SendScope scope, IReadOnlyDictionary<string, string> fields,
        ActorDecl? actor, SymbolTable symbols,
        Dictionary<string, HandledSurface> cache, List<Diagnostic> diagnostics)
    {
        void Scan(Expr e) => ScanForBadSends(e, scope, fields, actor, symbols, cache, diagnostics);

        switch (expr)
        {
            case MethodCallExpr { Method: "Tell" } mc when mc.Args.Count >= 1:
                CheckSend(mc.Target, mc.Args[0], mc.Span, scope, fields, actor, symbols, cache, diagnostics);
                Scan(mc.Target);
                foreach (var a in mc.Args) Scan(a);
                return;
            case AskExpr ask:
                CheckSend(ask.Target, ask.Message, ask.Span, scope, fields, actor, symbols, cache, diagnostics);
                Scan(ask.Target);
                Scan(ask.Message);
                return;
            case LambdaExpr lam:
            {
                var child = scope.Clone();
                foreach (var p in lam.Parameters) child.Masked.Add(p.Name);
                switch (lam.Body)
                {
                    case Expr ex:
                        ScanForBadSends(ex, child, fields, actor, symbols, cache, diagnostics);
                        break;
                    case BlockStmt bs:
                        WalkSendBlock(bs, child, fields, actor, symbols, cache, diagnostics);
                        break;
                }
                return;
            }
        }

        // Everything else: recurse into children the same way ExprsIn does.
        foreach (var child in ClassSymbols.ExprsIn(expr).Skip(1))
        {
            // ExprsIn flattens the whole subtree, so recursing on each child
            // would double-visit; instead re-dispatch only sends and lambdas.
            switch (child)
            {
                case MethodCallExpr { Method: "Tell" } mc when mc.Args.Count >= 1:
                    CheckSend(mc.Target, mc.Args[0], mc.Span, scope, fields, actor, symbols, cache, diagnostics);
                    break;
                case AskExpr ask:
                    CheckSend(ask.Target, ask.Message, ask.Span, scope, fields, actor, symbols, cache, diagnostics);
                    break;
            }
        }
    }

    private static void CheckSend(
        Expr target, Expr payload, SourceSpan sendSpan,
        SendScope scope, IReadOnlyDictionary<string, string> fields,
        ActorDecl? actor, SymbolTable symbols,
        Dictionary<string, HandledSurface> cache, List<Diagnostic> diagnostics)
    {
        while (target is ParenExpr p) target = p.Inner;

        string? targetActor = null;
        var includePrivate = false;

        switch (target)
        {
            case SelfExpr when actor is not null:
                targetActor = actor.Name;
                includePrivate = true;   // self.Tell is the one legal private route
                break;
            case NameExpr n when n.Name.Parts.Count == 1:
                var name = n.Name.Simple;
                if (scope.Masked.Contains(name)) return;                      // shadowed / unknown
                if (scope.RefLocals.TryGetValue(name, out var fromLocal)) targetActor = fromLocal;
                else if (fields.TryGetValue(name, out var fromField)) targetActor = fromField;
                break;
        }
        if (targetActor is null) return;
        if (symbols.ResolveActor(targetActor) is not { } decl) return;

        var msg = StaticMessageName(payload, scope, symbols);
        if (msg is null) return;   // unknown payload type — CE0020's territory

        var surface = HandledSurfaceOf(decl, symbols, cache);
        if (SurfaceHandles(surface, msg, includePrivate, symbols)) return;

        diagnostics.Add(Diagnostic.At("CE0126", sendSpan,
            $"Actor '{targetActor}' has no handler for message '{msg}' in any behavior — " +
            $"this send can never be processed and will dead-letter. Add an " +
            $"'on {msg}' handler to '{targetActor}', fix the send target, or (for a " +
            $"deliberate pass-through) give '{targetActor}' an 'on any' catch-all."));
    }

    /// <summary>
    /// Union of an actor's handled message names across every behavior and its
    /// base-actor chain, split by handler visibility, with `on any` collapsing
    /// the whole check.
    /// </summary>
    private static HandledSurface HandledSurfaceOf(
        ActorDecl actor, SymbolTable symbols, Dictionary<string, HandledSurface> cache)
    {
        if (cache.TryGetValue(actor.Name, out var hit)) return hit;

        var pub = new HashSet<string>(StringComparer.Ordinal);
        var priv = new HashSet<string>(StringComparer.Ordinal);
        var any = false;

        var seen = new HashSet<string>(StringComparer.Ordinal);
        var current = actor;
        while (current is not null && seen.Add(current.Name))
        {
            foreach (var beh in current.Members.OfType<BehaviorDecl>())
                foreach (var h in beh.Handlers)
                    switch (h.Pattern)
                    {
                        case CatchAllPattern: any = true; break;
                        case NamedBindPattern nb:
                            (h.Visibility == Visibility.Private ? priv : pub).Add(nb.MessageType.Simple);
                            break;
                        case NoBindPattern nbp:
                            (h.Visibility == Visibility.Private ? priv : pub).Add(nbp.MessageType.Simple);
                            break;
                    }
            current = ResolveBaseActorDecl(current, symbols);
        }

        var surface = new HandledSurface(pub, priv, any);
        cache[actor.Name] = surface;
        return surface;
    }

    /// <summary>
    /// True when the surface handles <paramref name="messageName"/> or any of
    /// its `abstract message` ancestors (an `on Base` handler receives every
    /// variant).
    /// </summary>
    private static bool SurfaceHandles(
        HandledSurface surface, string messageName, bool includePrivate, SymbolTable symbols)
    {
        if (surface.HandlesAny) return true;

        var seen = new HashSet<string>(StringComparer.Ordinal);
        var current = messageName;
        while (current is not null && seen.Add(current))
        {
            if (surface.Public.Contains(current)) return true;
            if (includePrivate && surface.Private.Contains(current)) return true;
            current = symbols.ResolveMessage(current)?.BaseMessage?.Simple;
        }
        return false;
    }

    /// <summary>The base *actor* in the colon list (the rest are channels).</summary>
    private static ActorDecl? ResolveBaseActorDecl(ActorDecl actor, SymbolTable symbols)
    {
        var names = new List<QualifiedName>();
        if (actor.BaseActor is not null) names.Add(actor.BaseActor);
        names.AddRange(actor.ImplementedChannels);
        foreach (var n in names)
            if (symbols.ResolveActor(n) is { } a) return a;
        return null;
    }

    /// <summary>
    /// After a conditionally-executed nested scope, any parent-visible local it
    /// assigns becomes unknown in the parent — the branch may or may not have
    /// run, so its value can't be trusted either way.
    /// </summary>
    private static void MaskAssignedLocals(SendScope parent, Stmt nested)
    {
        foreach (var e in ClassSymbols.ExprsIn(nested))
            if (e is AssignExpr { Left: NameExpr n } && n.Name.Parts.Count == 1
                && parent.IsLocal(n.Name.Simple))
                parent.SetUnknown(n.Name.Simple);
    }

    /// <summary>
    /// Compilation-wide duplicate-declaration check. The single-file
    /// <see cref="CheckDuplicateDeclarations"/> catches duplicates inside
    /// one file; this one catches message / actor names that collide
    /// across different files within the same <c>SpekCompilation</c>.
    /// Reports CE0013 on the first cross-file collision for each name
    /// and then stops (one diagnostic per duplicated name).
    /// </summary>
    public static IReadOnlyList<Diagnostic> CheckCrossFileDuplicates(IReadOnlyList<SpekFile> files)
    {
        var diagnostics = new List<Diagnostic>();
        if (files.Count < 2) return diagnostics;

        // Namespace-scoped dedup: the key is (namespace, simple-name)
        // so two files declaring `message Ping();` under different
        // `namespace A;` vs `namespace B;` produce two distinct C# types
        // (`A.Ping` and `B.Ping`) and don't trip CE0013. Files without an
        // explicit namespace share a sentinel "" namespace bucket, so two
        // unscoped files declaring the same name still flag.
        var firstSeen = new Dictionary<(string Namespace, string Name),
                                       (SpekFile File, AstNode Decl)>();
        var reported  = new HashSet<(string, string)>();

        foreach (var file in files)
        {
            var fileNs = file.Namespace?.Name.ToString() ?? "";
            foreach (var decl in file.Declarations)
            {
                var (name, kind) = decl switch
                {
                    MessageDecl m      => (m.Name, "message"),
                    ActorDecl   a      => (a.Name, "actor"),
                    EnumDecl    e      => (e.Name, "enum"),
                    ClassDecl   c      => (c.Name, "class"),
                    ModuleDecl  mo     => (mo.Name, "module"),
                    ChannelDecl ch     => (ch.Name, "channel"),
                    InterfaceDecl i    => (i.Name, "interface"),
                    SharedRegionDecl s => (s.Name, "shared region"),
                    _                  => ((string?)null, ""),
                };
                if (name is null) continue;
                var key = (fileNs, name);

                if (firstSeen.TryGetValue(key, out var first))
                {
                    // Same-file duplicates are the per-file check's job.
                    if (ReferenceEquals(first.File, file)) continue;
                    if (!reported.Add(key)) continue;

                    var nsPart = fileNs.Length > 0 ? $"{fileNs}." : "";
                    diagnostics.Add(Diagnostic.At("CE0013", decl.Span,
                        $"Duplicate {kind} declaration '{nsPart}{name}' across files " +
                        $"(previously declared in another file at " +
                        $"{first.Decl.Span.StartLine}:{first.Decl.Span.StartColumn})."));
                }
                else
                {
                    firstSeen[key] = (file, decl);
                }
            }
        }

        return diagnostics;
    }

    // ─── CE0080 — hostile namespace imports ─────────────────────────────────

    /// <summary>
    /// Namespaces a Spek file isn't allowed to <c>using</c>-import because they
    /// can reach through Spek's compile-time guarantees — reflection can
    /// mutate fields we classified as immutable, unsafe can do pointer stuff,
    /// the registry / marshaller APIs escape the sandbox entirely.
    /// Match rule: an import is hostile if its qualified name *starts with*
    /// one of these prefixes (so e.g. banning <c>System.Reflection</c> also
    /// catches <c>System.Reflection.Emit</c>).
    /// </summary>
    private static readonly string[] HostileNamespacePrefixes =
    [
        "System.Reflection",
        "System.Reflection.Emit",
        "System.Runtime.CompilerServices",
        "System.Runtime.InteropServices",
        "System.Runtime.Serialization",
        "Microsoft.Win32",
    ];

    // ─── CE0083 / CE0084 — runtime call blocklists ──────────────────────────

    /// <summary>
    /// Static-style calls (Type.Method) that block the dispatcher
    /// thread. Keyed on "Type.Method"; the value is the message tail
    /// shown to the user (the leading <c>'X.Y' </c> is prepended by
    /// the diagnostic).
    /// </summary>
    private static readonly Dictionary<string, string> DispatcherBlockingStaticCalls =
        new(StringComparer.Ordinal)
        {
            ["Thread.Sleep"]      = "blocks the dispatcher thread; use 'await Task.Delay(...)' or model the wait as a delayed message instead",
            ["Task.Wait"]         = "synchronously waits on a Task; await it instead",
            ["Task.WaitAll"]      = "synchronously waits on tasks; use 'await Task.WhenAll(...)' instead",
            ["Task.WaitAny"]      = "synchronously waits on tasks; use 'await Task.WhenAny(...)' instead",
            ["Console.ReadLine"]  = "blocks the dispatcher waiting for terminal input — never appropriate inside an actor handler",
            ["Console.ReadKey"]   = "blocks the dispatcher waiting for terminal input — never appropriate inside an actor handler",
            ["Monitor.Wait"]      = "blocks the dispatcher waiting for a pulse — use message passing for coordination",
            ["SpinWait.SpinUntil"]= "spins the dispatcher thread until a condition holds — await the underlying signal instead",
        };

    /// <summary>
    /// Bare instance method names that, when called with <c>.Wait()</c>
    /// / <c>.WaitAll(...)</c> / <c>.GetAwaiter().GetResult()</c>
    /// shape, are diagnostic-worthy regardless of static target. Keyed
    /// on the method simple-name. False-positive rate on these names
    /// in Spek code is low — actors don't share the .NET threading
    /// vocabulary.
    /// </summary>
    private static readonly Dictionary<string, string> DispatcherBlockingInstanceMethods =
        new(StringComparer.Ordinal)
        {
            // NB: `.Wait()` and `.GetResult()` are NOT here — the invisible-async
            // pass rewrites them into `await` (AsyncRewriter.TryRewriteSyncOverAsync),
            // so they're fixed, not errored. The instance `.WaitAll()` /
            // `.WaitAny()` shapes are unusual and have no value-preserving rewrite
            // (WaitAny returns an index, not the Task), so they stay errors.
            ["WaitAll"]      = "synchronously waits on tasks; use 'await Task.WhenAll(...)' instead",
            ["WaitAny"]      = "synchronously waits on tasks; use 'await Task.WhenAny(...)' instead",
            // Distinctly-named blocking-wait primitives (no collision with the
            // rewritten Task `.Wait()`): WaitHandle/ManualResetEvent.WaitOne,
            // Barrier.SignalAndWait. NB: `Semaphore(Slim).Wait()` is shaped like
            // the rewritten `.Wait()`, so it's not caught here — see the
            // async-footguns doc's known-gap note.
            ["WaitOne"]      = "blocks the dispatcher on a wait handle — coordinate with messages instead",
            ["SignalAndWait"]= "blocks the dispatcher at a barrier — coordinate with messages instead",
        };

    /// <summary>
    /// Static-style calls (Type.Method) that terminate or escape the
    /// hosting process — bypassing supervision and killing other
    /// actors in the same system. Same shape as
    /// <see cref="DispatcherBlockingStaticCalls"/>.
    /// </summary>
    private static readonly Dictionary<string, string> ProcessEscapeStaticCalls =
        new(StringComparer.Ordinal)
        {
            ["Environment.Exit"]     = "terminates the entire process, killing every other actor in this system — call 'self.System.Shutdown()' to wind the node down gracefully instead",
            ["Environment.FailFast"] = "terminates the entire process; let supervision handle failures via 'on Failure', or call 'self.System.Shutdown()' for an intentional graceful stop",
            ["Process.Kill"]         = "killing processes from inside an actor handler bypasses supervision — call 'self.System.Shutdown()' to drive a graceful node shutdown instead",
            ["Process.GetCurrentProcess"] = "reaching the current Process from an actor invites unscoped lifecycle changes; route shutdown through your hosting adapter instead",
        };

    /// <summary>
    /// CE0115 (warning). Synchronous BCL I/O calls that block the
    /// dispatcher thread but have a drop-in <c>*Async</c> sibling the
    /// invisible-async pass then awaits — same value, same shape, just the
    /// suffix. Unlike CE0083 (hard error: no async form, or never acceptable)
    /// this is a <i>warning</i>: sync I/O is occasionally legitimate (a small
    /// config read in <c>init</c>, where no await context exists), so the dev
    /// decides. The quick-fix appends <c>Async</c>.
    ///
    /// Kept to the curated <c>File.*</c> static surface — high signal, low
    /// false-positive (bare <c>Read</c>/<c>Write</c> instance names collide
    /// with too much). Fires only on the bare <c>File.Method</c> form, like
    /// the CE0083 static blocklist.
    /// </summary>
    private static readonly HashSet<string> SyncIoStaticCallsWithAsyncSibling =
        new(StringComparer.Ordinal)
        {
            "File.ReadAllText",  "File.ReadAllBytes",  "File.ReadAllLines",
            "File.WriteAllText", "File.WriteAllBytes", "File.AppendAllText",
        };

    /// <summary>
    /// CE0119 (error). Calls that spawn raw concurrency: a delegate run
    /// on a thread-pool / OS thread <i>outside</i> any actor's turn, bypassing
    /// the serialization that makes Spek race-free and re-opening the data races
    /// the language exists to prevent. Forbidden in Spek source — concurrency
    /// comes from actors. Keyed "Type.Method" like
    /// <see cref="DispatcherBlockingStaticCalls"/>. The constructor forms
    /// (<c>new Thread</c> / <c>new Timer</c>) are caught in the NewExpr walk;
    /// the chained <c>Task.Factory.StartNew</c> form is a documented gap
    /// (<see cref="TryGetStaticCallName"/> only reduces single-part targets).
    /// </summary>
    private static readonly Dictionary<string, string> ConcurrencySpawningStaticCalls =
        new(StringComparer.Ordinal)
        {
            ["Task.Run"]                     = "starts work on a thread-pool thread that runs outside any actor's turn — Spek's unit of concurrency is the actor: 'spawn<Worker>()' a child and 'Tell' it the work instead",
            ["Parallel.For"]                 = "runs iterations on multiple threads outside any actor's turn — fan the work out to a pool of child actors and collect the replies instead",
            ["Parallel.ForEach"]             = "runs iterations on multiple threads outside any actor's turn — fan the work out to a pool of child actors and collect the replies instead",
            ["Parallel.Invoke"]              = "runs the delegates on multiple threads outside any actor's turn — model each as a child actor and 'Tell' it the work instead",
            ["ThreadPool.QueueUserWorkItem"] = "queues work to a thread-pool thread outside any actor's turn — 'spawn<Worker>()' a child and 'Tell' it the work instead",
        };

    /// <summary>
    /// CE0119 (error), instance-shaped. Same family as
    /// <see cref="ConcurrencySpawningStaticCalls"/> but keyed on the bare
    /// method simple-name, the way <see cref="DispatcherBlockingInstanceMethods"/>
    /// catches <c>task.Wait()</c>-style calls: the receiver's type is
    /// unknowable, but these names don't collide with anything actors write.
    /// <c>.AsParallel()</c> is PLINQ's entry point — every operator downstream
    /// of it runs on thread-pool threads outside any actor's turn, so it is
    /// raw parallelism by another spelling.
    /// </summary>
    private static readonly Dictionary<string, string> ConcurrencySpawningInstanceMethods =
        new(StringComparer.Ordinal)
        {
            ["AsParallel"] = "turns the query into PLINQ, which runs its operators on multiple thread-pool threads outside any actor's turn — raw parallelism by another spelling. Iterate with 'foreach' in the handler, or fan the work out to a pool of child actors and collect the replies instead",
        };

    /// <summary>
    /// CE0087 — the in-place-mutating method names of the common BCL mutable
    /// collections (<c>List</c>, <c>Dictionary</c>, <c>HashSet</c>,
    /// <c>Queue</c>, <c>Stack</c>, <c>LinkedList</c>, <c>StringBuilder</c>,
    /// <c>ConcurrentDictionary</c>). A reader handler calling one of these on a
    /// mutable actor/region field writes shared state and races other readers.
    /// <para>
    /// This is a NAME table, matched with no receiver-type check, so it is kept
    /// deliberately conservative: every name here mutates in place on its
    /// owning collection AND has no widely-used pure/immutable counterpart of
    /// the same name. Names that collide with a pure LINQ or immutable-builder
    /// operator — <c>Append</c>/<c>Prepend</c> (<c>Enumerable.Append</c>),
    /// <c>Reverse</c> (<c>Enumerable.Reverse</c>), <c>Union</c>/<c>Except</c>/
    /// <c>Intersect</c> (LINQ), <c>SetItem</c>/<c>Add</c>-on-<c>Immutable*</c>
    /// (returns a new collection) — are OMITTED so a pure call is never flagged.
    /// The honest residue: a genuinely-mutating call spelled with an unlisted
    /// name, or a mutation through an <c>Immutable*.Add</c>-style API, is not
    /// caught (name-table honesty, same stance as CE0119 / CE0136).
    /// </para>
    /// </summary>
    private static readonly HashSet<string> MutatingBclMethodNames =
        new(StringComparer.Ordinal)
        {
            // List / IList
            "Add", "AddRange", "Insert", "InsertRange",
            "Remove", "RemoveAt", "RemoveRange", "RemoveAll", "Sort",
            // LinkedList
            "AddFirst", "AddLast", "AddBefore", "AddAfter", "RemoveFirst", "RemoveLast",
            // Dictionary / HashSet / ISet
            "Clear", "TryAdd",
            "UnionWith", "IntersectWith", "ExceptWith", "SymmetricExceptWith",
            // Queue / Stack / concurrent collections
            "Push", "Pop", "Enqueue", "Dequeue",
            "TryPop", "TryPush", "TryTake", "TryDequeue", "TryRemove", "TryUpdate",
            "GetOrAdd", "AddOrUpdate",
            // StringBuilder (in-place; `Append` is omitted — it collides with
            // the pure `Enumerable.Append`)
            "AppendLine", "AppendFormat", "AppendJoin",
        };

    /// <summary>
    /// The ambient <c>self.X</c> accessors that resolve to
    /// <c>ActorBase</c> properties (not <c>ActorRef</c> members), so
    /// they're exempt from the CE0012 "actor-refs are Tell/ask only" rule.
    /// Must stay in sync with <c>ExpressionEmitter.IsSelfAccessor</c>.
    /// </summary>
    private static bool IsSelfAccessor(string member) =>
        member is "TraceContext" or "Metrics" or "Log" or "System" or "Clock";

    /// <summary>
    /// Reduce a <see cref="MethodCallExpr"/> to its dotted source form
    /// for static-style call detection. Returns <c>"Type.Method"</c>
    /// when the call's target is a <see cref="NameExpr"/> wrapping a
    /// single-part identifier (e.g. <c>Thread.Sleep</c> →
    /// <c>"Thread.Sleep"</c>); returns null for instance-shaped calls
    /// or chained members. Conservative on purpose — prefers misses
    /// over false hits.
    /// </summary>
    private static string? TryGetStaticCallName(MethodCallExpr call)
    {
        // The blocklists key on `Type.Method` (e.g. `Task.Run`). A call target
        // that is a dotted type name — bare (`Task`) OR namespace-qualified
        // (`System.Threading.Tasks.Task`) — is a NameExpr; reduce it to its
        // LAST segment plus the method, so a fully-qualified spelling can't
        // slip the ban. (Before this, any qualification returned null and
        // skipped CE0119/CE0083/CE0084/CE0115 entirely — a red-team find.)
        // Instance-shaped targets (a value's member, a chained call) are
        // MemberAccessExpr/MethodCallExpr, not NameExpr, so they don't match.
        if (call.Target is NameExpr name && name.Name.Parts.Count >= 1)
            return $"{name.Name.Parts[^1]}.{call.Method}";
        return null;
    }

    /// <summary>
    /// True when the first constructor/call argument looks like a delegate — a
    /// lambda, a parenthesised lambda, or a bare method-group name. CE0119 uses
    /// it to tell a real concurrency primitive (<c>new Thread(() => …)</c>,
    /// <c>new Timer(Tick, …)</c>) from a domain type that merely shares the name
    /// <c>Thread</c>/<c>Timer</c> and is constructed with ordinary arguments.
    /// </summary>
    private static bool HasDelegateFirstArg(IReadOnlyList<Expr> args)
    {
        if (args.Count == 0) return false;
        var first = args[0];
        if (first is ParenExpr pe) first = pe.Inner;
        return first is LambdaExpr or NameExpr;
    }

    private static void CheckHostileImports(SpekFile file, List<Diagnostic> diagnostics)
    {
        foreach (var u in file.Usings)
        {
            var name = u.Name.ToString();
            var match = HostileNamespacePrefixes.FirstOrDefault(p =>
                name == p || name.StartsWith(p + ".", StringComparison.Ordinal));

            if (match is null) continue;

            if (u.IsInterop)
            {
                // `interop using` lets the user opt into a hostile
                // namespace knowingly. CE0080 is suppressed; CE0086
                // is emitted as an advisory so the choice shows up in
                // diagnostics.
                diagnostics.Add(Diagnostic.At("CE0086", u.Span,
                    $"'interop using {name}' bypasses Spek's safety guarantees " +
                    $"(reflection / unsafe / unmanaged interop reachable via '{match}'). " +
                    $"Code that uses this namespace is no longer covered by CE0010, " +
                    $"CE0083, or CE0084 — the user owns correctness."));
            }
            else
            {
                diagnostics.Add(Diagnostic.At("CE0080", u.Span,
                    $"Import '{name}' is not permitted — '{match}' can reach through " +
                    $"Spek's compile-time guarantees (e.g. reflection-based field " +
                    $"mutation bypasses CE0010). Use 'interop using {name};' if you " +
                    $"need this and accept the safety trade-off."));
            }
        }
    }

    // ─── CE0013 — duplicate declarations ────────────────────────────────────

    private static void CheckDuplicateDeclarations(SpekFile file, List<Diagnostic> diagnostics)
    {
        // File-scope: duplicate messages, actors, channels, enums, modules.
        var seenMessages = new Dictionary<string, MessageDecl>();
        var seenActors = new Dictionary<string, ActorDecl>();
        var seenChannels = new Dictionary<string, ChannelDecl>();
        var seenEnums = new Dictionary<string, EnumDecl>();
        var seenModules = new Dictionary<string, ModuleDecl>();
        var seenClasses = new Dictionary<string, ClassDecl>();
        var seenInterfaces = new Dictionary<string, InterfaceDecl>();

        foreach (var decl in file.Declarations)
        {
            switch (decl)
            {
                case MessageDecl m when seenMessages.TryGetValue(m.Name, out var prev):
                    diagnostics.Add(Diagnostic.At("CE0013", m.Span,
                        $"Duplicate message declaration '{m.Name}' (previously declared at {prev.Span.StartLine}:{prev.Span.StartColumn})."));
                    break;
                case MessageDecl m:
                    seenMessages[m.Name] = m;
                    break;

                case ActorDecl a when seenActors.TryGetValue(a.Name, out var prev):
                    diagnostics.Add(Diagnostic.At("CE0013", a.Span,
                        $"Duplicate actor declaration '{a.Name}' (previously declared at {prev.Span.StartLine}:{prev.Span.StartColumn})."));
                    break;
                case ActorDecl a:
                    seenActors[a.Name] = a;
                    break;

                case ChannelDecl c when seenChannels.TryGetValue(c.Name, out var prev):
                    diagnostics.Add(Diagnostic.At("CE0013", c.Span,
                        $"Duplicate channel declaration '{c.Name}' (previously declared at {prev.Span.StartLine}:{prev.Span.StartColumn})."));
                    break;
                case ChannelDecl c:
                    seenChannels[c.Name] = c;
                    break;

                case EnumDecl e when seenEnums.TryGetValue(e.Name, out var prev):
                    diagnostics.Add(Diagnostic.At("CE0013", e.Span,
                        $"Duplicate enum declaration '{e.Name}' (previously declared at {prev.Span.StartLine}:{prev.Span.StartColumn})."));
                    break;
                case EnumDecl e:
                    seenEnums[e.Name] = e;
                    break;

                case ModuleDecl m when seenModules.TryGetValue(m.Name, out var prev):
                    diagnostics.Add(Diagnostic.At("CE0013", m.Span,
                        $"Duplicate module declaration '{m.Name}' (previously declared at {prev.Span.StartLine}:{prev.Span.StartColumn})."));
                    break;
                case ModuleDecl m:
                    seenModules[m.Name] = m;
                    // Recurse: functions inside the module must be unique by name;
                    // nested modules must be unique by name within their parent.
                    CheckModuleMemberDuplicates(m, diagnostics);
                    break;

                case ClassDecl cl when seenClasses.TryGetValue(cl.Name, out var prev):
                    diagnostics.Add(Diagnostic.At("CE0013", cl.Span,
                        $"Duplicate class declaration '{cl.Name}' (previously declared at {prev.Span.StartLine}:{prev.Span.StartColumn})."));
                    break;
                case ClassDecl cl:
                    seenClasses[cl.Name] = cl;
                    CheckClassMemberDuplicates(cl, diagnostics);
                    break;

                case InterfaceDecl i when seenInterfaces.TryGetValue(i.Name, out var prev):
                    diagnostics.Add(Diagnostic.At("CE0013", i.Span,
                        $"Duplicate interface declaration '{i.Name}' (previously declared at {prev.Span.StartLine}:{prev.Span.StartColumn})."));
                    break;
                case InterfaceDecl i:
                    seenInterfaces[i.Name] = i;
                    break;
            }
        }

        // Within each actor: duplicate behavior / field / method names.
        foreach (var actor in file.Declarations.OfType<ActorDecl>())
        {
            CheckActorMemberDuplicates(actor, diagnostics);
        }
    }

    // Duplicate-name detection inside a module. Functions
    // must be unique by name; nested modules must be unique by name
    // within their parent module. The two namespaces are independent
    // (a module and a function can share a simple name); same
    // separation C# applies between nested types and methods.
    private static void CheckModuleMemberDuplicates(ModuleDecl module, List<Diagnostic> diagnostics)
    {
        var seenMethods = new Dictionary<string, MethodDecl>();
        var seenNested  = new Dictionary<string, ModuleDecl>();

        foreach (var m in module.Methods)
        {
            if (m.Name is "To" or "TryTo")
            {
                diagnostics.Add(Diagnostic.At("CE0128", m.Span,
                    $"'{m.Name}' is reserved for the Spek conversion family " +
                    $"(x.To<T>() / x.TryTo<T>()), whose calls the compiler " +
                    $"rewrites. Rename the method."));
            }
            if (seenMethods.TryGetValue(m.Name, out var prev))
            {
                diagnostics.Add(Diagnostic.At("CE0013", m.Span,
                    $"Duplicate method '{m.Name}' in module '{module.Name}' " +
                    $"(previously declared at {prev.Span.StartLine}:{prev.Span.StartColumn})."));
            }
            else
            {
                seenMethods[m.Name] = m;
            }
        }

        foreach (var nested in module.NestedModules)
        {
            if (seenNested.TryGetValue(nested.Name, out var prev))
            {
                diagnostics.Add(Diagnostic.At("CE0013", nested.Span,
                    $"Duplicate nested module '{nested.Name}' in module '{module.Name}' " +
                    $"(previously declared at {prev.Span.StartLine}:{prev.Span.StartColumn})."));
            }
            else
            {
                seenNested[nested.Name] = nested;
                CheckModuleMemberDuplicates(nested, diagnostics);
            }
        }
    }

    private static void CheckActorMemberDuplicates(ActorDecl actor, List<Diagnostic> diagnostics)
    {
        var seenBehaviors = new Dictionary<string, BehaviorDecl>();
        var seenFields = new Dictionary<string, FieldDecl>();
        var seenMethods = new Dictionary<string, MethodDecl>();
        var seenEvents = new Dictionary<string, EventPattern>();

        foreach (var member in actor.Members)
        {
            switch (member)
            {
                case BehaviorDecl b when seenBehaviors.TryGetValue(b.Name, out var prev):
                    diagnostics.Add(Diagnostic.At("CE0013", b.Span,
                        $"Duplicate behavior '{b.Name}' in actor '{actor.Name}' (previously declared at {prev.Span.StartLine}:{prev.Span.StartColumn})."));
                    break;
                case BehaviorDecl b:
                    seenBehaviors[b.Name] = b;
                    // `on event Name(...)` handlers emit a bridge
                    // method named `Name` on the actor class, so two
                    // event handlers sharing a name would emit duplicate
                    // C# methods. Disallow regardless of which behavior
                    // they live in.
                    foreach (var handler in b.Handlers)
                    {
                        if (handler.Pattern is not EventPattern ep) continue;
                        if (seenEvents.TryGetValue(ep.HandlerName, out var prevEp))
                        {
                            diagnostics.Add(Diagnostic.At("CE0013", ep.Span,
                                $"Duplicate 'on event {ep.HandlerName}' in actor '{actor.Name}' (previously declared at {prevEp.Span.StartLine}:{prevEp.Span.StartColumn}). Each event-handler name must be unique because it emits a bridge method on the actor class."));
                        }
                        else
                        {
                            seenEvents[ep.HandlerName] = ep;
                        }
                    }
                    break;

                case FieldDecl f when seenFields.TryGetValue(f.Name, out var prev):
                    diagnostics.Add(Diagnostic.At("CE0013", f.Span,
                        $"Duplicate field '{f.Name}' in actor '{actor.Name}' (previously declared at {prev.Span.StartLine}:{prev.Span.StartColumn})."));
                    break;
                case FieldDecl f:
                    seenFields[f.Name] = f;
                    break;

                case MethodDecl m when seenMethods.TryGetValue(m.Name, out var prev):
                    diagnostics.Add(Diagnostic.At("CE0013", m.Span,
                        $"Duplicate method '{m.Name}' in actor '{actor.Name}' (previously declared at {prev.Span.StartLine}:{prev.Span.StartColumn}). Method overloading is not supported."));
                    break;
                case MethodDecl m:
                    seenMethods[m.Name] = m;
                    break;
            }
        }
    }

    // Duplicate-name detection inside a class: fields and methods must
    // each be unique by name (no overloading).
    private static void CheckClassMemberDuplicates(ClassDecl cls, List<Diagnostic> diagnostics)
    {
        var seenFields = new Dictionary<string, FieldDecl>();
        foreach (var f in cls.Fields)
        {
            if (seenFields.TryGetValue(f.Name, out var prev))
                diagnostics.Add(Diagnostic.At("CE0013", f.Span,
                    $"Duplicate field '{f.Name}' in class '{cls.Name}' (previously declared at {prev.Span.StartLine}:{prev.Span.StartColumn})."));
            else
                seenFields[f.Name] = f;
        }

        var seenMethods = new Dictionary<string, MethodDecl>();
        foreach (var m in cls.Methods)
        {
            if (seenMethods.TryGetValue(m.Name, out var prev))
                diagnostics.Add(Diagnostic.At("CE0013", m.Span,
                    $"Duplicate method '{m.Name}' in class '{cls.Name}' (previously declared at {prev.Span.StartLine}:{prev.Span.StartColumn}). Method overloading is not supported."));
            else
                seenMethods[m.Name] = m;
        }

        // Properties share the member namespace with fields (a property and a
        // field of the same name would be CS0102 in C#); flag it as CE0013.
        foreach (var p in cls.Properties)
        {
            if (seenFields.ContainsKey(p.Name) || seenMethods.ContainsKey(p.Name))
                diagnostics.Add(Diagnostic.At("CE0013", p.Span,
                    $"Member '{p.Name}' in class '{cls.Name}' is already declared as a field or method."));
        }
    }

    // ─── Message-level checks ───────────────────────────────────────────────

    private static void AnalyzeMessage(MessageDecl message, SymbolTable symbols, List<Diagnostic> diagnostics)
    {
        foreach (var field in message.Fields)
        {
            // CE0010 — message field type must be immutable by design.
            var classification = ImmutableTypeClassifier.Classify(
                field.Type, symbols, message.TypeParameters);

            if (classification == ImmutableTypeClassifier.Classification.Disallowed)
            {
                diagnostics.Add(Diagnostic.At("CE0010", field.Type.Span,
                    $"Message '{message.Name}' field '{field.Name}' has type '{field.Type}', which is not a known immutable type. " +
                    "Use a primitive, string, DateTime/Guid/etc., a Spek 'message', or a System.Collections.Immutable.* container."));
            }
        }

        // CE0125 — an `abstract message` base must be empty. Base-carries-fields
        // (a derived variant passing shared fields through to its base) is
        // deferred; ship the empty-family-marker case first.
        if (message.IsAbstract && message.Fields.Count > 0)
        {
            diagnostics.Add(Diagnostic.At("CE0125", message.Span,
                $"Abstract message '{message.Name}' declares fields. A polymorphic " +
                $"message base must be empty for now — put the fields on each " +
                $"variant instead ('message Variant(...) : {message.Name}'). A base " +
                $"that carries shared fields is not supported yet."));
        }

        // CE0124 — a message base must resolve to an `abstract message`.
        if (message.BaseMessage is { } baseName)
        {
            var baseMsg = symbols.ResolveMessage(baseName);
            if (baseMsg is null)
            {
                diagnostics.Add(Diagnostic.At("CE0124", baseName.Span,
                    $"Message '{message.Name}' extends unknown message '{baseName}'. " +
                    $"A message variant's base must be a declared " +
                    $"'abstract message {baseName}();'."));
            }
            else if (!baseMsg.IsAbstract)
            {
                diagnostics.Add(Diagnostic.At("CE0124", baseName.Span,
                    $"Message '{message.Name}' extends '{baseName}', which is not " +
                    $"abstract. A message family's base must be an 'abstract message' " +
                    $"— mark '{baseName}' abstract, or remove the base."));
            }
        }
    }

    // ─── Scope tracking ─────────────────────────────────────────────────────

    private enum Scope
    {
        /// <summary>Inside an OnHandler body — message handler in a BehaviorDecl.</summary>
        OnHandler,
        /// <summary>Inside an InitBlock body.</summary>
        Init,
        /// <summary>Inside an `on Restore` lifecycle hook.</summary>
        Restore,
        /// <summary>Inside `on PreStart` / `on PostStop` lifecycle hook.</summary>
        LifecycleOther,
        /// <summary>Inside a plain MethodDecl body.</summary>
        Method,
        /// <summary>Inside a `term { }` block. Same scope rules
        /// as Init (no Tell, ask, become, persist) — this is for
        /// resource release, not for state mutation or messaging.</summary>
        Term,
    }

    private sealed record Context(
        ActorSymbols Actor,
        SymbolTable Symbols,
        Scope Scope,
        ExpressionTyper Typer,
        HandlerMode HandlerMode = HandlerMode.Writer,
        IReadOnlySet<string>? ReaderUnsafeMethods = null,
        // Set while walking a stream-operator chain (`=> distinct(by: x => …)`).
        // Forces WalkExpr to descend into selector lambda bodies regardless of
        // handler mode: an operator selector runs OFF the actor's turn, so the
        // concurrency/cast/time CE family must see inside it (red-team H2).
        bool InStreamOperator = false)
    {
        public string ActorName => Actor.Declaration.Name;
        public IReadOnlySet<string> BehaviorNames => Actor.BehaviorNames;

        /// <summary>The actor's own methods a reader handler may not call —
        /// those that mutate actor/region state directly or transitively
        /// (CE0087, HOLE #1). Falls back to the state-mutation set the escape
        /// analysis already computes when the richer set wasn't supplied
        /// (non-handler scopes never consult it, being gated on reader mode).
        /// See <see cref="ComputeReaderUnsafeMethods"/>.</summary>
        public IReadOnlySet<string> UnsafeMethods => ReaderUnsafeMethods ?? Actor.MutatingMethods;
    }

    // ─── Actor-level checks ─────────────────────────────────────────────────

    private static void AnalyzeActor(ActorDecl actor, SymbolTable symbols, List<Diagnostic> diagnostics)
    {
        var actorSymbols = symbols.GetActorSymbols(actor.Name)!;

        // CE0087 (HOLE #1) — the actor's own methods a reader handler may not
        // call, computed once and threaded into every handler Context.
        var readerUnsafe = ComputeReaderUnsafeMethods(actorSymbols, symbols);

        var passivateDecls = actor.Members.OfType<PassivateDecl>().ToList();
        var restoreHooks = actor.Members.OfType<LifecycleHook>()
            .Where(h => h.Event is RestoreEvent)
            .ToList();
        var hasPersistStmt = ContainsPersist(actor);

        // CE0014 — behaviors declared but never reached via `become`. Skip on
        // abstract actors (derived actors may reference the inherited behaviors).
        if (!actor.IsAbstract)
            CheckUnusedBehaviors(actor, diagnostics);

        // CE0060 — Restore handler declared but actor never persists/passivates.
        if (restoreHooks.Count > 0 && !hasPersistStmt && passivateDecls.Count == 0)
        {
            var hook = restoreHooks[0];
            diagnostics.Add(Diagnostic.At("CE0060", hook.Span,
                $"Actor '{actor.Name}' declares 'on Restore' but has no 'persist' statement or 'passivate' declaration."));
        }

        // CE0061 retired: a `passivate`/`persist` actor with no
        // explicit `on Restore` is no longer an error — the emitter auto-generates a
        // symmetric OnRestore that rehydrates every captured field. An explicit
        // `on Restore` still wins for custom restore logic.

        // CE0081 / CE0082 — unreachable or duplicate on-Failure arms within
        // a supervise strategy.
        CheckSuperviseArms(actor, diagnostics);

        // CE0118 — `supervise` decl + an explicit OnChildFailure override collide
        // (the supervise decl already generates OnChildFailure).
        CheckSuperviseOnChildFailureConflict(actor, diagnostics);

        // CE0090 / CE0091 — channel implementation checks.
        CheckChannelImplementations(actor, symbols, diagnostics);

        // CE0097 — `use X foo;` must reference a declared `shared X { ... }`.
        foreach (var use in actor.Members.OfType<UseDecl>())
        {
            if (symbols.ResolveSharedRegion(use.RegionType) is null)
            {
                diagnostics.Add(Diagnostic.At("CE0097", use.Span,
                    $"Unknown shared region '{use.RegionType}' in actor '{actor.Name}'. " +
                    $"Declare it with 'shared {use.RegionType} {{ ... }}' first."));
            }
        }

        // Walk member bodies with the correct scope set.
        foreach (var member in actor.Members)
        {
            switch (member)
            {
                case InitBlock init:
                {
                    var typer = new ExpressionTyper(symbols, actorSymbols);
                    foreach (var p in init.Parameters) typer.RegisterParameter(p.Name, p.Type);
                    WalkBlock(init.Body,
                        new Context(actorSymbols, symbols, Scope.Init, typer),
                        diagnostics);
                    CheckMovedValueFlow(init.Body, diagnostics);
                    CheckLambdaCaptureEscape(actorSymbols, symbols, init.Body, diagnostics,
                        init.Parameters);
                    break;
                }

                case TermBlock term:
                {
                    var typer = new ExpressionTyper(symbols, actorSymbols);
                    WalkBlock(term.Body,
                        new Context(actorSymbols, symbols, Scope.Term, typer),
                        diagnostics);
                    CheckMovedValueFlow(term.Body, diagnostics);
                    CheckLambdaCaptureEscape(actorSymbols, symbols, term.Body, diagnostics);
                    break;
                }

                case BehaviorDecl beh:
                    foreach (var handler in beh.Handlers)
                    {
                        var typer = new ExpressionTyper(symbols, actorSymbols);
                        // Message pattern binding becomes a local of the handler's message type.
                        RegisterHandlerBinding(handler.Pattern, typer, symbols);
                        // CE0096 — public/internal handlers must bind to a
                        // declared `message`. Private handlers may bind to
                        // any CLR type so actors can handle BCL event-args
                        // (FileSystemEventArgs, Timer callbacks, etc.)
                        // directly without a wrapper-message ceremony.
                        CheckHandlerMessageType(handler, symbols, diagnostics);
                        WalkHandlerBody(handler.Body,
                            new Context(actorSymbols, symbols, Scope.OnHandler, typer,
                                HandlerMode: handler.Mode,
                                ReaderUnsafeMethods: readerUnsafe),
                            diagnostics);
                        if (handler.Body is BlockHandlerBody bhb)
                            CheckMovedValueFlow(bhb.Block, diagnostics);
                        // A message handler's `return` is the ask-reply, so it
                        // is subject to CE0010's immutability admission.
                        CheckLambdaCaptureEscape(actorSymbols, symbols, handler.Body, diagnostics,
                            returnsAreReplies: true);

                        // Stream-operator chain (`=> debounce(…) => distinct(by:
                        // x => …) =>`): the operator args and their selector
                        // lambdas run OFF the actor's turn but were never walked,
                        // so the CE family (CE0119 raw concurrency, CE0127/CE0129
                        // conversions/casts) silently leaked inside them (red-team
                        // H2). Walk them under InStreamOperator so WalkExpr's
                        // lambda arm descends into selectors regardless of mode.
                        // A fresh typer: operators build before any message, so
                        // the handler's message binding is NOT in scope here.
                        if (handler.StreamOperators is { Count: > 0 } streamOps)
                        {
                            var opCtx = new Context(actorSymbols, symbols, Scope.OnHandler,
                                new ExpressionTyper(symbols, actorSymbols),
                                HandlerMode: handler.Mode,
                                ReaderUnsafeMethods: readerUnsafe,
                                InStreamOperator: true);
                            foreach (var op in streamOps)
                                WalkExpr(op, opCtx, diagnostics);
                        }
                    }
                    break;

                case LifecycleHook hook:
                    var hookScope = hook.Event is RestoreEvent ? Scope.Restore : Scope.LifecycleOther;
                    WalkHandlerBody(hook.Body,
                        new Context(actorSymbols, symbols, hookScope,
                            new ExpressionTyper(symbols, actorSymbols)),
                        diagnostics);
                    if (hook.Body is BlockHandlerBody hbhb)
                        CheckMovedValueFlow(hbhb.Block, diagnostics);
                    CheckLambdaCaptureEscape(actorSymbols, symbols, hook.Body, diagnostics);
                    break;

                case MethodDecl method:
                {
                    var typer = new ExpressionTyper(symbols, actorSymbols);
                    foreach (var p in method.Parameters) typer.RegisterParameter(p.Name, p.Type);
                    WalkBlock(method.Body,
                        new Context(actorSymbols, symbols, Scope.Method, typer),
                        diagnostics);
                    CheckMovedValueFlow(method.Body, diagnostics);
                    CheckLambdaCaptureEscape(actorSymbols, symbols, method.Body, diagnostics,
                        method.Parameters);
                    break;
                }
            }
        }
    }

    // ─── CE0135 / CE0136 / CE0137 — capture-escape & spawn-argument sharing ─

    /// <summary>
    /// CE0135 (error). A lambda that writes actor state may not <em>escape</em>
    /// the body it was written in.
    /// <para>
    /// A lambda is <em>state-mutating</em> when its body assigns to something
    /// rooted at an actor field or property, calls one of the actor's own
    /// methods that does (the classification comes from <see cref="StateMutation"/>,
    /// the same machinery CE0087 uses for classes), calls a <em>mutating method
    /// on a confined-class actor field</em> (<c>helper.Mutate()</c>, judged by
    /// <see cref="ClassSymbols.MutatingMethods"/> exactly as CE0087 judges
    /// reader handlers), or assigns through a <c>use</c> shared-region handle
    /// (a foreign thread invoking that write bypasses the region's
    /// reader/writer lock entirely). Read-only capture is not mutation and
    /// stays unrestricted: <c>items.Where(x =&gt; x.Id == filterId)</c>
    /// is the overwhelmingly common case and must keep compiling.
    /// </para>
    /// <para>
    /// CE0136 (warning) is the read-side companion. A lambda that captures
    /// actor state <em>read-only</em> and is passed as a call argument to a
    /// callee the compiler cannot see into gets a warning: if the callee
    /// stores or parallelizes the callback, its reads race the actor's writes.
    /// Trusted callees stay silent — the LINQ operator name set (immediate,
    /// synchronous callbacks; a name table in the CE0083/CE0119 tradition,
    /// since the analyzer has no foreign type resolution), the actor's own and
    /// sibling Spek types (their code compiles under these same rules), and
    /// the Spek.Streams factory names. Field stores are not CE0136 escape
    /// sites (the field is inside the same actor), and neither are returns
    /// (rare, and CE0010 blocks the message shapes) — call arguments are where
    /// read captures leave in practice.
    /// </para>
    /// <para>
    /// A state-mutating lambda is fine <em>inside</em> the turn — bind it to a
    /// local, invoke it directly, both run on the thread that owns the actor.
    /// It escapes when it is passed as a call argument, stored in an actor
    /// field, or returned, because whoever holds it can then invoke it later on
    /// a thread the actor does not own. That is a write to actor state outside
    /// the mailbox's serialization — exactly the race CE0119 exists to prevent,
    /// arriving through a closure instead of a thread primitive, so it is an
    /// error for the same reason.
    /// </para>
    /// <para>
    /// Locals are tainted intra-body: <c>var bump = () =&gt; seen = seen + 1;</c>
    /// makes <c>bump</c> carry the lambda's write, so a later
    /// <c>registry.Register(bump)</c> is the same escape as inlining it. Flow is
    /// statement-ordered within one body; there is no interprocedural analysis,
    /// and taint is not tracked through a lambda's own locals.
    /// </para>
    /// <para>
    /// The analysis cannot see whether a foreign callee stores the delegate, so
    /// it assumes the worst at every call. That over-approximates for immediate
    /// callbacks such as <c>List.ForEach</c>, which is a known and accepted cost:
    /// Spek has <c>foreach</c>, so the loop the rule pushes you toward is the
    /// better code anyway, and the diagnostic says so.
    /// </para>
    /// <para>
    /// CE0137 (error) rides the same walk: a spawn argument whose static type
    /// is a mutable Spek <c>class</c> may not alias the spawning actor's state.
    /// <c>spawn&lt;Child&gt;(reg)</c> where <c>reg</c> is a class-typed actor
    /// field hands the child a reference the sender also keeps — two actors
    /// holding one mutable object, which is the sharing CE0010 blocks on the
    /// message route and CE0112 blocks on the region route. The legal idiom is
    /// the constructor gift: <c>spawn&lt;Child&gt;(new Registry())</c>, or a
    /// <c>new</c>-initialized local that is never also stored in a field.
    /// See <see cref="CheckSpawnClassArgs"/>.
    /// </para>
    /// </summary>
    private static void CheckLambdaCaptureEscape(
        ActorSymbols actor, SymbolTable symbols, HandlerBody body, List<Diagnostic> diagnostics,
        bool returnsAreReplies = false)
    {
        switch (body)
        {
            case BlockHandlerBody b:
                CheckLambdaCaptureEscape(actor, symbols, b.Block, diagnostics,
                    returnsAreReplies: returnsAreReplies);
                break;
            case InlineHandlerBody i:
                var st = new EscapeState(actor, symbols, returnsAreReplies: returnsAreReplies);
                ScanEscapes(i.Expr, st, diagnostics);
                // An inline handler body IS the ask-reply: `on GetReg => reg`
                // hands a confined class back exactly as a block `return reg;`.
                if (returnsAreReplies) CheckReturnClassShare(i.Expr, st, diagnostics);
                break;
        }
    }

    private static void CheckLambdaCaptureEscape(
        ActorSymbols actor, SymbolTable symbols, BlockStmt block, List<Diagnostic> diagnostics,
        IReadOnlyList<Param>? parameters = null, bool returnsAreReplies = false)
    {
        var st = new EscapeState(actor, symbols, CollectFieldAssignedLocals(block, actor),
            returnsAreReplies);

        // CE0137 — a class-typed parameter is a gift received: the caller
        // handed this actor sole ownership (the caller's own spawn or `new`
        // was checked at ITS site). It behaves exactly like a gifted local:
        // relaying it onward to a spawn is a legitimate gift chain, but
        // storing it in actor state AND forwarding it puts two owners on one
        // mutable object.
        if (parameters is not null)
        {
            foreach (var p in parameters)
                if (symbols.ResolveClass(p.Type.Name.Simple) is not null)
                    st.BindClassParam(p.Name, p.Type.Name.Simple);
        }

        ScanBlockEscapes(block, st, diagnostics);
    }

    /// <summary>Every local that is assigned into actor state anywhere in this
    /// body (<c>reg = r;</c>, <c>self.reg = r;</c>, <c>reg.slot = r;</c>).
    /// Collected body-wide, deliberately ignoring statement order: gifting a
    /// <c>new</c>-initialized local to a spawn and THEN storing it in a field
    /// is the same sharing as storing it first, so CE0137 must see the store
    /// in both directions.</summary>
    private static IReadOnlySet<string> CollectFieldAssignedLocals(
        BlockStmt block, ActorSymbols actor)
    {
        var stored = new HashSet<string>(StringComparer.Ordinal);
        foreach (var e in ClassSymbols.ExprsIn(block))
        {
            if (e is AssignExpr a
                && StateMutation.Root(a.Left, actor.StateNames) is not null
                && UnwrapEscapee(a.Right) is NameExpr n && n.Name.Parts.Count == 1)
                stored.Add(n.Name.Simple);
        }
        return stored;
    }

    /// <summary>
    /// CE0135/CE0136 state for one body: which locals currently hold a
    /// state-mutating lambda (mapped to a description of the write, for the
    /// message), which hold a read-only state-capturing lambda (mapped to the
    /// captured name), plus the lambdas already reported — used to suppress a
    /// second report for anything nested inside one. One reported set serves
    /// both rules: a read-only lambda cannot contain a mutating one (any
    /// nested mutation makes the outer lambda classify as mutating), so a
    /// CE0136 report never swallows a CE0135.
    /// </summary>
    private sealed class EscapeState(
        ActorSymbols actor, SymbolTable symbols,
        IReadOnlySet<string>? fieldAssignedLocals = null,
        bool returnsAreReplies = false)
    {
        public ActorSymbols Actor { get; } = actor;
        public SymbolTable Symbols { get; } = symbols;

        /// <summary>True only for message-handler bodies, where a <c>return</c>
        /// is the ask-reply. A private method or lifecycle hook returning a
        /// confined class internally is fine — only a reply hands it to another
        /// actor — so CE0010's return-channel check gates on this.</summary>
        public bool ReturnsAreReplies { get; } = returnsAreReplies;

        /// <summary>See <see cref="CollectFieldAssignedLocals"/> — the CE0137
        /// gift check consults this to catch a spawned local that is ALSO
        /// stored in actor state, before or after the spawn.</summary>
        public IReadOnlySet<string> FieldAssignedLocals { get; } =
            fieldAssignedLocals ?? new HashSet<string>(StringComparer.Ordinal);

        /// <summary>The actor's <c>use X foo;</c> handle names — writes rooted
        /// at one are shared-region writes, reads rooted at one are shared-state
        /// captures.</summary>
        public IReadOnlySet<string> RegionHandles { get; } =
            actor.Declaration.Members.OfType<UseDecl>()
                .Select(u => u.LocalName)
                .ToHashSet(StringComparer.Ordinal);

        private readonly Dictionary<string, string> tainted = new(StringComparer.Ordinal);
        private readonly Dictionary<string, string> readCaptures = new(StringComparer.Ordinal);

        /// <summary>The escape <em>carriers</em> already reported — the exact
        /// lambda or tainted-local node a diagnostic fired on. Reference
        /// identity, so the same carrier reached again through an outer and an
        /// inner escape site (e.g. <c>Register(new Holder(bump))</c> visits both
        /// the call and the <c>new</c>) is reported once.</summary>
        private readonly HashSet<Expr> reported = new(ReferenceEqualityComparer.Instance);

        // CE0137 taint, same statement-ordered pattern as the lambda maps
        // above: which locals alias a class-typed actor field (local →
        // (field, class)), and which hold a fresh `new SpekClass(...)` — the
        // gift candidates (local → class).
        private readonly Dictionary<string, (string Field, string Class)> classAliases =
            new(StringComparer.Ordinal);
        private readonly Dictionary<string, string> giftedClasses = new(StringComparer.Ordinal);
        private readonly HashSet<string> classParams = new(StringComparer.Ordinal);

        // CE0137 — locals `new`ed while CAPTURING a class-typed actor field:
        // `var w = new Wrapper(reg);`. `w` is a fresh object (gifted), but it
        // holds `reg` internally, so spawning `w` shares `reg` with the child.
        private readonly Dictionary<string, (string Field, string Class)> capturingLocals =
            new(StringComparer.Ordinal);

        /// <summary>Binds (or clears) a local's write-taint. A redeclaration with
        /// a non-lambda initializer clears it, so a sibling block reusing the name
        /// doesn't inherit the other branch's taint.</summary>
        public void Set(string name, string? writes)
        {
            if (writes is null) tainted.Remove(name);
            else tainted[name] = writes;
        }

        public string? Writes(string name) => tainted.GetValueOrDefault(name);

        /// <summary>Binds (or clears) a local's read-capture taint — the CE0136
        /// analogue of <see cref="Set"/>.</summary>
        public void SetRead(string name, string? captured)
        {
            if (captured is null) readCaptures.Remove(name);
            else readCaptures[name] = captured;
        }

        public string? Reads(string name) => readCaptures.GetValueOrDefault(name);

        public void MarkReported(Expr carrier) => reported.Add(carrier);

        /// <summary>The Spek class name of the actor field or property
        /// <paramref name="slot"/>, or <c>null</c> when the slot doesn't exist
        /// or its type isn't a declared <c>class</c>. Mirrors CE0010's stance:
        /// there is no immutable-class carve-out — any declared class is
        /// mutable by definition.</summary>
        public string? ClassTypedState(string slot)
        {
            var type = Actor.Fields.TryGetValue(slot, out var f)
                ? f.Type
                : Actor.Declaration.Members.OfType<PropertyDecl>()
                    .FirstOrDefault(p => p.Name == slot)?.Type;
            return type is not null && Symbols.ResolveClass(type.Name.Simple) is not null
                ? type.Name.Simple
                : null;
        }

        /// <summary>The class-typed actor field this local aliases, or null.</summary>
        public (string Field, string Class)? ClassAlias(string name)
            => classAliases.TryGetValue(name, out var a) ? a : null;

        /// <summary>The Spek class this local was freshly <c>new</c>ed as (or
        /// received as a parameter — see <see cref="BindClassParam"/>), or null.</summary>
        public string? GiftedClass(string name) => giftedClasses.GetValueOrDefault(name);

        /// <summary>The class-typed actor field a <c>new</c>-initialized local
        /// captured (<c>var w = new Wrapper(reg);</c>), or null. Spawning such a
        /// local shares the captured field with the child.</summary>
        public (string Field, string Class)? CapturedClassField(string name)
            => capturingLocals.TryGetValue(name, out var c) ? c : null;

        /// <summary>Records (or clears) that a local was <c>new</c>ed capturing a
        /// class-typed actor field. Cleared on rebind exactly like the other
        /// class-taint maps.</summary>
        public void SetCapturingLocal(string name, (string Field, string Class)? captured)
        {
            if (captured is { } c) capturingLocals[name] = c;
            else capturingLocals.Remove(name);
        }

        /// <summary>Seeds a class-typed <c>init</c>/method parameter as a gift
        /// received at body entry. It rides the same gifted map, so the spawn
        /// check and the body-wide store set do the rest; the param bit only
        /// steers the diagnostic's wording.</summary>
        public void BindClassParam(string name, string cls)
        {
            giftedClasses[name] = cls;
            classParams.Add(name);
        }

        /// <summary>True while <paramref name="name"/> still holds the
        /// parameter it entered the body as (a rebind sheds the bit).</summary>
        public bool IsClassParam(string name) => classParams.Contains(name);

        /// <summary>Records what a local now holds for CE0137 — an alias of a
        /// class-typed actor field, a fresh <c>new</c> of a declared class, or
        /// neither (which clears both maps, so a rebind sheds stale taint).
        /// Called for <c>var</c> declarations and plain local reassignments,
        /// the same statement-ordered flow as the lambda taint.</summary>
        public void BindClassLocal(string name, Expr value)
        {
            classAliases.Remove(name);
            giftedClasses.Remove(name);
            classParams.Remove(name);
            switch (value)
            {
                // `var r = reg;` — r now aliases the field.
                case NameExpr n when n.Name.Parts.Count == 1
                                     && ClassTypedState(n.Name.Simple) is { } cls:
                    classAliases[name] = (n.Name.Simple, cls);
                    break;

                // `var r2 = r;` where r already aliases a field — same object.
                case NameExpr n when n.Name.Parts.Count == 1
                                     && ClassAlias(n.Name.Simple) is { } alias:
                    classAliases[name] = alias;
                    break;

                // `var r = self.reg;`.
                case MemberAccessExpr ma when ma.Target is SelfExpr
                                              && ClassTypedState(ma.Member) is { } cls:
                    classAliases[name] = (ma.Member, cls);
                    break;

                // `var r = new Registry();` — the gift candidate.
                case NewExpr nw when Symbols.ResolveClass(nw.Type.Parts[^1]) is not null:
                    giftedClasses[name] = nw.Type.Parts[^1];
                    break;
            }
        }

        /// <summary>The Spek class name of region field
        /// <paramref name="fieldName"/> read through <c>use</c> handle
        /// <paramref name="handle"/>, or <c>null</c>. Such a field is already
        /// CE0112 at its declaration; CE0137 also flags the spawn that would
        /// hand it to a child.</summary>
        public string? RegionFieldClass(string handle, string fieldName)
        {
            var use = Actor.Declaration.Members.OfType<UseDecl>()
                .FirstOrDefault(u => u.LocalName == handle);
            if (use is null) return null;
            var field = Symbols.ResolveSharedRegion(use.RegionType)
                ?.Fields.FirstOrDefault(f => f.Name == fieldName);
            return field is not null && Symbols.ResolveClass(field.Type.Name.Simple) is not null
                ? field.Type.Name.Simple
                : null;
        }

        /// <summary>True when <paramref name="node"/> has already been reported
        /// as a carrier, or sits inside a lambda that already escaped. The outer
        /// report subsumes it: the walk is pre-order, so the enclosing lambda is
        /// always recorded before its contents are visited.</summary>
        public bool AlreadyCovered(Expr node) =>
            reported.Contains(node)
            || reported.OfType<LambdaExpr>().Any(r => ClassSymbols.ExprsIn(r).Any(x => ReferenceEquals(x, node)));
    }

    /// <summary>The actor state a lambda writes, phrased for the diagnostic, or
    /// <c>null</c> when the lambda only reads.</summary>
    private static string? StateWritten(LambdaExpr lam, EscapeState st)
    {
        var actor = st.Actor;
        var body = lam.Body switch
        {
            Expr ex      => ClassSymbols.ExprsIn(ex),
            BlockStmt bs => ClassSymbols.ExprsIn(bs),
            _            => Enumerable.Empty<Expr>(),
        };

        foreach (var e in body)
        {
            // A direct write: `seen = …`, `self.seen = …`, `buffer[i] = …`.
            if (e is AssignExpr a)
            {
                if (StateMutation.Root(a.Left, actor.StateNames) is { } slot)
                    return $"assigns to actor state ('{slot}')";

                // A write through a `use` handle: `cache.hits = …`. Invoked
                // from a foreign thread this bypasses the region's
                // reader/writer lock entirely — no lock is taken at all.
                if (TryGetSharedRegionRoot(a.Left, actor.Declaration) is { } handle)
                    return $"writes shared-region state through 'use' handle ('{handle}')";
            }

            // A call to one of the actor's own methods that writes state.
            var sibling = e switch
            {
                MethodCallExpr mc when mc.Target is SelfExpr => mc.Method,
                InvocationExpr inv                           => inv.Callee,
                _                                            => null,
            };
            if (sibling is not null && actor.MutatingMethods.Contains(sibling))
                return $"mutates actor state by calling '{sibling}'";

            // An INDIRECT write: the lambda invokes a local that itself carries a
            // state write — `() => bump()` where `bump` is a tainted local. The
            // write reaches actor state one call-hop away, on whatever thread the
            // outer lambda runs on.
            if (e is InvocationExpr indirect
                && st.Writes(indirect.Callee) is { } indirectWrite)
                return indirectWrite;

            // A mutating method on a confined class the lambda reaches — a direct
            // actor field (`helper.Mutate()` / `self.helper.Mutate()`) OR a local
            // that aliases one (`var h = helper; h.Mutate();`). Same
            // ClassSymbols.MutatingMethods set CE0087 consults for reader
            // handlers. Method calls on foreign-typed fields are NOT flagged —
            // whether they mutate is unknowable; that residue belongs to the
            // runtime turn guard.
            if (e is MethodCallExpr call
                && ReceiverClass(call.Target, st) is { } recv
                && st.Symbols.GetClassSymbols(recv.Class) is { } classSyms
                && classSyms.MutatingMethods.Contains(call.Method))
            {
                return $"mutates actor state by calling '{recv.Receiver}.{call.Method}'";
            }
        }
        return null;
    }

    /// <summary>The confined-class receiver of a method call — a direct actor
    /// field (bare or <c>self.</c>) or a local that aliases one — as
    /// (display receiver, class name), or <c>null</c> for any other shape. This
    /// widened to follow the CE0137
    /// class-alias map, so a mutating method invoked through
    /// <c>var h = helper;</c> is judged exactly as <c>helper.Mutate()</c> is.</summary>
    private static (string Receiver, string Class)? ReceiverClass(Expr target, EscapeState st)
    {
        switch (target)
        {
            case NameExpr n when n.Name.Parts.Count == 1:
                var name = n.Name.Simple;
                if (st.Actor.Fields.TryGetValue(name, out var f)
                    && st.Symbols.ResolveClass(f.Type.Name.Simple) is not null)
                    return (name, f.Type.Name.Simple);
                if (st.ClassAlias(name) is { } alias)
                    return (name, alias.Class);
                return null;

            case MemberAccessExpr ma when ma.Target is SelfExpr
                                          && st.Actor.Fields.TryGetValue(ma.Member, out var sf)
                                          && st.Symbols.ResolveClass(sf.Type.Name.Simple) is not null:
                return ($"self.{ma.Member}", sf.Type.Name.Simple);

            default:
                return null;
        }
    }

    /// <summary>The first piece of actor state a lambda captures by reference
    /// — an actor field or property, read directly or via <c>self.</c>, or a
    /// <c>use</c> region handle — or <c>null</c> when the lambda touches none.
    /// Callers check <see cref="StateWritten"/> first; a non-null result here
    /// therefore means the capture is read-only. Lambda parameters shadow
    /// state names, C#-style, and are excluded.</summary>
    private static string? StateRead(LambdaExpr lam, EscapeState st)
    {
        var shadowed = lam.Parameters
            .Select(p => p.Name)
            .ToHashSet(StringComparer.Ordinal);

        var body = lam.Body switch
        {
            Expr ex      => ClassSymbols.ExprsIn(ex),
            BlockStmt bs => ClassSymbols.ExprsIn(bs),
            _            => Enumerable.Empty<Expr>(),
        };

        foreach (var e in body)
        {
            switch (e)
            {
                // `seen`, or the greedy-parsed `items.Count` — the root name
                // decides. (Multi-part roots can't be shadowed by a parameter.)
                case NameExpr n when !(n.Name.Parts.Count == 1 && shadowed.Contains(n.Name.Simple)):
                    var root = n.Name.Parts[0];
                    if (st.Actor.StateNames.Contains(root) || st.RegionHandles.Contains(root))
                        return root;
                    break;

                // `self.seen`.
                case MemberAccessExpr ma when ma.Target is SelfExpr
                                              && st.Actor.StateNames.Contains(ma.Member):
                    return ma.Member;
            }
        }
        return null;
    }

    /// <summary>Sees a value through wrappers that don't change which lambda is
    /// being handed over.</summary>
    private static Expr UnwrapEscapee(Expr e) => e switch
    {
        ParenExpr p    => UnwrapEscapee(p.Inner),
        NamedArgExpr n => UnwrapEscapee(n.Value),
        RefArgExpr r   => UnwrapEscapee(r.Inner),
        TypeOpExpr { Kind: TypeOpKind.Cast or TypeOpKind.As } t => UnwrapEscapee(t.Operand),
        _              => e,
    };

    /// <summary>
    /// The <em>reachability</em> heart of CE0135/CE0136/CE0137. Given a value
    /// about to escape (a call argument, a return, a field-store RHS, a spawn
    /// argument), this yields the leaf expressions the value carries out —
    /// descending through every construct that packages a value without
    /// consuming it: parentheses/casts, ternaries, <c>??</c>, <c>switch</c>
    /// arms, tuples, array literals, <c>new</c> arguments AND object-initializer
    /// values, and the returned expression of a projection lambda
    /// (<c>items.Select(x =&gt; bump)</c> carries <c>bump</c> out). Every leaf
    /// is then judged by the same three questions the rules ask: does it carry a
    /// state-mutating lambda (CE0135), a read-only state capture (CE0136), or an
    /// alias of a confined class (CE0137). One walk closes the "hide the escapee
    /// inside a container" family of holes in every rule at once.
    /// </summary>
    private static IEnumerable<Expr> EscapeeLeaves(Expr e)
    {
        e = UnwrapEscapee(e);
        switch (e)
        {
            case ConditionalExpr c:
                foreach (var x in EscapeeLeaves(c.Then)) yield return x;
                foreach (var x in EscapeeLeaves(c.Else)) yield return x;
                break;

            case BinaryExpr { Op: BinaryOp.Coalesce } b:
                foreach (var x in EscapeeLeaves(b.Left)) yield return x;
                foreach (var x in EscapeeLeaves(b.Right)) yield return x;
                break;

            case SwitchExpr sw:
                foreach (var arm in sw.Arms)
                    foreach (var x in EscapeeLeaves(arm.Result)) yield return x;
                break;

            case TupleExpr t:
                foreach (var el in t.Elements)
                    foreach (var x in EscapeeLeaves(el)) yield return x;
                break;

            case ArrayExpr arr:
                foreach (var el in arr.Elements)
                    foreach (var x in EscapeeLeaves(el)) yield return x;
                break;

            case NewExpr n:
                foreach (var a in n.Args)
                    foreach (var x in EscapeeLeaves(a)) yield return x;
                if (n.Initializer is not null)
                    foreach (var init in n.Initializer)
                    {
                        // An object-initializer element `Prop = v` is an
                        // AssignExpr; the value is what gets carried in.
                        var v = init is AssignExpr ae ? ae.Right : init;
                        foreach (var x in EscapeeLeaves(v)) yield return x;
                    }
                break;

            case MethodCallExpr mc:
                foreach (var x in CallArgLeaves(mc.Args)) yield return x;
                break;

            case InvocationExpr inv:
                foreach (var x in CallArgLeaves(inv.Args)) yield return x;
                break;

            default:
                // LambdaExpr, NameExpr, MemberAccessExpr, literals … — a leaf.
                yield return e;
                break;
        }
    }

    /// <summary>Leaves reachable through a call's arguments: a non-lambda arg is
    /// carried in directly (<c>ImmutableArray.Create(reg)</c> carries
    /// <c>reg</c>), a projection lambda carries whatever it RETURNS
    /// (<c>Select(x =&gt; bump)</c> carries <c>bump</c>).</summary>
    private static IEnumerable<Expr> CallArgLeaves(IReadOnlyList<Expr> args)
    {
        foreach (var a in args)
        {
            if (UnwrapEscapee(a) is LambdaExpr lam)
            {
                foreach (var ret in LambdaReturns(lam))
                    foreach (var x in EscapeeLeaves(ret)) yield return x;
            }
            else
            {
                foreach (var x in EscapeeLeaves(a)) yield return x;
            }
        }
    }

    /// <summary>The expressions a lambda hands back to its caller — its body
    /// (expression-bodied) or every <c>return</c> value (block-bodied).</summary>
    private static IEnumerable<Expr> LambdaReturns(LambdaExpr lam) => lam.Body switch
    {
        Expr ex      => [ex],
        BlockStmt bs => ReturnedValues(bs),
        _            => [],
    };

    private static IEnumerable<Expr> ReturnedValues(Stmt s)
    {
        switch (s)
        {
            case BlockStmt b:
                foreach (var st in b.Statements)
                    foreach (var v in ReturnedValues(st)) yield return v;
                break;
            case ReturnStmt { Value: { } v }:
                yield return v;
                break;
            case IfStmt i:
                foreach (var v in ReturnedValues(i.Then)) yield return v;
                if (i.Else is not null)
                    foreach (var v in ReturnedValues(i.Else)) yield return v;
                break;
        }
    }

    /// <summary>The state-mutating write carried by a single escapee leaf — an
    /// inline lambda or a local tainted by one — or <c>null</c>.</summary>
    private static string? LeafWrite(Expr leaf, EscapeState st) => leaf switch
    {
        LambdaExpr lam => StateWritten(lam, st),
        NameExpr n when n.Name.Parts.Count == 1 => st.Writes(n.Name.Simple),
        _ => null,
    };

    /// <summary>The read-only state capture carried by a single escapee leaf —
    /// the CE0136 analogue of <see cref="LeafWrite"/>.</summary>
    private static string? LeafRead(Expr leaf, EscapeState st) => leaf switch
    {
        LambdaExpr lam => StateRead(lam, st),
        NameExpr n when n.Name.Parts.Count == 1 => st.Reads(n.Name.Simple),
        _ => null,
    };

    /// <summary>The first state-write carried anywhere in <paramref name="value"/>'s
    /// reachable leaves, or <c>null</c>.</summary>
    private static string? FirstLeafWrite(Expr value, EscapeState st)
        => EscapeeLeaves(value).Select(l => LeafWrite(l, st)).FirstOrDefault(w => w is not null);

    /// <summary>The first read-only state capture carried anywhere in
    /// <paramref name="value"/>'s reachable leaves, or <c>null</c>.</summary>
    private static string? FirstLeafRead(Expr value, EscapeState st)
        => EscapeeLeaves(value).Select(l => LeafRead(l, st)).FirstOrDefault(r => r is not null);

    /// <summary>Re-binds a local's write/read taint from what its initializer or
    /// new RHS reaches. A write anywhere in the value taints the write map; a
    /// read-only capture taints the read map; a value that carries neither
    /// clears both (so a sibling block reusing the name sheds stale taint).</summary>
    private static void RebindLocalTaint(string name, Expr value, EscapeState st)
    {
        var write = FirstLeafWrite(value, st);
        st.Set(name, write);
        st.SetRead(name, write is null ? FirstLeafRead(value, st) : null);
    }

    /// <summary>
    /// CE0136's trusted-callee name set: the System.Linq operators plus the
    /// <c>List&lt;T&gt;</c> members with LINQ-identical synchronous semantics.
    /// These invoke their callback immediately, on the calling thread, and do
    /// not retain it — a read capture handed to one never outlives the turn.
    /// A name table in the CE0083/CE0119 tradition: the analyzer has no
    /// foreign type resolution, so trust rides on the name. A third-party
    /// method that happens to share one of these names slips through, which is
    /// the accepted cost of every blocklist/allowlist in this family.
    /// </summary>
    private static readonly HashSet<string> LinqStyleCallbackConsumers =
        new(StringComparer.Ordinal)
        {
            "Where", "Select", "SelectMany",
            "OrderBy", "OrderByDescending", "ThenBy", "ThenByDescending",
            "First", "FirstOrDefault", "Single", "SingleOrDefault",
            "Last", "LastOrDefault",
            "Any", "All", "Count", "Sum", "Min", "Max", "Average", "Aggregate",
            "GroupBy", "Join", "GroupJoin", "Zip", "TakeWhile", "SkipWhile",
            "Distinct", "DistinctBy", "ToDictionary", "ToLookup",
            // List<T> members with LINQ-identical synchronous semantics.
            "Find", "FindAll", "Exists", "TrueForAll", "RemoveAll", "Sort",
            "ConvertAll",
        };

    /// <summary>The Spek.Streams factory names — the pipeline operators the
    /// chain emitter itself generates calls to. Spek-owned code; trusted.</summary>
    private static readonly HashSet<string> StreamFactoryNames =
        new(StringComparer.Ordinal) { "debounce", "throttle", "distinct", "compose" };

    /// <summary>
    /// True when a method call's target is one CE0136 trusts with a read-only
    /// state capture: a LINQ-style immediate callback consumer (by name), the
    /// actor's own method, or a confined-class / module method that PROVABLY
    /// keeps the delegate on this thread.
    /// <para>
    /// The confined-class and module trust is <em>earned</em>, not assumed. The
    /// original rule trusted any Spek-source method on the theory that it
    /// compiles under these same rules — but CE0135/CE0136 never run on class or
    /// module method BODIES, so a method that quietly hands the delegate to a
    /// foreign sink (<c>void Take(Action a) {{ Acme.Global.Store(a); }}</c>) was
    /// trusted while doing exactly what the rule forbids. So the trust is now
    /// backed by <see cref="MethodKeepsDelegatesOnThread"/>: a confined-class or
    /// module method is trusted only when none of its delegate-typed parameters
    /// escapes its body. A method that leaks one drops to the untrusted boundary
    /// and its callers get their CE0136.
    /// </para>
    /// </summary>
    private static bool IsTrustedCallbackConsumer(MethodCallExpr call, EscapeState st)
    {
        // `self.M(...)` — the actor's own method.
        if (call.Target is SelfExpr && st.Actor.Methods.ContainsKey(call.Method)) return true;

        var receiver = call.Target switch
        {
            NameExpr n when n.Name.Parts.Count == 1 => n.Name.Simple,
            MemberAccessExpr ma when ma.Target is SelfExpr => ma.Member,
            _ => null,
        };

        if (receiver is not null)
        {
            // A method on a confined-class actor field — Spek source. Trust is
            // EARNED (keeps its delegate params on-thread), and it is judged
            // here BEFORE the LINQ name table: a Spek class with a method named
            // `Where` that stores the callback is a sink, not a LINQ operator,
            // and must not borrow the name table's trust.
            if (st.Actor.Fields.TryGetValue(receiver, out var field)
                && st.Symbols.GetClassSymbols(field.Type.Name.Simple) is { } classSyms
                && classSyms.Methods.TryGetValue(call.Method, out var classMethod))
                return MethodKeepsDelegatesOnThread(classMethod);

            // A Spek module's function — same earned trust, same precedence.
            if (st.Symbols.ResolveModule(receiver) is { } module
                && module.Methods.FirstOrDefault(m => m.Name == call.Method) is { } moduleMethod)
                return MethodKeepsDelegatesOnThread(moduleMethod);
        }

        // Otherwise the callee is foreign (a BCL collection, a third-party type
        // Spek cannot resolve). Trust rides on the LINQ operator name — the
        // accepted name-table residue: a foreign method that merely shares the
        // name slips through, as with every allowlist in this family.
        return LinqStyleCallbackConsumers.Contains(call.Method);
    }

    /// <summary>Delegate-typed parameter names, recognised syntactically —
    /// <c>Action</c> / <c>Func</c> / <c>Predicate</c> / <c>Comparison</c> /
    /// <c>Converter</c> / <c>EventHandler</c> (with or without a namespace
    /// qualifier or generic args). A user-declared delegate type would be missed
    /// (Spek has no delegate declaration), which only ever makes the trust check
    /// more permissive on a shape that does not occur in Spek source.</summary>
    private static readonly HashSet<string> DelegateTypeNames = new(StringComparer.Ordinal)
    {
        "Action", "Func", "Predicate", "Comparison", "Converter", "EventHandler",
    };

    private static bool IsDelegateType(TypeRef type) => DelegateTypeNames.Contains(type.Name.Simple);

    /// <summary>
    /// True when <paramref name="method"/> never lets a delegate-typed parameter
    /// escape to a place off the owning thread — the property that makes trusting
    /// it with a read capture sound. A delegate param may be invoked directly
    /// (<c>a()</c>), handed to a LINQ-style synchronous consumer, or STORED in
    /// the class's own field (<c>saved = a;</c>): the stored delegate is reachable
    /// only through this object, and the object's confinement — enforced by
    /// CE0137 / CE0010 / CE0112, which block every route that would hand the
    /// object to another actor — keeps it on the owning thread. What DOES escape
    /// is forwarding the param to a foreign sink: an argument to a call, a
    /// <c>new</c>, or a return, whose destination Spek cannot see
    /// (<c>Acme.Global.Store(a)</c>). Conservative: no interprocedural follow.
    /// <para>
    /// Residue (documented): a confined class that STORES a read-capturing
    /// delegate and later fires it from a CONCURRENT reader handler races — the
    /// storage looks on-thread here, and the concurrent-fire is a whole-actor,
    /// reader/writer property this per-method check does not model. It is the
    /// narrower sibling of the a3/a4 foreign-sink hole this rule does close.
    /// </para>
    /// </summary>
    private static bool MethodKeepsDelegatesOnThread(MethodDecl method)
    {
        var delegateParams = method.Parameters
            .Where(p => IsDelegateType(p.Type))
            .Select(p => p.Name)
            .ToHashSet(StringComparer.Ordinal);
        if (delegateParams.Count == 0) return true;   // nothing to leak

        bool NamesDelegateParam(Expr e) =>
            UnwrapEscapee(e) is NameExpr n && n.Name.Parts.Count == 1
            && delegateParams.Contains(n.Name.Simple);

        foreach (var e in ClassSymbols.ExprsIn(method.Body))
        {
            switch (e)
            {
                // `a()` invokes the delegate here, on-thread — not an escape.
                case InvocationExpr inv when delegateParams.Contains(inv.Callee):
                    break;

                // Passing a delegate param as an argument to anything but a
                // LINQ-style synchronous consumer forwards it somewhere Spek
                // cannot see — an escape.
                case MethodCallExpr mc when mc.Args.Any(NamesDelegateParam):
                    if (!LinqStyleCallbackConsumers.Contains(mc.Method)) return false;
                    break;
                case InvocationExpr inv when inv.Args.Any(NamesDelegateParam):
                    return false;
                case NewExpr nw when nw.Args.Any(NamesDelegateParam)
                                     || (nw.Initializer?.Any(x =>
                                            x is AssignExpr ae ? NamesDelegateParam(ae.Right) : NamesDelegateParam(x)) ?? false):
                    return false;
            }
        }

        // A `return a;` of a delegate param hands it to the caller — an escape.
        foreach (var v in ReturnedValues(method.Body))
            if (NamesDelegateParam(v)) return false;

        return true;
    }

    private static void ScanBlockEscapes(BlockStmt block, EscapeState st, List<Diagnostic> diagnostics)
    {
        foreach (var stmt in block.Statements)
            ScanStmtEscapes(stmt, st, diagnostics);
    }

    /// <summary>Statement-ordered walk: expressions are scanned for escape
    /// sites, then a <c>var</c> binding records (or clears) the local's taint so
    /// later statements see it.</summary>
    private static void ScanStmtEscapes(Stmt stmt, EscapeState st, List<Diagnostic> diagnostics)
    {
        switch (stmt)
        {
            case BlockStmt nested:
                ScanBlockEscapes(nested, st, diagnostics);
                break;

            case VarDeclStmt v:
                ScanEscapes(v.Initializer, st, diagnostics);
                // Binding a state-capturing value to a local is legal on its
                // own — it only matters where the local is used. The taint
                // propagates by REACHABILITY, not shape: a mutating lambda, a
                // copy of a tainted local (`var b = bump;`), or a tainted lambda
                // hidden in a container (`var arr = new Action[] { bump };`) all
                // make the new local carry the write. A read-only capturer taints
                // the read map instead (never both — mutation subsumes).
                RebindLocalTaint(v.Name, v.Initializer, st);
                // CE0137 — record whether the local now aliases a class-typed
                // actor field, holds a fresh `new` of a declared class, or was
                // `new`ed capturing a class-typed field.
                st.BindClassLocal(v.Name, UnwrapEscapee(v.Initializer));
                st.SetCapturingLocal(v.Name, NewlyCapturedClassField(v.Initializer, st));
                break;

            case ReturnStmt r when r.Value is not null:
                var reportedReturnWrite = false;
                foreach (var leaf in EscapeeLeaves(r.Value))
                    if (LeafWrite(leaf, st) is { } returned)
                    {
                        ReportEscape(leaf, returned,
                            "is returned, which hands it to a caller that may invoke it " +
                            "on another thread, outside the actor's turn",
                            callee: null, st, diagnostics);
                        reportedReturnWrite = true;
                    }
                // CE0010 — a reply that hands back a confined class (directly,
                // `return reg;`, or nested in a message, `return new Box(reg);`)
                // shares mutable state with the asker: the reply IS a message.
                // Only in a handler, where a return is an ask-reply.
                if (st.ReturnsAreReplies) CheckReturnClassShare(r.Value, st, diagnostics);
                if (!reportedReturnWrite) ScanEscapes(r.Value, st, diagnostics);
                break;

            case ExpressionStmt es:
                ScanEscapes(es.Expr, st, diagnostics);
                // A plain local reassignment re-binds its taint, exactly as a
                // `var` declaration does — the write/read taint (CE0135/CE0136)
                // and the class taint (CE0137). Assignments into actor state are
                // not local re-binds and are skipped.
                if (es.Expr is AssignExpr { Op: AssignOp.Assign } la
                    && la.Left is NameExpr ln && ln.Name.Parts.Count == 1
                    && !st.Actor.StateNames.Contains(ln.Name.Simple))
                {
                    RebindLocalTaint(ln.Name.Simple, la.Right, st);
                    st.BindClassLocal(ln.Name.Simple, UnwrapEscapee(la.Right));
                    st.SetCapturingLocal(ln.Name.Simple, NewlyCapturedClassField(la.Right, st));
                }
                break;

            case IfStmt ifs:
                ScanEscapes(ifs.Condition, st, diagnostics);
                ScanBlockEscapes(ifs.Then, st, diagnostics);
                if (ifs.Else is not null) ScanStmtEscapes(ifs.Else, st, diagnostics);
                break;

            case WhileStmt ws:
                ScanEscapes(ws.Condition, st, diagnostics);
                ScanBlockEscapes(ws.Body, st, diagnostics);
                break;

            case DoWhileStmt dw:
                ScanBlockEscapes(dw.Body, st, diagnostics);
                ScanEscapes(dw.Condition, st, diagnostics);
                break;

            case ForStmt fs:
                ScanStmtEscapes(fs.Init, st, diagnostics);
                ScanEscapes(fs.Condition, st, diagnostics);
                ScanEscapes(fs.Increment, st, diagnostics);
                ScanBlockEscapes(fs.Body, st, diagnostics);
                break;

            case ForeachStmt fe:
                ScanEscapes(fe.Collection, st, diagnostics);
                ScanBlockEscapes(fe.Body, st, diagnostics);
                break;

            case SwitchStmt sw:
                ScanEscapes(sw.Subject, st, diagnostics);
                foreach (var sec in sw.Sections)
                {
                    foreach (var l in sec.Labels)
                        if (l.Guard is not null) ScanEscapes(l.Guard, st, diagnostics);
                    foreach (var s in sec.Body) ScanStmtEscapes(s, st, diagnostics);
                }
                break;

            case TryStmt t:
                ScanBlockEscapes(t.Try, st, diagnostics);
                foreach (var c in t.Catches)
                {
                    if (c.When is not null) ScanEscapes(c.When, st, diagnostics);
                    ScanBlockEscapes(c.Body, st, diagnostics);
                }
                if (t.Finally is not null) ScanBlockEscapes(t.Finally, st, diagnostics);
                break;

            case ThrowStmt th when th.Value is not null:
                ScanEscapes(th.Value, st, diagnostics);
                break;
        }
    }

    /// <summary>
    /// Scans one expression tree for escape sites: any call argument, and any
    /// assignment into actor state. The walk is the shared
    /// <see cref="ClassSymbols.ExprsIn(Expr)"/> enumeration, so nested calls are
    /// each visited once and reported at the innermost call that receives the
    /// lambda.
    /// </summary>
    private static void ScanEscapes(Expr expr, EscapeState st, List<Diagnostic> diagnostics)
    {
        foreach (var node in ClassSymbols.ExprsIn(expr))
        {
            switch (node)
            {
                // `registry.Register(bump)` / `list.ForEach(x => total = total + x)`
                case MethodCallExpr mc:
                    ScanArgs(mc.Args, mc.Method,
                        trustedWithReads: IsTrustedCallbackConsumer(mc, st),
                        // A write handed to a LINQ-style synchronous consumer
                        // (`Sort`/`Where`/…, but NOT `ForEach`) runs on this
                        // thread and is never retained, so the write is safe.
                        syncWriteSafe: IsSynchronousWriteSafe(mc.Method),
                        st, diagnostics);
                    break;

                // A bare `Register(bump)`. Note the callee itself is NOT an
                // escape: `bump()` invokes the lambda here, inside the turn,
                // which is the legal case — only arguments travel. The actor's
                // own functions and the stream factories are Spek code, so a
                // read capture may travel into them without a CE0136.
                case InvocationExpr inv:
                    ScanArgs(inv.Args, inv.Callee,
                        trustedWithReads: st.Actor.Methods.ContainsKey(inv.Callee)
                                          || StreamFactoryNames.Contains(inv.Callee),
                        syncWriteSafe: IsSynchronousWriteSafe(inv.Callee),
                        st, diagnostics);
                    break;

                // `new` of a Spek class or message is Spek code; `new` of a
                // foreign type is the untrusted boundary.
                case NewExpr n:
                    ScanArgs(n.Args, n.Type.Parts[^1],
                        trustedWithReads: st.Symbols.ResolveClass(n.Type.Parts[^1]) is not null
                                          || st.Symbols.ResolveMessage(n.Type.Parts[^1]) is not null,
                        syncWriteSafe: false,
                        st, diagnostics);
                    break;

                // A spawned child is another actor on another thread — its
                // reads of this actor's state race by definition, so spawn
                // arguments are never trusted with a read capture.
                case SpawnExpr sp:
                    ScanArgs(sp.Args,
                        sp.TypeArgs.Count > 0 ? $"spawn<{sp.TypeArgs[0].Name.Simple}>" : "spawn",
                        trustedWithReads: false,
                        syncWriteSafe: false,
                        st, diagnostics);
                    // CE0137 — a class-typed argument that aliases the
                    // sender's state would make two actors share one mutable
                    // object.
                    CheckSpawnClassArgs(sp, st, diagnostics);
                    break;

                // `pending = bump;` — an actor field outlives every turn.
                case AssignExpr a when StateMutation.Root(a.Left, st.Actor.StateNames) is { } slot:
                    foreach (var leaf in EscapeeLeaves(a.Right))
                        if (LeafWrite(leaf, st) is { } stored)
                            ReportEscape(leaf, stored,
                                $"is stored in actor state ('{slot}'), which outlives this turn — " +
                                "anything holding it may invoke it on another thread",
                                callee: null, st, diagnostics);
                    break;
            }
        }
    }

    private static void ScanArgs(
        IReadOnlyList<Expr> args, string callee, bool trustedWithReads, bool syncWriteSafe,
        EscapeState st, List<Diagnostic> diagnostics)
    {
        foreach (var arg in args)
            foreach (var leaf in EscapeeLeaves(arg))
            {
                if (LeafWrite(leaf, st) is { } write)
                {
                    // A write handed to a trusted synchronous consumer is safe
                    // (it runs here, is not retained); anything else escapes.
                    if (!syncWriteSafe)
                        ReportEscape(leaf, write,
                            $"is passed to '{callee}', which may invoke it on another thread, " +
                            "outside the actor's turn",
                            callee, st, diagnostics);
                }
                else if (!trustedWithReads && LeafRead(leaf, st) is { } captured)
                {
                    ReportReadCapture(leaf, captured, callee, st, diagnostics);
                }
            }
    }

    /// <summary>The LINQ-style operators that invoke their callback immediately
    /// on this thread and never retain it, so even a state-<em>writing</em>
    /// callback handed to one runs inside the turn and is safe. This is the
    /// <see cref="LinqStyleCallbackConsumers"/> set — which deliberately EXCLUDES
    /// <c>ForEach</c>: a mutating <c>ForEach</c> stays a CE0135 (the documented
    /// "use a foreach loop" nudge, which the diagnostic still spells out).</summary>
    private static bool IsSynchronousWriteSafe(string method)
        => LinqStyleCallbackConsumers.Contains(method);

    /// <summary>CE0136 — a read-only state capture handed to a callee the
    /// compiler cannot see into. Warning, not error: the capture is safe if
    /// the callee only invokes it synchronously, and the compiler cannot tell.
    /// Caret on the argument.</summary>
    private static void ReportReadCapture(
        Expr node, string captured, string callee,
        EscapeState st, List<Diagnostic> diagnostics)
    {
        if (st.AlreadyCovered(node)) return;
        st.MarkReported(node);

        // The copy idiom, spelled with the user's own names: `var s = seen;`.
        var copy = captured.Length > 1
            ? char.ToLowerInvariant(captured[0]).ToString()
            : captured + "Copy";

        diagnostics.Add(Diagnostic.At("CE0136", node.Span,
            $"Lambda captures actor state ('{captured}') by reference and is passed to " +
            $"'{callee}', which Spek cannot see into — if it stores or parallelizes the " +
            $"callback, its reads race this actor's writes. Capture a copy instead " +
            $"('var {copy} = {captured};'), or have the callback Tell the actor.",
            DiagnosticSeverity.Warning));
    }

    private static void ReportEscape(
        Expr node, string write, string route, string? callee,
        EscapeState st, List<Diagnostic> diagnostics)
    {
        // Anything inside an already-reported lambda escapes *with* it; one
        // diagnostic at the outer boundary is the actionable one.
        if (st.AlreadyCovered(node)) return;
        st.MarkReported(node);

        // `ForEach` is the recognised over-approximation: the callback runs
        // immediately, so it is safe in fact, but the analysis cannot know that
        // and Spek has a better way to write it regardless.
        var fix = callee == "ForEach"
            ? "Use a 'foreach' loop instead — it runs in the handler, where writing actor state is safe."
            : "Capture what you need by value, or have the callback Tell the actor " +
              "and do the work in a handler.";

        diagnostics.Add(Diagnostic.At("CE0135", node.Span,
            $"State-capturing lambda escapes its handler — it {write} and {route}. {fix}"));
    }

    /// <summary>
    /// CE0137 — spawn arguments may not share a confined class with the child.
    /// The sibling routes are already gated (CE0010: message fields, CE0112:
    /// region fields); this closes the third, where <c>spawn&lt;Child&gt;(reg)</c>
    /// silently made the sender and the child co-owners of one mutable object —
    /// including any delegate laundered into it, which the child would then
    /// invoke on its own thread. Flagged shapes: a class-typed actor field or
    /// property (bare or <c>self.</c>-qualified), a local tainted by one
    /// (<c>var r = reg;</c>), a <c>new</c>-initialized local OR class-typed
    /// <c>init</c>/method parameter that is ALSO stored in actor state
    /// anywhere in the body, and a class-typed region field read through a
    /// <c>use</c> handle. A fresh <c>new</c> passed inline — or a gifted
    /// local / received parameter never stored in a field — is the legal
    /// hand-off: exactly one owner at every instant, and a pure relay is a
    /// legitimate gift chain (what the parent passed was checked at the
    /// parent's own spawn). A foreign-typed field
    /// argument is the documented residue, same stance as CE0135/CE0136:
    /// whether it is mutable is unknowable without foreign type resolution.
    /// </summary>
    private static void CheckSpawnClassArgs(
        SpawnExpr sp, EscapeState st, List<Diagnostic> diagnostics)
    {
        var child = sp.TypeArgs.Count > 0 ? sp.TypeArgs[0].Name.Simple : "Child";
        foreach (var raw in sp.Args)
            foreach (var leaf in EscapeeLeaves(raw))
                if (ClassShareLeaf(leaf, st) is { } share)
                    ReportSpawnShare(leaf, share.What, share.Class, child, st, diagnostics,
                        giftedButStored: share.Kind == ShareKind.GiftedStored);
    }

    /// <summary>The shapes in which a confined class reaches an escape site —
    /// a spawn argument (CE0137) or a handler return (CE0010).</summary>
    private enum ShareKind { Field, SelfField, Alias, Captured, GiftedStored, Region }

    /// <summary>
    /// Classifies a single escapee leaf as a confined-class share, or
    /// <c>null</c>. One definition serves both the spawn walk (CE0137) and the
    /// ask-reply return walk (CE0010): a class-typed actor field (bare or
    /// <c>self.</c>), a local aliasing one (<c>var r = reg;</c>), a local
    /// <c>new</c>ed capturing one (<c>var w = new Wrapper(reg);</c>), a gifted
    /// local / parameter that is ALSO stored in actor state, or a class-typed
    /// region field read through a <c>use</c> handle. A fresh <c>new</c> or a
    /// gifted local never stored is NOT a share — that is the legal one-owner
    /// hand-off.
    /// </summary>
    private static (string What, string Class, string Field, ShareKind Kind)? ClassShareLeaf(
        Expr leaf, EscapeState st)
    {
        switch (leaf)
        {
            case NameExpr n when n.Name.Parts.Count == 1:
            {
                var name = n.Name.Simple;
                if (st.ClassTypedState(name) is { } cls)
                    return ($"'{name}'", cls, name, ShareKind.Field);
                if (st.ClassAlias(name) is { } alias)
                    return ($"'{name}', which aliases actor field '{alias.Field}'",
                        alias.Class, alias.Field, ShareKind.Alias);
                if (st.CapturedClassField(name) is { } cap)
                    return ($"'{name}', which captures actor field '{cap.Field}'",
                        cap.Class, cap.Field, ShareKind.Captured);
                if (st.GiftedClass(name) is { } gifted && st.FieldAssignedLocals.Contains(name))
                    return (st.IsClassParam(name)
                            ? $"'{name}', a parameter that is also stored in actor state"
                            : $"'{name}', which is also stored in actor state",
                        gifted, name, ShareKind.GiftedStored);
                return null;
            }

            // `self.reg`.
            case MemberAccessExpr ma when ma.Target is SelfExpr
                                          && st.ClassTypedState(ma.Member) is { } cls:
                return ($"'self.{ma.Member}'", cls, ma.Member, ShareKind.SelfField);

            // `cache.obj` through a `use` handle — greedy-parsed as a two-part
            // name when no postfix operator follows.
            case NameExpr n2 when n2.Name.Parts.Count == 2
                                  && st.RegionFieldClass(n2.Name.Parts[0], n2.Name.Parts[1]) is { } cls:
                return ($"'{n2.Name.Parts[0]}.{n2.Name.Parts[1]}', read through a 'use' region handle",
                    cls, n2.Name.Parts[1], ShareKind.Region);

            // The MemberAccessExpr spelling of the same region read.
            case MemberAccessExpr ma2 when ma2.Target is NameExpr rn
                                           && rn.Name.Parts.Count == 1
                                           && st.RegionFieldClass(rn.Name.Simple, ma2.Member) is { } cls:
                return ($"'{rn.Name.Simple}.{ma2.Member}', read through a 'use' region handle",
                    cls, ma2.Member, ShareKind.Region);

            default:
                return null;
        }
    }

    /// <summary>The class-typed actor field a <c>new</c>-expression embeds in a
    /// constructor argument or initializer (<c>new Wrapper(reg)</c> /
    /// <c>new Holder { Reg = reg }</c>), or <c>null</c>. A fresh <c>new</c> whose
    /// arguments are themselves fresh or primitive captures nothing.</summary>
    private static (string Field, string Class)? NewlyCapturedClassField(Expr value, EscapeState st)
    {
        if (UnwrapEscapee(value) is not NewExpr n) return null;
        foreach (var leaf in EscapeeLeaves(n))
            if (ClassShareLeaf(leaf, st) is { Kind: ShareKind.Field or ShareKind.SelfField
                                              or ShareKind.Alias or ShareKind.Region } share)
                return (share.Field, share.Class);
        return null;
    }

    /// <summary>CE0010 — a handler <c>return</c> is an ask-reply, and a reply is
    /// a message: it may not hand back a confined class (directly,
    /// <c>return reg;</c>, or nested in a message envelope,
    /// <c>return new Box(reg);</c>). This is the immutability admission CE0010
    /// applies to declared message fields, enforced at the reply boundary the
    /// field declaration can't see. A fresh instance is the legal gift, exactly
    /// as with spawn.</summary>
    private static void CheckReturnClassShare(Expr value, EscapeState st, List<Diagnostic> diagnostics)
    {
        foreach (var leaf in EscapeeLeaves(value))
            if (ClassShareLeaf(leaf, st) is { } share)
            {
                if (st.AlreadyCovered(leaf)) continue;
                st.MarkReported(leaf);
                diagnostics.Add(Diagnostic.At("CE0010", leaf.Span,
                    $"Handler returns the confined class ({share.What}) as an ask-reply — " +
                    $"a reply is a message, and a class is mutable, so the asker and this actor " +
                    $"would share one mutable object. Reply with an immutable 'message' (copy the " +
                    $"fields you need), or hand over a fresh instance the actor no longer keeps."));
            }
    }

    /// <summary>CE0137 — hard error, caret on the argument.</summary>
    private static void ReportSpawnShare(
        Expr arg, string what, string className, string child,
        EscapeState st, List<Diagnostic> diagnostics, bool giftedButStored = false)
    {
        // Inside a lambda that already escaped, the outer CE0135 report is the
        // actionable one — same suppression the lambda rules apply.
        if (st.AlreadyCovered(arg)) return;
        st.MarkReported(arg);

        var fix = giftedButStored
            ? $"Let the child own it (remove the assignment into actor state), " +
              $"or share data by sending messages."
            : $"Pass a fresh instance ('spawn<{child}>(new {className}())'), " +
              $"or share data by sending messages.";

        diagnostics.Add(Diagnostic.At("CE0137", arg.Span,
            $"Spawn argument shares the confined class ({what}) with the child actor — " +
            $"the sender keeps its reference, so two actors would hold the same mutable " +
            $"object, and anything stored inside it (including callbacks) runs on the " +
            $"child's thread. {fix}"));
    }

    // ─── CE0085 — moved-value mutation tracking (with alias tracking) ───────

    /// <summary>
    /// Block-local affine dataflow catching mutation of a value after it has
    /// been sent via <c>Tell</c> / <c>ask</c>. Once a local is passed as the
    /// payload to <c>someRef.Tell(local)</c>, any later assignment reaching
    /// that value — directly <em>or through an alias</em> — is a mutation of
    /// state another actor now logically owns (a classic race source).
    /// <list type="bullet">
    ///   <item>Top-level statement order in each block.</item>
    ///   <item>Tracks moved locals (Tell/ask payloads) and <b>alias groups</b>
    ///         (<c>var w = u;</c> makes <c>w</c> alias <c>u</c>), so moving any
    ///         member of a group marks the whole group moved.</item>
    ///   <item>Reports CE0085 for assignments whose LHS reaches a moved name
    ///         through member or index access (<c>moved.field = X</c>,
    ///         <c>moved[i] = X</c>) — including via an alias.</item>
    /// </list>
    /// Reads of a moved value are <em>not</em> flagged: today the only sendable
    /// values are immutable <c>message</c>s, where post-send sharing (reads) is
    /// safe — only mutation is a race. A full read-also "use after move" arrives
    /// with the mutable-class move (which it gates on). Pure reassignment
    /// (<c>moved = newValue</c>) re-binds the local and isn't flagged. Nested
    /// blocks are analyzed with a cloned state so their writes don't escape.
    /// </summary>
    private static void CheckMovedValueFlow(BlockStmt block, List<Diagnostic> diagnostics)
    {
        AnalyzeBlockMoves(block, new MoveState(), diagnostics);
    }

    /// <summary>
    /// CE0085 move/alias state for one (block-local) scope. Tracks which locals
    /// have been <em>moved</em> (sent via Tell/ask) and which names <em>alias</em>
    /// the same underlying value (<c>var w = u;</c>), so moving any member of an
    /// alias group moves the whole group — mutating any alias of a moved value
    /// is then a CE0085. (This is the alias tracking the type/ownership model
    /// assumes; the deep/transitive case — aliasing through nested fields — is
    /// the parked isolation-proof problem.)
    /// </summary>
    private sealed class MoveState
    {
        public HashSet<string> Moved { get; } = new(StringComparer.Ordinal);
        // name -> the shared set of names it aliases (including itself).
        private readonly Dictionary<string, HashSet<string>> _group = new(StringComparer.Ordinal);

        private HashSet<string> GroupOf(string n)
        {
            if (!_group.TryGetValue(n, out var g)) { g = new(StringComparer.Ordinal) { n }; _group[n] = g; }
            return g;
        }

        public bool IsMoved(string n) => Moved.Contains(n);

        /// <summary><c>var a = b;</c> — a now refers to the same value as b.</summary>
        public void Alias(string a, string b)
        {
            var ga = GroupOf(a);
            var gb = GroupOf(b);
            if (ReferenceEquals(ga, gb)) return;
            foreach (var m in gb) { ga.Add(m); _group[m] = ga; }     // merge b's group into a's
            if (ga.Overlaps(Moved)) foreach (var m in ga) Moved.Add(m);   // moved-ness covers the union
        }

        /// <summary>The value <paramref name="n"/> names is sent — it and all its aliases move.</summary>
        public void Move(string n)
        {
            foreach (var m in GroupOf(n)) Moved.Add(m);
        }

        /// <summary>Independent copy (nested-scope writes don't escape to the parent).</summary>
        public MoveState Clone()
        {
            var c = new MoveState();
            foreach (var m in Moved) c.Moved.Add(m);
            var done = new HashSet<string>(StringComparer.Ordinal);
            foreach (var name in _group.Keys)
            {
                if (done.Contains(name)) continue;
                var members = _group[name];
                var ng = new HashSet<string>(members, StringComparer.Ordinal);
                foreach (var mm in members) { c._group[mm] = ng; done.Add(mm); }
            }
            return c;
        }
    }

    private static void AnalyzeBlockMoves(
        BlockStmt block, MoveState state, List<Diagnostic> diagnostics)
    {
        foreach (var stmt in block.Statements)
            AnalyzeStmtMoves(stmt, state, diagnostics);
    }

    private static void AnalyzeStmtMoves(
        Stmt stmt, MoveState state, List<Diagnostic> diagnostics)
    {
        switch (stmt)
        {
            case BlockStmt nested:
                // Nested blocks inherit reads but their writes don't escape.
                AnalyzeBlockMoves(nested, state.Clone(), diagnostics);
                break;

            case IfStmt ifs:
            {
                FindMutationsAndSends(ifs.Condition, state, diagnostics);
                // `if (x is T name)` — inside the then-branch, `name` IS x
                // (same object, refined type), so it must share x's alias
                // group or a send of one laundered past the other.
                var thenState = state.Clone();
                foreach (var (binding, source) in IsCaptureAliases(ifs.Condition))
                    thenState.Alias(binding, source);
                AnalyzeBlockMoves(ifs.Then, thenState, diagnostics);
                if (ifs.Else is not null)
                    AnalyzeStmtMoves(ifs.Else, state.Clone(), diagnostics);
                break;
            }

            case ForStmt fs:
                FindMutationsAndSends(fs.Init.Initializer, state, diagnostics);
                FindMutationsAndSends(fs.Condition, state, diagnostics);
                FindMutationsAndSends(fs.Increment, state, diagnostics);
                AnalyzeBlockMoves(fs.Body, state.Clone(), diagnostics);
                break;

            case ForeachStmt fe:
                FindMutationsAndSends(fe.Collection, state, diagnostics);
                AnalyzeBlockMoves(fe.Body, state.Clone(), diagnostics);
                break;
            case SwitchStmt sw:
                FindMutationsAndSends(sw.Subject, state, diagnostics);
                foreach (var sec in sw.Sections)
                {
                    foreach (var l in sec.Labels)
                        if (l.Guard is not null) FindMutationsAndSends(l.Guard, state, diagnostics);
                    AnalyzeBlockMoves(new BlockStmt(sec.Span, sec.Body), state.Clone(), diagnostics);
                }
                break;

            case WhileStmt ws:
            {
                FindMutationsAndSends(ws.Condition, state, diagnostics);
                var bodyState = state.Clone();
                foreach (var (binding, source) in IsCaptureAliases(ws.Condition))
                    bodyState.Alias(binding, source);
                AnalyzeBlockMoves(ws.Body, bodyState, diagnostics);
                break;
            }

            case DoWhileStmt dw:
                AnalyzeBlockMoves(dw.Body, state.Clone(), diagnostics);
                FindMutationsAndSends(dw.Condition, state, diagnostics);
                break;

            case ReturnStmt r when r.Value is not null:
                FindMutationsAndSends(r.Value, state, diagnostics);
                break;

            case VarDeclStmt v:
                FindMutationsAndSends(v.Initializer, state, diagnostics);
                // `var w = u;` — w aliases u (bare single-part name only; deeper
                // sub-object aliasing is the parked transitive case). Casts,
                // `as`, and parens are alias-transparent: `var w = (Base)u;`
                // still names the same object, so a cast must not launder the
                // moved-ness off a value.
                if (UnwrapAliasSource(v.Initializer) is { } aliasSrc)
                    state.Alias(v.Name, aliasSrc);
                break;

            case ExpressionStmt es:
                FindMutationsAndSends(es.Expr, state, diagnostics);
                break;

            case TryStmt t:
                AnalyzeBlockMoves(t.Try, state.Clone(), diagnostics);
                foreach (var c in t.Catches)
                {
                    if (c.When is not null) FindMutationsAndSends(c.When, state, diagnostics);
                    AnalyzeBlockMoves(c.Body, state.Clone(), diagnostics);
                }
                if (t.Finally is not null)
                    AnalyzeBlockMoves(t.Finally, state.Clone(), diagnostics);
                break;

            case ThrowStmt th when th.Value is not null:
                FindMutationsAndSends(th.Value, state, diagnostics);
                break;
        }
    }

    /// <summary>The bare local an initializer names, seen through
    /// alias-transparent wrappers: parens, cast <c>(T)x</c>, and <c>x as T</c>
    /// all still name the same object. (<c>is</c> produces a bool, so it is
    /// not an alias source.)</summary>
    private static string? UnwrapAliasSource(Expr e) => e switch
    {
        NameExpr ne when ne.Name.Parts.Count == 1 => ne.Name.Simple,
        ParenExpr p => UnwrapAliasSource(p.Inner),
        TypeOpExpr { Kind: TypeOpKind.Cast or TypeOpKind.As } t => UnwrapAliasSource(t.Operand),
        _ => null,
    };

    /// <summary>Every <c>x is T name</c> capture in a condition whose operand
    /// is a bare local: yields (name, x) pairs to alias in the guarded branch.</summary>
    private static IEnumerable<(string Binding, string Source)> IsCaptureAliases(Expr condition)
    {
        foreach (var t in ClassSymbols.ExprsIn(condition).OfType<TypeOpExpr>())
        {
            if (t is { Kind: TypeOpKind.Is, Binding: { } binding }
                && UnwrapAliasSource(t.Operand) is { } source)
                yield return (binding, source);
        }
    }

    /// <summary>
    /// Walks an expression tree once, in source order. Reports CE0085 for
    /// assignments whose LHS reaches a moved name (directly or via an alias);
    /// then promotes the names of locals passed as Tell/ask payloads into the
    /// <paramref name="state"/>'s moved set so subsequent statements see them.
    /// </summary>
    private static void FindMutationsAndSends(
        Expr expr, MoveState state, List<Diagnostic> diagnostics)
    {
        switch (expr)
        {
            case AssignExpr a:
                // Mutation check first — a moved value (or an alias of one)
                // being mutated.
                var rootName = TryGetMutationRoot(a.Left);
                if (rootName is not null && state.IsMoved(rootName))
                {
                    diagnostics.Add(Diagnostic.At("CE0085", a.Span,
                        $"Cannot mutate '{rootName}' after it was sent via 'Tell'/'ask' — " +
                        $"the receiving actor now logically owns this value, so mutating it " +
                        $"(or any alias of it) races. Build a new value and send that instead."));
                }
                FindMutationsAndSends(a.Left, state, diagnostics);
                FindMutationsAndSends(a.Right, state, diagnostics);
                break;

            case MethodCallExpr call:
                FindMutationsAndSends(call.Target, state, diagnostics);
                foreach (var arg in call.Args) FindMutationsAndSends(arg, state, diagnostics);

                // Promote send-payloads to the moved set after walking the
                // call's children, so a Tell whose argument was assigned
                // earlier in the same expression still sees the prior
                // assignment as the move source.
                if (call.Method == "Tell" && call.Args.Count >= 1
                    && call.Args[0] is NameExpr nameArg
                    && nameArg.Name.Parts.Count == 1)
                {
                    state.Move(nameArg.Name.Parts[0]);
                }
                break;

            case AskExpr ask:
                FindMutationsAndSends(ask.Target, state, diagnostics);
                FindMutationsAndSends(ask.Message, state, diagnostics);
                // Ask sends a message too — same ownership transfer applies.
                IReadOnlyList<Expr> askArgs = (ask.Message as NewExpr)?.Args ?? [];
                if (askArgs.Count >= 1
                    && askArgs[0] is NameExpr askArg
                    && askArg.Name.Parts.Count == 1)
                {
                    state.Move(askArg.Name.Parts[0]);
                }
                break;

            case BinaryExpr b:
                FindMutationsAndSends(b.Left, state, diagnostics);
                FindMutationsAndSends(b.Right, state, diagnostics);
                break;

            case UnaryExpr u: FindMutationsAndSends(u.Operand, state, diagnostics); break;
            case TypeOpExpr t: FindMutationsAndSends(t.Operand, state, diagnostics); break;
            case ConditionalExpr c:
                FindMutationsAndSends(c.Condition, state, diagnostics);
                FindMutationsAndSends(c.Then, state, diagnostics);
                FindMutationsAndSends(c.Else, state, diagnostics);
                break;
            case MemberAccessExpr m: FindMutationsAndSends(m.Target, state, diagnostics); break;
            case IndexExpr idx:
                FindMutationsAndSends(idx.Target, state, diagnostics);
                FindMutationsAndSends(idx.Index, state, diagnostics);
                break;
            case ParenExpr p: FindMutationsAndSends(p.Inner, state, diagnostics); break;
            case NewExpr n:
                foreach (var a in n.Args) FindMutationsAndSends(a, state, diagnostics);
                if (n.Initializer is not null)
                    foreach (var a in n.Initializer) FindMutationsAndSends(a, state, diagnostics);
                break;
            case SpawnExpr sp:
                foreach (var a in sp.Args) FindMutationsAndSends(a, state, diagnostics);
                break;
            case SwitchExpr sw:
                FindMutationsAndSends(sw.Subject, state, diagnostics);
                foreach (var arm in sw.Arms)
                {
                    if (arm.When is not null) FindMutationsAndSends(arm.When, state, diagnostics);
                    FindMutationsAndSends(arm.Result, state, diagnostics);
                }
                break;
            // NameExpr / Self / Sender / Literals — no recursion needed.
        }
    }

    // ─── CE0087 reachability helpers (reader/writer discipline) ─────────────

    /// <summary>
    /// CE0087 (HOLE #1). The actor's own methods a <c>reader on</c> handler may
    /// not call because they mutate actor or shared-region state — directly, or
    /// transitively through a sibling call. Seeded from the state-mutation
    /// classifier (<see cref="ActorSymbols.MutatingMethods"/>: field/property
    /// writes plus sibling calls, inherited methods folded in) and extended with
    /// the two mutation shapes that classifier can't model: a mutating method on
    /// a confined-class field (<c>c.Inc()</c>) and an in-place mutating BCL call
    /// on a collection field (<c>items.Add()</c>). A final sibling fixpoint
    /// propagates unsafety one call-hop up, so a helper that only reaches the
    /// mutation indirectly (<c>Outer()</c> → <c>Inner()</c> → write) is caught.
    /// </summary>
    private static IReadOnlySet<string> ComputeReaderUnsafeMethods(
        ActorSymbols actor, SymbolTable symbols)
    {
        var methods = actor.Methods;
        var names = methods.Keys.ToHashSet(StringComparer.Ordinal);

        // Direct state-writers + direct sibling-callers already classified by
        // StateMutation; start from them and add the class/BCL/region shapes.
        var unsafeSet = new HashSet<string>(actor.MutatingMethods, StringComparer.Ordinal);
        var calls = new Dictionary<string, HashSet<string>>(StringComparer.Ordinal);

        foreach (var (name, m) in methods)
        {
            var sib = new HashSet<string>(StringComparer.Ordinal);
            foreach (var e in ClassSymbols.ExprsIn(m.Body))
            {
                switch (e)
                {
                    // `field.M()` / `self.field.M()` — a mutating method on a
                    // confined-class field, or an in-place mutating BCL call on
                    // a (non-class) collection field.
                    case MethodCallExpr mc:
                    {
                        var recvField = mc.Target switch
                        {
                            NameExpr n when n.Name.Parts.Count == 1 => n.Name.Simple,
                            MemberAccessExpr ma when ma.Target is SelfExpr => ma.Member,
                            _ => null,
                        };
                        if (recvField is not null
                            && actor.Fields.TryGetValue(recvField, out var fd))
                        {
                            if (symbols.GetClassSymbols(fd.Type.Name.Simple) is { } cs
                                && cs.MutatingMethods.Contains(mc.Method))
                                unsafeSet.Add(name);
                            else if (symbols.ResolveClass(fd.Type.Name.Simple) is null
                                     && MutatingBclMethodNames.Contains(mc.Method))
                                unsafeSet.Add(name);
                        }
                        if (mc.Target is SelfExpr && names.Contains(mc.Method))
                            sib.Add(mc.Method);
                        break;
                    }

                    // Bare sibling invocation `M()`.
                    case InvocationExpr inv when names.Contains(inv.Callee):
                        sib.Add(inv.Callee);
                        break;

                    // A write through a `use` region handle inside the method.
                    case AssignExpr a
                        when TryGetSharedRegionRoot(a.Left, actor.Declaration) is not null:
                        unsafeSet.Add(name);
                        break;
                }
            }
            calls[name] = sib;
        }

        var changed = true;
        while (changed)
        {
            changed = false;
            foreach (var name in names)
            {
                if (unsafeSet.Contains(name)) continue;
                if (calls[name].Any(unsafeSet.Contains)) { unsafeSet.Add(name); changed = true; }
            }
        }
        return unsafeSet;
    }

    /// <summary>True when <paramref name="name"/> is one of this actor's
    /// <c>use X name;</c> shared-region handles.</summary>
    private static bool IsRegionHandle(string name, Context ctx) =>
        ctx.Actor.Declaration.Members.OfType<UseDecl>().Any(u => u.LocalName == name);

    /// <summary>The confined-class type name of actor field
    /// <paramref name="name"/>, or <c>null</c> when the name isn't an actor
    /// field or its type isn't a declared <c>class</c>.</summary>
    private static string? ConfinedClassField(string name, Context ctx) =>
        ctx.Typer.IsActorField(name)
        && ctx.Actor.Fields.TryGetValue(name, out var f)
        && ctx.Symbols.ResolveClass(f.Type.Name.Simple) is not null
            ? f.Type.Name.Simple
            : null;

    /// <summary>
    /// CE0087 (HOLE #2 / #4). What a <c>var h = …;</c> local aliases for reader
    /// reachability — a <c>use</c> region handle or a confined-class actor field
    /// — following <c>self.</c>, parentheses and a chain of local→local aliases.
    /// Only reference-typed roots are recorded: aliasing a value-typed field
    /// copies it, so a write through the alias never reaches the field. Returns
    /// <c>null</c> for any other initializer (which clears the name's alias).
    /// </summary>
    private static FieldAlias? ComputeFieldAlias(Expr init, Context ctx)
    {
        switch (init)
        {
            case ParenExpr p:
                return ComputeFieldAlias(p.Inner, ctx);

            // `var c = cache;` / `var h = counter;` / `var d = c;` (alias chain).
            case NameExpr n when n.Name.Parts.Count == 1:
            {
                var name = n.Name.Simple;
                if (IsRegionHandle(name, ctx))
                    return new FieldAlias(FieldAliasKind.Region, name, null);
                if (ConfinedClassField(name, ctx) is { } cls)
                    return new FieldAlias(FieldAliasKind.ConfinedClass, name, cls);
                return ctx.Typer.GetAlias(name);
            }

            // `var c = self.cache;` / `var h = self.counter;`.
            case MemberAccessExpr ma when ma.Target is SelfExpr:
            {
                var name = ma.Member;
                if (IsRegionHandle(name, ctx))
                    return new FieldAlias(FieldAliasKind.Region, name, null);
                if (ConfinedClassField(name, ctx) is { } cls)
                    return new FieldAlias(FieldAliasKind.ConfinedClass, name, cls);
                return null;
            }

            default:
                return null;
        }
    }

    /// <summary>
    /// CE0087 (HOLE #4). The confined-class receivers a method call could root
    /// at — a direct actor field (bare or <c>self.</c>), a local that aliases
    /// one, or either reached through parentheses or the arms of a ternary —
    /// each as (display, field, class). Empty for any other receiver shape.
    /// </summary>
    private static IEnumerable<(string Display, string Field, string Class)>
        ReceiverConfinedClasses(Expr target, Context ctx)
    {
        switch (target)
        {
            case ParenExpr p:
                foreach (var r in ReceiverConfinedClasses(p.Inner, ctx)) yield return r;
                break;

            case ConditionalExpr c:
                foreach (var r in ReceiverConfinedClasses(c.Then, ctx)) yield return r;
                foreach (var r in ReceiverConfinedClasses(c.Else, ctx)) yield return r;
                break;

            case NameExpr n when n.Name.Parts.Count == 1:
            {
                var name = n.Name.Simple;
                if (ConfinedClassField(name, ctx) is { } cls)
                    yield return (name, name, cls);
                else if (ctx.Typer.GetAlias(name) is { Kind: FieldAliasKind.ConfinedClass } a)
                    yield return (name, a.Root, a.ClassName!);
                break;
            }

            case MemberAccessExpr ma when ma.Target is SelfExpr
                                          && ConfinedClassField(ma.Member, ctx) is { } scls:
                yield return ($"self.{ma.Member}", ma.Member, scls);
                break;
        }
    }

    /// <summary>
    /// CE0087 (HOLE #3). The mutable, non-confined-class actor field or region
    /// field a method receiver roots at (through parentheses / greedy member
    /// paths), for the BCL-collection mutating-name check — or <c>null</c>.
    /// A confined-class receiver returns <c>null</c> here: its mutation is
    /// judged authoritatively by <see cref="ReceiverConfinedClasses"/> against
    /// the class's own <see cref="ClassSymbols.MutatingMethods"/> instead.
    /// </summary>
    private static string? ReceiverCollectionRoot(Expr target, Context ctx)
    {
        // `self.items.Add()` names the field off `self`; every other shape roots
        // at its leftmost name (`items`, greedy `cache.items`, `(items)`).
        var name = target is MemberAccessExpr { Target: SelfExpr } sma
            ? sma.Member
            : GetLeftmostName(target);
        if (name is null) return null;

        // A direct mutable actor field whose type is NOT a declared class.
        if (ctx.Typer.IsActorField(name)
            && ctx.Actor.Fields.TryGetValue(name, out var f)
            && ctx.Symbols.ResolveClass(f.Type.Name.Simple) is null)
            return name;

        // A shared-region field reached through a `use` handle (`cache.items`).
        if (IsRegionHandle(name, ctx))
            return name;

        return null;
    }

    /// <summary>
    /// CE0087 (HOLE #2 / #4). The actor-field or region root a write reaches
    /// THROUGH a local that aliases a confined-class field (member/index write:
    /// <c>h.n = …</c>), returned as (root, isRegion). A bare <c>h = …</c> local
    /// reassignment is never a field write and returns <c>null</c>.
    /// </summary>
    private static (string Root, bool IsRegion)? TryGetAliasedWriteRoot(Expr lhs, Context ctx)
    {
        // The write must reach through the local (a `.member` or `[i]` after
        // it), not be a bare single-name reassignment.
        var (name, through) = LhsRootThrough(lhs);
        if (name is null || !through) return null;
        return ctx.Typer.GetAlias(name) switch
        {
            { Kind: FieldAliasKind.Region } a        => (a.Root, true),
            { Kind: FieldAliasKind.ConfinedClass } a => (a.Root, false),
            _                                        => null,
        };
    }

    /// <summary>The leftmost root name of an assignment target plus whether the
    /// write reaches THROUGH it (a member/index access or greedy multi-part
    /// name) rather than being a bare single-name reassignment.</summary>
    private static (string? Name, bool Through) LhsRootThrough(Expr lhs) => lhs switch
    {
        NameExpr n when n.Name.Parts.Count == 1 => (n.Name.Simple, false),
        NameExpr n                              => (n.Name.Parts[0], true),
        MemberAccessExpr m                      => (GetLeftmostName(m.Target), true),
        IndexExpr idx                           => (GetLeftmostName(idx.Target), true),
        ParenExpr p                             => LhsRootThrough(p.Inner),
        _                                       => (null, false),
    };

    /// <summary>
    /// Walks the LHS of an <see cref="AssignExpr"/> looking for a
    /// root identifier that names an actor field (rather than a
    /// local / parameter). Returns the field name if the assignment
    /// would mutate actor state through this path, else null. Used
    /// by CE0087 to flag reader-handler mutations.
    /// <para>
    /// Three LHS shapes are mutations of actor state:
    /// <list type="bullet">
    ///   <item><c>field = X</c> — bare-name assignment where the
    ///         name resolves to an actor field.</item>
    ///   <item><c>field.x = X</c> — multi-part qualified name or
    ///         <c>MemberAccessExpr</c> rooted at a field.</item>
    ///   <item><c>field[i] = X</c> — index assignment rooted at
    ///         a field.</item>
    /// </list>
    /// Bare-name assignment where the root is a local (declared via
    /// <c>var</c> or a method parameter) is NOT a field mutation
    /// and returns null.
    /// </para>
    /// </summary>
    private static string? TryGetActorFieldRoot(Expr lhs, ExpressionTyper typer)
    {
        var name = lhs switch
        {
            NameExpr n         => n.Name.Parts[0],
            MemberAccessExpr m => GetLeftmostName(m.Target),
            IndexExpr idx       => GetLeftmostName(idx.Target),
            ParenExpr p        => GetLeftmostName(p.Inner),
            _                  => null,
        };
        if (name is null) return null;
        return typer.IsActorField(name) ? name : null;
    }

    /// <summary>
    /// Companion of <see cref="TryGetActorFieldRoot"/>
    /// for shared-region attachments. Returns the local name (e.g.
    /// <c>cache</c>) when the LHS root resolves to a <c>use X foo;</c>
    /// declaration on this actor — meaning the assignment writes to a
    /// region field through the actor's attached lock-protected
    /// accessor. CE0087 fires on this when the enclosing handler is
    /// in reader mode.
    /// </summary>
    /// <summary>
    /// CE0103 — every Spek `enum` is sealed by default. A
    /// `switch` over a value of an enum type must cover every variant
    /// or include a `_` discard arm.
    ///
    /// The check skips when the subject's type can't be resolved to a
    /// known enum (fail open, same convention as the rest of the
    /// semantic walker). The supported subject shapes are: typed
    /// locals (`MyEnum s = …;`), parameters, and actor fields. Untyped
    /// `var s = …;` locals are classified opportunistically — when the
    /// initializer's classification surfaces an enum, the local is
    /// covered.
    ///
    /// Variant matching shapes:
    /// <list type="bullet">
    ///   <item>`MyEnum.Variant` constant pattern — covers Variant.</item>
    ///   <item>`Variant` (single-part) constant pattern — covers Variant
    ///         when it resolves to a member of the subject's enum.</item>
    ///   <item>`_` discard pattern (or any catch-all type pattern) —
    ///         covers everything; exhaustiveness trivially satisfied.</item>
    ///   <item>Other pattern shapes — counted as "unknown coverage";
    ///         the CE only fires when the missing variants are
    ///         provably uncovered.</item>
    /// </list>
    /// </summary>
    private static void CheckExhaustiveSwitchOverEnum(
        SwitchExpr sw, Context ctx, List<Diagnostic> diagnostics)
    {
        // Resolve the subject's TypeRef and check whether it points at
        // a declared enum.
        var subjectType = ctx.Typer.TryGetType(sw.Subject);
        if (subjectType is null) return;

        var enumDecl = ctx.Symbols.ResolveEnum(subjectType.Name.Simple);
        if (enumDecl is null) return;

        // Walk the arms, collecting matched variants and watching for
        // a catch-all. A catch-all short-circuits exhaustiveness.
        var covered = new HashSet<string>();
        var hasCatchAll = false;
        foreach (var arm in sw.Arms)
        {
            // A `when` guard means the arm only matches conditionally —
            // it doesn't count as a definitive cover for that variant.
            // (Conservative: if the guard is `when true` it would, but
            // we don't constant-fold conditions in semantic analysis.)
            if (arm.When is not null) continue;

            switch (arm.Pattern)
            {
                case DiscardPattern:
                    hasCatchAll = true;
                    break;

                case ConstPattern cp when cp.Value is NameExpr nameC:
                    AddIfMatchesEnum(nameC.Name.Parts, enumDecl.Name, covered);
                    break;

                // `Status.Active` and `Active` parse as TypePattern via
                // the `type_ IDENTIFIER?` grammar branch — Spek can't
                // tell at parse time whether `Foo.Bar` is a type name
                // or a constant enum reference. The emit layer happily
                // produces both shapes as identical C# (a constant
                // pattern), so for CE0103 we treat unbound TypePatterns
                // with a matching qualified-name shape as covers.
                case TypePattern tp when tp.Binding is null
                                       && tp.Type.TypeArgs.Count == 0
                                       && !tp.Type.IsNullable:
                    AddIfMatchesEnum(tp.Type.Name.Parts, enumDecl.Name, covered);
                    break;
            }
        }

        if (hasCatchAll) return;

        var missing = enumDecl.Members
            .Select(m => m.Name)
            .Where(name => !covered.Contains(name))
            .ToList();
        if (missing.Count == 0) return;

        diagnostics.Add(Diagnostic.At("CE0103", sw.Span,
            $"`switch` over '{enumDecl.Name}' is not exhaustive — missing " +
            $"variant{(missing.Count == 1 ? "" : "s")}: " +
            $"{string.Join(", ", missing.Select(m => $"{enumDecl.Name}.{m}"))}. " +
            $"Add an arm for each missing variant or include a `_` discard arm."));
    }

    /// <summary>
    /// Used by CE0103 to record an arm pattern as covering a
    /// variant. Accepts both the qualified (`MyEnum.Variant`) and
    /// bare (`Variant`) shapes; bare matches assume the variant
    /// belongs to the subject's enum (the type-checker already
    /// constrains pattern types to the subject's type, so a bare
    /// name here can only refer to a variant of that enum).
    /// </summary>
    private static void AddIfMatchesEnum(
        IReadOnlyList<string> parts, string enumName, HashSet<string> covered)
    {
        if (parts.Count == 2 && parts[0] == enumName)
            covered.Add(parts[1]);
        else if (parts.Count == 1)
            covered.Add(parts[0]);
    }

    private static string? TryGetSharedRegionRoot(Expr lhs, ActorDecl actor)
    {
        var name = lhs switch
        {
            NameExpr n         => n.Name.Parts[0],
            MemberAccessExpr m => GetLeftmostName(m.Target),
            IndexExpr idx       => GetLeftmostName(idx.Target),
            ParenExpr p        => GetLeftmostName(p.Inner),
            _                  => null,
        };
        if (name is null) return null;
        return actor.Members.OfType<UseDecl>().Any(u => u.LocalName == name)
            ? name
            : null;
    }

    /// <summary>
    /// Checks a <c>useLocal.fieldName</c> access against the
    /// referenced region's field-lifecycle markers. A reference to a
    /// <c>deprecated</c> field surfaces CE0101 (warning); a reference
    /// to a <c>retired</c> field surfaces CE0102 (error). Other shapes
    /// (member access on a local, on `self`, on an arbitrary
    /// expression) bypass the check.
    /// </summary>
    private static void CheckRegionFieldLifecycle(
        MemberAccessExpr access, Context ctx, List<Diagnostic> diagnostics)
    {
        // The target must be a single-part NameExpr that matches a
        // `use X foo;` local on this actor.
        if (access.Target is not NameExpr targetName) return;
        if (targetName.Name.Parts.Count != 1)        return;

        ReportLifecycleIfRegionField(
            targetName.Name.Parts[0], access.Member,
            access.Span.StartLine, access.Span.StartColumn,
            ctx, diagnostics);
    }

    /// <summary>
    /// Companion to <see cref="CheckRegionFieldLifecycle"/> for
    /// the other parser shape: <c>cache.oldSymbol</c> parses as a
    /// greedy multi-part <see cref="NameExpr"/> rather than a
    /// <see cref="MemberAccessExpr"/> chain when no further postfix
    /// operator follows.
    /// </summary>
    private static void CheckRegionFieldLifecycleOnName(
        NameExpr name, Context ctx, List<Diagnostic> diagnostics)
    {
        if (name.Name.Parts.Count != 2) return;

        ReportLifecycleIfRegionField(
            name.Name.Parts[0], name.Name.Parts[1],
            name.Span.StartLine, name.Span.StartColumn,
            ctx, diagnostics);
    }

    private static void ReportLifecycleIfRegionField(
        string localName, string fieldName, int line, int column,
        Context ctx, List<Diagnostic> diagnostics)
    {
        var useDecl = ctx.Actor.Declaration.Members
            .OfType<UseDecl>()
            .FirstOrDefault(u => u.LocalName == localName);
        if (useDecl is null) return;

        var region = ctx.Symbols.ResolveSharedRegion(useDecl.RegionType);
        if (region is null) return;

        var field = region.Fields.FirstOrDefault(f => f.Name == fieldName);
        if (field is null) return;

        switch (field.Lifecycle)
        {
            case FieldLifecycle.Deprecated:
                diagnostics.Add(new Diagnostic("CE0101",
                    line, column,
                    $"Field '{region.Name}.{field.Name}' is deprecated. " +
                    $"Plan to migrate off — a future version may mark it 'retired' " +
                    $"and break this reference.",
                    DiagnosticSeverity.Warning));
                break;

            case FieldLifecycle.Retired:
                diagnostics.Add(new Diagnostic("CE0102",
                    line, column,
                    $"Field '{region.Name}.{field.Name}' is retired and cannot be " +
                    $"referenced. The name is reserved (no new field may reuse it) " +
                    $"and the persistence store will drop the key on the next save."));
                break;
        }
    }

    /// <summary>
    /// Companion of <see cref="TryGetSharedRegionRoot"/> used on
    /// the RHS of an assignment. Walks <i>through</i> method calls and
    /// member/index access to find a leaf rooted in a <c>use X foo;</c>
    /// local. Returns the local name when the RHS is a direct read of
    /// shared-region state. Returns null when the value passes through
    /// a local variable, function call argument, or anything else that
    /// signals an intentional borrow.
    ///
    /// Examples (with <c>use Cache cache;</c>):
    /// <list type="bullet">
    ///   <item><c>cache.X</c> → "cache" (direct read).</item>
    ///   <item><c>cache.X.Method()</c> → "cache" (method call on a region read).</item>
    ///   <item><c>SomeFunc(cache.X)</c> → null (sanitised through a function).</item>
    ///   <item><c>local</c> → null (sanitised through a local).</item>
    /// </list>
    /// </summary>
    private static string? TryGetRegionReadRoot(Expr rhs, ActorDecl actor)
    {
        // Peel transparent wrappers — member access, index access,
        // method calls on a region read (e.g. `cache.X.Get(k)`),
        // parenthesised forms — to find the true root.
        var current = rhs;
        while (true)
        {
            switch (current)
            {
                case NameExpr n:
                {
                    var head = n.Name.Parts[0];
                    return actor.Members.OfType<UseDecl>()
                        .Any(u => u.LocalName == head) ? head : null;
                }
                case MemberAccessExpr m:
                    current = m.Target;
                    continue;
                case IndexExpr idx:
                    current = idx.Target;
                    continue;
                case MethodCallExpr mc:
                    current = mc.Target;
                    continue;
                case ParenExpr p:
                    current = p.Inner;
                    continue;
                default:
                    // Function calls (InvocationExpr), `new`, literals,
                    // arithmetic — anything else counts as "not a direct
                    // borrow" and exits the rule.
                    return null;
            }
        }
    }

    private static string? GetLeftmostName(Expr expr)
    {
        while (true)
        {
            switch (expr)
            {
                case NameExpr n:    return n.Name.Parts[0];
                case MemberAccessExpr m: expr = m.Target; break;
                case IndexExpr idx:  expr = idx.Target; break;
                case ParenExpr p:    expr = p.Inner; break;
                default:             return null;
            }
        }
    }

    /// <summary>
    /// Returns the base local name being mutated by an assignment,
    /// or null if the LHS isn't a member-access / index-access onto
    /// a single-part local. Bare-local reassignment
    /// (<c>foo = bar</c>) returns null — that re-binds the local
    /// rather than mutating its previous value.
    /// </summary>
    private static string? TryGetMutationRoot(Expr lhs)
    {
        return lhs switch
        {
            // Multi-part qualified name parses greedily as NameExpr;
            // first part is the local being mutated, the trailing parts
            // are the field path (`u.v` → root "u").
            NameExpr n when n.Name.Parts.Count > 1 => n.Name.Parts[0],
            MemberAccessExpr m => TryGetLocalName(m.Target),
            IndexExpr idx       => TryGetLocalName(idx.Target),
            // Bare single-part NameExpr is reassignment, not mutation.
            _ => null,
        };
    }

    private static string? TryGetLocalName(Expr expr)
    {
        // Walk through any chain of member-access / index ops to find
        // the base single-part NameExpr.
        while (true)
        {
            switch (expr)
            {
                case NameExpr n when n.Name.Parts.Count == 1:
                    return n.Name.Parts[0];
                case MemberAccessExpr m:
                    expr = m.Target;
                    break;
                case IndexExpr idx:
                    expr = idx.Target;
                    break;
                case ParenExpr p:
                    expr = p.Inner;
                    break;
                default:
                    return null;
            }
        }
    }

    private static void RegisterHandlerBinding(MessagePattern pattern, ExpressionTyper typer, SymbolTable symbols)
    {
        switch (pattern)
        {
            case NamedBindPattern n:
                // Register the binding WITH its message type (not just the
                // Message kind) so member reads like `msg.value` can resolve
                // their field type — needed by CE0138's lossy-widening check.
                // The kind is unchanged: ClassifyTypeRef of a resolved message
                // is Message. An unresolved (external CLR) message type keeps
                // the Unknown kind, exactly as before.
                if (symbols.ResolveMessage(n.MessageType) is not null)
                    typer.RegisterLocalWithType(n.Binding,
                        new TypeRef(n.MessageType.Span, n.MessageType, Array.Empty<TypeRef>()));
                else
                    typer.RegisterLocal(n.Binding, ExprKind.Unknown);
                break;
            case CatchAllPattern c:
                typer.RegisterLocal(c.Binding, ExprKind.Unknown);
                break;
            case EventPattern e:
                // Register each delegate-signature param as a typed
                // local so the body can resolve `sender`, `e`, etc.
                foreach (var p in e.Parameters)
                    typer.RegisterParameter(p.Name, p.Type);
                break;
            // NoBindPattern — no local to register.
        }
    }

    /// <summary>
    /// CE0096 — handler-pattern type-resolution check.
    /// <list type="bullet">
    ///   <item><b>public / internal</b> handlers form the actor's API
    ///         surface (channel coverage, REST/gRPC route generation,
    ///         remote dispatch). The pattern type must resolve to a
    ///         declared <c>message</c> so cross-language and
    ///         cross-process callers can reach it.</item>
    ///   <item><b>private</b> handlers are reachable only via
    ///         <c>self.Tell</c> from inside the actor — they're not
    ///         part of the public surface, so the pattern can bind
    ///         to any CLR type. This is what lets actors take BCL
    ///         event-args (<c>FileSystemEventArgs</c>, <c>Timer</c>
    ///         callback args, etc.) directly without wrapping them
    ///         in a Spek <c>message</c>.</item>
    /// </list>
    /// </summary>
    private static void CheckHandlerMessageType(
        OnHandler handler, SymbolTable symbols, List<Diagnostic> diagnostics)
    {
        // Catch-alls don't bind a specific type; nothing to check.
        QualifiedName? msgType = handler.Pattern switch
        {
            NamedBindPattern n => n.MessageType,
            NoBindPattern n    => n.MessageType,
            _                  => null,
        };
        if (msgType is null) return;

        // CE0121 — a handler dispatches on a *message*, never on an
        // implementation contract (`interface` or `channel`). This holds for
        // every visibility, private included: routing on a provider contract
        // would collapse the provider/received distinction and make dispatch
        // multi-valued and non-local.
        var contractKind =
            symbols.ResolveInterface(msgType) is not null ? "interface" :
            symbols.ResolveChannel(msgType)   is not null ? "channel"   : null;
        if (contractKind is not null)
        {
            diagnostics.Add(Diagnostic.At("CE0121", msgType.Span,
                $"'on {msgType}' dispatches on a {contractKind}, but handlers " +
                $"dispatch on a 'message' type, never on an implementation " +
                $"contract. To handle a family of messages, give them a shared " +
                $"'abstract message' base and write 'on Base'. To react to a " +
                $"cross-cutting concern, use an ingress policy or a shared field — " +
                $"not a handler keyed on {contractKind} '{msgType}'."));
            return;
        }

        // CE0139 — a generic message can't be pattern-matched in a handler yet:
        // there is no `on Envelope<int>` grammar, so `on Envelope` emits an OPEN
        // generic `case Envelope e:` that Roslyn rejects (CS0305, red-team
        // emit-D). Fires for every visibility — a private handler hits the same
        // wall. Checked before the private-handler escape below for that reason.
        if (symbols.ResolveMessage(msgType) is { TypeParameters.Count: > 0 } generic)
        {
            var tp = string.Join(", ", generic.TypeParameters.Select(p => p.Name));
            diagnostics.Add(Diagnostic.At("CE0139", msgType.Span,
                $"'on {msgType}' can't handle the generic message '{msgType}<{tp}>' — a " +
                $"handler pattern has no way to name the concrete type argument (there is " +
                $"no 'on {msgType}<...>' form yet), so it emits an open generic type and " +
                $"fails to compile. Dispatch on a NON-generic message instead: wrap the " +
                $"payload in a concrete message (e.g. 'message {msgType}OfInt(int payload)') " +
                $"and handle that."));
            return;
        }

        // Private handlers escape the declared-message rule (they may bind BCL
        // or external CLR types, reachable only via self.Tell).
        if (handler.Visibility == Visibility.Private) return;

        if (symbols.ResolveMessage(msgType) is null)
        {
            var visibility = handler.Visibility.ToString().ToLowerInvariant();
            diagnostics.Add(Diagnostic.At("CE0096", msgType.Span,
                $"'{visibility} on {msgType}' must reference a declared 'message' type. " +
                $"Public and internal handlers form the actor's external API surface " +
                $"(channels, REST/gRPC routes, remote dispatch) and need declared messages " +
                $"for cross-language interop. To handle BCL or external CLR types directly, " +
                $"mark the handler 'private on {msgType} ...' — private handlers are " +
                $"reachable only via self.Tell and aren't part of the public surface."));
        }
    }

    private static bool ContainsPersist(ActorDecl actor)
    {
        var found = false;
        foreach (var member in actor.Members)
        {
            switch (member)
            {
                case InitBlock i: ScanForPersist(i.Body, ref found); break;
                case BehaviorDecl b:
                    foreach (var h in b.Handlers) ScanForPersist(h.Body, ref found);
                    break;
                case LifecycleHook l: ScanForPersist(l.Body, ref found); break;
                case MethodDecl m: ScanForPersist(m.Body, ref found); break;
            }
            if (found) return true;
        }
        return false;
    }

    private static void ScanForPersist(HandlerBody body, ref bool found)
    {
        if (found) return;
        switch (body)
        {
            case BlockHandlerBody b: ScanForPersist(b.Block, ref found); break;
            case InlineHandlerBody: break; // persist is a statement, not an expression
        }
    }

    private static void ScanForPersist(Stmt stmt, ref bool found)
    {
        if (found) return;
        switch (stmt)
        {
            case PersistStmt: found = true; break;
            case BlockStmt b:
                foreach (var s in b.Statements) { ScanForPersist(s, ref found); if (found) return; }
                break;
            case IfStmt i:
                ScanForPersist(i.Then, ref found);
                if (!found && i.Else is not null) ScanForPersist(i.Else, ref found);
                break;
            case ForStmt f: ScanForPersist(f.Body, ref found); break;
            case ForeachStmt fe: ScanForPersist(fe.Body, ref found); break;
            case SwitchStmt sw:
                foreach (var sec in sw.Sections) ScanForPersist(new BlockStmt(sec.Span, sec.Body), ref found);
                break;
            case WhileStmt w: ScanForPersist(w.Body, ref found); break;
            case DoWhileStmt dw: ScanForPersist(dw.Body, ref found); break;
            case TryStmt t:
                ScanForPersist(t.Try, ref found);
                if (!found) foreach (var c in t.Catches) { ScanForPersist(c.Body, ref found); if (found) break; }
                if (!found && t.Finally is not null) ScanForPersist(t.Finally, ref found);
                break;
        }
    }

    // ─── CE0118 — supervise decl vs explicit OnChildFailure override ────────

    /// <summary>
    /// A <c>supervise</c> declaration already generates an <c>OnChildFailure</c>
    /// override (<see cref="Spek.Compiler.Emit.ActorEmitter"/>), so a hand-written
    /// <c>OnChildFailure</c> on the same actor would be silently dropped to avoid a
    /// duplicate method. Rather than drop it quietly, CE0118 flags the conflict and
    /// makes the author pick one form — the declarative <c>supervise</c>, or the
    /// imperative override.
    /// </summary>
    private static void CheckSuperviseOnChildFailureConflict(
        ActorDecl actor, List<Diagnostic> diagnostics)
    {
        if (!actor.Members.OfType<SuperviseDecl>().Any()) return;

        var handwritten = actor.Members.OfType<MethodDecl>()
            .FirstOrDefault(m => m.Name == "OnChildFailure");
        if (handwritten is null) return;

        diagnostics.Add(Diagnostic.At("CE0118", handwritten.Span,
            "Actor has both a 'supervise' declaration and an explicit 'OnChildFailure' " +
            "override. The 'supervise' decl generates OnChildFailure, so this override would " +
            "be silently dropped. Use one form: the declarative 'supervise', or the imperative " +
            "OnChildFailure override (remove the 'supervise' decl)."));
    }

    // ─── CE0081 / CE0082 — supervise arm reachability ───────────────────────

    /// <summary>
    /// Per-strategy reachability check on <c>on Failure</c> arms. Emits:
    /// <list type="bullet">
    ///   <item><b>CE0081</b> — an arm is unreachable because a prior arm
    ///         subsumes it. Two subsume patterns are detected without a
    ///         type resolver:
    ///         <list type="bullet">
    ///           <item>Any arm after an untyped catch-all — the catch-all
    ///                 matches every cause first.</item>
    ///           <item>A typed arm whose exception type name matches a
    ///                 prior typed arm's name — exact duplicate.</item>
    ///         </list>
    ///   </item>
    ///   <item><b>CE0082</b> — two untyped <c>on Failure: Action</c> arms
    ///         in the same strategy. The second is dead code.</item>
    /// </list>
    /// A full subsumption check (arm B subsumed by arm A because B's type
    /// inherits from A's) needs the C# type system and is deferred to a
    /// later pass.
    /// </summary>
    private static void CheckSuperviseArms(ActorDecl actor, List<Diagnostic> diagnostics)
    {
        foreach (var sup in actor.Members.OfType<SuperviseDecl>())
        {
            var options = sup.Strategy switch
            {
                OneForOneStrategy o => o.Options,
                AllForOneStrategy a => a.Options,
                _                   => (IReadOnlyList<SuperviseOption>)Array.Empty<SuperviseOption>()
            };

            // CE0117 — a named option whose name isn't a recognized supervise option.
            // `maxRetries`/`withinTime` parse as ordinary named arguments, so a typo
            // (`maxRetres:`) reaches here rather than failing as a raw parse error.
            foreach (var unknown in options.OfType<UnknownSuperviseOption>())
            {
                diagnostics.Add(Diagnostic.At("CE0117", unknown.Span,
                    $"Unknown supervise option '{unknown.Name}'. Expected 'maxRetries' " +
                    $"or 'withinTime'."));
            }

            var arms = options.OfType<OnFailureOption>().ToList();
            if (arms.Count < 2) continue;

            var seenTypeNames = new Dictionary<string, OnFailureOption>();
            OnFailureOption? firstCatchAll = null;

            foreach (var arm in arms)
            {
                if (arm.ExceptionType is null)
                {
                    if (firstCatchAll is null)
                    {
                        firstCatchAll = arm;
                    }
                    else
                    {
                        // CE0082 — duplicate untyped catch-all.
                        diagnostics.Add(Diagnostic.At("CE0082", arm.Span,
                            $"Duplicate untyped 'on Failure' arm in supervise " +
                            $"strategy (previously declared at " +
                            $"{firstCatchAll.Span.StartLine}:{firstCatchAll.Span.StartColumn}). " +
                            $"Only one untyped catch-all is meaningful per strategy."));
                    }
                    continue;
                }

                // Typed arm after an untyped catch-all is unreachable.
                if (firstCatchAll is not null)
                {
                    diagnostics.Add(Diagnostic.At("CE0081", arm.Span,
                        $"Unreachable 'on Failure({arm.ExceptionType})' arm: a prior " +
                        $"untyped 'on Failure' at " +
                        $"{firstCatchAll.Span.StartLine}:{firstCatchAll.Span.StartColumn} " +
                        $"already matches every cause. Move typed arms before the catch-all."));
                    continue;
                }

                var typeName = arm.ExceptionType.ToString();
                if (seenTypeNames.TryGetValue(typeName, out var prev))
                {
                    // CE0081 — duplicate typed arm (same exception type twice).
                    diagnostics.Add(Diagnostic.At("CE0081", arm.Span,
                        $"Unreachable 'on Failure({typeName})' arm: the same " +
                        $"exception type was already matched at " +
                        $"{prev.Span.StartLine}:{prev.Span.StartColumn}. " +
                        $"Only the first match fires."));
                }
                else
                {
                    seenTypeNames[typeName] = arm;
                }
            }
        }
    }

    // ─── CE0090 / CE0091 — channel implementation checks ────────────────────

    /// <summary>
    /// For each channel the actor claims to implement, verify:
    /// <list type="bullet">
    ///   <item><b>CE0090</b> — every channel input has a corresponding
    ///         <c>on MessageType</c> handler somewhere in the actor's
    ///         behaviors. If a channel declares <c>on Shutdown;</c>,
    ///         the actor must have at least one handler matching
    ///         <c>on Shutdown ...</c>.</item>
    ///   <item><b>CE0091</b> — the name in the colon list resolves to a
    ///         declared channel (or declared actor, for
    ///         base-actor inheritance). Unknown names are an error.</item>
    /// </list>
    ///
    /// Both CE codes fire per channel, per actor, so users see every
    /// gap at once instead of one at a time. The grammar's naive colon
    /// list (first name → BaseActor, rest → ImplementedChannels) means
    /// the BaseActor slot might actually be a channel — this check
    /// handles that by looking up each name's kind in the symbol table.
    /// </summary>
    private static void CheckChannelImplementations(
        ActorDecl actor, SymbolTable symbols, List<Diagnostic> diagnostics)
    {
        // Collect every name the user wrote in the colon list, tagged
        // with its position span for diagnostic anchoring.
        var candidateNames = new List<QualifiedName>();
        if (actor.BaseActor is not null) candidateNames.Add(actor.BaseActor);
        candidateNames.AddRange(actor.ImplementedChannels);

        // Split each name into channel / actor / unknown.
        var resolvedChannels = new List<ChannelDecl>();
        var resolvedActor = (ActorDecl?)null;

        foreach (var name in candidateNames)
        {
            var ch = symbols.ResolveChannel(name);
            if (ch is not null)
            {
                resolvedChannels.Add(ch);
                continue;
            }
            var baseActor = symbols.ResolveActor(name);
            if (baseActor is not null)
            {
                // A name can only play the base-actor role once. A second
                // actor-typed name in the list is a misuse — flag it as
                // unknown channel so the user sees a pointed error.
                if (resolvedActor is null) resolvedActor = baseActor;
                else
                {
                    diagnostics.Add(Diagnostic.At("CE0091", name.Span,
                        $"Actor '{actor.Name}' lists '{name}' in the colon " +
                        $"list, but '{name}' is an actor — only one base actor " +
                        $"is permitted; remaining names must be channels."));
                }
                continue;
            }

            // Not a channel, not an actor.
            diagnostics.Add(Diagnostic.At("CE0091", name.Span,
                $"Unknown channel or base actor '{name}' in actor " +
                $"'{actor.Name}'. Declare it with 'channel {name} {{ ... }}' " +
                $"or 'actor {name} {{ ... }}' first."));
        }

        // Collect the set of message types this actor has `on X` handlers
        // for, across every behavior. A single handler satisfies the
        // input requirement for every channel that lists it (matches C#
        // explicit-interface-implementation semantics).
        var handledInputs = actor.Members
            .OfType<BehaviorDecl>()
            .SelectMany(b => b.Handlers)
            .Select(h => h.Pattern switch
            {
                NamedBindPattern n => n.MessageType.Simple,
                NoBindPattern n    => n.MessageType.Simple,
                _                  => null,   // catch-alls don't satisfy typed inputs
            })
            .Where(s => s is not null)
            .Select(s => s!)
            .ToHashSet();

        // CE0090 — every channel input (including those inherited from
        // base channels via `channel B : A`) must have a matching handler.
        // FlattenChannelInputs walks the inheritance graph and returns the
        // linearized set; diamond patterns dedupe naturally.
        foreach (var channel in resolvedChannels)
        {
            foreach (var inputName in symbols.FlattenChannelInputs(channel))
            {
                if (!handledInputs.Contains(inputName))
                {
                    diagnostics.Add(Diagnostic.At("CE0090", actor.Span,
                        $"Actor '{actor.Name}' implements channel " +
                        $"'{channel.Name}' but has no 'on {inputName}' " +
                        $"handler. Every channel input (including inherited) must " +
                        $"be covered by at least one behavior handler."));
                }
            }
        }

        // CE0092 — strict emits enforcement on `sender.Tell(new X())`.
        //
        // Rule (per design Q1b):
        //   self.Tell(X)   — free (internal message pump)
        //   someRef.Tell(X)— free (outbound to another actor)
        //   sender.Tell(X) — X must be either
        //                     (a) the inferred reply type of the enclosing
        //                         handler (return-equivalent), OR
        //                     (b) a message type in some implemented channel's
        //                         `emits` list.
        //                    Otherwise CE0092.
        //
        // `emits any;` on any implemented channel (or any of its
        // ancestors in the inheritance graph) disables the check entirely.
        if (resolvedChannels.Count == 0) return;

        // FlattenChannelEmits returns null when any channel in the
        // inheritance graph declares `emits any;` — that's the
        // advisory-mode opt-out. Otherwise it returns the union of
        // typed emits from this channel + every ancestor.
        var emitsSet = new HashSet<string>();
        foreach (var channel in resolvedChannels)
        {
            var flat = symbols.FlattenChannelEmits(channel);
            if (flat is null) return;       // hit `emits any;` somewhere in the graph
            emitsSet.UnionWith(flat);
        }

        foreach (var behavior in actor.Members.OfType<BehaviorDecl>())
        {
            foreach (var handler in behavior.Handlers)
            {
                // Compute the handler's inferred reply type (if any). A
                // `sender.Tell(new T())` sending the same T is equivalent
                // to `return new T();` — so it's a reply, not an emit.
                var replyTypeName = FindHandlerReplyTypeName(handler.Body);
                CheckSenderTellsInBody(handler.Body, replyTypeName, emitsSet,
                    actor.Name, diagnostics);
            }
        }
    }

    /// <summary>
    /// Returns the simple name of the first <c>new T(...)</c> on the RHS of
    /// any <c>return</c> statement anywhere in the handler body, or null
    /// if the handler doesn't end with a typed reply. Used by CE0092 to
    /// exempt <c>sender.Tell(new T())</c> that matches the handler's
    /// Option-D reply type from emits enforcement.
    /// </summary>
    private static string? FindHandlerReplyTypeName(HandlerBody body)
        => body switch
        {
            BlockHandlerBody b => FindReturnNewTypeName(b.Block),
            _                  => null,
        };

    private static string? FindReturnNewTypeName(Stmt stmt)
    {
        switch (stmt)
        {
            case ReturnStmt r when r.Value is NewExpr newExpr:
                return newExpr.Type.Simple;
            case BlockStmt b:
                foreach (var s in b.Statements)
                {
                    var t = FindReturnNewTypeName(s);
                    if (t is not null) return t;
                }
                return null;
            case IfStmt i:
                var thenType = FindReturnNewTypeName(i.Then);
                if (thenType is not null) return thenType;
                return i.Else is not null ? FindReturnNewTypeName(i.Else) : null;
            case ForStmt f:     return FindReturnNewTypeName(f.Body);
            case ForeachStmt f: return FindReturnNewTypeName(f.Body);
            case SwitchStmt sw:
                foreach (var sec in sw.Sections)
                {
                    var r = FindReturnNewTypeName(new BlockStmt(sec.Span, sec.Body));
                    if (r is not null) return r;
                }
                return null;
            case WhileStmt w:   return FindReturnNewTypeName(w.Body);
            case DoWhileStmt d: return FindReturnNewTypeName(d.Body);
            case TryStmt t:
                // Search the try block first; then catch clauses; then finally.
                // Order matches execution: a return from try short-circuits.
                var tryType = FindReturnNewTypeName(t.Try);
                if (tryType is not null) return tryType;
                foreach (var c in t.Catches)
                {
                    var ct = FindReturnNewTypeName(c.Body);
                    if (ct is not null) return ct;
                }
                return t.Finally is not null ? FindReturnNewTypeName(t.Finally) : null;
            default:          return null;
        }
    }

    private static void CheckSenderTellsInBody(
        HandlerBody body,
        string? replyTypeName,
        HashSet<string> emitsSet,
        string actorName,
        List<Diagnostic> diagnostics)
    {
        if (body is BlockHandlerBody block)
            CheckSenderTellsInStmt(block.Block, replyTypeName, emitsSet, actorName, diagnostics);
        else if (body is InlineHandlerBody inline)
            CheckSenderTellsInExpr(inline.Expr, replyTypeName, emitsSet, actorName, diagnostics);
    }

    private static void CheckSenderTellsInStmt(
        Stmt stmt,
        string? replyTypeName,
        HashSet<string> emitsSet,
        string actorName,
        List<Diagnostic> diagnostics)
    {
        switch (stmt)
        {
            case BlockStmt b:
                foreach (var s in b.Statements)
                    CheckSenderTellsInStmt(s, replyTypeName, emitsSet, actorName, diagnostics);
                break;
            case ExpressionStmt e:
                CheckSenderTellsInExpr(e.Expr, replyTypeName, emitsSet, actorName, diagnostics);
                break;
            case IfStmt i:
                CheckSenderTellsInExpr(i.Condition, replyTypeName, emitsSet, actorName, diagnostics);
                CheckSenderTellsInStmt(i.Then, replyTypeName, emitsSet, actorName, diagnostics);
                if (i.Else is not null)
                    CheckSenderTellsInStmt(i.Else, replyTypeName, emitsSet, actorName, diagnostics);
                break;
            case ForStmt f:
                CheckSenderTellsInStmt(f.Body, replyTypeName, emitsSet, actorName, diagnostics);
                break;
            case ForeachStmt fe:
                CheckSenderTellsInExpr(fe.Collection, replyTypeName, emitsSet, actorName, diagnostics);
                CheckSenderTellsInStmt(fe.Body, replyTypeName, emitsSet, actorName, diagnostics);
                break;
            case SwitchStmt sw:
                CheckSenderTellsInExpr(sw.Subject, replyTypeName, emitsSet, actorName, diagnostics);
                foreach (var sec in sw.Sections)
                    CheckSenderTellsInStmt(new BlockStmt(sec.Span, sec.Body), replyTypeName, emitsSet, actorName, diagnostics);
                break;
            case WhileStmt w:
                CheckSenderTellsInExpr(w.Condition, replyTypeName, emitsSet, actorName, diagnostics);
                CheckSenderTellsInStmt(w.Body, replyTypeName, emitsSet, actorName, diagnostics);
                break;
            case DoWhileStmt dw:
                CheckSenderTellsInStmt(dw.Body, replyTypeName, emitsSet, actorName, diagnostics);
                CheckSenderTellsInExpr(dw.Condition, replyTypeName, emitsSet, actorName, diagnostics);
                break;
            case TryStmt t:
                CheckSenderTellsInStmt(t.Try, replyTypeName, emitsSet, actorName, diagnostics);
                foreach (var c in t.Catches)
                {
                    if (c.When is not null)
                        CheckSenderTellsInExpr(c.When, replyTypeName, emitsSet, actorName, diagnostics);
                    CheckSenderTellsInStmt(c.Body, replyTypeName, emitsSet, actorName, diagnostics);
                }
                if (t.Finally is not null)
                    CheckSenderTellsInStmt(t.Finally, replyTypeName, emitsSet, actorName, diagnostics);
                break;
            case ThrowStmt th when th.Value is not null:
                CheckSenderTellsInExpr(th.Value, replyTypeName, emitsSet, actorName, diagnostics);
                break;
            case ReturnStmt r when r.Value is not null:
                CheckSenderTellsInExpr(r.Value, replyTypeName, emitsSet, actorName, diagnostics);
                break;
            case VarDeclStmt v:
                CheckSenderTellsInExpr(v.Initializer, replyTypeName, emitsSet, actorName, diagnostics);
                break;
        }
    }

    private static void CheckSenderTellsInExpr(
        Expr expr,
        string? replyTypeName,
        HashSet<string> emitsSet,
        string actorName,
        List<Diagnostic> diagnostics)
    {
        if (expr is MethodCallExpr mc
            && mc.Target is SenderExpr
            && mc.Method == "Tell"
            && mc.Args.Count == 1
            && mc.Args[0] is NewExpr newExpr)
        {
            var typeName = newExpr.Type.Simple;
            if (typeName != replyTypeName && !emitsSet.Contains(typeName))
            {
                diagnostics.Add(Diagnostic.At("CE0092", mc.Span,
                    $"'sender.Tell(new {typeName}())' in actor '{actorName}' " +
                    $"is not the handler's inferred reply type and is not " +
                    $"declared in any implemented channel's 'emits' list. " +
                    $"Use 'return new {typeName}();' if it's a reply, add " +
                    $"'emits {typeName};' to a channel, or emit 'emits any;' " +
                    $"to opt out of strict enforcement."));
            }
        }

        // Recurse into sub-expressions so nested method calls are checked too.
        switch (expr)
        {
            case MethodCallExpr m:
                CheckSenderTellsInExpr(m.Target, replyTypeName, emitsSet, actorName, diagnostics);
                foreach (var a in m.Args)
                    CheckSenderTellsInExpr(a, replyTypeName, emitsSet, actorName, diagnostics);
                break;
            case MemberAccessExpr ma:
                CheckSenderTellsInExpr(ma.Target, replyTypeName, emitsSet, actorName, diagnostics);
                break;
            case NewExpr ne:
                foreach (var a in ne.Args)
                    CheckSenderTellsInExpr(a, replyTypeName, emitsSet, actorName, diagnostics);
                if (ne.Initializer is not null)
                    foreach (var a in ne.Initializer)
                        CheckSenderTellsInExpr(a, replyTypeName, emitsSet, actorName, diagnostics);
                break;
            case ParenExpr p:
                CheckSenderTellsInExpr(p.Inner, replyTypeName, emitsSet, actorName, diagnostics);
                break;
            case BinaryExpr b:
                CheckSenderTellsInExpr(b.Left, replyTypeName, emitsSet, actorName, diagnostics);
                CheckSenderTellsInExpr(b.Right, replyTypeName, emitsSet, actorName, diagnostics);
                break;
            case TypeOpExpr t:
                CheckSenderTellsInExpr(t.Operand, replyTypeName, emitsSet, actorName, diagnostics);
                break;
            case UnaryExpr u:
                CheckSenderTellsInExpr(u.Operand, replyTypeName, emitsSet, actorName, diagnostics);
                break;
            case AssignExpr ae:
                CheckSenderTellsInExpr(ae.Left, replyTypeName, emitsSet, actorName, diagnostics);
                CheckSenderTellsInExpr(ae.Right, replyTypeName, emitsSet, actorName, diagnostics);
                break;
            case IndexExpr ix:
                CheckSenderTellsInExpr(ix.Target, replyTypeName, emitsSet, actorName, diagnostics);
                CheckSenderTellsInExpr(ix.Index, replyTypeName, emitsSet, actorName, diagnostics);
                break;
            case ConditionalExpr c:
                CheckSenderTellsInExpr(c.Condition, replyTypeName, emitsSet, actorName, diagnostics);
                CheckSenderTellsInExpr(c.Then, replyTypeName, emitsSet, actorName, diagnostics);
                CheckSenderTellsInExpr(c.Else, replyTypeName, emitsSet, actorName, diagnostics);
                break;
            case AskExpr ax:
                CheckSenderTellsInExpr(ax.Target, replyTypeName, emitsSet, actorName, diagnostics);
                CheckSenderTellsInExpr(ax.Message, replyTypeName, emitsSet, actorName, diagnostics);
                break;
        }
    }

    // ─── CE0014 — unused behavior detection ─────────────────────────────────

    private static void CheckUnusedBehaviors(ActorDecl actor, List<Diagnostic> diagnostics)
    {
        var targets = new HashSet<string>();
        foreach (var member in actor.Members)
        {
            switch (member)
            {
                case InitBlock i:     CollectBecomeTargets(i.Body, targets); break;
                case BehaviorDecl b:
                    foreach (var h in b.Handlers) CollectBecomeTargets(h.Body, targets);
                    break;
                case LifecycleHook l: CollectBecomeTargets(l.Body, targets); break;
                case MethodDecl m:    CollectBecomeTargets(m.Body, targets); break;
            }
        }

        // The first behavior is always implicitly the entry point — if no
        // init/become exists, the actor starts there. Only flag subsequent
        // behaviors that aren't reached by an explicit become.
        var behaviors = actor.Members.OfType<BehaviorDecl>().ToList();
        if (behaviors.Count > 0) targets.Add(behaviors[0].Name);

        foreach (var beh in behaviors)
        {
            if (!targets.Contains(beh.Name))
            {
                diagnostics.Add(Diagnostic.At("CE0014", beh.Span,
                    $"Behavior '{beh.Name}' in actor '{actor.Name}' is declared but never reached via 'become'."));
            }
        }
    }

    private static void CollectBecomeTargets(HandlerBody body, HashSet<string> targets)
    {
        switch (body)
        {
            case BlockHandlerBody b: CollectBecomeTargets(b.Block, targets); break;
            case InlineHandlerBody: break; // become is a statement, not an expression
        }
    }

    private static void CollectBecomeTargets(Stmt stmt, HashSet<string> targets)
    {
        switch (stmt)
        {
            case BecomeStmt b: targets.Add(b.BehaviorName); break;
            case BlockStmt block:
                foreach (var s in block.Statements) CollectBecomeTargets(s, targets);
                break;
            case IfStmt i:
                CollectBecomeTargets(i.Then, targets);
                if (i.Else is not null) CollectBecomeTargets(i.Else, targets);
                break;
            case ForStmt f: CollectBecomeTargets(f.Body, targets); break;
            case ForeachStmt fe: CollectBecomeTargets(fe.Body, targets); break;
            case SwitchStmt sw:
                foreach (var sec in sw.Sections) CollectBecomeTargets(new BlockStmt(sec.Span, sec.Body), targets);
                break;
            case WhileStmt w: CollectBecomeTargets(w.Body, targets); break;
            case DoWhileStmt dw: CollectBecomeTargets(dw.Body, targets); break;
            case TryStmt t:
                CollectBecomeTargets(t.Try, targets);
                foreach (var c in t.Catches) CollectBecomeTargets(c.Body, targets);
                if (t.Finally is not null) CollectBecomeTargets(t.Finally, targets);
                break;
        }
    }

    // ─── Walkers ────────────────────────────────────────────────────────────

    private static void WalkHandlerBody(HandlerBody body, Context ctx, List<Diagnostic> diagnostics)
    {
        switch (body)
        {
            case BlockHandlerBody b: WalkBlock(b.Block, ctx, diagnostics); break;
            case InlineHandlerBody i: WalkExpr(i.Expr, ctx, diagnostics); break;
        }
    }

    private static void WalkBlock(BlockStmt block, Context ctx, List<Diagnostic> diagnostics)
    {
        foreach (var stmt in block.Statements)
            WalkStmt(stmt, ctx, diagnostics);
    }

    private static void WalkStmt(Stmt stmt, Context ctx, List<Diagnostic> diagnostics)
    {
        switch (stmt)
        {
            case BlockStmt b:
                WalkBlock(b, ctx, diagnostics);
                break;

            case BecomeStmt bs:
                // CE0011 — become target must be a declared behavior on this actor.
                if (!ctx.BehaviorNames.Contains(bs.BehaviorName))
                {
                    diagnostics.Add(Diagnostic.At("CE0011", bs.Span,
                        $"'become {bs.BehaviorName}' — actor '{ctx.ActorName}' has no behavior named '{bs.BehaviorName}'."));
                }
                // CE0051 — become is a lifecycle operation: allowed in OnHandler,
                // Init, and any lifecycle hook (PreStart / PostStop / Restore).
                // Only plain methods are forbidden — they should stay pure helpers.
                // Matches Akka's convention of allowing `context.become` in `preStart`.
                if (ctx.Scope is Scope.Method)
                {
                    diagnostics.Add(Diagnostic.At("CE0051", bs.Span,
                        "'become' is not allowed in plain methods — use an 'on' handler, 'init', or a lifecycle hook."));
                }
                // CE0087 — `become` swaps the active behavior, which
                // is a writer-class state mutation. Reader handlers
                // are not allowed to do this.
                if (ctx.HandlerMode == HandlerMode.Reader)
                {
                    diagnostics.Add(Diagnostic.At("CE0087", bs.Span,
                        $"'reader on ...' handler may not 'become {bs.BehaviorName}' — " +
                        $"changing the active behavior mutates actor state. Mark this " +
                        $"handler 'writer on ...' or move the become into a writer arm."));
                }
                break;

            case PersistStmt ps:
                // CE0050 — persist only allowed in an on-handler body.
                if (ctx.Scope is not Scope.OnHandler)
                {
                    diagnostics.Add(Diagnostic.At("CE0050", ps.Span,
                        "'persist' is only allowed inside an 'on' handler body."));
                }
                // CE0087 — persist is a writer-class operation
                // (it captures and writes through actor state).
                if (ctx.HandlerMode == HandlerMode.Reader)
                {
                    diagnostics.Add(Diagnostic.At("CE0087", ps.Span,
                        "'reader on ...' handler may not 'persist' — persistence is " +
                        "a writer-class operation. Mark this handler 'writer on ...' " +
                        "or move the persist into a writer arm."));
                }
                break;

            case ReturnStmt r when r.Value is not null:
                WalkExpr(r.Value, ctx, diagnostics);
                break;

            case VarDeclStmt v:
                WalkExpr(v.Initializer, ctx, diagnostics);
                // Track the local so later member accesses on it can be classified.
                // When the local has an explicit type (`MyEnum s = …;` rather
                // than `var s = …;`), record the TypeRef too — CE0103 needs
                // it to detect switches over enum values.
                if (v.Type is not null)
                    ctx.Typer.RegisterLocalWithType(v.Name, v.Type);
                else
                    ctx.Typer.RegisterLocal(v.Name, ctx.Typer.Classify(v.Initializer));
                // CE0087 reachability (HOLE #2 / #4): remember when the local
                // aliases a region handle or a confined-class field, so a write
                // or mutating call reached through it is judged as a state
                // mutation. Registration above already cleared any stale alias.
                ctx.Typer.SetAlias(v.Name, ComputeFieldAlias(v.Initializer, ctx));
                break;

            case TryStmt tryStmt:
                WalkBlock(tryStmt.Try, ctx, diagnostics);
                foreach (var c in tryStmt.Catches)
                {
                    if (c.Binding is not null && c.ExceptionType is not null)
                        ctx.Typer.RegisterParameter(c.Binding, c.ExceptionType);
                    if (c.When is not null) WalkExpr(c.When, ctx, diagnostics);
                    WalkBlock(c.Body, ctx, diagnostics);
                }
                if (tryStmt.Finally is not null)
                    WalkBlock(tryStmt.Finally, ctx, diagnostics);
                break;

            case ThrowStmt t when t.Value is not null:
                WalkExpr(t.Value, ctx, diagnostics);
                break;

            case ThrowStmt:
                // Bare `throw;` — no expression to walk.
                break;

            case IfStmt ifs:
                WalkExpr(ifs.Condition, ctx, diagnostics);
                WalkBlock(ifs.Then, ctx, diagnostics);
                if (ifs.Else is not null) WalkStmt(ifs.Else, ctx, diagnostics);
                break;

            case ForStmt f:
                WalkStmt(f.Init, ctx, diagnostics);
                WalkExpr(f.Condition, ctx, diagnostics);
                WalkExpr(f.Increment, ctx, diagnostics);
                WalkBlock(f.Body, ctx, diagnostics);
                break;

            case SwitchStmt sw:
                WalkExpr(sw.Subject, ctx, diagnostics);
                foreach (var sec in sw.Sections)
                {
                    foreach (var label in sec.Labels)
                    {
                        // Type-pattern case labels bind a variable usable in the
                        // body (`case int n:` → `n`); register it before walking.
                        if (label.Pattern is not null) RegisterPatternBindings(label.Pattern, ctx);
                        if (label.Guard is not null) WalkExpr(label.Guard, ctx, diagnostics);
                    }
                    WalkBlock(new BlockStmt(sec.Span, sec.Body), ctx, diagnostics);
                }
                break;

            case ForeachStmt fe:
                WalkExpr(fe.Collection, ctx, diagnostics);
                // CE0116 (hint) — an `*Async` call inside a `foreach` is awaited
                // once per iteration, so the waits run back-to-back. If the
                // iterations are independent, overlapping them is faster. Hint
                // only, no auto-fix: parallelizing changes side-effect ordering
                // and exception aggregation, so it's the dev's call (see the
                // async-footguns catalog, #10).
                if (LoopBodyHasAsyncCall(fe.Body))
                    diagnostics.Add(Diagnostic.At("CE0116", fe.Span,
                        "calls ending in 'Async' inside a 'foreach' run sequentially — each is " +
                        "awaited before the next iteration. If the iterations are independent, " +
                        "consider overlapping them (await the tasks together) instead.",
                        DiagnosticSeverity.Hint));
                WalkBlock(fe.Body, ctx, diagnostics);
                break;

            case WhileStmt w:
                WalkExpr(w.Condition, ctx, diagnostics);
                WalkBlock(w.Body, ctx, diagnostics);
                break;

            case DoWhileStmt dw:
                WalkBlock(dw.Body, ctx, diagnostics);
                WalkExpr(dw.Condition, ctx, diagnostics);
                break;

            case ExpressionStmt e:
                WalkExpr(e.Expr, ctx, diagnostics);
                break;
        }
    }

    /// <summary>
    /// CE0116 support — true when a loop body contains a call to a method whose
    /// name ends in <c>Async</c> (the .NET async convention; invisible async
    /// awaits it). Conservative: does NOT descend into a nested loop — that
    /// inner loop earns its own hint.
    /// </summary>
    private static bool LoopBodyHasAsyncCall(Stmt stmt) => stmt switch
    {
        BlockStmt b      => b.Statements.Any(LoopBodyHasAsyncCall),
        ExpressionStmt e => ExprHasAsyncCall(e.Expr),
        VarDeclStmt v    => ExprHasAsyncCall(v.Initializer),
        ReturnStmt r     => r.Value is not null && ExprHasAsyncCall(r.Value),
        ThrowStmt t      => t.Value is not null && ExprHasAsyncCall(t.Value),
        IfStmt i         => ExprHasAsyncCall(i.Condition) || LoopBodyHasAsyncCall(i.Then)
                            || (i.Else is not null && LoopBodyHasAsyncCall(i.Else)),
        _ => false,
    };

    private static bool ExprHasAsyncCall(Expr expr) => expr switch
    {
        MethodCallExpr mc  => mc.Method.EndsWith("Async", StringComparison.Ordinal)
                              || ExprHasAsyncCall(mc.Target) || mc.Args.Any(ExprHasAsyncCall),
        InvocationExpr inv => inv.Args.Any(ExprHasAsyncCall),
        MemberAccessExpr m => ExprHasAsyncCall(m.Target),
        BinaryExpr b       => ExprHasAsyncCall(b.Left) || ExprHasAsyncCall(b.Right),
        UnaryExpr u        => ExprHasAsyncCall(u.Operand),
        ParenExpr p        => ExprHasAsyncCall(p.Inner),
        _ => false,
    };

    private static void WalkExpr(Expr expr, Context ctx, List<Diagnostic> diagnostics)
    {
        switch (expr)
        {
            case AskExpr ask:
                // CE0042 — ask only inside an on-handler body.
                if (ctx.Scope is not Scope.OnHandler)
                {
                    diagnostics.Add(Diagnostic.At("CE0042", ask.Span,
                        "'.Ask(...)' is only allowed inside an 'on' handler body."));
                }
                // CE0020 — the asked message type must resolve to a declared message.
                if (ask.Message is NewExpr askMsg && ctx.Symbols.ResolveMessage(askMsg.Type) is null)
                {
                    diagnostics.Add(Diagnostic.At("CE0020", askMsg.Type.Span,
                        $"'.Ask(...)' message type '{askMsg.Type}' is not a declared 'message'."));
                }
                WalkExpr(ask.Target, ctx, diagnostics);
                WalkExpr(ask.Message, ctx, diagnostics);
                break;

            case SelfExpr:
            case SenderExpr:
                // CE0043 — self/sender only inside an on-handler body.
                if (ctx.Scope is not Scope.OnHandler)
                {
                    var name = expr is SelfExpr ? "self" : "sender";
                    diagnostics.Add(Diagnostic.At("CE0043", expr.Span,
                        $"'{name}' is only allowed inside an 'on' handler body."));
                }
                break;

            case NamedArgExpr na:
                // Named argument `name: value` — the name is a label; diagnose
                // the value so CE checks still fire inside named args.
                WalkExpr(na.Value, ctx, diagnostics);
                break;

            case RefArgExpr ra:
                // CE0087 — an `out`/`ref` argument is a write to its target,
                // so in a reader handler it is the same race an assignment
                // would be. `in` arguments are read-only and pass freely.
                if (ctx.HandlerMode == HandlerMode.Reader
                    && ra.Modifier is ParamModifier.Out or ParamModifier.Ref)
                {
                    var kw = ra.Modifier == ParamModifier.Out ? "out" : "ref";
                    var fieldRoot = TryGetActorFieldRoot(ra.Inner, ctx.Typer);
                    if (fieldRoot is not null)
                    {
                        diagnostics.Add(Diagnostic.At("CE0087", ra.Span,
                            $"'reader on ...' handler may not pass actor field " +
                            $"'{fieldRoot}' by '{kw}' — the callee writes it. Mark " +
                            $"this handler 'writer on ...' or copy the field to a " +
                            $"local first."));
                    }
                    else
                    {
                        var regionRoot = TryGetSharedRegionRoot(ra.Inner, ctx.Actor.Declaration);
                        if (regionRoot is not null)
                        {
                            diagnostics.Add(Diagnostic.At("CE0087", ra.Span,
                                $"'reader on ...' handler may not pass shared-region " +
                                $"state '{regionRoot}' by '{kw}' — the callee writes " +
                                $"it. Mark this handler 'writer on ...'."));
                        }
                    }
                }
                WalkExpr(ra.Inner, ctx, diagnostics);
                break;

            case ArrayExpr arr:
                foreach (var el in arr.Elements) WalkExpr(el, ctx, diagnostics);
                if (arr.Size is not null) WalkExpr(arr.Size, ctx, diagnostics);
                break;

            case TupleExpr tup:
                foreach (var el in tup.Elements) WalkExpr(el, ctx, diagnostics);
                break;

            case ThrowExpr th:
                WalkExpr(th.Value, ctx, diagnostics);
                break;

            case AssignExpr a:
                // CE0087 — assignment that touches actor state from a
                // reader handler. Three LHS shapes count as field
                // mutation:
                //   foo = X           (NameExpr resolving to an actor field)
                //   foo.bar = X       (multi-part NameExpr or MemberAccessExpr rooted at a field)
                //   foo[i] = X        (IndexExpr rooted at a field)
                // Bare reassignment of a local variable is fine.
                //
                // The same rule applies to shared-region
                // fields reached through a `use X foo;` local. Reader
                // handlers see only the region's reader lock; mutations
                // would race with other readers on the same region.
                if (ctx.HandlerMode == HandlerMode.Reader)
                {
                    // The write may name the field directly, or reach it THROUGH
                    // a local that aliases a confined-class field / region handle
                    // (`var h = counter; h.n = …`, `var c = cache; c.field = …`).
                    var aliased = TryGetAliasedWriteRoot(a.Left, ctx);
                    var fieldRoot = TryGetActorFieldRoot(a.Left, ctx.Typer)
                                    ?? (aliased is { IsRegion: false } cf ? cf.Root : null);
                    if (fieldRoot is not null)
                    {
                        diagnostics.Add(Diagnostic.At("CE0087", a.Span,
                            $"'reader on ...' handler may not mutate actor field " +
                            $"'{fieldRoot}'. Mark this handler 'writer on ...' or move " +
                            $"the mutation into a writer arm. Reader handlers are bound " +
                            $"to read-only access to actor state so the future runtime can " +
                            $"run them concurrently with other readers."));
                    }
                    else
                    {
                        var regionRoot = TryGetSharedRegionRoot(a.Left, ctx.Actor.Declaration)
                                         ?? (aliased is { IsRegion: true } rf ? rf.Root : null);
                        if (regionRoot is not null)
                        {
                            diagnostics.Add(Diagnostic.At("CE0087", a.Span,
                                $"'reader on ...' handler may not mutate shared-region " +
                                $"state '{regionRoot}'. Reader handlers hold the region's " +
                                $"reader lock — mutations would race against other " +
                                $"concurrent readers. Mark this handler 'writer on ...' " +
                                $"or move the mutation into a writer arm."));
                        }
                    }
                }

                // CE0100 — deep-copy at the region edge.
                //
                // Reading a shared-region field directly into an actor field
                // means the actor holds a reference to data the region still
                // owns. After the handler releases the region's lock, a
                // concurrent writer on the region could mutate that data
                // while the actor is reading from it. Force the user to
                // route the value through a local (which signals "I know
                // this is a snapshot") or a deep-copy call.
                {
                    var lhsField = TryGetActorFieldRoot(a.Left, ctx.Typer);
                    if (lhsField is not null)
                    {
                        var rhsRegion = TryGetRegionReadRoot(a.Right, ctx.Actor.Declaration);
                        if (rhsRegion is not null)
                        {
                            diagnostics.Add(Diagnostic.At("CE0100", a.Span,
                                $"Assigning a shared-region read ('{rhsRegion}.…') directly " +
                                $"into actor field '{lhsField}' lets the actor hold a " +
                                $"reference past the region's lock. Route through a local " +
                                $"or an explicit deep-copy ('var snap = {rhsRegion}.…; " +
                                $"_{lhsField} = Clone(snap);') so the borrow is intentional."));
                        }
                    }
                }
                WalkExpr(a.Left, ctx, diagnostics);
                WalkExpr(a.Right, ctx, diagnostics);
                break;

            case ConditionalExpr c:
                WalkExpr(c.Condition, ctx, diagnostics);
                WalkExpr(c.Then, ctx, diagnostics);
                WalkExpr(c.Else, ctx, diagnostics);
                break;

            case BinaryExpr b:
                WalkExpr(b.Left, ctx, diagnostics);
                WalkExpr(b.Right, ctx, diagnostics);
                break;
            case TypeOpExpr t:
                WalkExpr(t.Operand, ctx, diagnostics);
                break;

            case UnaryExpr u:
                WalkExpr(u.Operand, ctx, diagnostics);
                break;

            case MemberAccessExpr m:
                // CE0012 — property access on an actor-ref is forbidden, EXCEPT
                // the ambient self-accessors (self.Log / Metrics / TraceContext
                // / System), which resolve to ActorBase properties rather than
                // members of the ActorRef (see ExpressionEmitter.IsSelfAccessor).
                if (!(m.Target is SelfExpr && IsSelfAccessor(m.Member))
                    && ctx.Typer.Classify(m.Target) == ExprKind.ActorRef)
                {
                    diagnostics.Add(Diagnostic.At("CE0012", m.Span,
                        $"Cannot read member '{m.Member}' on an actor reference — actors are only reachable via 'Tell' or 'ask'."));
                }

                // NB: `.Result` on a Task is NOT diagnosed here — the
                // invisible-async pass rewrites `task.Result` into
                // `(await task)` (AsyncRewriter.VisitMemberAccessExpression),
                // so it never blocks the dispatcher. Silently fixing it is
                // consistent with how invisible async auto-awaits any
                // Task-returning call. (The blocking *method* forms `.Wait()`
                // / `.GetResult()` stay hard CE0083 errors.)

                // CE0101 / CE0102 — lifecycle markers on shared-region fields.
                // When the access shape is `useLocal.fieldName`, look up the
                // field on the underlying region and surface a warning for
                // deprecated fields, an error for retired ones.
                CheckRegionFieldLifecycle(m, ctx, diagnostics);

                WalkExpr(m.Target, ctx, diagnostics);
                break;

            case MethodCallExpr call:
            {
                // Underline the WHOLE call from its receiver (Target), not just the
                // postfix `.method(...)` part — `call.Span` starts at the '.', which
                // both gives a lopsided caret and breaks the CE0083/CE0115 editor
                // quick-fixes (they replace `Thread.Sleep` / insert after
                // `File.ReadAllText` relative to the range start, assuming it sits at
                // the call target).
                var callSpan = new SourceSpan(
                    call.Target.Span.StartLine, call.Target.Span.StartColumn,
                    call.Span.EndLine, call.Span.EndColumn);

                // CE0083 / CE0084 — static-style call blocklists.
                // Both checks run independently so an instance-shaped
                // call (`task.Wait()`) that doesn't match a static
                // entry still gets caught by the bare-method check.
                var staticName = TryGetStaticCallName(call);
                if (staticName is not null
                    && DispatcherBlockingStaticCalls.TryGetValue(staticName, out var blockMsg))
                {
                    diagnostics.Add(Diagnostic.At("CE0083", callSpan,
                        $"'{staticName}' {blockMsg}."));
                }
                else if (DispatcherBlockingInstanceMethods.TryGetValue(call.Method, out var instanceMsg))
                {
                    diagnostics.Add(Diagnostic.At("CE0083", callSpan,
                        $"'.{call.Method}()' {instanceMsg}."));
                }
                if (staticName is not null
                    && ProcessEscapeStaticCalls.TryGetValue(staticName, out var escapeMsg))
                {
                    diagnostics.Add(Diagnostic.At("CE0084", callSpan,
                        $"'{staticName}' {escapeMsg}."));
                }
                // CE0115 (warning) — synchronous BCL I/O with an async sibling.
                // The invisible-async pass rewrites this to the *Async sibling
                // and awaits it in handler/method bodies (AsyncRewriter), so it
                // doesn't block there; the warning steers you to write the async
                // form directly (and still earns its keep in init/ctor, where
                // there's no await context to rewrite into).
                if (staticName is not null
                    && SyncIoStaticCallsWithAsyncSibling.Contains(staticName))
                {
                    diagnostics.Add(Diagnostic.At("CE0115", callSpan,
                        $"'{staticName}' is synchronous I/O — prefer '{staticName}Async(...)' " +
                        "(Spek awaits the async form for you in handler/method bodies).",
                        DiagnosticSeverity.Warning));
                }

                // CE0119 — raw concurrency-spawning calls. Work started on a
                // pool/OS thread runs OUTSIDE any actor's turn, bypassing the
                // serialization that makes Spek race-free; concurrency in Spek
                // comes from actors, so these have no place in Spek source.
                // (Constructor forms — new Thread / new Timer — are caught in
                // the NewExpr walk.)
                if (staticName is not null
                    && ConcurrencySpawningStaticCalls.TryGetValue(staticName, out var spawnMsg))
                {
                    diagnostics.Add(Diagnostic.At("CE0119", callSpan,
                        $"'{staticName}' {spawnMsg}."));
                }
                // CE0119, instance-shaped — `source.AsParallel()` and friends.
                // Matched on the bare method name (the receiver's type is
                // unknowable), the same mechanism the CE0083 instance
                // blocklist uses for `.WaitAll()`.
                if (ConcurrencySpawningInstanceMethods.TryGetValue(call.Method, out var parallelMsg))
                {
                    diagnostics.Add(Diagnostic.At("CE0119", callSpan,
                        $"'.{call.Method}()' {parallelMsg}."));
                }

                // CE0138 — `To<T>` admits only LOSSLESS conversions, but C#'s
                // implicit numeric widenings include lossy ones (int/long →
                // float, long → double): the target's mantissa is narrower than
                // the source's integer range, so large values round silently —
                // 16777217.To<float>() == 16777216. That falsifies "you cannot
                // silently lose data", so steer to TryTo. Only fires when the
                // source type is pinned down (literal, local/field, msg.field);
                // an unresolved receiver stays silent rather than guess.
                if (call.Method == "To" && call.TypeArgs.Count == 1
                    && NormalizePrimitive(call.TypeArgs[0].ToString()) is { } toTarget
                    && TryNumericSourceType(call.Target, ctx.Typer, ctx.Symbols) is { } toSource
                    && LossyImplicitWidenings.Contains((toSource, toTarget)))
                {
                    diagnostics.Add(Diagnostic.At("CE0138", callSpan,
                        $"'.To<{toTarget}>()' on a {toSource} can silently lose precision — " +
                        $"a {toTarget} cannot exactly represent every {toSource} (its mantissa " +
                        $"is narrower than the source's integer range), so large values round. " +
                        $"'To' admits only lossless conversions. Use '.TryTo<{toTarget}>()' " +
                        $"({toTarget}?), which is null when the value isn't exactly representable, " +
                        $"or '.TryTo<{toTarget}>(MidpointRounding.…)' to round explicitly."));
                }

                var targetKind = ctx.Typer.Classify(call.Target);

                if (targetKind == ExprKind.ActorRef)
                {
                    if (call.Method != "Tell")
                    {
                        // CE0012 — method calls on an actor-ref are restricted to 'Tell'.
                        diagnostics.Add(Diagnostic.At("CE0012", call.Span,
                            $"Cannot call '{call.Method}' on an actor reference — actors are only reachable via 'Tell' or 'ask'."));
                    }
                    else if (call.Args.Count == 1)
                    {
                        // CE0020 — Tell's payload must be a 'message' type.
                        // Fail open on Unknown (insufficient info) to avoid
                        // false positives on values whose type isn't statically
                        // resolvable with the current typer.
                        var argKind = ctx.Typer.Classify(call.Args[0]);
                        if (argKind is ExprKind.Primitive or ExprKind.ActorRef)
                        {
                            var what = argKind == ExprKind.Primitive ? "a primitive value" : "an actor reference";
                            diagnostics.Add(Diagnostic.At("CE0020", call.Args[0].Span,
                                $"'Tell' payload must be a declared 'message' type, not {what}."));
                        }
                    }
                }

                // CE0087 — a reader handler may not call a state-mutating method.
                // Three mutation shapes are caught here (field writes via
                // `classField.x = …` are the AssignExpr arm's job):
                //   HOLE #4 — a mutating method on a confined-class field, the
                //     receiver reached through parens / a local alias / a ternary
                //     (`(c).Inc()`, `var h = c; h.Inc()`, `(f ? a : b).Inc()`),
                //     judged against the class's own MutatingMethods.
                //   HOLE #3 — an in-place mutating BCL call on a mutable
                //     collection field (`items.Add(v)`), judged by method NAME.
                //   HOLE #1 — `self.M()` calling one of the actor's own methods
                //     that mutates state directly or transitively.
                if (ctx.HandlerMode == HandlerMode.Reader)
                {
                    var classCands = ReceiverConfinedClasses(call.Target, ctx).ToList();
                    if (classCands.Count > 0)
                    {
                        // A confined-class receiver: its own MutatingMethods set
                        // is authoritative — a pure method (Value(), Peek()) is
                        // clean, so the BCL name-heuristic never runs for it.
                        foreach (var (display, _field, cls) in classCands)
                        {
                            if (ctx.Symbols.GetClassSymbols(cls) is { } classSyms
                                && classSyms.MutatingMethods.Contains(call.Method))
                            {
                                diagnostics.Add(Diagnostic.At("CE0087", call.Span,
                                    $"'reader on ...' handler may not call mutating method " +
                                    $"'{call.Method}' on class-typed field '{display}'. " +
                                    $"'{cls}.{call.Method}' writes the object's " +
                                    $"state, which would race against other concurrent readers. Mark this " +
                                    $"handler 'writer on ...', or move the call into a writer arm."));
                                break;
                            }
                        }
                    }
                    else if (ReceiverCollectionRoot(call.Target, ctx) is { } collField
                             && MutatingBclMethodNames.Contains(call.Method))
                    {
                        diagnostics.Add(Diagnostic.At("CE0087", call.Span,
                            $"'reader on ...' handler may not call mutating method " +
                            $"'.{call.Method}()' on collection field '{collField}' — it " +
                            $"modifies the collection in place, which would race against " +
                            $"other concurrent readers. Mark this handler 'writer on ...', " +
                            $"or move the mutation into a writer arm."));
                    }

                    // HOLE #1 — `self.M()` reaching a reader-unsafe own method.
                    if (call.Target is SelfExpr && ctx.UnsafeMethods.Contains(call.Method))
                    {
                        diagnostics.Add(Diagnostic.At("CE0087", call.Span,
                            $"'reader on ...' handler may not call '{call.Method}' — it " +
                            $"mutates actor or shared-region state. Mark this handler " +
                            $"'writer on ...', or move the call into a writer arm."));
                    }
                }

                WalkExpr(call.Target, ctx, diagnostics);
                foreach (var a in call.Args) WalkExpr(a, ctx, diagnostics);
                break;
            }

            case IndexExpr idx:
                WalkExpr(idx.Target, ctx, diagnostics);
                WalkExpr(idx.Index, ctx, diagnostics);
                break;

            case InvocationExpr inv:
                // CE0087 (HOLE #1) — a bare `M()` calling one of the actor's own
                // reader-unsafe methods (`DoMutate()`, `Outer()`, `Helper()`).
                // `self.M()` is the MethodCallExpr sibling of this check.
                if (ctx.HandlerMode == HandlerMode.Reader
                    && ctx.UnsafeMethods.Contains(inv.Callee))
                {
                    diagnostics.Add(Diagnostic.At("CE0087", inv.Span,
                        $"'reader on ...' handler may not call '{inv.Callee}' — it " +
                        $"mutates actor or shared-region state. Mark this handler " +
                        $"'writer on ...', or move the call into a writer arm."));
                }
                foreach (var a in inv.Args) WalkExpr(a, ctx, diagnostics);
                break;

            case LambdaExpr lam:
                // Descend into the body when either holds:
                //  • CE0087 (HOLE #5) — a reader lock is held while a lambda run
                //    synchronously in-turn (List.ForEach, LINQ over an in-memory
                //    sequence, …) executes, so a mutation inside it is the same
                //    race a handler-body mutation would be. (An ESCAPING mutating
                //    lambda is additionally CE0135's job; both firing is fine.)
                //  • red-team H2 — this is a stream-operator selector, which runs
                //    OFF the actor's turn; the concurrency/cast/time CE family
                //    (CE0119/0127/0129/0134) must see inside it. CE0087's mutation
                //    arms stay reader-gated, so a writer-mode descent can't make
                //    them over-fire.
                // The lambda's parameters register as locals first so they shadow
                // any same-named field.
                if (ctx.HandlerMode == HandlerMode.Reader || ctx.InStreamOperator)
                {
                    foreach (var lp in lam.Parameters)
                    {
                        if (lp.Type is not null) ctx.Typer.RegisterParameter(lp.Name, lp.Type);
                        else ctx.Typer.RegisterLocal(lp.Name, ExprKind.Unknown);
                    }
                    switch (lam.Body)
                    {
                        case Expr bodyExpr:  WalkExpr(bodyExpr, ctx, diagnostics); break;
                        case BlockStmt block: WalkBlock(block, ctx, diagnostics); break;
                    }
                }
                break;

            case NewExpr n:
                {
                    // CE0119 — `new Thread(...)` / `new Timer(...)` start a
                    // delegate on an OS / pool thread outside the actor system.
                    // Matched by simple type name plus a delegate first argument,
                    // so a domain type coincidentally named `Timer` (built from
                    // ordinary args) isn't flagged.
                    var newType = n.Type.Parts[^1];
                    if (newType is "Thread" or "Timer" && HasDelegateFirstArg(n.Args))
                    {
                        diagnostics.Add(Diagnostic.At("CE0119", n.Span,
                            newType == "Thread"
                                ? "'new Thread(...)' creates an unsupervised OS thread outside the actor system — model the concurrent work as a child actor ('spawn<Worker>()') instead."
                                : "'new Timer(...)' fires a callback on a thread-pool thread outside any actor's turn — schedule a delayed message to yourself instead."));
                    }
                    foreach (var a in n.Args) WalkExpr(a, ctx, diagnostics);
                    if (n.Initializer is not null)
                        foreach (var a in n.Initializer) WalkExpr(a, ctx, diagnostics);
                    break;
                }

            case SpawnExpr sp:
                foreach (var a in sp.Args) WalkExpr(a, ctx, diagnostics);
                break;

            case ParenExpr p:
                WalkExpr(p.Inner, ctx, diagnostics);
                break;

            case SwitchExpr sw:
                WalkExpr(sw.Subject, ctx, diagnostics);
                foreach (var arm in sw.Arms)
                {
                    // Bind any type-pattern variables (including those
                    // nested inside property patterns) so the body's
                    // expressions can resolve them.
                    RegisterPatternBindings(arm.Pattern, ctx);
                    if (arm.When is not null) WalkExpr(arm.When, ctx, diagnostics);
                    WalkExpr(arm.Result, ctx, diagnostics);
                }
                // CE0103 — exhaustive switch over a sealed enum.
                CheckExhaustiveSwitchOverEnum(sw, ctx, diagnostics);
                break;

            case NameExpr nm:
                // A multi-part qualified name like `peer.name` is parsed greedily
                // into a single NameExpr rather than a MemberAccessExpr chain.
                // Treat it as an implicit member access for CE0012 purposes.
                if (nm.Name.Parts.Count > 1 &&
                    ctx.Typer.ClassifySimple(nm.Name.Parts[0]) == ExprKind.ActorRef)
                {
                    diagnostics.Add(Diagnostic.At("CE0012", nm.Span,
                        $"Cannot read member '{nm.Name.Parts[1]}' on an actor reference — actors are only reachable via 'Tell' or 'ask'."));
                }

                // CE0101 / CE0102 — same lifecycle check as the
                // MemberAccessExpr branch, applied to the parser's other
                // shape for `cache.fieldName` (greedy qualifiedName).
                CheckRegionFieldLifecycleOnName(nm, ctx, diagnostics);
                break;

            case LiteralExpr:
                break;
        }
    }

    /// <summary>
    /// Registers any variable bindings introduced by a switch-arm
    /// pattern. Handles type patterns at any nesting depth, including
    /// inside property patterns. Bindings persist for the rest of the
    /// switch (consistent with the existing top-level type-pattern
    /// behaviour).
    /// </summary>
    private static void RegisterPatternBindings(SwitchPattern pattern, Context ctx)
    {
        switch (pattern)
        {
            case TypePattern tp when tp.Binding is not null:
                ctx.Typer.RegisterParameter(tp.Binding, tp.Type);
                break;
            case PropertyPattern pp:
                foreach (var sub in pp.Properties)
                    RegisterPatternBindings(sub.Inner, ctx);
                break;
            case NotPattern n:
                RegisterPatternBindings(n.Inner, ctx);
                break;
            case AndPattern a:
                RegisterPatternBindings(a.Left, ctx);
                RegisterPatternBindings(a.Right, ctx);
                break;
            case OrPattern o:
                RegisterPatternBindings(o.Left, ctx);
                RegisterPatternBindings(o.Right, ctx);
                break;
        }
    }
}
