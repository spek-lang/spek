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
    /// <see cref="SpekCompilation"/> uses this overload to pass a combined
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
                if (symbols.ResolveClass(field.Type.Name.Simple) is null) continue;
                diagnostics.Add(Diagnostic.At("CE0112", field.Span,
                    $"Shared-region field '{region.Name}.{field.Name}' has mutable class type " +
                    $"'{field.Type.Name.Simple}'. A class is mutable and single-owner — placing it " +
                    $"in a shared region would let multiple actors reach mutable state. Use an " +
                    $"immutable 'message' type or a primitive for shared state, or keep the class " +
                    $"confined to a single actor."));
            }
        }
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
    /// The ambient <c>self.X</c> accessors that resolve to
    /// <see cref="ActorBase"/> properties (not <c>ActorRef</c> members), so
    /// they're exempt from the CE0012 "actor-refs are Tell/ask only" rule.
    /// Must stay in sync with <c>ExpressionEmitter.IsSelfAccessor</c>.
    /// </summary>
    private static bool IsSelfAccessor(string member) =>
        member is "TraceContext" or "Metrics" or "Log" or "System";

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
        if (call.Target is NameExpr name && name.Name.Parts.Count == 1)
            return $"{name.Name.Parts[0]}.{call.Method}";
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
        HandlerMode HandlerMode = HandlerMode.Writer)
    {
        public string ActorName => Actor.Declaration.Name;
        public IReadOnlySet<string> BehaviorNames => Actor.BehaviorNames;
    }

    // ─── Actor-level checks ─────────────────────────────────────────────────

    private static void AnalyzeActor(ActorDecl actor, SymbolTable symbols, List<Diagnostic> diagnostics)
    {
        var actorSymbols = symbols.GetActorSymbols(actor.Name)!;

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
                    break;
                }

                case TermBlock term:
                {
                    var typer = new ExpressionTyper(symbols, actorSymbols);
                    WalkBlock(term.Body,
                        new Context(actorSymbols, symbols, Scope.Term, typer),
                        diagnostics);
                    CheckMovedValueFlow(term.Body, diagnostics);
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
                                HandlerMode: handler.Mode),
                            diagnostics);
                        if (handler.Body is BlockHandlerBody bhb)
                            CheckMovedValueFlow(bhb.Block, diagnostics);
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
                    break;

                case MethodDecl method:
                {
                    var typer = new ExpressionTyper(symbols, actorSymbols);
                    foreach (var p in method.Parameters) typer.RegisterParameter(p.Name, p.Type);
                    WalkBlock(method.Body,
                        new Context(actorSymbols, symbols, Scope.Method, typer),
                        diagnostics);
                    CheckMovedValueFlow(method.Body, diagnostics);
                    break;
                }
            }
        }
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

    /// <summary>
    /// Returns the base local name being mutated by an assignment,
    /// or null if the LHS isn't a member-access / index-access onto
    /// a single-part local. Bare-local reassignment
    /// (<c>foo = bar</c>) returns null — that re-binds the local
    /// rather than mutating its previous value.
    /// </summary>
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
                typer.RegisterLocal(n.Binding,
                    symbols.ResolveMessage(n.MessageType) is not null
                        ? ExprKind.Message
                        : ExprKind.Unknown);
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
                    var fieldRoot = TryGetActorFieldRoot(a.Left, ctx.Typer);
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
                        var regionRoot = TryGetSharedRegionRoot(a.Left, ctx.Actor.Declaration);
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

                // CE0087 — a reader handler may not call a *mutating*
                // method on a confined class instance held in an actor field.
                // Field writes via `classField.x = …` are already caught by the
                // AssignExpr arm; this catches `classField.Mutate()`. We only
                // match a *direct* field receiver (`field.M()` / `self.field.M()`),
                // not deeper paths — the deep/transitive case is the parked
                // §6b problem. The mutating-method set is precomputed in
                // ClassSymbols.
                if (ctx.HandlerMode == HandlerMode.Reader)
                {
                    var classFieldName = call.Target switch
                    {
                        NameExpr n when n.Name.Parts.Count == 1 && ctx.Typer.IsActorField(n.Name.Simple) => n.Name.Simple,
                        MemberAccessExpr ma when ma.Target is SelfExpr && ctx.Typer.IsActorField(ma.Member) => ma.Member,
                        _ => null,
                    };
                    if (classFieldName is not null
                        && ctx.Actor.Fields.TryGetValue(classFieldName, out var classField)
                        && ctx.Symbols.GetClassSymbols(classField.Type.Name.Simple) is { } classSyms
                        && classSyms.MutatingMethods.Contains(call.Method))
                    {
                        diagnostics.Add(Diagnostic.At("CE0087", call.Span,
                            $"'reader on ...' handler may not call mutating method " +
                            $"'{call.Method}' on class-typed field '{classFieldName}'. " +
                            $"'{classField.Type.Name.Simple}.{call.Method}' writes the object's " +
                            $"state, which would race against other concurrent readers. Mark this " +
                            $"handler 'writer on ...', or move the call into a writer arm."));
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
