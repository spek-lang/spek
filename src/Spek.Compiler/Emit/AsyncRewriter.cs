using System.Diagnostics.CodeAnalysis;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace Spek.Compiler.Emit;

/// <summary>
/// Invisible async/await. Post-processes the provisionally-emitted C# so
/// the developer never writes <c>async</c> or <c>await</c>. Detection uses
/// a real Roslyn <see cref="SemanticModel"/> (NOT a naming heuristic),
/// BCL-seeded with an injectable reference set. Full design:
/// the async chapter of the language guide.
///
/// The governing rule: auto-await a Task wherever it meets a *value
/// context*; leave it in a *task context*.
///
/// Stage 1 (this implementation):
///   * A Task-returning call in a *value position* is awaited
///     (statements, value-typed bindings, operands, member access, args).
///   * A Task-bound local *defers*: an explicit <c>Task&lt;T&gt;</c> local
///     always (the developer named the Task type), and a <c>var</c>-bound
///     <c>Task</c> local when its method is *single-exit* (no early
///     returns) — so deferral is only applied where provably safe; every
///     other case falls back to eager await, which is always safe.
///   * Uses of a deferred Task local are awaited at the value-use
///     (generalized to Task-typed identifiers, not just invocations) —
///     this is what makes lazy <c>var</c> concurrent and makes the
///     explicit-Task hatch usable.
///   * A *structured join* awaits any deferred Task local not otherwise
///     awaited, before the method returns — required so no Task outlives
///     the actor's writer lock (and it closes a latent leak in the
///     explicit-Task hatch).
///   * The pass iterates to a fixpoint: a method that gains an await
///     becomes <c>async</c>, so its callers await it next iteration.
///
/// <c>ValueTask</c> stays eager (it can't be awaited twice). Constructors,
/// lambdas, local functions and accessors are never auto-awaited.
/// Not yet covered (eager fallback, safe): deferral across early returns /
/// nested scopes, and excluding forwarded Tasks from the join.
/// </summary>
public static class AsyncRewriter
{
    private const int MaxIterations = 32;

    private static readonly Lazy<IReadOnlyList<MetadataReference>> DefaultReferences =
        new(BuildFrameworkReferences);

    // Filenames the framework seed already provides. Extra references that
    // duplicate one (e.g. a project's net10.0 ref-pack System.Runtime.dll vs the
    // runtime's, or a netcoreapp3.1 test-host assembly) are skipped — two copies
    // of a core assembly make types like Task ambiguous, which would silently
    // stop them being recognized as awaitable.
    private static readonly Lazy<HashSet<string>> SeededFileNames =
        new(() => DefaultReferences.Value
            .Select(r => Path.GetFileName((r as PortableExecutableReference)?.FilePath ?? ""))
            .Where(n => n.Length > 0)
            .ToHashSet(StringComparer.OrdinalIgnoreCase));

    /// <summary>
    /// Rewrites <paramref name="provisionalCSharp"/>, inserting auto-await
    /// and async signatures. Returns the original string unchanged when no
    /// await is needed (preserves exact formatting for non-async code).
    /// </summary>
    public static string Rewrite(
        string provisionalCSharp,
        IReadOnlyList<MetadataReference>? references = null,
        IEnumerable<string>? extraReferencePaths = null)
    {
        // Default seed is the BCL (DefaultReferences). When the caller supplies
        // extra assembly paths (the CLI's --ref, the MSBuild target's package
        // refs), append them so the rewriter can SEE their Task-returning APIs
        // and auto-await them — this is what makes e.g. AspNetCore's
        // app.RunAsync() / context.Response.WriteAsync() auto-await. The
        // locked design called for "BCL-seeded + injectable"; this is the
        // injectable half.
        if (references is null)
        {
            references = DefaultReferences.Value;
            if (extraReferencePaths is not null)
            {
                var combined = new List<MetadataReference>(references);
                var seeded   = SeededFileNames.Value;
                foreach (var p in extraReferencePaths)
                    if (!string.IsNullOrEmpty(p) && File.Exists(p)
                        && !seeded.Contains(Path.GetFileName(p)))
                        combined.Add(MetadataReference.CreateFromFile(p));
                references = combined;
            }
        }

        var source = provisionalCSharp;
        for (var i = 0; i < MaxIterations; i++)
        {
            var tree        = CSharpSyntaxTree.ParseText(source);
            var compilation = CSharpCompilation.Create(
                "SpekAsyncPass",
                new[] { tree },
                references,
                new CSharpCompilationOptions(
                    OutputKind.DynamicallyLinkedLibrary,
                    nullableContextOptions: NullableContextOptions.Enable));
            var model = compilation.GetSemanticModel(tree);

            var pass    = new Pass(model);
            var newRoot = pass.Visit(tree.GetRoot());
            if (!pass.Changed)
                return source;            // converged — return as-is

            source = newRoot.ToFullString();
        }
        return source;
    }

    private static IReadOnlyList<MetadataReference> BuildFrameworkReferences()
    {
        // Same technique as the test harness (RoslynCompileHelper): the
        // trusted-platform-assemblies list is the full framework BCL,
        // available to any .NET process. This resolves every framework
        // async API (Task.Delay, File.*Async, HttpClient, ...). Spek's
        // own runtime types may resolve as error-types here (Spek.Compiler
        // doesn't reference Spek.Runtime), but that's harmless: the
        // already-awaited runtime calls (ask/persist) are skipped, and an
        // unresolved type simply isn't classified as awaitable.
        var tpa = (AppContext.GetData("TRUSTED_PLATFORM_ASSEMBLIES") as string) ?? "";
        return tpa.Split(Path.PathSeparator)
            .Where(p => !string.IsNullOrEmpty(p))
            .Select(p => (MetadataReference)MetadataReference.CreateFromFile(p))
            .ToList();
    }

    private sealed class Pass(SemanticModel model) : CSharpSyntaxRewriter
    {
        public bool Changed { get; private set; }

        // ─── Awaiting call results (value-position invocations) ──────────

        public override SyntaxNode? VisitInvocationExpression(InvocationExpressionSyntax node)
        {
            var visited = (ExpressionSyntax)base.VisitInvocationExpression(node)!;
            // Sync-over-async rewrites: task.Wait() → await task,
            // x.GetAwaiter().GetResult() → (await x). Same wait/value, no
            // blocked thread. (`.Result` is handled in VisitMemberAccessExpression.)
            if (TryRewriteSyncOverAsync(node, out var deblocked)) { Changed = true; return deblocked!; }
            // Sync BCL I/O → its *Async sibling, awaited:
            // File.ReadAllText(p) → await File.ReadAllTextAsync(p). Same value,
            // no blocked dispatcher. (Pairs with the CE0115 warning, which
            // teaches the idiom; this makes it correct regardless.)
            if (TryRewriteSyncIo(node, visited, out var asyncified)) { Changed = true; return asyncified!; }
            if (!ShouldAwaitInvocation(node)) return visited;
            Changed = true;
            if (visited is InvocationExpressionSyntax inv)
                visited = MaybeThreadToken(inv, model.GetSymbolInfo(node).Symbol as IMethodSymbol,
                                           node.ArgumentList.Arguments.Count, node);
            return WrapAwait(visited, NeedsParens(node));
        }

        /// <summary>
        /// Rewrites the value-preserving blocking-wait method forms into an
        /// await: <c>task.Wait()</c> → <c>await task</c> (void) and
        /// <c>x.GetAwaiter().GetResult()</c> → <c>(await x)</c> (value). Only
        /// when the receiver is a real Task/ValueTask and an await is legal
        /// here. The receiver is taken from the original node — for these
        /// shapes it's never itself rewritten (it's a "used-as-task" position).
        /// </summary>
        private bool TryRewriteSyncOverAsync(InvocationExpressionSyntax node, out SyntaxNode? result)
        {
            result = null;
            if (node.ArgumentList.Arguments.Count != 0) return false;
            if (node.Expression is not MemberAccessExpressionSyntax ma) return false;
            if (!AwaitContextAllowed(node)) return false;

            ExpressionSyntax receiver;
            bool valuePosition;
            switch (ma.Name.Identifier.Text)
            {
                case "Wait":                                   // task.Wait()
                    receiver = ma.Expression;
                    valuePosition = false;
                    break;
                case "GetResult"                               // x.GetAwaiter().GetResult()
                    when ma.Expression is InvocationExpressionSyntax inner
                         && inner.ArgumentList.Arguments.Count == 0
                         && inner.Expression is MemberAccessExpressionSyntax innerMa
                         && innerMa.Name.Identifier.Text == "GetAwaiter":
                    receiver = innerMa.Expression;
                    valuePosition = true;
                    break;
                default:
                    return false;
            }

            if (!IsAwaitable(model.GetTypeInfo(receiver).Type)) return false;

            var awaited = AwaitExpression(
                Token(SyntaxKind.AwaitKeyword).WithTrailingTrivia(Space),
                receiver.WithoutTrivia());
            ExpressionSyntax replacement = valuePosition ? ParenthesizedExpression(awaited) : awaited;
            result = replacement
                .WithLeadingTrivia(node.GetLeadingTrivia())
                .WithTrailingTrivia(node.GetTrailingTrivia());
            return true;
        }

        // ─── Synchronous BCL I/O → its *Async sibling ───────────────────────
        // `System.IO.File.ReadAllText(p)` blocks the dispatcher on disk I/O,
        // but has a drop-in `File.ReadAllTextAsync(p)` that returns a Task of
        // the same value. We swap the method name to its `*Async` sibling and
        // await it — value-preserving (same result, same exceptions). The
        // curated `File.*` surface matches CE0115; the semantic warning teaches
        // the idiom, this rewrite makes it non-blocking either way.

        private static readonly HashSet<string> SyncIoFileMethods = new(StringComparer.Ordinal)
        {
            "ReadAllText",  "ReadAllBytes",  "ReadAllLines",
            "WriteAllText", "WriteAllBytes", "AppendAllText",
        };

        private bool TryRewriteSyncIo(
            InvocationExpressionSyntax node, ExpressionSyntax visited, out SyntaxNode? result)
        {
            result = null;
            if (node.Expression is not MemberAccessExpressionSyntax ma) return false;
            var methodName = ma.Name.Identifier.Text;
            if (!SyncIoFileMethods.Contains(methodName)) return false;
            if (!AwaitContextAllowed(node)) return false;

            // Type-accurate: confirm the call binds to System.IO.File (a user's
            // own `File.ReadAllText` is left alone), and that the `*Async`
            // sibling actually exists on the type before swapping.
            if (model.GetSymbolInfo(node).Symbol is not IMethodSymbol sym) return false;
            if (sym.ContainingType?.ToDisplayString() != "System.IO.File") return false;
            if (!sym.ContainingType.GetMembers(methodName + "Async").OfType<IMethodSymbol>().Any())
                return false;

            // Build `await File.{name}Async(args)` from the visited node so any
            // rewrites already applied inside the argument list are preserved.
            var visitedInv = (InvocationExpressionSyntax)visited;
            var visitedMa  = (MemberAccessExpressionSyntax)visitedInv.Expression;
            var asyncInv   = visitedInv.WithExpression(
                visitedMa.WithName(IdentifierName(methodName + "Async")));

            // The *Async sibling accepts a CancellationToken — thread it in too
            // (in actor bodies only), so the swapped-in async I/O is cancellable.
            if (InActorContext(node))
            {
                IMethodSymbol? asyncSym = null;
                foreach (var m in sym.ContainingType.GetMembers(methodName + "Async").OfType<IMethodSymbol>())
                    if (m.Parameters.Length >= node.ArgumentList.Arguments.Count + 1
                        && (asyncSym is null || m.Parameters.Length < asyncSym.Parameters.Length))
                        asyncSym = m;
                var name = asyncSym is null ? null
                    : CancellationParamName(asyncSym, node.ArgumentList.Arguments.Count);
                if (name is not null) asyncInv = WithShutdownToken(asyncInv, name);
            }

            result = WrapAwait(asyncInv, NeedsParens(node))
                .WithLeadingTrivia(node.GetLeadingTrivia())
                .WithTrailingTrivia(node.GetTrailingTrivia());
            return true;
        }

        // ─── Rewriting `task.Result` into `(await task)` ────────────────
        // `Task<T>.Result` / `ValueTask<T>.Result` block the calling thread
        // until the Task completes — under load that parks a dispatcher pool
        // thread and starves siblings. `await task` yields the same T without
        // blocking (and unwraps the exception instead of AggregateException),
        // so invisible async rewrites it for you. Type-checked against the
        // real receiver type, so a non-Task `.Result` property is untouched;
        // only applied where an await is legal (AwaitContextAllowed).

        public override SyntaxNode? VisitMemberAccessExpression(MemberAccessExpressionSyntax node)
        {
            var visited = (MemberAccessExpressionSyntax)base.VisitMemberAccessExpression(node)!;
            if (node.Name.Identifier.Text != "Result") return visited;
            if (!IsAwaitable(model.GetTypeInfo(node.Expression).Type)) return visited;
            if (!AwaitContextAllowed(node)) return visited;
            Changed = true;
            // `(await <receiver>)` — receiver from the visited node so any
            // rewrites inside it are preserved; take the member access's trivia.
            return WrapAwait(visited.Expression, parens: true)
                .WithLeadingTrivia(visited.GetLeadingTrivia())
                .WithTrailingTrivia(visited.GetTrailingTrivia());
        }

        // ─── Awaiting uses of deferred Task locals (identifiers) ─────────

        public override SyntaxNode? VisitIdentifierName(IdentifierNameSyntax node)
        {
            var visited = (ExpressionSyntax)base.VisitIdentifierName(node)!;
            if (!ShouldAwaitIdentifier(node)) return visited;
            Changed = true;
            return WrapAwait(visited, NeedsParens(node));
        }

        // ─── Dropping a `var` binding that awaits to void ────────────────
        // A `var` local whose initializer the pass eager-awaits to a
        // *non-generic* Task/ValueTask binds `void` — illegal C# (CS0815).
        // Such a local can't hold a usable value anyway, so drop the binding
        // and keep just the awaited call as a statement:
        //   `var x = await F();`  →  `await F();`
        // Conservative: only single-declarator `var` locals whose initializer
        // is now an `await` of a void-typed expression, and which nothing else
        // references (dropping a referenced local would turn CS0815 into a
        // CS0103 "name does not exist" — a referenced void binding is malformed
        // either way, so we leave it). Explicit-Task locals (`Task x = …`, the
        // escape hatch — deferred, then joined) and `Task<T>`/`ValueTask<T>`
        // bindings (which await to a real value) are untouched. The void check
        // reads the ORIGINAL initializer's type via the model (the rewritten
        // `await …` node isn't in the model).

        public override SyntaxNode? VisitLocalDeclarationStatement(LocalDeclarationStatementSyntax node)
        {
            var visited = (LocalDeclarationStatementSyntax)base.VisitLocalDeclarationStatement(node)!;

            if (node.Declaration.Type is not IdentifierNameSyntax { Identifier.Text: "var" })
                return visited;                                  // explicit type — leave it
            if (visited.Declaration.Variables.Count != 1)
                return visited;                                  // single-declarator only

            var visitedInit = visited.Declaration.Variables[0].Initializer?.Value;
            if (visitedInit is not AwaitExpressionSyntax await)
                return visited;                                  // binding wasn't awaited

            // The await landed on a void result iff the original call returned a
            // non-generic Task/ValueTask. Read the type from the original node.
            var origVd   = node.Declaration.Variables[0];
            var origInit = origVd.Initializer?.Value;
            if (origInit is null || !IsVoidAwaitable(model.GetTypeInfo(origInit).Type))
                return visited;

            if (IsLocalReferenced(origVd))
                return visited;                                  // referenced — leave as-is

            Changed = true;
            // Replace the whole declaration with `await F();`, carrying the
            // statement's trivia (indentation + trailing newline) across.
            return ExpressionStatement(await.WithoutTrivia())
                .WithLeadingTrivia(visited.GetLeadingTrivia())
                .WithTrailingTrivia(visited.GetTrailingTrivia());
        }

        /// <summary>
        /// True when the name declared by <paramref name="vd"/> appears again
        /// (beyond its own declarator) anywhere in the enclosing method — in
        /// which case its binding must not be dropped. Matching is by NAME, not
        /// symbol: a void-typed binding is already a Roslyn error declaration,
        /// so its use sites don't bind to the local and a symbol-based check
        /// would miss them. A bare name collision is rare and the conservative
        /// direction is to keep the binding, so name-matching is the safe signal.
        /// </summary>
        private static bool IsLocalReferenced(VariableDeclaratorSyntax vd)
        {
            var name  = vd.Identifier.Text;
            var scope = vd.FirstAncestorOrSelf<MethodDeclarationSyntax>()?.Body
                        ?? (SyntaxNode?)vd.FirstAncestorOrSelf<BlockSyntax>();
            if (scope is null) return false;
            foreach (var id in scope.DescendantNodes().OfType<IdentifierNameSyntax>())
                if (id.Identifier.Text == name && !id.Ancestors().Contains(vd))
                    return true;
            return false;
        }

        // ─── Method: structured join + async signature ──────────────────

        public override SyntaxNode? VisitMethodDeclaration(MethodDeclarationSyntax node)
        {
            var visited = (MethodDeclarationSyntax)base.VisitMethodDeclaration(node)!;

            // Structured join: await every deferred Task local before each
            // exit in its live range (and at fall-through), so no Task
            // outlives the method (actor safety) and unused results still
            // complete. Path-aware — handles returns after a deferred binding.
            if (visited.Body is not null)
            {
                var deferred = DeferredTaskLocalNames(node);
                if (deferred.Count > 0 &&
                    InjectJoins(visited.Body, deferred.ToHashSet(StringComparer.Ordinal), out var joined))
                {
                    visited = visited.WithBody(joined);
                    Changed = true;
                }
            }

            // Already async, or no await landed directly in this body → leave.
            if (visited.Modifiers.Any(SyntaxKind.AsyncKeyword)) return visited;
            if (visited.Body is null || !ContainsDirectAwait(visited.Body)) return visited;

            Changed = true;
            var asyncMod = Token(SyntaxKind.AsyncKeyword).WithTrailingTrivia(Space);
            return visited
                .WithReturnType(MakeAsyncReturnType(visited.ReturnType))
                .WithModifiers(visited.Modifiers.Add(asyncMod));
        }

        // ─── Lambda: add `async` once an await lands in its body ─────────
        // Children are visited first (base.Visit), so any statement-position
        // await is already inserted; if the body now contains a direct await
        // and the delegate type returns Task, mark the lambda `async`. Only
        // Task-returning lambdas qualify (LambdaReturnsTask), so a
        // value-returning delegate's signature is never altered.

        public override SyntaxNode? VisitParenthesizedLambdaExpression(ParenthesizedLambdaExpressionSyntax node)
        {
            var visited = (ParenthesizedLambdaExpressionSyntax)base.VisitParenthesizedLambdaExpression(node)!;
            return AddAsyncIfAwaited(node, visited, visited.Body, visited.Modifiers, visited.WithModifiers);
        }

        public override SyntaxNode? VisitSimpleLambdaExpression(SimpleLambdaExpressionSyntax node)
        {
            var visited = (SimpleLambdaExpressionSyntax)base.VisitSimpleLambdaExpression(node)!;
            return AddAsyncIfAwaited(node, visited, visited.Body, visited.Modifiers, visited.WithModifiers);
        }

        private SyntaxNode AddAsyncIfAwaited(
            ExpressionSyntax original, SyntaxNode visited, CSharpSyntaxNode body,
            SyntaxTokenList modifiers, Func<SyntaxTokenList, SyntaxNode> withModifiers)
        {
            if (modifiers.Any(SyntaxKind.AsyncKeyword)) return visited;
            if (!ContainsDirectAwait(body)) return visited;
            if (!LambdaReturnsTask(original)) return visited;   // safety net

            Changed = true;
            // async takes the lambda's leading trivia; the rest of the lambda
            // shifts a space to the right of it.
            var asyncMod = Token(SyntaxKind.AsyncKeyword)
                .WithLeadingTrivia(visited.GetLeadingTrivia())
                .WithTrailingTrivia(Space);
            return withModifiers(modifiers.Insert(0, asyncMod))
                .WithLeadingTrivia();   // strip now-duplicated leading trivia off the body token
        }

        // ─── Await-decision helpers ──────────────────────────────────────

        private bool ShouldAwaitInvocation(InvocationExpressionSyntax node)
        {
            if (node.Parent is AwaitExpressionSyntax) return false;

            var type = model.GetTypeInfo(node).Type;
            if (!IsAwaitable(type)) return false;

            // Used AS a Task (`.ConfigureAwait`, `.Result`, ...) → leave it.
            if (UsedAsTaskViaMember(node, type)) return false;

            if (!AwaitContextAllowed(node)) return false;

            // Inside a lambda, only await a call used as a bare statement —
            // the after-next pattern `next(ctx);` followed by more work.
            // A call in return / expression-body / argument position is left
            // as a forwarded Task, so before-next middleware (`return
            // next(ctx)`) and value-forwarding lambdas are unchanged, and we
            // never emit `return await <non-generic Task>` (which wouldn't
            // compile).
            if (EnclosingCallableIsLambda(node) && node.Parent is not ExpressionStatementSyntax)
                return false;

            // Deferred bindings (explicit Task local, or var Task in a
            // single-exit method) are not awaited here — the use / join is.
            if (IsDeferredBinding(node)) return false;

            return true;
        }

        private bool ShouldAwaitIdentifier(IdentifierNameSyntax node)
        {
            if (node.Parent is AwaitExpressionSyntax) return false;

            var type = model.GetTypeInfo(node).Type;
            if (!IsAwaitable(type)) return false;

            // Only await a use of a local we actually KEPT as a Task
            // (deferred). A local whose binding was eager-awaited holds the
            // unwrapped value, so its uses must not be awaited — awaiting
            // both the binding and the use would double-unwrap.
            if (!RefersToDeferredTaskLocal(node)) return false;

            if (UsedAsTaskViaMember(node, type)) return false;
            if (!AwaitContextAllowed(node)) return false;
            // Identifier uses never sit in bare-statement position, so this
            // disables awaiting deferred Task-locals inside a lambda — an
            // edge the lambda support doesn't need and stays clear of.
            if (EnclosingCallableIsLambda(node) && node.Parent is not ExpressionStatementSyntax)
                return false;
            if (IsWriteOrTaskPosition(node)) return false;

            return true;
        }

        /// <summary>
        /// True when <paramref name="id"/> references a local whose binding
        /// was deferred (kept as a Task) — mirrors <see cref="IsDeferredBinding"/>
        /// but resolved from the use site, so the use and the binding agree.
        /// </summary>
        private bool RefersToDeferredTaskLocal(IdentifierNameSyntax id)
        {
            if (model.GetSymbolInfo(id).Symbol is not ILocalSymbol local) return false;
            if (local.DeclaringSyntaxReferences.FirstOrDefault()?.GetSyntax()
                    is not VariableDeclaratorSyntax vd) return false;
            if (vd.Initializer is null || vd.Parent is not VariableDeclarationSyntax decl)
                return false;

            if (decl.Type is IdentifierNameSyntax { Identifier.Text: "var" })
                return IsTask(model.GetTypeInfo(vd.Initializer.Value).Type)
                    && SafeToDeferAt(vd);

            return IsAwaitable(model.GetTypeInfo(decl.Type).Type);
        }

        /// <summary>
        /// A Task-returning call bound to a local that should keep the Task
        /// (defer the await): an explicit Task/ValueTask-typed local always;
        /// a <c>var</c>-bound <c>Task</c> only in a single-exit method (where
        /// the join can guarantee completion). Everything else awaits eagerly.
        /// </summary>
        private bool IsDeferredBinding(InvocationExpressionSyntax node)
        {
            if (node.Parent is not EqualsValueClauseSyntax eq || eq.Value != node)
                return false;
            if (eq.Parent is not VariableDeclaratorSyntax vd ||
                vd.Parent is not VariableDeclarationSyntax decl)
                return false;

            if (decl.Type is IdentifierNameSyntax { Identifier.Text: "var" })
                return IsTask(model.GetTypeInfo(node).Type) && SafeToDeferAt(vd);

            // Explicit declared type that is itself awaitable → keep the Task.
            return IsAwaitable(model.GetTypeInfo(decl.Type).Type);
        }

        // ─── Join helpers ────────────────────────────────────────────────

        /// <summary>Names of Task-bound locals that were deferred in <paramref name="m"/>.</summary>
        private List<string> DeferredTaskLocalNames(MethodDeclarationSyntax m)
        {
            var names = new List<string>();
            if (m.Body is null) return names;

            foreach (var local in m.Body.DescendantNodes().OfType<LocalDeclarationStatementSyntax>())
            {
                var decl = local.Declaration;
                var isVar = decl.Type is IdentifierNameSyntax { Identifier.Text: "var" };
                var explicitAwaitable = !isVar && IsAwaitable(model.GetTypeInfo(decl.Type).Type);

                foreach (var v in decl.Variables)
                {
                    if (v.Initializer is null) continue;
                    var initType = model.GetTypeInfo(v.Initializer.Value).Type;
                    // The join only covers TOP-LEVEL locals (joinable at the
                    // method's trailing exit): explicit Task locals that are
                    // top-level, or var Tasks that are safe to defer.
                    var deferred = explicitAwaitable
                        ? IsAwaitable(initType) && IsTopLevelInMethodBody(v)
                        : isVar && IsTask(initType) && SafeToDeferAt(v);
                    if (deferred) names.Add(v.Identifier.Text);
                }
            }
            return names;
        }

        private static HashSet<string> AwaitedIdentifierNames(SyntaxNode body)
        {
            var set = new HashSet<string>(StringComparer.Ordinal);
            foreach (var aw in body.DescendantNodes().OfType<AwaitExpressionSyntax>())
            {
                var e = aw.Expression is ParenthesizedExpressionSyntax p ? p.Expression : aw.Expression;
                if (e is IdentifierNameSyntax id) set.Add(id.Identifier.Text);
            }
            return set;
        }

        /// <summary>
        /// Path-aware structured join. Walks the method body's top-level
        /// statements tracking which deferred locals are live (declared so
        /// far); before every <c>return</c>/<c>throw</c> in a live local's
        /// range — at any nesting — inserts <c>await L;</c> (unless that exit
        /// already awaits L, or a prior join already does). At fall-through
        /// (no trailing exit) appends awaits for any still-unawaited locals.
        /// Since Spek emits braces on every control-flow body, every exit is a
        /// direct block statement, so awaits go in as siblings (no wrapping)
        /// and liveness is tracked by name (no fragile span arithmetic).
        /// </summary>
        private static bool InjectJoins(BlockSyntax body, HashSet<string> deferred, out BlockSyntax result)
        {
            var changed = false;
            var live    = new HashSet<string>(StringComparer.Ordinal);
            var outList = new List<StatementSyntax>(body.Statements.Count);

            for (var i = 0; i < body.Statements.Count; i++)
            {
                var stmt = body.Statements[i];

                if (stmt is ReturnStatementSyntax or ThrowStatementSyntax)
                {
                    AppendJoinsBefore(outList, body.Statements, i, live, ExitExpr(stmt), ref changed);
                    outList.Add(stmt);
                }
                else if (live.Count > 0 && ContainsExit(stmt))
                {
                    var sub = new SubJoiner(live);
                    var rewritten = (StatementSyntax)sub.Visit(stmt)!;
                    if (sub.Changed) changed = true;
                    outList.Add(rewritten);
                }
                else
                {
                    outList.Add(stmt);
                }

                if (stmt is LocalDeclarationStatementSyntax lds)
                    foreach (var v in lds.Declaration.Variables)
                        if (deferred.Contains(v.Identifier.Text)) live.Add(v.Identifier.Text);
            }

            // Fall-through: if control can run off the end, await anything still
            // pending (covers unused/abandoned deferred locals).
            var fallsThrough = outList.Count == 0 ||
                               outList[^1] is not (ReturnStatementSyntax or ThrowStatementSyntax);
            if (fallsThrough)
            {
                var awaited = AwaitedIdentifierNames(body.WithStatements(List(outList)));
                var indent  = outList.Count > 0 ? outList[^1].GetLeadingTrivia() : default;
                foreach (var n in live.OrderBy(x => x, StringComparer.Ordinal))
                    if (!awaited.Contains(n)) { outList.Add(AwaitStmt(n, indent)); changed = true; }
            }

            result = changed ? body.WithStatements(List(outList)) : body;
            return changed;
        }

        /// <summary>Inserts auto-await joins before exits inside a single
        /// top-level compound statement, with a fixed live set (no new
        /// top-level locals are declared within it). Does not descend into
        /// lambdas or local functions.</summary>
        private sealed class SubJoiner(HashSet<string> live) : CSharpSyntaxRewriter
        {
            public bool Changed { get; private set; }

            public override SyntaxNode? VisitBlock(BlockSyntax node)
            {
                var visited = (BlockSyntax)base.VisitBlock(node)!;
                var outList = new List<StatementSyntax>(visited.Statements.Count);
                var ch = false;
                for (var i = 0; i < visited.Statements.Count; i++)
                {
                    var stmt = visited.Statements[i];
                    if (stmt is ReturnStatementSyntax or ThrowStatementSyntax)
                        AppendJoinsBefore(outList, visited.Statements, i, live, ExitExpr(stmt), ref ch);
                    outList.Add(stmt);
                }
                if (!ch) return visited;
                Changed = true;
                return visited.WithStatements(List(outList));
            }

            // Returns inside a lambda / local function belong to it, not the
            // enclosing method — leave them alone.
            public override SyntaxNode? VisitSimpleLambdaExpression(SimpleLambdaExpressionSyntax node) => node;
            public override SyntaxNode? VisitParenthesizedLambdaExpression(ParenthesizedLambdaExpressionSyntax node) => node;
            public override SyntaxNode? VisitAnonymousMethodExpression(AnonymousMethodExpressionSyntax node) => node;
            public override SyntaxNode? VisitLocalFunctionStatement(LocalFunctionStatementSyntax node) => node;
        }

        private static void AppendJoinsBefore(
            List<StatementSyntax> outList,
            SyntaxList<StatementSyntax> blockStmts,
            int exitIndex,
            HashSet<string> live,
            ExpressionSyntax? exitExpr,
            ref bool changed)
        {
            var already = TrailingJoinNames(blockStmts, exitIndex);
            var indent  = blockStmts[exitIndex].GetLeadingTrivia();
            foreach (var n in live.OrderBy(x => x, StringComparer.Ordinal))
            {
                if (already.Contains(n)) continue;                       // a prior join already awaits it
                if (exitExpr is not null && ExprAwaits(exitExpr, n)) continue; // awaited in the exit itself
                outList.Add(AwaitStmt(n, indent));
                changed = true;
            }
        }

        private static ExpressionSyntax? ExitExpr(StatementSyntax exit) => exit switch
        {
            ReturnStatementSyntax r => r.Expression,
            ThrowStatementSyntax t  => t.Expression,
            _ => null,
        };

        private static ExpressionStatementSyntax AwaitStmt(string name, SyntaxTriviaList indent) =>
            ExpressionStatement(
                AwaitExpression(
                    Token(SyntaxKind.AwaitKeyword).WithTrailingTrivia(Space),
                    IdentifierName(name)))
            // Leading = the exit's indentation (whitespace); trailing newline so
            // the following statement lands on its own line (this emit style
            // keeps newlines as trailing trivia).
            .WithLeadingTrivia(indent)
            .WithTrailingTrivia(LineFeed);

        private static bool ExprAwaits(ExpressionSyntax expr, string name) =>
            expr.DescendantNodesAndSelf().OfType<AwaitExpressionSyntax>()
                .Any(aw => AwaitedName(aw) == name);

        /// <summary>Names awaited by the contiguous run of `await X;` statements
        /// immediately preceding <paramref name="exitIndex"/> — the prior-iteration
        /// join, so we don't duplicate it.</summary>
        private static HashSet<string> TrailingJoinNames(SyntaxList<StatementSyntax> stmts, int exitIndex)
        {
            var names = new HashSet<string>(StringComparer.Ordinal);
            for (var i = exitIndex - 1; i >= 0; i--)
            {
                if (stmts[i] is ExpressionStatementSyntax { Expression: AwaitExpressionSyntax aw }
                    && AwaitedName(aw) is { } n)
                    names.Add(n);
                else
                    break;
            }
            return names;
        }

        private static string? AwaitedName(AwaitExpressionSyntax aw)
        {
            var e = aw.Expression is ParenthesizedExpressionSyntax p ? p.Expression : aw.Expression;
            return (e as IdentifierNameSyntax)?.Identifier.Text;
        }

        private static bool ContainsExit(SyntaxNode stmt) =>
            stmt.DescendantNodesAndSelf(descendIntoChildren: c =>
                    c is not (
                        SimpleLambdaExpressionSyntax or
                        ParenthesizedLambdaExpressionSyntax or
                        AnonymousMethodExpressionSyntax or
                        LocalFunctionStatementSyntax))
                .Any(n => n is ReturnStatementSyntax or ThrowStatementSyntax);

        // ─── Shared classification ───────────────────────────────────────

        private static ExpressionSyntax WrapAwait(ExpressionSyntax visited, bool parens)
        {
            var awaited = AwaitExpression(
                Token(SyntaxKind.AwaitKeyword).WithTrailingTrivia(Space),
                visited.WithoutTrivia());
            ExpressionSyntax replacement = parens ? ParenthesizedExpression(awaited) : awaited;
            return replacement
                .WithLeadingTrivia(visited.GetLeadingTrivia())
                .WithTrailingTrivia(visited.GetTrailingTrivia());
        }

        // ─── Invisible cooperative cancellation ──────────────────────────────
        // When an auto-awaited call accepts a CancellationToken, thread the
        // actor's ShutdownToken into it — invisibly, in the emitted C# only.
        // Gated to actor-handler bodies (where `this` is a Spek.ActorBase, so
        // `this.ShutdownToken` resolves); module/static methods and non-actor
        // class methods are left alone.

        private readonly Dictionary<MethodDeclarationSyntax, bool> _actorCtx = new();

        // Actor context is detected SYNTACTICALLY, not semantically: the rewriter's
        // compilation doesn't reference Spek.Runtime (see BuildFrameworkReferences),
        // so `Spek.ActorBase` resolves as an error type and a symbol-based base-type
        // walk is unreliable. The emitter always writes `: Spek.ActorBase` on actor
        // classes, so checking the enclosing class's base list is robust. (The callee
        // CancellationToken resolution stays semantic — BCL types DO resolve.)
        private bool InActorContext(SyntaxNode node)
        {
            var method = node.FirstAncestorOrSelf<MethodDeclarationSyntax>();
            if (method is null || method.Modifiers.Any(SyntaxKind.StaticKeyword)) return false;
            if (_actorCtx.TryGetValue(method, out var cached)) return cached;
            var cls = method.FirstAncestorOrSelf<ClassDeclarationSyntax>();
            var ok = cls?.BaseList is { } bl
                     && bl.Types.Any(t => t.Type.ToString() is "Spek.ActorBase" or "ActorBase");
            _actorCtx[method] = ok;
            return ok;
        }

        private static bool IsCancellationToken(ITypeSymbol? t) =>
            t?.ToDisplayString() == "System.Threading.CancellationToken";

        /// <summary>
        /// The name of the CancellationToken parameter to fill for <paramref name="sym"/>
        /// given <paramref name="suppliedArgs"/> positional arguments, or null if the
        /// call can't take one. Two shapes: an unfilled optional CT parameter on the
        /// bound method (<c>File.ReadAllTextAsync(path)</c>), or a sibling overload
        /// that adds a trailing CT (<c>Task.Delay(int)</c> → <c>Task.Delay(int, CancellationToken)</c>).
        /// </summary>
        private static string? CancellationParamName(IMethodSymbol sym, int suppliedArgs)
        {
            for (int i = suppliedArgs; i < sym.Parameters.Length; i++)
                if (IsCancellationToken(sym.Parameters[i].Type))
                    return sym.Parameters[i].Name;          // optional CT omitted at the call

            var overloads = sym.ContainingType?.GetMembers(sym.Name).OfType<IMethodSymbol>();
            if (overloads is not null)
                foreach (var o in overloads)
                {
                    if (o.Parameters.Length != suppliedArgs + 1) continue;
                    if (!IsCancellationToken(o.Parameters[suppliedArgs].Type)) continue;
                    var match = true;
                    for (int i = 0; i < suppliedArgs && i < sym.Parameters.Length; i++)
                        if (!SymbolEqualityComparer.Default.Equals(o.Parameters[i].Type, sym.Parameters[i].Type))
                        { match = false; break; }
                    if (match) return o.Parameters[suppliedArgs].Name;   // sibling +CT overload
                }
            return null;
        }

        private static InvocationExpressionSyntax WithShutdownToken(
            InvocationExpressionSyntax inv, string paramName) =>
            inv.AddArgumentListArguments(
                Argument(
                    NameColon(IdentifierName(paramName),
                              Token(SyntaxKind.ColonToken).WithTrailingTrivia(Space)),
                    default,
                    ParseExpression("this.ShutdownToken"))
                .WithLeadingTrivia(Space));        // `, cancellationToken: this.ShutdownToken`

        /// <summary>Threads the actor's ShutdownToken into <paramref name="inv"/> when the
        /// call is in an actor body and the callee accepts a CancellationToken; else a no-op.</summary>
        private InvocationExpressionSyntax MaybeThreadToken(
            InvocationExpressionSyntax inv, IMethodSymbol? sym, int suppliedArgs, SyntaxNode ctx)
        {
            if (sym is null || !InActorContext(ctx)) return inv;
            var name = CancellationParamName(sym, suppliedArgs);
            return name is null ? inv : WithShutdownToken(inv, name);
        }

        private static bool IsWriteOrTaskPosition(IdentifierNameSyntax id)
        {
            // Assignment target (write, not a value-use).
            if (id.Parent is AssignmentExpressionSyntax asn && asn.Left == id) return true;
            // `out`/`ref`/`in` argument — passed by reference, not a value.
            if (id.Parent is ArgumentSyntax { RefKindKeyword.RawKind: not (int)SyntaxKind.None })
                return true;
            // Inside nameof(...) — the identifier isn't evaluated.
            foreach (var anc in id.Ancestors())
                if (anc is InvocationExpressionSyntax { Expression: IdentifierNameSyntax { Identifier.Text: "nameof" } })
                    return true;
            return false;
        }

        private bool AwaitContextAllowed(SyntaxNode node)
        {
            foreach (var anc in node.Ancestors())
            {
                switch (anc)
                {
                    case SimpleLambdaExpressionSyntax:
                    case ParenthesizedLambdaExpressionSyntax:
                    case AnonymousMethodExpressionSyntax:
                        // Awaiting inside a lambda is only safe when the
                        // lambda's converted delegate type already returns
                        // Task/ValueTask — then marking it `async` doesn't
                        // change its signature. A value-returning delegate
                        // (Func<int,int>) must stay synchronous.
                        return LambdaReturnsTask((ExpressionSyntax)anc);
                    case LocalFunctionStatementSyntax:
                    case ConstructorDeclarationSyntax:
                    case AccessorDeclarationSyntax:
                        return false;
                    case MethodDeclarationSyntax:
                        return true;
                }
            }
            return false;
        }

        /// <summary>True when an anonymous function's converted delegate type
        /// returns Task/ValueTask (e.g. <c>Func&lt;HttpContext, Func&lt;Task&gt;, Task&gt;</c>).</summary>
        private bool LambdaReturnsTask(ExpressionSyntax lambda)
        {
            var conv = model.GetTypeInfo(lambda).ConvertedType;
            return conv is INamedTypeSymbol { DelegateInvokeMethod: { } invoke }
                && IsAwaitable(invoke.ReturnType);
        }

        /// <summary>True when the nearest enclosing callable around
        /// <paramref name="node"/> is a lambda / anonymous method (rather than
        /// a method or local function).</summary>
        private static bool EnclosingCallableIsLambda(SyntaxNode node)
        {
            foreach (var anc in node.Ancestors())
                switch (anc)
                {
                    case SimpleLambdaExpressionSyntax:
                    case ParenthesizedLambdaExpressionSyntax:
                    case AnonymousMethodExpressionSyntax:
                        return true;
                    case LocalFunctionStatementSyntax:
                    case MethodDeclarationSyntax:
                    case ConstructorDeclarationSyntax:
                    case AccessorDeclarationSyntax:
                        return false;
                }
            return false;
        }

        private static bool UsedAsTaskViaMember(ExpressionSyntax node, ITypeSymbol taskType)
        {
            if (node.Parent is not MemberAccessExpressionSyntax ma || ma.Expression != node)
                return false;
            var name = ma.Name.Identifier.Text;
            for (var t = taskType; t is not null; t = t.BaseType)
                if (t.GetMembers(name).Any())
                    return true;
            return false;
        }

        private static bool NeedsParens(ExpressionSyntax node)
        {
            return node.Parent switch
            {
                MemberAccessExpressionSyntax m      => m.Expression == node,
                ElementAccessExpressionSyntax e     => e.Expression == node,
                ConditionalAccessExpressionSyntax c => c.Expression == node,
                PostfixUnaryExpressionSyntax        => true,
                MemberBindingExpressionSyntax       => true,
                _ => false,
            };
        }

        /// <summary>
        /// A `var`-bound Task local is deferrable when it is declared at the
        /// method body's top level. The path-aware join then awaits it before
        /// every exit in its live range (guard-clause returns *before* the
        /// declaration don't count — it isn't in scope there). Loop bodies and
        /// nested blocks aren't top-level, so locals there fall back to eager
        /// await (always safe). Pure syntax — no model needed.
        /// </summary>
        private static bool SafeToDeferAt(VariableDeclaratorSyntax vd) =>
            IsTopLevelInMethodBody(vd);

        private static bool IsTopLevelInMethodBody(VariableDeclaratorSyntax vd) =>
            IsTopLevelInMethodBody(vd, out _, out _);

        private static bool IsTopLevelInMethodBody(
            VariableDeclaratorSyntax vd,
            out MethodDeclarationSyntax method,
            out LocalDeclarationStatementSyntax lds)
        {
            method = null!;
            lds = null!;
            if (vd.Parent is not VariableDeclarationSyntax vds) return false;
            if (vds.Parent is not LocalDeclarationStatementSyntax l) return false;
            if (l.Parent is not BlockSyntax block) return false;
            if (block.Parent is not MethodDeclarationSyntax m || m.Body != block) return false;
            method = m;
            lds = l;
            return true;
        }

        private static TypeSyntax MakeAsyncReturnType(TypeSyntax ret)
        {
            var lead = ret.GetLeadingTrivia();
            var trail = ret.GetTrailingTrivia();

            TypeSyntax result =
                ret is PredefinedTypeSyntax p && p.Keyword.IsKind(SyntaxKind.VoidKeyword)
                    ? ParseTypeName("System.Threading.Tasks.Task")
                // A method the author already declared Task/ValueTask-returning
                // (Task, Task<T>, ValueTask, ValueTask<T>) is ALREADY async-shaped:
                // adding `async` keeps the same return type. Wrapping it again gave
                // `Task<Task<int>>` and broke the body's `return` (red-team emit-C).
                : IsTaskLikeSyntax(ret)
                    ? ret.WithoutTrivia()
                    : ParseTypeName($"System.Threading.Tasks.Task<{ret.WithoutTrivia()}>");

            return result.WithLeadingTrivia(lead).WithTrailingTrivia(trail);
        }

        // Syntactic Task/ValueTask test for the DECLARED return type (no
        // semantic model here). Matches `Task`, `Task<…>`, `ValueTask`,
        // `ValueTask<…>`, bare or namespace-qualified.
        private static bool IsTaskLikeSyntax(TypeSyntax ret)
        {
            var name = ret switch
            {
                QualifiedNameSyntax q => (NameSyntax)q.Right,
                NameSyntax n => n,
                _ => null,
            };
            var id = name switch
            {
                GenericNameSyntax g => g.Identifier.ValueText,
                IdentifierNameSyntax i => i.Identifier.ValueText,
                _ => null,
            };
            return id is "Task" or "ValueTask";
        }

        private static bool ContainsDirectAwait(SyntaxNode body) =>
            body.DescendantNodes(descendIntoChildren: child =>
                    child is not (
                        SimpleLambdaExpressionSyntax or
                        ParenthesizedLambdaExpressionSyntax or
                        AnonymousMethodExpressionSyntax or
                        LocalFunctionStatementSyntax))
                .Any(n => n is AwaitExpressionSyntax);

        private static bool IsAwaitable([NotNullWhen(true)] ITypeSymbol? type)
        {
            if (type is not INamedTypeSymbol named) return false;
            var def = named.OriginalDefinition;
            if (def.ContainingNamespace?.ToDisplayString() != "System.Threading.Tasks")
                return false;
            return def.Name is "Task" or "ValueTask";
        }

        private static bool IsTask(ITypeSymbol? type)
        {
            if (type is not INamedTypeSymbol named) return false;
            var def = named.OriginalDefinition;
            return def.ContainingNamespace?.ToDisplayString() == "System.Threading.Tasks"
                && def.Name == "Task";
        }

        /// <summary>
        /// True for a NON-generic <c>Task</c>/<c>ValueTask</c> — i.e. an
        /// awaitable whose <c>await</c> yields <c>void</c>. Used to spot a
        /// <c>var</c> binding that would become <c>var x = await &lt;void&gt;</c>
        /// (CS0815). <c>Task&lt;T&gt;</c>/<c>ValueTask&lt;T&gt;</c> await to a
        /// value, so they return false.
        /// </summary>
        private static bool IsVoidAwaitable(ITypeSymbol? type) =>
            IsAwaitable(type) && type is INamedTypeSymbol { IsGenericType: false };
    }
}
