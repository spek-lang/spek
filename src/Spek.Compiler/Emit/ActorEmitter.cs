using Spek.Compiler.AST;
using Spek.Compiler.Semantic;

namespace Spek.Compiler.Emit;

/// <summary>
/// Emits a C# class for a Spek actor declaration.
///
/// Mapping summary (from CONTEXT.md):
///   actor Foo               → internal sealed class Foo : ActorBase
///   public actor Foo        → public sealed class Foo : ActorBase
///   abstract actor Foo      → public abstract class Foo : ActorBase
///   field balance           → private decimal _balance
///   init(args) { }          → public Foo(args) constructor
///   behavior NormalOp { }   → async Task NormalOp_HandleAsync(object _msg, ActorRef _sender)
///   become NormalOp         → _behavior = NormalOp_HandleAsync
///   on Deposit d => { }     → case Deposit d: { ... } break;
///   on PreStart => ...       → protected override void OnPreStart()
///   on Restore(Snapshot s)  → protected override void OnRestore(Snapshot s)
///   persist;                 → await PersistAsync()
///   self                    → _selfRef
///   sender                  → _currentSender
/// </summary>
public sealed class ActorEmitter
{
    private readonly CSharpWriter _w;
    private readonly SymbolTable? _symbols;

    public ActorEmitter(CSharpWriter writer, SymbolTable? symbols = null)
    {
        _w = writer;
        _symbols = symbols;
    }

    public void Emit(ActorDecl actor)
    {
        // Collect field names so the expression emitter can prefix them with _.
        // Include a base actor's protected/public fields so a derived actor's
        // handlers can reuse inherited state (the `_`-rename applies to them too).
        var fieldNames = actor.Members
            .OfType<FieldDecl>()
            .Select(f => f.Name)
            .ToHashSet();
        AddInheritedFieldNames(actor, fieldNames);

        var exprEmitter = new ExpressionEmitter(fieldNames, _symbols, inActor: true);
        var stmtEmitter = new StatementEmitter(_w, exprEmitter);

        EmitClassHeader(actor);
        _w.Line("{");
        _w.Indent();

        EmitFields(actor, fieldNames);
        EmitBehaviorDelegate(actor);
        EmitEventInfrastructure(actor);
        EmitUseRegions(actor);
        EmitConstructor(actor, fieldNames, stmtEmitter);
        EmitDispatch(actor);
        EmitBehaviors(actor, fieldNames, stmtEmitter);
        EmitMethods(actor, stmtEmitter);
        EmitLifecycleHooks(actor, stmtEmitter);
        EmitCaptureFields(actor, fieldNames);
        EmitAutoRestore(actor, fieldNames);
        EmitPassivationTimeout(actor, exprEmitter);
        EmitSuperviseOverrides(actor, exprEmitter);

        _w.Dedent();
        _w.Line("}");
        _w.Line();
    }

    // ─── Class header ─────────────────────────────────────────────────────────

    private void EmitClassHeader(ActorDecl actor)
    {
        var vis = actor.Visibility switch
        {
            Visibility.Public    => "public",
            Visibility.Internal  => "internal",
            Visibility.Protected => "protected",
            _                    => "internal"
        };

        var typeParams = actor.TypeParameters.Count > 0
            ? $"<{string.Join(", ", actor.TypeParameters.Select(p => p.Name))}>"
            : "";

        // Disambiguate the colon list: the parser puts the first name
        // in `BaseActor` and the rest in `ImplementedChannels`, but
        // either slot might actually be a channel — only the symbol
        // table knows. Resolve each name and split into (base actor,
        // channel list).
        string? actorBaseName = null;
        var channelNames = new List<string>();

        var colonNames = new List<QualifiedName>();
        if (actor.BaseActor is not null) colonNames.Add(actor.BaseActor);
        colonNames.AddRange(actor.ImplementedChannels);

        foreach (var name in colonNames)
        {
            if (_symbols.ResolveChannel(name) is not null)
                channelNames.Add(name.ToString());
            else if (_symbols.ResolveActor(name) is not null && actorBaseName is null)
                actorBaseName = name.ToString();
            // else: unresolved — the semantic analyzer surfaces CE0091.
            // Fall through; treat as a base-actor candidate to avoid
            // emitting bogus inheritance from a non-existent type.
            else if (actorBaseName is null)
                actorBaseName = name.ToString();
        }

        // Build the bases list: base class first, then any implemented
        // channels, then IAsyncDisposable when there's a term
        // block.
        var basesList = new List<string> { actorBaseName ?? "Spek.ActorBase" };

        // Actors implementing channels emit `: ChannelName` so
        // hosting adapters (REST, gRPC) can discover the channel
        // through reflection on the actor's interface list. The
        // channel itself is emitted as a marker interface decorated
        // with [SpekChannelMetadata].
        basesList.AddRange(channelNames);

        // Actors with a `term { }` block also implement
        // IAsyncDisposable so external consumers can use `await using`.
        // Internal actor lifecycle is driven by ActorBase.OnTerm; the
        // IAsyncDisposable surface is for C# code that holds an actor
        // class directly (uncommon but supported).
        if (actor.Members.OfType<TermBlock>().Any())
            basesList.Add("System.IAsyncDisposable");

        var bases = string.Join(", ", basesList);

        var where = ExpressionEmitter.FormatWhereClauses(actor.WhereClauses);
        if (actor.IsAbstract)
            _w.Line($"public abstract class {actor.Name}{typeParams} : {bases}{where}");
        else
            _w.Line($"{vis} sealed class {actor.Name}{typeParams} : {bases}{where}");
    }

    // ─── Fields ───────────────────────────────────────────────────────────────

    private void EmitFields(ActorDecl actor, HashSet<string> fieldNames)
    {
        foreach (var field in actor.Members.OfType<FieldDecl>())
        {
            // Honor `protected`/`internal` so an abstract base actor can share
            // state with its derived actors (reuse); the default stays private.
            var vis = field.Visibility switch
            {
                Visibility.Public    => "public",
                Visibility.Protected => "protected",
                Visibility.Internal  => "internal",
                _                    => "private",
            };
            var init = field.Initializer != null
                ? $" = {new ExpressionEmitter(fieldNames).Emit(field.Initializer)}"
                : $" = default!";
            _w.Line($"{vis} {field.Type} _{field.Name}{init};");
        }
        _w.Line();
    }

    // ─── Behavior delegate field ──────────────────────────────────────────────

    private void EmitBehaviorDelegate(ActorDecl actor)
    {
        if (!actor.Members.OfType<BehaviorDecl>().Any()) return;
        _w.Line("private Func<object, Spek.ActorRef, Task> _behavior = null!;");
        // Parallel classifier delegate. Swapped alongside `_behavior`
        // on `become` so the slot's dispatch loop knows which arm a given
        // message would hit (reader vs writer) in the current behavior.
        _w.Line("private Func<object, bool> _isReaderClassifier = _ => false;");
        _w.Line();
    }

    // ─── Constructor (from init block) ────────────────────────────────────────

    private void EmitConstructor(ActorDecl actor, HashSet<string> fieldNames, StatementEmitter stmtEmitter)
    {
        var init = actor.Members.OfType<InitBlock>().FirstOrDefault();
        var firstBehavior = actor.Members.OfType<BehaviorDecl>().FirstOrDefault();

        // If the actor has no behaviors at all, there's nothing to
        // dispatch — skip the constructor too.
        if (firstBehavior is null) return;

        var parms = init is null
            ? ""
            : string.Join(", ", init.Parameters.Select(p => $"{p.Type} {p.Name}"));

        // `init(...) : base(args)` chains to an abstract base actor's
        // parameterized constructor — same emit as the class side.
        var baseCall = init?.BaseArgs is { } ba
            ? $" : base({string.Join(", ", ba.Select(a => new ExpressionEmitter(fieldNames, _symbols, inActor: true).Emit(a)))})"
            : "";

        _w.Line($"public {actor.Name}({parms}){baseCall}");
        _w.Line("{");
        _w.Indent();

        // Implicit-entry-point: default `_behavior` to the first declared
        // behavior. Matches CE0014's reachability rule. If the user's
        // `init` block ends with an explicit `become X;`, that overrides
        // this assignment — emitted immediately below.
        _w.Line($"_behavior = {firstBehavior.Name}_HandleAsync;");
        // Parallel classifier swap.
        _w.Line($"_isReaderClassifier = {firstBehavior.Name}_IsReaderMessage;");

        if (init is not null)
        {
            stmtEmitter.SetShadowed(init.Parameters.Select(p => p.Name));   // init params shadow same-named fields
            foreach (var stmt in init.Body.Statements)
                stmtEmitter.EmitStatement(stmt);
        }

        _w.Dedent();
        _w.Line("}");
        _w.Line();
    }

    // ─── DispatchAsync override ───────────────────────────────────────────────

    private void EmitDispatch(ActorDecl actor)
    {
        if (!actor.Members.OfType<BehaviorDecl>().Any()) return;

        _w.Line("protected override async Task DispatchAsync(object message, Spek.ActorRef sender)");
        _w.Line("{");
        _w.Indent();
        _w.Line("_currentSender = sender;");
        _w.Line("await _behavior(message, sender);");
        _w.Dedent();
        _w.Line("}");
        _w.Line();
    }

    // ─── Behavior handler methods ─────────────────────────────────────────────

    private void EmitBehaviors(ActorDecl actor, HashSet<string> fieldNames, StatementEmitter stmtEmitter)
    {
        var uses = actor.Members.OfType<UseDecl>().ToList();

        // Collect all stream-shaped handlers across behaviors so
        // the actor-level scaffolding (operator fields, lazy-init
        // helpers, synthetic body-trigger records) can be emitted
        // alongside the dispatch logic.
        var streamHandlers = new List<(BehaviorDecl Behavior, OnHandler Handler)>();

        foreach (var behavior in actor.Members.OfType<BehaviorDecl>())
        {
            _w.Line($"private async Task {behavior.Name}_HandleAsync(object _msg, Spek.ActorRef _sender)");
            _w.Line("{");
            _w.Indent();

            if (behavior.Handlers.Count > 0)
            {
                _w.Line("switch (_msg)");
                _w.Line("{");
                _w.Indent();

                foreach (var handler in behavior.Handlers)
                {
                    if (handler.StreamOperators is { Count: > 0 })
                    {
                        EmitStreamShapedDispatchArm(behavior, handler, stmtEmitter, uses);
                        streamHandlers.Add((behavior, handler));
                    }
                    else
                    {
                        EmitOnHandler(handler, stmtEmitter, uses);
                    }
                }

                // Emit the synthetic body-trigger arms after the
                // regular ones so a self-Tell of the trigger record from
                // a stream operator runs the user's body under the actor
                // lock.
                foreach (var handler in behavior.Handlers)
                {
                    if (handler.StreamOperators is { Count: > 0 })
                        EmitStreamBodyTriggerArm(behavior, handler, stmtEmitter, uses);
                }

                // If the user didn't declare a catch-all, route unmatched
                // messages to the base class's Unhandled hook (which forwards
                // to the actor system's dead-letter sink by default).
                if (!behavior.Handlers.Any(h => h.Pattern is CatchAllPattern))
                {
                    _w.Line("default:");
                    _w.Indent();
                    _w.Line("Unhandled(_msg);");
                    _w.Line("break;");
                    _w.Dedent();
                }

                _w.Dedent();
                _w.Line("}");
            }

            _w.Dedent();
            _w.Line("}");
            _w.Line();

            // Per-behavior reader/writer classifier. The slot's
            // dispatch loop calls this (via the swappable
            // _isReaderClassifier delegate) to decide reader vs writer
            // dispatch for each message. Default arms (no explicit
            // mode) are writers; only handlers that wrote `reader on X`
            // appear in this method's true-cases.
            EmitReaderClassifier(behavior);
        }

        // Override ClassifyAsReader on ActorBase to defer to the
        // currently-active classifier delegate. The base is
        // `protected internal`; cross-assembly overrides see only
        // the `protected` half, which is the modifier we emit.
        if (actor.Members.OfType<BehaviorDecl>().Any())
        {
            _w.Line("protected override bool ClassifyAsReader(object message)");
            _w.Line("    => _isReaderClassifier(message);");
            _w.Line();
        }

        // Actor-level scaffolding for stream-shaped handlers:
        // synthetic body-trigger records, operator instance fields,
        // and lazy-init helpers.
        foreach (var (behavior, handler) in streamHandlers)
            EmitStreamHandlerScaffolding(behavior, handler);
    }

    // ─── Stream-shaped handlers ──────────────────────────────────────────────

    /// <summary>
    /// Returns the unique stream-handler name for a (behavior,
    /// handler) pair, e.g. `Default_Foo` or `Idle_FileChanged`.
    /// Used to name the operator field, the synthetic body-trigger
    /// record, and the lazy-init helper.
    /// </summary>
    private static string StreamHandlerKey(BehaviorDecl behavior, OnHandler handler)
    {
        var suffix = handler.Pattern switch
        {
            NamedBindPattern p => p.MessageType.Simple,
            NoBindPattern p    => p.MessageType.Simple,
            EventPattern ev    => ev.HandlerName,
            _                  => throw new InvalidOperationException(
                                    "Stream operators not supported on this handler pattern; " +
                                    "compiler should have rejected earlier."),
        };
        return $"{behavior.Name}_{suffix}";
    }

    /// <summary>
    /// Returns the C# type name the operator chain operates on.
    /// For event handlers this is the synthetic <c>__Event_Name</c>
    /// record; for everything else it's the user's message type.
    /// </summary>
    private static string StreamMessageTypeName(OnHandler handler) => handler.Pattern switch
    {
        NamedBindPattern p => p.MessageType.ToString(),
        NoBindPattern p    => p.MessageType.ToString(),
        EventPattern ev    => $"__Event_{ev.HandlerName}",
        _                  => throw new InvalidOperationException(
                                "Stream operators not supported on this handler pattern."),
    };

    /// <summary>
    /// Emits the stream-shaped variant of a handler's dispatch arm:
    /// the case routes the message into the operator chain via
    /// <c>OfferAsync</c>; the actual body runs in the synthetic
    /// body-trigger arm emitted separately.
    /// </summary>
    private void EmitStreamShapedDispatchArm(
        BehaviorDecl behavior, OnHandler handler,
        StatementEmitter stmtEmitter, IReadOnlyList<UseDecl> uses)
    {
        var caseLabel = handler.Pattern switch
        {
            NamedBindPattern p => $"case {p.MessageType} _streamMsg_{p.Binding}:",
            NoBindPattern p    => $"case {p.MessageType} _streamMsg:",
            EventPattern ev    => $"case __Event_{ev.HandlerName} _streamMsg:",
            _                  => throw new InvalidOperationException(
                                    "Stream operators on catch-all not supported."),
        };

        var key = StreamHandlerKey(behavior, handler);
        var bindingExpr = handler.Pattern switch
        {
            NamedBindPattern p => $"_streamMsg_{p.Binding}",
            NoBindPattern _    => "_streamMsg",
            EventPattern _     => "_streamMsg",
            _                  => throw new InvalidOperationException(),
        };

        _w.Line(caseLabel);
        _w.Line("{");
        _w.Indent();
        _w.Line($"await Ops_{key}().OfferAsync({bindingExpr}).ConfigureAwait(false);");
        _w.Line("break;");
        _w.Dedent();
        _w.Line("}");
    }

    /// <summary>
    /// Emits the synthetic body-trigger arm — the dispatch case for
    /// the private nested record that the operator chain Tells back
    /// to self when ready to fire the user's body. Runs the body
    /// under the actor lock with the original binding restored.
    /// </summary>
    private void EmitStreamBodyTriggerArm(
        BehaviorDecl behavior, OnHandler handler,
        StatementEmitter stmtEmitter, IReadOnlyList<UseDecl> uses)
    {
        var key = StreamHandlerKey(behavior, handler);
        _w.Line($"case __FireBody_{key} __trig:");
        _w.Line("{");
        _w.Indent();

        // Restore the user's original binding from the trigger record.
        switch (handler.Pattern)
        {
            case NamedBindPattern np:
                _w.Line($"var {np.Binding} = __trig.Inner;");
                break;
            case EventPattern ev:
                _w.Line($"var __ev = __trig.Inner;");
                foreach (var p in ev.Parameters)
                    _w.Line($"var {p.Name} = __ev.{p.Name};");
                break;
            // NoBindPattern: no binding to restore.
        }

        var wasInsideOn = stmtEmitter.InsideOnHandler;
        stmtEmitter.InsideOnHandler = true;
        try
        {
            EmitBodyWithRegionLocks(handler, uses, stmtEmitter);
        }
        finally
        {
            stmtEmitter.InsideOnHandler = wasInsideOn;
        }

        _w.Line("break;");
        _w.Dedent();
        _w.Line("}");
    }

    /// <summary>
    /// Emits the actor-class members supporting one stream-shaped
    /// handler: a private nested record for the body-trigger
    /// message, a backing-field for the operator instance, a
    /// lazy-getter that initialises the chain on first access, and
    /// a private builder method that constructs and wires the chain.
    /// </summary>
    private void EmitStreamHandlerScaffolding(BehaviorDecl behavior, OnHandler handler)
    {
        var key       = StreamHandlerKey(behavior, handler);
        var msgType   = StreamMessageTypeName(handler);
        var operators = handler.StreamOperators!;

        // Synthetic body-trigger record. Carries the original message
        // (or synthetic event record) so the body-trigger arm can
        // restore the user's binding.
        _w.Line($"private sealed record __FireBody_{key}({msgType} Inner);");
        _w.Line();

        // Backing field + lazy getter.
        _w.Line($"private Spek.Streams.StreamOperator<{msgType}>? __ops_{key};");
        _w.Line($"private Spek.Streams.StreamOperator<{msgType}> Ops_{key}()");
        _w.Line($"    => __ops_{key} ??= BuildOps_{key}();");
        _w.Line();

        // Builder method — constructs the chain, wires the final
        // dispatch to a self-Tell of the body-trigger record.
        _w.Line($"private Spek.Streams.StreamOperator<{msgType}> BuildOps_{key}()");
        _w.Line("{");
        _w.Indent();

        // Each chain step is emitted as a Spek expression. For bare-name
        // factory calls (the common case — `debounce(500)`, `throttle(16)`),
        // the compiler injects an explicit `<T>` type argument so the call
        // resolves without leaning on target-typed inference. Multiple
        // steps wrap in `compose<T>(...)`; a single step is assigned
        // directly so C# can pick the correct `StreamOperator<T>` at the
        // declaration site.
        var exprEmitter = new ExpressionEmitter(new HashSet<string>(), _symbols);
        if (operators.Count == 1)
        {
            _w.Line($"Spek.Streams.StreamOperator<{msgType}> op = {EmitChainStep(operators[0], msgType, exprEmitter)};");
        }
        else
        {
            _w.Line($"var op = Spek.Streams.StreamOperators.compose<{msgType}>(");
            _w.Indent();
            for (int i = 0; i < operators.Count; i++)
            {
                var sep = i < operators.Count - 1 ? "," : "";
                _w.Line($"{EmitChainStep(operators[i], msgType, exprEmitter)}{sep}");
            }
            _w.Dedent();
            _w.Line(");");
        }

        // Wire dispatch: when the chain emits, post a body-trigger
        // self-Tell so the body runs through the mailbox under the
        // actor lock.
        _w.Line("op.Configure(msg =>");
        _w.Line("{");
        _w.Indent();
        _w.Line($"_selfRef.Tell(new __FireBody_{key}(msg));");
        _w.Line("return System.Threading.Tasks.Task.CompletedTask;");
        _w.Dedent();
        _w.Line("});");
        _w.Line("return op;");

        _w.Dedent();
        _w.Line("}");
        _w.Line();
    }

    // Emit a single chain step. For bare-name factory calls
    // (`debounce(500)`), inject `<T>` so the call resolves without
    // relying on target-typed inference through compose<T>(...) or
    // assignment context. Other expression shapes (already-typed calls,
    // method calls on a receiver) emit verbatim.
    private static string EmitChainStep(Expr step, string msgType, ExpressionEmitter ee)
    {
        if (step is InvocationExpr inv)
        {
            var args = string.Join(", ", inv.Args.Select(ee.Emit));
            return $"{inv.Callee}<{msgType}>({args})";
        }
        return ee.Emit(step);
    }

    private void EmitReaderClassifier(BehaviorDecl behavior)
    {
        _w.Line($"private bool {behavior.Name}_IsReaderMessage(object _msg)");
        _w.Line("{");
        _w.Indent();

        var readerHandlers = behavior.Handlers
            .Where(h => h.Mode == HandlerMode.Reader)
            .ToList();

        if (readerHandlers.Count == 0)
        {
            // No reader arms in this behavior — fast path.
            _w.Line("return false;");
        }
        else
        {
            _w.Line("return _msg switch");
            _w.Line("{");
            _w.Indent();
            foreach (var handler in readerHandlers)
            {
                var msgType = handler.Pattern switch
                {
                    NamedBindPattern p => p.MessageType.ToString(),
                    NoBindPattern p    => p.MessageType.ToString(),
                    _                  => null,   // catch-all: no specific type
                };
                if (msgType is null) continue;
                _w.Line($"{msgType} => true,");
            }
            _w.Line("_ => false,");
            _w.Dedent();
            _w.Line("};");
        }

        _w.Dedent();
        _w.Line("}");
        _w.Line();
    }

    // ─── Shared-region attachments (`use X foo;`) ────────────────────────────
    //
    // For each `use X foo;` in the actor body, emit a lazy-initialised
    // property accessor on the actor class. The backing field is null
    // until first access; first access calls the inherited
    // `GetSharedRegion<X>()` helper on `ActorBase`, which routes to the
    // per-`ActorSystem` registry. After the first access, subsequent
    // accesses are O(1) and lock-free (the backing field caches it).
    //
    // Initialising lazily (rather than in the constructor) avoids the
    // chicken-and-egg with `_system` — that field is set during
    // `Initialize`, not when the constructor runs, so a constructor-
    // time fetch would NRE on systems running plain `new MyActor(...)`.

    private void EmitUseRegions(ActorDecl actor)
    {
        var uses = actor.Members.OfType<UseDecl>().ToList();
        if (uses.Count == 0) return;

        foreach (var use in uses)
        {
            _w.Line($"private {use.RegionType}? __region_{use.LocalName};");
            _w.Line($"private {use.RegionType} {use.LocalName}");
            _w.Line($"    => __region_{use.LocalName} ??= GetSharedRegion<{use.RegionType}>();");
            _w.Line();
        }
    }

    // ─── Event-handler infrastructure ─────────────────────────────────────────
    //
    // For each `on event Name(params) => body` handler in the actor, emit:
    //   1. A nested private record carrying the params (the synthetic
    //      message that gets `Tell`ed when the event fires).
    //   2. A bridge method on the actor class with the user's chosen
    //      name and the verbatim delegate signature. Body is a single
    //      `_selfRef.Tell(new __Event_Name(args))`. User code wires it
    //      via plain C# method-group conversion: `source.Event += Name;`
    //
    // CE0013 (semantic) catches duplicate names, so dedup here is just
    // a defensive guard.

    private void EmitEventInfrastructure(ActorDecl actor)
    {
        var eventHandlers = actor.Members
            .OfType<BehaviorDecl>()
            .SelectMany(b => b.Handlers)
            .Where(h => h.Pattern is EventPattern)
            .Select(h => (Handler: h, Pattern: (EventPattern)h.Pattern))
            .ToList();

        if (eventHandlers.Count == 0) return;

        var seen = new HashSet<string>();
        foreach (var (handler, pattern) in eventHandlers)
        {
            if (!seen.Add(pattern.HandlerName)) continue;

            var paramSig = string.Join(", ",
                pattern.Parameters.Select(p => $"{p.Type} {p.Name}"));
            var paramArgs = string.Join(", ",
                pattern.Parameters.Select(p => p.Name));

            // Synthetic message record (private nested type — actor-scoped).
            _w.Line($"private sealed record __Event_{pattern.HandlerName}({paramSig});");
            _w.Line();

            // Bridge method. Visibility mirrors the on-handler — public
            // handlers expose the bridge as part of the actor's API
            // surface (callable across assemblies); private handlers
            // hide it (only init/this can wire `+=`).
            var visibility = handler.Visibility switch
            {
                Visibility.Public    => "public",
                Visibility.Internal  => "internal",
                Visibility.Protected => "protected",
                _                    => "private"
            };

            _w.Line($"{visibility} void {pattern.HandlerName}({paramSig})");
            _w.Line($"    => _selfRef.Tell(new __Event_{pattern.HandlerName}({paramArgs}));");
            _w.Line();
        }
    }

    private void EmitOnHandler(OnHandler handler, StatementEmitter stmtEmitter, IReadOnlyList<UseDecl> uses)
    {
        var caseLabel = handler.Pattern switch
        {
            NamedBindPattern p => $"case {p.MessageType} {p.Binding}:",
            NoBindPattern p    => $"case {p.MessageType} _:",
            CatchAllPattern    => "default:",
            EventPattern ev    => $"case __Event_{ev.HandlerName} __ev:",
            _                  => "default:"
        };

        _w.Line(caseLabel);
        _w.Line("{");
        _w.Indent();

        // Bind the catch-all variable before the handler body
        if (handler.Pattern is CatchAllPattern cp)
            _w.Line($"var {cp.Binding} = _msg;");

        // Unbox the synthetic event message back into the
        // user's declared parameter names so the body sees them
        // as locals (e.g., `s`, `e` for an EventHandler<T> shape).
        if (handler.Pattern is EventPattern eb)
        {
            foreach (var p in eb.Parameters)
                _w.Line($"var {p.Name} = __ev.{p.Name};");
        }

        // `private on T` handlers are reachable only when the
        // sender is the actor itself (i.e., via self.Tell). Anything
        // else targeting a private handler is dead-lettered so the
        // visibility marker is a real runtime contract, not just a
        // documentation hint.
        if (handler.Visibility == Visibility.Private)
        {
            var msgLabel = handler.Pattern switch
            {
                NamedBindPattern p => p.MessageType.ToString(),
                NoBindPattern p    => p.MessageType.ToString(),
                CatchAllPattern    => "<catch-all>",
                EventPattern ep    => $"event {ep.HandlerName}",
                _                  => "<unknown>",
            };
            _w.Line($"if (!ReferenceEquals(_sender, _selfRef))");
            _w.Line("{");
            _w.Indent();
            _w.Line($"ToDeadLetter(_msg, \"private handler '{msgLabel}' is reachable only via self.Tell\");");
            _w.Line("break;");
            _w.Dedent();
            _w.Line("}");
        }

        // Option D: inside a behavior on-handler, `return expr;` is the
        // reply idiom — StatementEmitter routes it to _currentSender.Tell.
        var wasInsideOn = stmtEmitter.InsideOnHandler;
        stmtEmitter.InsideOnHandler = true;
        try
        {
            EmitBodyWithRegionLocks(handler, uses, stmtEmitter);
        }
        finally
        {
            stmtEmitter.InsideOnHandler = wasInsideOn;
        }

        _w.Line("break;");
        _w.Dedent();
        _w.Line("}");
    }

    /// <summary>
    /// Wrap the handler body in nested try/finally pairs that
    /// acquire/release the per-region RW lock for each `use X foo;`
    /// the actor declared. Reader handlers acquire reader locks;
    /// writer handlers (the default, including catch-all and event
    /// handlers) acquire writer locks. Lock acquisition order matches
    /// declaration order in the actor body — users wanting deadlock-
    /// safe multi-region access should declare attachments in a
    /// consistent global order across actors. Phase 1 always acquires
    /// (no body-scan optimisation); over-locking is preferred to
    /// missing a lock.
    /// </summary>
    // Names a handler pattern binds into the body (the message binding or event params),
    // which shadow same-named actor fields.
    private static IEnumerable<string> HandlerBindingNames(MessagePattern pattern) => pattern switch
    {
        NamedBindPattern p => new[] { p.Binding },
        CatchAllPattern cp => new[] { cp.Binding },
        EventPattern ev    => ev.Parameters.Select(p => p.Name),
        _                  => Array.Empty<string>(),
    };

    private void EmitBodyWithRegionLocks(
        OnHandler handler,
        IReadOnlyList<UseDecl> uses,
        StatementEmitter stmtEmitter)
    {
        // The message binding (`on Msg binding`) and event params shadow same-named fields.
        stmtEmitter.SetShadowed(HandlerBindingNames(handler.Pattern));

        if (uses.Count == 0)
        {
            EmitHandlerBody(handler.Body, stmtEmitter);
            return;
        }

        var (enter, exit) = handler.Mode == HandlerMode.Reader
            ? ("EnterReaderAsync", "ExitReader")
            : ("EnterWriterAsync", "ExitWriter");

        EmitNestedRegionLocks(handler, uses, 0, enter, exit, stmtEmitter);
    }

    private void EmitNestedRegionLocks(
        OnHandler handler,
        IReadOnlyList<UseDecl> uses,
        int index,
        string enterMethod,
        string exitMethod,
        StatementEmitter stmtEmitter)
    {
        if (index >= uses.Count)
        {
            EmitHandlerBody(handler.Body, stmtEmitter);
            return;
        }

        var local = uses[index].LocalName;
        _w.Line($"await {local}.{enterMethod}();");
        _w.Line("try");
        _w.Line("{");
        _w.Indent();

        EmitNestedRegionLocks(handler, uses, index + 1, enterMethod, exitMethod, stmtEmitter);

        _w.Dedent();
        _w.Line("}");
        _w.Line("finally");
        _w.Line("{");
        _w.Indent();
        _w.Line($"{local}.{exitMethod}();");
        _w.Dedent();
        _w.Line("}");
    }

    private void EmitHandlerBody(HandlerBody body, StatementEmitter stmtEmitter)
    {
        switch (body)
        {
            case BlockHandlerBody b:
                foreach (var stmt in b.Block.Statements)
                    stmtEmitter.EmitStatement(stmt);
                break;
            case InlineHandlerBody i:
                // Inline expression — emit as expression statement
                stmtEmitter.EmitStatement(new ExpressionStmt(i.Span, i.Expr));
                break;
        }
    }

    // ─── Lifecycle hooks ─────────────────────────────────────────────────────

    private void EmitLifecycleHooks(ActorDecl actor, StatementEmitter stmtEmitter)
    {
        foreach (var hook in actor.Members.OfType<LifecycleHook>())
        {
            var signature = hook.Event switch
            {
                PreStartEvent    => "protected override void OnPreStart()",
                PostStopEvent    => "protected override void OnPostStop()",
                RestoreEvent re  => $"protected override void OnRestore(Spek.Persistence.Snapshot {re.Binding})",
                _                => throw new InvalidOperationException($"unknown lifecycle event {hook.Event.GetType().Name}")
            };

            _w.Line(signature);
            _w.Line("{");
            _w.Indent();
            // `on Restore(Snapshot s)` binds `s` — it shadows a same-named field.
            stmtEmitter.SetShadowed(hook.Event is RestoreEvent rb
                ? new[] { rb.Binding } : System.Array.Empty<string>());
            EmitHandlerBody(hook.Body, stmtEmitter);
            _w.Dedent();
            _w.Line("}");
            _w.Line();
        }

        // `term { ... }` block emits as an OnTerm override plus
        // a DisposeAsync wrapper for external `await using` consumers.
        // Runtime invocation is wired through ActorSlot's stop sequence,
        // which calls InvokeOnTerm() after InvokeOnPostStop().
        var term = actor.Members.OfType<TermBlock>().FirstOrDefault();
        if (term is not null)
        {
            _w.Line("protected override void OnTerm()");
            _w.Line("{");
            _w.Indent();
            stmtEmitter.SetShadowed(System.Array.Empty<string>());   // no params; clear any prior body's scope
            // Use foreach+EmitStatement (not EmitBlock) so we don't
            // produce a redundant inner `{ ... }` wrapper inside the
            // method body.
            foreach (var stmt in term.Body.Statements)
                stmtEmitter.EmitStatement(stmt);
            _w.Dedent();
            _w.Line("}");
            _w.Line();

            _w.Line("public System.Threading.Tasks.ValueTask DisposeAsync()");
            _w.Line("{");
            _w.Indent();
            _w.Line("OnTerm();");
            _w.Line("return System.Threading.Tasks.ValueTask.CompletedTask;");
            _w.Dedent();
            _w.Line("}");
            _w.Line();
        }
    }

    // ─── CaptureFields (for persist/passivate) ────────────────────────────────

    private void EmitCaptureFields(ActorDecl actor, HashSet<string> fieldNames)
    {
        if (!actor.Members.OfType<PassivateDecl>().Any() &&
            !actor.Members.OfType<BehaviorDecl>()
                .SelectMany(b => b.Handlers)
                .Any(h => ContainsPersist(h.Body)))
            return;

        // Actor-field capture honours the field lifecycle
        // marker. `transient` and `retired` fields are skipped (the
        // first because it's in-memory only, the second because the
        // field is no longer reachable from new code). `deprecated`
        // fields still roundtrip so existing snapshots survive the
        // deprecation period without data loss.
        var persistedNames = actor.Members
            .OfType<FieldDecl>()
            .Where(f => !f.IsTransient && !f.IsRetired && fieldNames.Contains(f.Name))
            .Select(f => f.Name)
            .ToList();

        _w.Line("protected override System.Collections.Generic.IReadOnlyDictionary<string, object?> CaptureFields() =>");
        _w.Indent();
        _w.Line("new System.Collections.Generic.Dictionary<string, object?>");
        _w.Line("{");
        _w.Indent();
        foreach (var name in persistedNames)
            _w.Line($"[\"{name}\"] = _{name},");
        _w.Dedent();
        _w.Line("};");
        _w.Dedent();
        _w.Line();
    }

    // ─── Auto-restore (symmetric with CaptureFields) ──────────────────────────

    /// <summary>
    /// When a persistent actor has no explicit <c>on Restore</c> hook, emit one that
    /// rehydrates each captured field — mirroring <see cref="EmitCaptureFields"/>. Without
    /// this, an actor would <c>persist</c> its state yet silently never reload it (the
    /// counter-persists-3-rehydrates-0 footgun). An explicit <c>on Restore</c> still wins;
    /// transient/retired fields are excluded, exactly as they are from capture.
    /// </summary>
    private void EmitAutoRestore(ActorDecl actor, HashSet<string> fieldNames)
    {
        // Explicit `on Restore` wins — EmitLifecycleHooks already emitted it.
        if (actor.Members.OfType<LifecycleHook>().Any(h => h.Event is RestoreEvent)) return;

        // Only persistent actors (same trigger as CaptureFields).
        if (!actor.Members.OfType<PassivateDecl>().Any() &&
            !actor.Members.OfType<BehaviorDecl>()
                .SelectMany(b => b.Handlers)
                .Any(h => ContainsPersist(h.Body)))
            return;

        var persisted = actor.Members
            .OfType<FieldDecl>()
            .Where(f => !f.IsTransient && !f.IsRetired && fieldNames.Contains(f.Name))
            .ToList();
        if (persisted.Count == 0) return;

        _w.Line("// Auto-generated symmetric restore (no explicit `on Restore`): rehydrate");
        _w.Line("// each captured field from the snapshot. Mirrors CaptureFields.");
        _w.Line("protected override void OnRestore(Spek.Persistence.Snapshot __snapshot)");
        _w.Line("{");
        _w.Indent();
        foreach (var f in persisted)
            _w.Line($"_{f.Name} = __snapshot.Get<{f.Type}>(\"{f.Name}\");");
        _w.Dedent();
        _w.Line("}");
        _w.Line();
    }

    private static bool ContainsPersist(HandlerBody body) => body switch
    {
        BlockHandlerBody b => b.Block.Statements.Any(s => s is PersistStmt),
        _                  => false
    };

    // ─── User-written methods (incl. ActorBase lifecycle overrides) ──────────
    // Actor bodies may declare ordinary methods and override ActorBase hooks
    // (OnFailure / OnChildFailure / OnPreStart / …). Override names emit as
    // `protected override`; everything else takes its declared visibility. Without
    // this, methods parse but are silently dropped — e.g. a hand-written
    // OnChildFailure (the only way to return Resume) would never take effect.
    // Exactly the virtuals ActorBase declares — emitting `protected override` for a
    // name not in this set would be CS0115. (OnPreRestart/OnPostRestart are NOT
    // ActorBase virtuals; they were here by mistake.)
    private static readonly HashSet<string> ActorBaseOverrides = new()
    {
        "OnPreStart", "OnPostStop", "OnRestore", "OnFailure", "OnChildFailure",
    };

    private void EmitMethods(ActorDecl actor, StatementEmitter stmtEmitter)
    {
        // A `supervise` decl already emits OnChildFailure (EmitSuperviseOverrides).
        // CE0118 rejects an actor that declares both, so this skip is now just
        // defense-in-depth against a duplicate method should emit ever run anyway.
        var hasSupervise = actor.Members.OfType<SuperviseDecl>().Any();

        foreach (var m in actor.Members.OfType<MethodDecl>())
        {
            if (hasSupervise && m.Name == "OnChildFailure") continue;

            _w.Line();
            var ret = m.ReturnType?.ToString() ?? "void";
            var tps = m.TypeParameters.Count > 0
                ? $"<{string.Join(", ", m.TypeParameters.Select(p => p.Name))}>"
                : "";
            var parms = string.Join(", ", m.Parameters.Select(MethodParam));

            if (m.IsAbstract)
            {
                // `abstract T M(params);` on an abstract actor — the subclass
                // implements it, no body.
                _w.Line($"{MethodVisibility(m.Visibility)} abstract {ret} {m.Name}{tps}({parms}){ExpressionEmitter.FormatWhereClauses(m.WhereClauses)};");
                continue;
            }

            // `protected override` for the ActorBase lifecycle virtuals; else
            // infer `override` when this implements a base *actor's* abstract
            // method (same reuse+abstract-only model as classes — no keyword).
            var head = ActorBaseOverrides.Contains(m.Name) && m.TypeParameters.Count == 0
                ? "protected override"
                : OverridesBaseActorAbstract(actor, m)
                    ? $"{MethodVisibility(m.Visibility)} override"
                    : MethodVisibility(m.Visibility);

            _w.Line($"{head} {ret} {m.Name}{tps}({parms}){ExpressionEmitter.FormatWhereClauses(m.WhereClauses)}");
            stmtEmitter.SetShadowed(m.Parameters.Select(p => p.Name));   // params shadow same-named fields
            stmtEmitter.EmitBlock(m.Body);
        }
    }

    /// <summary>
    /// True when <paramref name="method"/> implements an <c>abstract</c> method
    /// declared up <paramref name="actor"/>'s base-actor chain — matched by name
    /// and parameter count. Used to infer <c>override</c> so Spek source never
    /// spells it (the reuse + abstract-only model, mirrored from classes).
    /// </summary>
    private bool OverridesBaseActorAbstract(ActorDecl actor, MethodDecl method)
    {
        var seen = new HashSet<string>(StringComparer.Ordinal);
        var current = actor;
        while (current is not null)
        {
            var baseActor = ResolveBaseActor(current);
            if (baseActor is null) return false;
            if (!seen.Add(baseActor.Name)) return false;   // cycle guard

            if (baseActor.Members.OfType<MethodDecl>().Any(bm =>
                    bm.IsAbstract &&
                    bm.Name == method.Name &&
                    bm.Parameters.Count == method.Parameters.Count))
                return true;

            current = baseActor;
        }
        return false;
    }

    /// <summary>
    /// Adds a base actor's <em>accessible</em> field names (protected/public,
    /// walking the base chain) to <paramref name="fieldNames"/> so a derived
    /// actor's handlers resolve them as fields and the <c>_</c>-rename applies.
    /// Private base fields stay encapsulated and are deliberately excluded.
    /// </summary>
    private void AddInheritedFieldNames(ActorDecl actor, HashSet<string> fieldNames)
    {
        var seen = new HashSet<string>(StringComparer.Ordinal);
        var current = ResolveBaseActor(actor);
        while (current is not null && seen.Add(current.Name))
        {
            foreach (var f in current.Members.OfType<FieldDecl>())
                if (f.Visibility is Visibility.Protected or Visibility.Public or Visibility.Internal)
                    fieldNames.Add(f.Name);
            current = ResolveBaseActor(current);
        }
    }

    /// <summary>
    /// The base *actor* of <paramref name="actor"/>, if any — the colon name
    /// that resolves to an actor (the rest are channels). Null when the actor
    /// extends only channels or nothing.
    /// </summary>
    private ActorDecl? ResolveBaseActor(ActorDecl actor)
    {
        var names = new List<QualifiedName>();
        if (actor.BaseActor is not null) names.Add(actor.BaseActor);
        names.AddRange(actor.ImplementedChannels);
        foreach (var n in names)
            if (_symbols.ResolveActor(n) is { } a)
                return a;
        return null;
    }

    private static string MethodParam(Param p)
    {
        var mod = ExpressionEmitter.ParamModifierKeyword(p.Modifier);
        return mod.Length == 0 ? $"{p.Type} {p.Name}" : $"{mod} {p.Type} {p.Name}";
    }

    private static string MethodVisibility(Visibility v) => v switch
    {
        Visibility.Public    => "public",
        Visibility.Internal  => "internal",
        Visibility.Protected => "protected",
        _                    => "private",
    };

    // ─── Supervise declarations → OnChildFailure override ────────────────────

    /// <summary>
    /// Emits a <c>protected override OnChildFailure(...)</c> method that
    /// translates each <c>supervise(...)</c> declaration in the actor body
    /// into a decision for the runtime supervision engine.
    ///
    /// Per-child <c>supervise(child, strategy: ...)</c> entries become
    /// <c>if (ReferenceEquals(child, _field)) return ApplyRestartPolicy(...);</c>
    /// checks. A single default <c>supervise strategy: ...</c> entry
    /// becomes the fallthrough return. Without any supervise decls, nothing
    /// is emitted and the base class's "stop on any child failure" default
    /// holds.
    /// </summary>
    private void EmitSuperviseOverrides(ActorDecl actor, ExpressionEmitter exprEmitter)
    {
        var decls = actor.Members.OfType<SuperviseDecl>().ToList();
        if (decls.Count == 0) return;

        var perChild = decls.Where(d => d.Target is not null).ToList();
        var defaults = decls.Where(d => d.Target is null).ToList();

        _w.Line("protected override Spek.FailureDirective OnChildFailure(");
        _w.Indent();
        _w.Line("Spek.ActorRef child,");
        _w.Line("System.Exception cause,");
        _w.Line("object message)");
        _w.Dedent();
        _w.Line("{");
        _w.Indent();

        foreach (var decl in perChild)
        {
            var targetExpr = exprEmitter.Emit(decl.Target!);
            _w.Line($"if (System.Object.ReferenceEquals(child, {targetExpr}))");
            _w.Line("{");
            _w.Indent();
            EmitSuperviseDeclBody(decl, exprEmitter);
            _w.Dedent();
            _w.Line("}");
        }

        if (defaults.Count > 0)
        {
            EmitSuperviseDeclBody(defaults[0], exprEmitter);
        }
        else
        {
            _w.Line("return base.OnChildFailure(child, cause, message);");
        }

        _w.Dedent();
        _w.Line("}");
        _w.Line();
    }

    /// <summary>
    /// Emits the body of a single <c>supervise</c> decl's branch — an
    /// optional <c>RestartSiblingsOf</c> broadcast for AllForOne, followed
    /// by the on-Failure dispatch ladder: one <c>if (cause is ExType)</c>
    /// per typed arm plus a catch-all <c>return</c>. Multiple arms match
    /// top-to-bottom; the first match wins, matching Akka's
    /// supervisor-strategy semantics.
    /// </summary>
    private void EmitSuperviseDeclBody(SuperviseDecl decl, ExpressionEmitter exprEmitter)
    {
        EmitStrategyBroadcastIfNeeded(decl);

        var options = StrategyOptions(decl.Strategy);
        var arms = options.OfType<OnFailureOption>().ToList();
        var maxRetriesExpr = options.OfType<MaxRetriesOption>().FirstOrDefault()?.Value is { } m
            ? exprEmitter.Emit(m) : null;
        var windowExpr = options.OfType<WithinTimeOption>().FirstOrDefault()?.Window is { } w
            ? exprEmitter.Emit(w) : null;

        // If no arms at all (user wrote `supervise OneForOne();` — unusual
        // but legal), default to Stop.
        if (arms.Count == 0)
        {
            _w.Line($"return {ApplyRestartPolicyCall(RestartAction.Stop, maxRetriesExpr, windowExpr)};");
            return;
        }

        // Typed arms first — each becomes `if (cause is ExType) return ...;`.
        // Untyped catch-all(s) become the fallthrough. The first untyped
        // arm wins; anything after it is dead (caught here as unreachable).
        OnFailureOption? fallback = null;
        foreach (var arm in arms)
        {
            if (arm.ExceptionType is null)
            {
                fallback ??= arm;
                continue;
            }
            _w.Line($"if (cause is {arm.ExceptionType})");
            _w.Line("{");
            _w.Indent();
            _w.Line($"return {ApplyRestartPolicyCall(arm.Action, maxRetriesExpr, windowExpr)};");
            _w.Dedent();
            _w.Line("}");
        }

        // No untyped catch-all → fall back to Stop (conservative default,
        // matches Akka's "unmatched → Escalate" except Spek defaults to
        // Stop for consistency with single-arm supervise today).
        var fallbackAction = fallback?.Action ?? RestartAction.Stop;
        _w.Line($"return {ApplyRestartPolicyCall(fallbackAction, maxRetriesExpr, windowExpr)};");
    }

    /// <summary>
    /// For <c>AllForOne</c> strategies, emit a call to broadcast the
    /// restart to all sibling children before applying the policy to the
    /// failing one. <c>OneForOne</c> has no broadcast — each child fails
    /// and is handled individually.
    /// </summary>
    private void EmitStrategyBroadcastIfNeeded(SuperviseDecl decl)
    {
        if (decl.Strategy is AllForOneStrategy)
            _w.Line("RestartSiblingsOf(child);");
    }

    private static IReadOnlyList<SuperviseOption> StrategyOptions(SuperviseStrategy strategy) =>
        strategy switch
        {
            OneForOneStrategy o => o.Options,
            AllForOneStrategy a => a.Options,
            _                   => Array.Empty<SuperviseOption>()
        };

    private static string ApplyRestartPolicyCall(
        RestartAction action,
        string? maxRetriesExpr,
        string? windowExpr)
    {
        var args = new List<string>
        {
            "child",
            $"action: Spek.FailureDirective.{action}",
        };
        if (maxRetriesExpr is not null)
            args.Add($"maxRetries: {maxRetriesExpr}");
        if (windowExpr is not null)
            args.Add($"window: {windowExpr}");

        return $"ApplyRestartPolicy({string.Join(", ", args)})";
    }

    // ─── Passivation timeout override ─────────────────────────────────────────

    private void EmitPassivationTimeout(ActorDecl actor, ExpressionEmitter exprEmitter)
    {
        var passivate = actor.Members.OfType<PassivateDecl>().FirstOrDefault();
        if (passivate is null) return;

        _w.Line($"protected override System.TimeSpan? PassivationTimeout =>");
        _w.Indent();
        _w.Line($"{exprEmitter.Emit(passivate.Timeout)};");
        _w.Dedent();
        _w.Line();
    }
}
