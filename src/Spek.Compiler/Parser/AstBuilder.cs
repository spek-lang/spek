using Antlr4.Runtime;
using Antlr4.Runtime.Tree;
using Spek.Compiler.AST;
using Spek.Compiler.Grammar;
using static Spek.Compiler.Grammar.SpekParser;

namespace Spek.Compiler.Parser;

/// <summary>
/// Walks the ANTLR4 parse tree and produces a typed AST.
/// Uses the visitor pattern as recommended in CONTEXT.md.
/// </summary>
public sealed class AstBuilder : SpekParserBaseVisitor<AstNode>
{
    private readonly CommonTokenStream? _tokens;

    /// <summary>
    /// <paramref name="tokens"/> is the same stream the parser ran on; it lets
    /// the builder recover the hidden-channel <c>///</c> doc comments that
    /// precede each declaration. Pass null (the default) to skip doc capture.
    /// </summary>
    public AstBuilder(CommonTokenStream? tokens = null) => _tokens = tokens;

    // ─── Helpers ─────────────────────────────────────────────────────────────

    private static SourceSpan Span(ParserRuleContext ctx)
    {
        // EndColumn is 1-based exclusive (one past the stop token's last
        // character), so a single-token span carries the token's true width.
        // A stop token spanning lines (block text) has no meaningful end
        // column on its end line; fall back to width 1 rather than guess.
        var stop = ctx.Stop;
        var stopLen = stop?.Text is { } t && !t.Contains('\n') ? t.Length : 1;
        return new(ctx.Start.Line, ctx.Start.Column + 1,
            stop?.Line ?? ctx.Start.Line, (stop?.Column ?? ctx.Start.Column) + 1 + stopLen);
    }

    /// <summary>Span from <paramref name="start"/>'s first character to one
    /// past <paramref name="stop"/>'s last — same math as
    /// <see cref="Span(ParserRuleContext)"/>, for spans that straddle rule
    /// contexts (the collapsed name chains in VisitPostfixExpr).</summary>
    private static SourceSpan SpanFrom(IToken start, IToken stop)
    {
        var stopLen = stop.Text is { } t && !t.Contains('\n') ? t.Length : 1;
        return new(start.Line, start.Column + 1, stop.Line, stop.Column + 1 + stopLen);
    }

    // Token types the grammar's `softName` rule accepts — the names that may
    // extend a qualified name. Keep in sync with SpekParser.g4.
    private static bool IsSoftNameToken(int type) => type
        is SpekLexer.IDENTIFIER or SpekLexer.ACTOR or SpekLexer.MESSAGE
        or SpekLexer.CHANNEL or SpekLexer.INTERFACE or SpekLexer.AFTER
        or SpekLexer.STRATEGY or SpekLexer.RESUME or SpekLexer.FLAGS;

    // The `///` doc-comment block written immediately above `ctx`, or null.
    // Whitespace is `-> skip`ped by the lexer, so the hidden channel holds only
    // comments; we keep the trailing run of consecutive triple-slash lines (the
    // block adjacent to the declaration), dropping anything a plain comment or
    // an unrelated earlier block puts in between.
    private IReadOnlyList<string>? DocBefore(ParserRuleContext ctx)
    {
        var hidden = _tokens?.GetHiddenTokensToLeft(ctx.Start.TokenIndex);
        if (hidden is null) return null;

        // Keep only the run of consecutive `///` lines that sits directly above
        // ctx. A plain comment, or a blank line (which the lexer skips, so it
        // shows as a line-number jump between hidden tokens), starts the run
        // fresh — this stops a floating file-overview block from being merged
        // into the first declaration's docs.
        var lines = new List<string>();
        int lastLine = -1;
        foreach (var t in hidden)
        {
            var isDoc = t.Type == SpekLexer.LINE_COMMENT
                        && t.Text.StartsWith("///", StringComparison.Ordinal);
            if (!isDoc || (lastLine >= 0 && t.Line > lastLine + 1))
                lines.Clear();
            if (isDoc)
                lines.Add(t.Text.TrimEnd());
            lastLine = t.Line;
        }

        // The block must sit immediately above the declaration (no blank line
        // between it and ctx), or it isn't this declaration's doc comment.
        if (lines.Count == 0 || ctx.Start.Line > lastLine + 1)
            return null;
        return lines;
    }

    private T As<T>(IParseTree tree) where T : AstNode => (T)Visit(tree);

    private QualifiedName QName(QualifiedNameContext ctx)
    {
        var parts = ctx.softName().Select(t => t.GetText()).ToList();
        return new QualifiedName(Span(ctx), parts);
    }

    private TypeRef TypeRef(Type_Context ctx)
    {
        var name = QName(ctx.qualifiedName());
        var typeArgs = ctx.typeArgs() != null
            ? ctx.typeArgs().type_().Select(TypeRef).ToList()
            : (IReadOnlyList<AST.TypeRef>)[];
        // `T?` declares a nullable reference type. Emitted
        // verbatim into the generated C# so Roslyn's nullable
        // analysis catches null-deref at the C# layer.
        var isNullable = ctx.QUESTION() != null;
        // Each `[]` pair adds an array rank: `int[]` → 1, `int[][]` → 2.
        var arrayRank = ctx.LBRACKET().Length;
        return new AST.TypeRef(Span(ctx), name, typeArgs, isNullable, arrayRank);
    }

    private Visibility ParseVisibility(VisibilityContext? ctx) => ctx?.Start.Type switch
    {
        SpekLexer.PUBLIC    => Visibility.Public,
        SpekLexer.INTERNAL  => Visibility.Internal,
        SpekLexer.PROTECTED => Visibility.Protected,
        SpekLexer.PRIVATE   => Visibility.Private,
        _                   => Visibility.Private
    };

    // Builds the `<T, U>` declaration list shared by generic messages,
    // actors, classes, methods, and functions.
    private static IReadOnlyList<TypeParameter> BuildTypeParams(TypeParamsContext? ctx) =>
        ctx is null
            ? []
            : ctx.typeParam()
                 .Select(tp => new TypeParameter(Span(tp), tp.IDENTIFIER().GetText()))
                 .ToList();

    // Builds the `where T : C1, C2` constraint clauses. Each constraint
    // is rendered to its verbatim C# form (`class`, `new()`, or a type) so the
    // emitter can pass it straight through.
    private IReadOnlyList<WhereClause> BuildWhereClauses(WhereClauseContext[] ctxs) =>
        ctxs.Length == 0
            ? []
            : ctxs.Select(w => new WhereClause(
                  Span(w),
                  w.IDENTIFIER().GetText(),
                  w.typeConstraint().Select(RenderConstraint).ToList())).ToList();

    private string RenderConstraint(TypeConstraintContext c) => c switch
    {
        ClassConstraintContext      => "class",
        NewConstraintContext        => "new()",
        TypeRefConstraintContext t  => TypeRef(t.type_()).ToString(),
        _                           => c.GetText(),
    };

    // ─── Top-level ───────────────────────────────────────────────────────────

    public override AstNode VisitFile_(File_Context ctx)
    {
        var ns = ctx.fileNamespace() != null ? As<NamespaceDecl>(ctx.fileNamespace()) : null;

        var usings = new List<UsingDecl>();
        var decls  = new List<TopLevelDecl>();

        foreach (var declCtx in ctx.declaration())
        {
            if      (declCtx.usingDecl()   != null) usings.Add(As<UsingDecl>(declCtx.usingDecl()));
            else if (declCtx.messageDecl() != null) decls.Add(As<MessageDecl>(declCtx.messageDecl()));
            else if (declCtx.enumDecl()    != null) decls.Add(As<EnumDecl>(declCtx.enumDecl()));
            else if (declCtx.channelDecl() != null) decls.Add(As<ChannelDecl>(declCtx.channelDecl()));
            else if (declCtx.interfaceDecl() != null) decls.Add(As<InterfaceDecl>(declCtx.interfaceDecl()));
            else if (declCtx.sharedDecl()  != null) decls.Add(As<SharedRegionDecl>(declCtx.sharedDecl()));
            else if (declCtx.actorDecl()   != null) decls.Add(As<ActorDecl>(declCtx.actorDecl()));
            else if (declCtx.classDecl()   != null) decls.Add(As<ClassDecl>(declCtx.classDecl()));
            else if (declCtx.moduleDecl()  != null) decls.Add(As<ModuleDecl>(declCtx.moduleDecl()));
            else if (declCtx.programDecl() != null) decls.Add(As<ProgramDecl>(declCtx.programDecl()));
        }

        return new SpekFile(Span(ctx), ns, usings, decls);
    }

    public override AstNode VisitFileNamespace(FileNamespaceContext ctx) =>
        new NamespaceDecl(Span(ctx), QName(ctx.qualifiedName()));

    public override AstNode VisitUsingDecl(UsingDeclContext ctx) =>
        new UsingDecl(Span(ctx), QName(ctx.qualifiedName()),
            IsInterop: ctx.INTEROP() != null);

    // ─── Messages ────────────────────────────────────────────────────────────

    public override AstNode VisitMessageDecl(MessageDeclContext ctx)
    {
        var typeParams = ctx.typeParams() != null
            ? ctx.typeParams().typeParam()
                  .Select(tp => new TypeParameter(Span(tp), tp.IDENTIFIER().GetText()))
                  .ToList()
            : (IReadOnlyList<TypeParameter>)[];

        var fields = ctx.messageFields() != null
            ? ctx.messageFields().messageField().Select(f => As<MessageField>(f)).ToList()
            : (IReadOnlyList<MessageField>)[];

        var baseMessage = ctx.messageBase() is { } mb ? QName(mb.qualifiedName()) : null;

        return new MessageDecl(
            Span(ctx), ctx.IDENTIFIER().GetText(), typeParams, fields,
            ctx.ABSTRACT() != null, baseMessage)
            { DocComment = DocBefore(ctx) };
    }

    // ─── Enums ───────────────────────────────────────────────────────────────

    public override AstNode VisitEnumDecl(EnumDeclContext ctx)
    {
        // Default visibility is Public, not Private — enums are typically
        // used as `message` field types, which emit as public records.
        // An explicit modifier still wins (`internal enum X { }`).
        var visibility = ctx.visibility() != null
            ? ParseVisibility(ctx.visibility())
            : Visibility.Public;
        var name = ctx.IDENTIFIER().GetText();
        var members = new List<EnumMember>();
        if (ctx.enumMembers() != null)
        {
            foreach (var m in ctx.enumMembers().enumMember())
            {
                long? value = null;
                IReadOnlyList<string>? unionOf = null;
                var v = m.enumMemberValue();
                if (v != null)
                {
                    if (v.INTEGER_LITERAL() != null)
                    {
                        var parsed = ParseIntValue(v.INTEGER_LITERAL().GetText());
                        value = v.MINUS() != null ? -parsed : parsed;
                    }
                    else
                    {
                        unionOf = v.IDENTIFIER().Select(i => i.GetText()).ToList();
                    }
                }
                members.Add(new EnumMember(Span(m), m.IDENTIFIER().GetText(), value, unionOf)
                    { DocComment = DocBefore(m) });
            }
        }
        return new EnumDecl(Span(ctx), visibility, name, members, ctx.FLAGS() != null)
            { DocComment = DocBefore(ctx) };
    }

    public override AstNode VisitMessageField(MessageFieldContext ctx)
    {
        var type        = TypeRef(ctx.type_());
        var name        = ctx.softName().GetText();
        var defaultVal  = ctx.defaultValue() != null ? As<Expr>(ctx.defaultValue().expression()) : null;
        return new MessageField(Span(ctx), type, name, defaultVal);
    }

    // ─── Actors ──────────────────────────────────────────────────────────────

    public override AstNode VisitActorDecl(ActorDeclContext ctx)
    {
        var visibility  = ParseVisibility(ctx.visibility());
        var isAbstract  = ctx.ABSTRACT() != null;
        var name        = ctx.IDENTIFIER().GetText();
        var typeParams  = ctx.typeParams() != null
            ? ctx.typeParams().typeParam()
                  .Select(tp => new TypeParameter(Span(tp), tp.IDENTIFIER().GetText()))
                  .ToList()
            : (IReadOnlyList<TypeParameter>)[];
        // `actor Foo : Bar, Ch1, Ch2 { }` — the colon list may contain a base
        // actor followed by one or more channel names. The builder splits
        // naively: first name → BaseActor (could turn out to be a channel after
        // symbol resolution; semantic analyzer fixes that up); remaining
        // names → ImplementedChannels. The emitter uses BaseActor for the
        // C# base class; channels are compile-time-verified and emit
        // nothing into the C# class header.
        QualifiedName? baseActor = null;
        var channels = new List<QualifiedName>();
        if (ctx.baseActor() != null)
        {
            var names = ctx.baseActor().qualifiedName();
            for (int i = 0; i < names.Length; i++)
            {
                var qn = QName(names[i]);
                if (i == 0) baseActor = qn;
                else channels.Add(qn);
            }
        }
        // Bare on-handlers at actor scope (the "single-behavior
        // shorthand") are collected and folded into a synthesised
        // `behavior Default { ... }` appended to the member list. The
        // name "Default" is fixed so stack traces and supervision logs
        // remain readable. Mixed mode (some bare, some inside an
        // explicit `behavior X {}`) is allowed — bare handlers go to
        // Default, explicit behaviors keep their own names. In that
        // case the auto-become picks whichever behavior comes first
        // in source order (typically the explicit one); the user can
        // call `become Default;` from init to start there instead.
        var members = new List<ActorMember>();
        var bareHandlers = new List<OnHandler>();
        foreach (var memberCtx in ctx.actorMember())
        {
            var node = Visit(memberCtx);
            if (node is OnHandler bareHandler)
                bareHandlers.Add(bareHandler);
            else if (node is ActorMember member)
                members.Add(member);
        }
        if (bareHandlers.Count > 0)
        {
            members.Add(new BehaviorDecl(
                bareHandlers[0].Span,
                IsAbstract: false,
                IsOverride: false,
                Name: "Default",
                Handlers: bareHandlers));
        }

        var wcs = BuildWhereClauses(ctx.whereClause());
        return new ActorDecl(Span(ctx), visibility, isAbstract, name, typeParams, wcs, baseActor, channels, members);
    }

    // `class X { fields; init(...) {...}; methods }`. Members reuse the
    // actor-side visitors (VisitFieldDecl / VisitInitBlock / VisitMethodDecl)
    // via the generic Visit dispatch, exactly as VisitActorDecl does.
    public override AstNode VisitClassDecl(ClassDeclContext ctx)
    {
        var visibility = ParseVisibility(ctx.visibility());
        var name       = ctx.IDENTIFIER().GetText();
        var typeParams = ctx.typeParams() != null
            ? ctx.typeParams().typeParam()
                  .Select(tp => new TypeParameter(Span(tp), tp.IDENTIFIER().GetText()))
                  .ToList()
            : (IReadOnlyList<TypeParameter>)[];

        var fields  = new List<FieldDecl>();
        var methods = new List<MethodDecl>();
        var properties = new List<PropertyDecl>();
        InitBlock? init = null;
        foreach (var memberCtx in ctx.classMember())
        {
            switch (Visit(memberCtx))
            {
                case FieldDecl f:     fields.Add(f);  break;
                case MethodDecl m:    methods.Add(m); break;
                case PropertyDecl p:  properties.Add(p); break;
                case InitBlock ib:    init = ib;      break;   // duplicate init is a semantic check
            }
        }

        // Base list — `: Base, IFoo, ...`. An optional base class (first) plus
        // implemented interfaces; semantic analysis classifies each name.
        var bases = new List<QualifiedName>();
        if (ctx.classBases() is { } cbctx)
        {
            foreach (var qn in cbctx.qualifiedName())
                bases.Add(QName(qn));
        }

        var wcs = BuildWhereClauses(ctx.whereClause());
        return new ClassDecl(Span(ctx), visibility, ctx.ABSTRACT() != null, name, typeParams, wcs, fields, init, methods, properties, bases);
    }

    public override AstNode VisitInterfaceDecl(InterfaceDeclContext ctx)
    {
        var visibility = ParseVisibility(ctx.visibility());
        var name       = ctx.IDENTIFIER().GetText();
        var typeParams = BuildTypeParams(ctx.typeParams());

        // Base interfaces — `: Base, ...`. Cycle-checking happens in semantics.
        var bases = new List<QualifiedName>();
        if (ctx.interfaceBases() is { } bctx)
        {
            foreach (var qn in bctx.qualifiedName())
                bases.Add(QName(qn));
        }

        var methods    = new List<MethodSignature>();
        var properties = new List<PropertyDecl>();
        var fields     = new List<FieldDecl>();   // illegal; kept for CE0120
        foreach (var memberCtx in ctx.interfaceMember())
        {
            switch (Visit(memberCtx))
            {
                case MethodSignature ms: methods.Add(ms);    break;
                case PropertyDecl p:     properties.Add(p);  break;
                case FieldDecl f:        fields.Add(f);      break;
            }
        }

        var wcs = BuildWhereClauses(ctx.whereClause());
        return new InterfaceDecl(Span(ctx), visibility, name, typeParams, wcs, bases, methods, properties, fields)
            with { DocComment = DocBefore(ctx) };
    }

    public override AstNode VisitInterfaceMember(InterfaceMemberContext ctx)
    {
        if (ctx.interfaceMethod() != null) return Visit(ctx.interfaceMethod());
        if (ctx.propertyDecl()    != null) return Visit(ctx.propertyDecl());
        if (ctx.fieldDecl()       != null) return Visit(ctx.fieldDecl());
        throw new InvalidOperationException($"Unknown interfaceMember at {Span(ctx)}");
    }

    public override AstNode VisitInterfaceMethod(InterfaceMethodContext ctx)
    {
        var visibility = ParseVisibility(ctx.visibility());
        var returnType = ctx.returnType().VOID() != null ? null : TypeRef(ctx.returnType().type_());
        var name       = ctx.IDENTIFIER().GetText();
        var typeParams = BuildTypeParams(ctx.typeParams());
        var parms = ctx.params_() != null
            ? ctx.params_().param().Select(p => As<Param>(p)).ToList()
            : (IReadOnlyList<Param>)[];
        var wcs = BuildWhereClauses(ctx.whereClause());

        // A block here is illegal (default-method body) but parses so semantics
        // can flag CE0120; the signature form leaves Body null.
        var body = ctx.block() != null ? As<BlockStmt>(ctx.block()) : null;
        return new MethodSignature(Span(ctx), visibility, returnType, name, typeParams, wcs, parms, body)
            with { DocComment = DocBefore(ctx) };
    }

    public override AstNode VisitPropertyDecl(PropertyDeclContext ctx)
    {
        var visibility = ParseVisibility(ctx.visibility());
        var type = TypeRef(ctx.type_());
        var name = ctx.IDENTIFIER().GetText();
        var accessors = ctx.propertyAccessor()
            .Select(a => new PropertyAccessor(
                Span(a),
                a.visibility() != null ? ParseVisibility(a.visibility()) : (Visibility?)null,
                a.IDENTIFIER()?.GetText() ?? "init",   // INIT token → "init"
                a.expression() != null ? As<Expr>(a.expression()) : null))
            .ToList();
        var initializer = ctx.expression() != null ? As<Expr>(ctx.expression()) : null;
        return new PropertyDecl(Span(ctx), visibility, type, name, accessors, initializer);
    }

    public override AstNode VisitChannelDecl(ChannelDeclContext ctx)
    {
        var visibility = ParseVisibility(ctx.visibility());
        var name       = ctx.IDENTIFIER().GetText();

        // Inheritance list — `: Base1, Base2, ...`. Empty when
        // the channel doesn't inherit from anything. Resolution and
        // cycle-checking happen in the semantic analyzer.
        var bases = new List<QualifiedName>();
        if (ctx.channelBases() is { } bctx)
        {
            foreach (var qn in bctx.qualifiedName())
                bases.Add(QName(qn));
        }

        var members = ctx.channelMember().Select(m => As<ChannelMember>(m)).ToList();
        return new ChannelDecl(Span(ctx), visibility, name, bases, members);
    }

    public override AstNode VisitChannelMember(ChannelMemberContext ctx)
    {
        if (ctx.channelInput() != null) return Visit(ctx.channelInput());
        if (ctx.channelEmits() != null) return Visit(ctx.channelEmits());
        throw new InvalidOperationException($"Unknown channelMember at {Span(ctx)}");
    }

    public override AstNode VisitChannelInput(ChannelInputContext ctx)
    {
        return new ChannelInput(Span(ctx), QName(ctx.qualifiedName()));
    }

    public override AstNode VisitChannelEmits(ChannelEmitsContext ctx)
    {
        if (ctx.ANY() != null)
        {
            return new ChannelEmits(Span(ctx), null, IsAny: true);
        }
        return new ChannelEmits(Span(ctx), QName(ctx.qualifiedName()), IsAny: false);
    }

    public override AstNode VisitActorMember(ActorMemberContext ctx)
    {
        if (ctx.fieldDecl()    != null) return Visit(ctx.fieldDecl());
        if (ctx.initBlock()    != null) return Visit(ctx.initBlock());
        if (ctx.termBlock()    != null) return Visit(ctx.termBlock());
        if (ctx.behaviorDecl() != null) return Visit(ctx.behaviorDecl());
        // Bare on-handler at actor scope. VisitActorDecl folds
        // these into a synthesised `behavior Default { ... }`.
        if (ctx.onHandler()    != null) return Visit(ctx.onHandler());
        if (ctx.useDecl()      != null) return Visit(ctx.useDecl());
        if (ctx.lifecycleHook()!= null) return Visit(ctx.lifecycleHook());
        if (ctx.passivateDecl()!= null) return Visit(ctx.passivateDecl());
        if (ctx.superviseDecl()!= null) return Visit(ctx.superviseDecl());
        if (ctx.methodDecl()   != null) return Visit(ctx.methodDecl());
        throw new InvalidOperationException($"Unknown actorMember at {Span(ctx)}");
    }

    public override AstNode VisitSharedDecl(SharedDeclContext ctx)
    {
        var visibility = ctx.visibility() != null
            ? ParseVisibility(ctx.visibility())
            : Visibility.Internal;

        // Two IDENTIFIER tokens are possible: the region name (always
        // present) and an optional capability marker (Persisted, etc.)
        // when `: BaseName` is present after the region name.
        var idents = ctx.IDENTIFIER();
        var name = idents[0].GetText();
        string? baseCapability = ctx.COLON() != null && idents.Length > 1
            ? idents[1].GetText()
            : null;

        var fields = new List<FieldDecl>();
        BlockStmt? init = null;
        BlockStmt? term = null;
        foreach (var memberCtx in ctx.sharedMember())
        {
            if (memberCtx.fieldDecl() != null)
                fields.Add(As<FieldDecl>(memberCtx.fieldDecl()));
            else if (memberCtx.sharedInit() != null)
                init = As<BlockStmt>(memberCtx.sharedInit().block());
            else if (memberCtx.sharedTerm() != null)
                term = As<BlockStmt>(memberCtx.sharedTerm().block());
        }

        return new SharedRegionDecl(Span(ctx), visibility, name, fields, init, baseCapability, term);
    }

    public override AstNode VisitUseDecl(UseDeclContext ctx)
    {
        var idents = ctx.IDENTIFIER();
        return new UseDecl(Span(ctx), idents[0].GetText(), idents[1].GetText());
    }

    public override AstNode VisitFieldDecl(FieldDeclContext ctx)
    {
        var vis  = ParseVisibility(ctx.visibility());
        var type = TypeRef(ctx.type_());
        var name = ctx.softName().GetText();
        var init = ctx.expression() != null ? As<Expr>(ctx.expression()) : null;

        // At most one of TRANSIENT, DEPRECATED, RETIRED can
        // appear; the grammar enforces mutual exclusion.
        var lifecycle = FieldLifecycle.Normal;
        if      (ctx.TRANSIENT()  != null) lifecycle = FieldLifecycle.Transient;
        else if (ctx.DEPRECATED() != null) lifecycle = FieldLifecycle.Deprecated;
        else if (ctx.RETIRED()    != null) lifecycle = FieldLifecycle.Retired;

        // Whether the user explicitly wrote a visibility modifier
        // on the field. Lets the emitter apply owner-specific defaults
        // (actor: private; region: public) without losing user intent.
        var isExplicitVis = ctx.visibility() != null;

        return new FieldDecl(Span(ctx), vis, type, name, init, lifecycle, isExplicitVis);
    }

    public override AstNode VisitInitBlock(InitBlockContext ctx)
    {
        var parms = ctx.params_() != null
            ? ctx.params_().param().Select(p => As<Param>(p)).ToList()
            : (IReadOnlyList<Param>)[];
        var body = As<BlockStmt>(ctx.block());

        // `: base(args)` — chained base-constructor call. Null when absent;
        // an empty list means `: base()`.
        IReadOnlyList<Expr>? baseArgs = ctx.baseInit() is { } bctx
            ? BuildArgs(bctx.argList())
            : null;

        return new InitBlock(Span(ctx), parms, body, baseArgs);
    }

    // `term { ... }` actor member.
    public override AstNode VisitTermBlock(TermBlockContext ctx)
    {
        var body = As<BlockStmt>(ctx.block());
        return new TermBlock(Span(ctx), body);
    }

    public override AstNode VisitBehaviorDecl(BehaviorDeclContext ctx)
    {
        var isAbstract = ctx.ABSTRACT() != null;
        var isOverride = ctx.OVERRIDE() != null;
        var name       = ctx.IDENTIFIER().GetText();
        var handlers   = ctx.onHandler().Select(h => As<OnHandler>(h)).ToList();
        return new BehaviorDecl(Span(ctx), isAbstract, isOverride, name, handlers);
    }

    public override AstNode VisitOnHandler(OnHandlerContext ctx)
    {
        var pattern = As<MessagePattern>(ctx.messagePattern());

        // The grammar is right-recursive on `handlerTail`. Walk the
        // chain, collecting any `streamChainStep` expressions in order, and
        // settle on a body alternative at the leaf.
        var streamOperatorList = new List<Expr>();
        HandlerBody body = WalkHandlerTail(ctx.handlerTail(), streamOperatorList);

        // Handlers may carry a visibility modifier. Default
        // (omitted) is Public, so every handler is reachable from any caller.
        var visibility = ctx.visibility() != null
            ? ParseVisibility(ctx.visibility())
            : Visibility.Public;

        // Handlers may carry a reader/writer mode. Default
        // (omitted) is Writer — the single-threaded
        // semantics where every handler runs alone.
        var mode = ctx.handlerMode() switch
        {
            { } hm when hm.READER() != null => HandlerMode.Reader,
            { } hm when hm.WRITER() != null => HandlerMode.Writer,
            _                                => HandlerMode.Writer,
        };

        IReadOnlyList<Expr>? streamOperators = streamOperatorList.Count > 0
            ? streamOperatorList
            : null;

        return new OnHandler(Span(ctx), pattern, body, visibility, mode, streamOperators);
    }

    private HandlerBody WalkHandlerTail(HandlerTailContext tail, List<Expr> stream)
    {
        switch (tail)
        {
            case BodyBlockContext bb:
                return new BlockHandlerBody(Span(bb.block()), As<BlockStmt>(bb.block()));

            case BodyReturnContext br:
                {
                    // Inline `on X => return expr;` — wrap as a single-statement
                    // block so the emitter's on-handler return-routing handles
                    // it identically to `on X => { return expr; }`.
                    var returnStmt = As<ReturnStmt>(br.returnStmt());
                    var block = new BlockStmt(returnStmt.Span, new[] { (Stmt)returnStmt });
                    return new BlockHandlerBody(returnStmt.Span, block);
                }

            case BodyInlineContext bi:
                return new InlineHandlerBody(Span(bi.inlineExpr()), As<Expr>(bi.inlineExpr().expression()));

            case StreamChainStepContext sc:
                stream.Add(As<Expr>(sc.expression()));
                return WalkHandlerTail(sc.handlerTail(), stream);

            default:
                throw new InvalidOperationException(
                    $"Unrecognised handlerTail alternative: {tail.GetType().Name}");
        }
    }

    public override AstNode VisitCatchAllPattern(CatchAllPatternContext ctx) =>
        new CatchAllPattern(Span(ctx), ctx.softName().GetText());

    public override AstNode VisitNamedBindPattern(NamedBindPatternContext ctx) =>
        new NamedBindPattern(Span(ctx), QName(ctx.qualifiedName()), ctx.softName().GetText());

    public override AstNode VisitNoBindPattern(NoBindPatternContext ctx) =>
        new NoBindPattern(Span(ctx), QName(ctx.qualifiedName()));

    public override AstNode VisitEventPattern(EventPatternContext ctx)
    {
        var name = ctx.IDENTIFIER().GetText();
        var parms = ctx.params_() != null
            ? ctx.params_().param().Select(p => As<Param>(p)).ToList()
            : (IReadOnlyList<Param>)[];
        return new EventPattern(Span(ctx), name, parms);
    }

    public override AstNode VisitLifecycleHook(LifecycleHookContext ctx)
    {
        var evt  = As<LifecycleEvent>(ctx.lifecycleEvent());
        HandlerBody body = ctx.block() != null
            ? new BlockHandlerBody(Span(ctx.block()), As<BlockStmt>(ctx.block()))
            : new InlineHandlerBody(Span(ctx.inlineExpr()), As<Expr>(ctx.inlineExpr().expression()));
        return new LifecycleHook(Span(ctx), evt, body);
    }

    public override AstNode VisitLifecycleEvent(LifecycleEventContext ctx)
    {
        if (ctx.PRESTART()  != null) return new PreStartEvent(Span(ctx));
        if (ctx.POSTSTOP()  != null) return new PostStopEvent(Span(ctx));
        // Restore(Type id)
        var type    = TypeRef(ctx.type_());
        var binding = ctx.IDENTIFIER().GetText();
        return new RestoreEvent(Span(ctx), type, binding);
    }

    public override AstNode VisitPassivateDecl(PassivateDeclContext ctx) =>
        new PassivateDecl(Span(ctx), As<Expr>(ctx.expression()));

    public override AstNode VisitPerChildSuperviseDecl(PerChildSuperviseDeclContext ctx)
    {
        var target   = As<Expr>(ctx.expression());
        var strategy = As<SuperviseStrategy>(ctx.superviseStrategy());
        return new SuperviseDecl(Span(ctx), target, strategy);
    }

    public override AstNode VisitDefaultSuperviseDecl(DefaultSuperviseDeclContext ctx)
    {
        var strategy = As<SuperviseStrategy>(ctx.superviseStrategy());
        // Null Target means "apply to all children as the default policy."
        return new SuperviseDecl(Span(ctx), Target: null, strategy);
    }

    public override AstNode VisitSuperviseStrategy(SuperviseStrategyContext ctx)
    {
        var options = ctx.superviseOptions().superviseOption()
                        .Select(o => As<SuperviseOption>(o)).ToList();
        return ctx.ONE_FOR_ONE() != null
            ? new OneForOneStrategy(Span(ctx), options)
            : (SuperviseStrategy)new AllForOneStrategy(Span(ctx), options);
    }

    public override AstNode VisitSuperviseOption(SuperviseOptionContext ctx)
    {
        if (ctx.restartAction() != null)
        {
            var action = ctx.restartAction().RESTART() != null ? RestartAction.Restart
                       : ctx.restartAction().STOP()    != null ? RestartAction.Stop
                       : ctx.restartAction().RESUME()  != null ? RestartAction.Resume
                       : RestartAction.Escalate;
            // `on Failure(ExceptionType): Action` narrows this arm to only
            // match causes that are `cause is ExceptionType`. Without the
            // parenthesised type the arm is the untyped catch-all.
            var exceptionType = ctx.qualifiedName() is { } qn ? QName(qn) : null;
            return new OnFailureOption(Span(ctx), exceptionType, action);
        }
        // Named option `name: expression` — dispatch on the option name. Unknown
        // names parse here and are reported as CE0117 in semantics (a graceful
        // diagnostic, not a raw parse error / builder crash).
        var optionName = ctx.IDENTIFIER().GetText();
        var optionValue = As<Expr>(ctx.expression());
        return optionName switch
        {
            "maxRetries" => new MaxRetriesOption(Span(ctx), optionValue),
            "withinTime" => new WithinTimeOption(Span(ctx), optionValue),
            _            => new UnknownSuperviseOption(Span(ctx), optionName),
        };
    }

    public override AstNode VisitMethodDecl(MethodDeclContext ctx)
    {
        var vis    = ParseVisibility(ctx.visibility());
        TypeRef?  ret   = ctx.returnType().VOID() != null ? null : TypeRef(ctx.returnType().type_());
        var name   = ctx.IDENTIFIER().GetText();
        var tps    = BuildTypeParams(ctx.typeParams());
        var wcs    = BuildWhereClauses(ctx.whereClause());
        var parms  = ctx.params_() != null
            ? ctx.params_().param().Select(p => As<Param>(p)).ToList()
            : (IReadOnlyList<Param>)[];
        var isAbstract = ctx.ABSTRACT() != null;
        // A bodyless method (`abstract T M();` — or a stray `;` that semantics
        // rejects) carries a synthesized empty block; IsAbstract gates emit.
        var body = ctx.block() != null
            ? As<BlockStmt>(ctx.block())
            : new BlockStmt(Span(ctx), []);
        return new MethodDecl(Span(ctx), vis, ret, name, tps, wcs, parms, body, isAbstract);
    }

    // Module declaration. Visibility defaults to public for
    // modules (they're cross-namespace utility homes; defaulting
    // private would be surprising). Nested modules walk recursively.
    public override AstNode VisitModuleDecl(ModuleDeclContext ctx)
    {
        var vis = ctx.visibility() != null
            ? ParseVisibility(ctx.visibility())
            : Visibility.Public;
        var name = ctx.IDENTIFIER().GetText();

        var methods        = new List<MethodDecl>();
        var nestedModules  = new List<ModuleDecl>();

        foreach (var memberCtx in ctx.moduleMember())
        {
            // A module's methods use the same methodDecl as actors/classes.
            if      (memberCtx.methodDecl()  != null) methods.Add(As<MethodDecl>(memberCtx.methodDecl()));
            else if (memberCtx.moduleDecl()  != null) nestedModules.Add(As<ModuleDecl>(memberCtx.moduleDecl()));
        }

        return new ModuleDecl(Span(ctx), vis, name, methods, nestedModules);
    }

    // ─── Statements ──────────────────────────────────────────────────────────

    public override AstNode VisitBlock(BlockContext ctx)
    {
        var stmts = ctx.statement().Select(s => As<Stmt>(s)).ToList();
        return new BlockStmt(Span(ctx), stmts);
    }

    public override AstNode VisitStatement(StatementContext ctx)
    {
        if (ctx.becomeStmt()     != null) return Visit(ctx.becomeStmt());
        if (ctx.persistStmt()    != null) return Visit(ctx.persistStmt());
        if (ctx.returnStmt()     != null) return Visit(ctx.returnStmt());
        if (ctx.varDecl()        != null) return Visit(ctx.varDecl());
        if (ctx.ifStmt()         != null) return Visit(ctx.ifStmt());
        if (ctx.forStmt()        != null) return Visit(ctx.forStmt());
        if (ctx.foreachStmt()    != null) return Visit(ctx.foreachStmt());
        if (ctx.whileStmt()      != null) return Visit(ctx.whileStmt());
        if (ctx.doWhileStmt()    != null) return Visit(ctx.doWhileStmt());
        if (ctx.breakStmt()      != null) return Visit(ctx.breakStmt());
        if (ctx.continueStmt()   != null) return Visit(ctx.continueStmt());
        if (ctx.tryStmt()        != null) return Visit(ctx.tryStmt());
        if (ctx.throwStmt()      != null) return Visit(ctx.throwStmt());
        if (ctx.switchStmt()     != null) return Visit(ctx.switchStmt());
        if (ctx.expressionStmt() != null) return Visit(ctx.expressionStmt());
        throw new InvalidOperationException($"Unknown statement at {Span(ctx)}");
    }

    public override AstNode VisitBecomeStmt(BecomeStmtContext ctx) =>
        new BecomeStmt(Span(ctx), ctx.IDENTIFIER().GetText());

    public override AstNode VisitPersistStmt(PersistStmtContext ctx) =>
        new PersistStmt(Span(ctx));

    public override AstNode VisitReturnStmt(ReturnStmtContext ctx)
    {
        var val = ctx.expression() != null ? As<Expr>(ctx.expression()) : null;
        return new ReturnStmt(Span(ctx), val);
    }

    public override AstNode VisitSwitchStmt(SwitchStmtContext ctx)
    {
        var subject  = As<Expr>(ctx.expression());
        var sections = ctx.switchSection().Select(BuildSwitchSection).ToList();
        return new SwitchStmt(Span(ctx), subject, sections);
    }

    private SwitchSection BuildSwitchSection(SwitchSectionContext s)
    {
        var labels = s.switchLabel().Select(BuildCaseLabel).ToList();
        var body   = s.statement().Select(st => As<Stmt>(st)).ToList();
        return new SwitchSection(Span(s), labels, body);
    }

    private CaseLabel BuildCaseLabel(SwitchLabelContext l)
    {
        // `default:` → no pattern; `case <pattern> [when <guard>]:` reuses the
        // switch-expression pattern builder.
        if (l.DEFAULT() != null)
            return new CaseLabel(Span(l), null, null);
        var pattern = BuildPattern(l.pattern());
        var guard   = l.expression() != null ? As<Expr>(l.expression()) : null;
        return new CaseLabel(Span(l), pattern, guard);
    }

    public override AstNode VisitVarDecl(VarDeclContext ctx)
    {
        TypeRef? type = ctx.VAR() != null ? null : TypeRef(ctx.type_());
        var name = ctx.softName().GetText();
        var init = As<Expr>(ctx.expression());
        return new VarDeclStmt(Span(ctx), type, name, init, IsUsing: ctx.USING() != null);
    }

    public override AstNode VisitIfStmt(IfStmtContext ctx)
    {
        var cond = As<Expr>(ctx.expression());
        var then = As<BlockStmt>(ctx.block(0));
        Stmt? els = null;
        if (ctx.ifStmt() != null)
            els = As<IfStmt>(ctx.ifStmt());
        else if (ctx.block().Length > 1)
            els = As<BlockStmt>(ctx.block(1));
        return new IfStmt(Span(ctx), cond, then, els);
    }

    public override AstNode VisitForStmt(ForStmtContext ctx)
    {
        var init      = As<VarDeclStmt>(ctx.varDecl());
        var condition = As<Expr>(ctx.expression(0));
        var increment = As<Expr>(ctx.expression(1));
        var body      = As<BlockStmt>(ctx.block());
        return new ForStmt(Span(ctx), init, condition, increment, body);
    }

    public override AstNode VisitWhileStmt(WhileStmtContext ctx) =>
        new WhileStmt(Span(ctx), As<Expr>(ctx.expression()), As<BlockStmt>(ctx.block()));

    public override AstNode VisitForeachStmt(ForeachStmtContext ctx)
    {
        TypeRef? type = ctx.type_() != null ? TypeRef(ctx.type_()) : null;   // null = var
        var name = ctx.softName().GetText();
        var coll = As<Expr>(ctx.expression());
        var body = As<BlockStmt>(ctx.block());
        return new ForeachStmt(Span(ctx), type, name, coll, body);
    }

    public override AstNode VisitDoWhileStmt(DoWhileStmtContext ctx) =>
        new DoWhileStmt(Span(ctx), As<BlockStmt>(ctx.block()), As<Expr>(ctx.expression()));

    public override AstNode VisitBreakStmt(BreakStmtContext ctx) => new BreakStmt(Span(ctx));
    public override AstNode VisitContinueStmt(ContinueStmtContext ctx) => new ContinueStmt(Span(ctx));

    public override AstNode VisitExpressionStmt(ExpressionStmtContext ctx) =>
        new ExpressionStmt(Span(ctx), As<Expr>(ctx.expression()));

    public override AstNode VisitTryStmt(TryStmtContext ctx)
    {
        var tryBlock = As<BlockStmt>(ctx.block());
        var catches = ctx.catchClause()
            .Select(c => As<CatchClause>(c))
            .ToList();
        var finallyBlock = ctx.finallyClause() is { } fc
            ? As<BlockStmt>(fc.block())
            : null;
        return new TryStmt(Span(ctx), tryBlock, catches, finallyBlock);
    }

    public override AstNode VisitCatchClause(CatchClauseContext ctx)
    {
        TypeRef? excType = ctx.type_() is { } t ? TypeRef(t) : null;
        var binding = ctx.IDENTIFIER()?.GetText();
        Expr? when = ctx.expression() is { } w ? As<Expr>(w) : null;
        var body = As<BlockStmt>(ctx.block());
        return new CatchClause(Span(ctx), excType, binding, when, body);
    }

    public override AstNode VisitThrowStmt(ThrowStmtContext ctx)
    {
        Expr? value = ctx.expression() is { } e ? As<Expr>(e) : null;
        return new ThrowStmt(Span(ctx), value);
    }

    // ─── Expressions ─────────────────────────────────────────────────────────

    public override AstNode VisitExpression(ExpressionContext ctx)
    {
        if (ctx.lambdaExpr() != null) return Visit(ctx.lambdaExpr());
        if (ctx.assignExpr() != null) return Visit(ctx.assignExpr());
        throw new InvalidOperationException($"Unknown expression at {Span(ctx)}");
    }

    public override AstNode VisitLambdaExpr(LambdaExprContext ctx)
    {
        var parameters = BuildLambdaParams(ctx.lambdaParams());
        AstNode body = ctx.block() != null
            ? As<BlockStmt>(ctx.block())
            : As<Expr>(ctx.expression());
        return new LambdaExpr(Span(ctx), parameters, body);
    }

    private IReadOnlyList<LambdaParam> BuildLambdaParams(LambdaParamsContext ctx)
    {
        switch (ctx)
        {
            case LambdaSingleBareContext bare:
                return new[]
                {
                    new LambdaParam(Span(bare), Type: null,
                        Name: bare.IDENTIFIER().GetText())
                };
            case LambdaNoParamsContext:
                return Array.Empty<LambdaParam>();
            case LambdaParenListContext list:
                return list.lambdaParam()
                    .Select(p => new LambdaParam(
                        Span(p),
                        Type: p.type_() != null ? TypeRef(p.type_()) : null,
                        Name: p.IDENTIFIER().GetText()))
                    .ToList();
            default:
                throw new InvalidOperationException(
                    $"Unknown lambdaParams shape at {Span(ctx)}");
        }
    }

    public override AstNode VisitAssignExpr(AssignExprContext ctx)
    {
        if (ctx.assignOp() == null)
            return Visit(ctx.conditionalExpr(0));

        var left  = As<Expr>(ctx.conditionalExpr(0));
        var right = As<Expr>(ctx.conditionalExpr(1));
        var op    = ctx.assignOp().Start.Type switch
        {
            SpekLexer.ASSIGN       => AssignOp.Assign,
            SpekLexer.PLUS_ASSIGN  => AssignOp.PlusAssign,
            SpekLexer.MINUS_ASSIGN => AssignOp.MinusAssign,
            SpekLexer.STAR_ASSIGN  => AssignOp.StarAssign,
            SpekLexer.SLASH_ASSIGN => AssignOp.SlashAssign,
            SpekLexer.PERCENT_ASSIGN => AssignOp.ModAssign,
            SpekLexer.AMP_ASSIGN     => AssignOp.AndAssign,
            SpekLexer.PIPE_ASSIGN    => AssignOp.OrAssign,
            SpekLexer.CARET_ASSIGN   => AssignOp.XorAssign,
            SpekLexer.QQ_ASSIGN      => AssignOp.CoalesceAssign,
            _                      => AssignOp.Assign
        };
        return new AssignExpr(Span(ctx), left, op, right);
    }

    public override AstNode VisitConditionalExpr(ConditionalExprContext ctx)
    {
        if (ctx.expression().Length == 0)
            return Visit(ctx.coalesceExpr());
        return new ConditionalExpr(Span(ctx),
            As<Expr>(ctx.coalesceExpr()),
            As<Expr>(ctx.expression(0)),
            As<Expr>(ctx.expression(1)));
    }

    public override AstNode VisitCoalesceExpr(CoalesceExprContext ctx)
    {
        if (ctx.logicalOrExpr().Length == 1 && ctx.throwExpr() == null)
            return Visit(ctx.logicalOrExpr(0));
        var acc = ctx.logicalOrExpr().Skip(1).Aggregate(
            As<Expr>(ctx.logicalOrExpr(0)),
            (a, r) => new BinaryExpr(Span(ctx), a, BinaryOp.Coalesce, As<Expr>(r)));
        // Optional trailing `?? throw ...`.
        if (ctx.throwExpr() != null)
            acc = new BinaryExpr(Span(ctx), acc, BinaryOp.Coalesce, As<Expr>(ctx.throwExpr()));
        return acc;
    }

    public override AstNode VisitThrowExpr(ThrowExprContext ctx)
        => new ThrowExpr(Span(ctx), As<Expr>(ctx.expression()));

    public override AstNode VisitLogicalOrExpr(LogicalOrExprContext ctx)
    {
        if (ctx.logicalAndExpr().Length == 1)
            return Visit(ctx.logicalAndExpr(0));
        return ctx.logicalAndExpr().Skip(1).Aggregate(
            As<Expr>(ctx.logicalAndExpr(0)),
            (acc, r) => new BinaryExpr(Span(ctx), acc, BinaryOp.Or, As<Expr>(r)));
    }

    public override AstNode VisitLogicalAndExpr(LogicalAndExprContext ctx)
    {
        if (ctx.bitOrExpr().Length == 1)
            return Visit(ctx.bitOrExpr(0));
        return ctx.bitOrExpr().Skip(1).Aggregate(
            As<Expr>(ctx.bitOrExpr(0)),
            (acc, r) => new BinaryExpr(Span(ctx), acc, BinaryOp.And, As<Expr>(r)));
    }

    public override AstNode VisitBitOrExpr(BitOrExprContext ctx)
    {
        if (ctx.bitXorExpr().Length == 1)
            return Visit(ctx.bitXorExpr(0));
        return ctx.bitXorExpr().Skip(1).Aggregate(
            As<Expr>(ctx.bitXorExpr(0)),
            (acc, r) => new BinaryExpr(Span(ctx), acc, BinaryOp.BitOr, As<Expr>(r)));
    }

    public override AstNode VisitBitXorExpr(BitXorExprContext ctx)
    {
        if (ctx.bitAndExpr().Length == 1)
            return Visit(ctx.bitAndExpr(0));
        return ctx.bitAndExpr().Skip(1).Aggregate(
            As<Expr>(ctx.bitAndExpr(0)),
            (acc, r) => new BinaryExpr(Span(ctx), acc, BinaryOp.BitXor, As<Expr>(r)));
    }

    public override AstNode VisitBitAndExpr(BitAndExprContext ctx)
    {
        if (ctx.equalityExpr().Length == 1)
            return Visit(ctx.equalityExpr(0));
        return ctx.equalityExpr().Skip(1).Aggregate(
            As<Expr>(ctx.equalityExpr(0)),
            (acc, r) => new BinaryExpr(Span(ctx), acc, BinaryOp.BitAnd, As<Expr>(r)));
    }

    public override AstNode VisitEqualityExpr(EqualityExprContext ctx)
    {
        if (ctx.relationalExpr().Length == 1)
            return Visit(ctx.relationalExpr(0));
        // Walk token children to match operators with operands
        return FoldBinary(ctx, ctx.relationalExpr().Select(r => As<Expr>(r)).ToList(),
            ctx.children.OfType<ITerminalNode>()
               .Where(t => t.Symbol.Type is SpekLexer.EQ or SpekLexer.NEQ)
               .Select(t => t.Symbol.Type == SpekLexer.EQ ? BinaryOp.Eq : BinaryOp.Neq)
               .ToList());
    }

    public override AstNode VisitRelationalExpr(RelationalExprContext ctx)
    {
        if (ctx.typeTestExpr().Length == 1)
            return Visit(ctx.typeTestExpr(0));
        var ops = ctx.children.OfType<ITerminalNode>()
            .Where(t => t.Symbol.Type is SpekLexer.LT or SpekLexer.LTE or SpekLexer.GT or SpekLexer.GTE)
            .Select(t => t.Symbol.Type switch {
                SpekLexer.LT  => BinaryOp.Lt,
                SpekLexer.LTE => BinaryOp.Lte,
                SpekLexer.GT  => BinaryOp.Gt,
                _             => BinaryOp.Gte
            }).ToList();
        return FoldBinary(ctx, ctx.typeTestExpr().Select(r => As<Expr>(r)).ToList(), ops);
    }

    public override AstNode VisitTypeTestExpr(TypeTestExprContext ctx)
    {
        var operand = As<Expr>(ctx.shiftExpr());
        if (ctx.type_() == null)            // no `is`/`as` suffix
            return operand;
        var type = TypeRef(ctx.type_());
        if (ctx.IS() != null)
            return new TypeOpExpr(Span(ctx), TypeOpKind.Is, operand, type,
                ctx.IDENTIFIER()?.GetText());
        return new TypeOpExpr(Span(ctx), TypeOpKind.As, operand, type);
    }

    public override AstNode VisitShiftExpr(ShiftExprContext ctx)
    {
        if (ctx.additiveExpr().Length == 1)
            return Visit(ctx.additiveExpr(0));
        // Each shift op is two adjacent terminals (LT LT or GT GT). Pair them up
        // in order: a leading LT → Shl, a leading GT → Shr.
        var terms = ctx.children.OfType<ITerminalNode>()
            .Where(t => t.Symbol.Type is SpekLexer.LT or SpekLexer.GT).ToList();
        var ops = new List<BinaryOp>();
        for (int i = 0; i + 1 < terms.Count; i += 2)
            ops.Add(terms[i].Symbol.Type == SpekLexer.LT ? BinaryOp.Shl : BinaryOp.Shr);
        return FoldBinary(ctx, ctx.additiveExpr().Select(r => As<Expr>(r)).ToList(), ops);
    }

    public override AstNode VisitAdditiveExpr(AdditiveExprContext ctx)
    {
        if (ctx.multiplicativeExpr().Length == 1)
            return Visit(ctx.multiplicativeExpr(0));
        var ops = ctx.children.OfType<ITerminalNode>()
            .Where(t => t.Symbol.Type is SpekLexer.PLUS or SpekLexer.MINUS)
            .Select(t => t.Symbol.Type == SpekLexer.PLUS ? BinaryOp.Add : BinaryOp.Sub)
            .ToList();
        return FoldBinary(ctx, ctx.multiplicativeExpr().Select(r => As<Expr>(r)).ToList(), ops);
    }

    public override AstNode VisitMultiplicativeExpr(MultiplicativeExprContext ctx)
    {
        if (ctx.unaryExpr().Length == 1)
            return Visit(ctx.unaryExpr(0));
        var ops = ctx.children.OfType<ITerminalNode>()
            .Where(t => t.Symbol.Type is SpekLexer.STAR or SpekLexer.SLASH or SpekLexer.PERCENT)
            .Select(t => t.Symbol.Type switch {
                SpekLexer.STAR    => BinaryOp.Mul,
                SpekLexer.SLASH   => BinaryOp.Div,
                _                 => BinaryOp.Mod
            }).ToList();
        return FoldBinary(ctx, ctx.unaryExpr().Select(r => As<Expr>(r)).ToList(), ops);
    }

    private static Expr FoldBinary(ParserRuleContext ctx, List<Expr> operands, List<BinaryOp> ops)
    {
        var result = operands[0];
        for (int i = 0; i < ops.Count; i++)
            result = new BinaryExpr(Span(ctx), result, ops[i], operands[i + 1]);
        return result;
    }

    public override AstNode VisitUnaryExpr(UnaryExprContext ctx)
    {
        if (ctx.postfixExpr() != null)
            return Visit(ctx.postfixExpr());
        // Cast: (Type)operand  — the only unaryExpr alternative with a type_.
        if (ctx.type_() != null)
            return new TypeOpExpr(Span(ctx), TypeOpKind.Cast,
                As<Expr>(ctx.unaryExpr()), TypeRef(ctx.type_()));
        var op = ctx.Start.Type switch
        {
            SpekLexer.BANG  => UnaryOp.Not,
            SpekLexer.TILDE => UnaryOp.BitNot,
            _               => UnaryOp.Negate
        };
        var operand = As<Expr>(ctx.unaryExpr());
        return new UnaryExpr(Span(ctx), op, operand);
    }

    public override AstNode VisitPostfixExpr(PostfixExprContext ctx)
    {
        var ops = ctx.postfixOp();
        var start = 0;
        Expr result;

        // A leading run of `.name` member accesses on a softName primary is a
        // qualified name, not a member-access chain: `a.b.c` is one
        // NameExpr(a.b.c) and `a.b.c(x)` is a call on NameExpr(a.b). The
        // grammar used to encode this with a greedy `qualifiedName` primary;
        // since the SLL refactor the parse tree is a single-name atom plus
        // postfix ops and the collapse happens here instead, so AST consumers
        // see the exact same shapes as before. The run stops at the first op
        // that isn't `.name`, or whose name isn't a legal softName
        // (`x.Stop` was always a MemberAccessExpr).
        if (ctx.primaryExpr().softName() is { } atom)
        {
            var parts = new List<string> { atom.GetText() };
            var stop  = atom.Stop;
            while (start < ops.Length
                   && ops[start] is MemberAccessOpContext ma
                   && IsSoftNameToken(ma.memberName().Start.Type))
            {
                parts.Add(ma.memberName().GetText());
                stop = ma.memberName().Stop;
                start++;
            }
            var nameSpan = SpanFrom(atom.Start, stop);
            result = new NameExpr(nameSpan, new QualifiedName(nameSpan, parts));

            // `a.b.Foo<T>(x)` — a typed call ending a pure name chain used to
            // match the `typedCallExpr` primary, whose AST spans differ from a
            // postfix typed call's: the call node spans the whole
            // `a.b.Foo<T>(x)` and the receiver NameExpr covers `a.b.Foo`
            // (method name included). Reproduce that shape exactly.
            if (start < ops.Length
                && ops[start] is TypedMethodCallOpContext tc
                && IsSoftNameToken(tc.memberName().Start.Type))
            {
                var qnSpan   = SpanFrom(atom.Start, tc.memberName().Stop);
                var receiver = new NameExpr(qnSpan, new QualifiedName(qnSpan, parts));
                result = MakeCallOrAsk(SpanFrom(atom.Start, tc.Stop),
                    receiver, tc.memberName().GetText(),
                    tc.typeArgs().type_().Select(TypeRef).ToList(),
                    BuildArgs(tc.argList()));
                start++;
            }
        }
        else
        {
            result = As<Expr>(ctx.primaryExpr());
        }

        for (var i = start; i < ops.Length; i++)
        {
            var opCtx = ops[i];
            var span = Span(opCtx);
            result = opCtx switch
            {
                TypedMethodCallOpContext tmc =>
                    MakeCallOrAsk(span, result, tmc.memberName().GetText(),
                        tmc.typeArgs().type_().Select(TypeRef).ToList(),
                        BuildArgs(tmc.argList())),
                MethodCallOpContext mc =>
                    MakeCallOrAsk(span, result, mc.memberName().GetText(),
                        (IReadOnlyList<AST.TypeRef>)[],
                        BuildArgs(mc.argList())),
                MemberAccessOpContext ma =>
                    new MemberAccessExpr(span, result, ma.memberName().GetText()),
                IndexAccessOpContext ia =>
                    new IndexExpr(span, result, As<Expr>(ia.expression())),
                // null-conditional variants (?. / ?[ ) — same nodes, flag set
                NullTypedMethodCallOpContext ntmc =>
                    new MethodCallExpr(span, result, ntmc.memberName().GetText(),
                        ntmc.typeArgs().type_().Select(TypeRef).ToList(),
                        BuildArgs(ntmc.argList()), NullConditional: true),
                NullMethodCallOpContext nmc =>
                    new MethodCallExpr(span, result, nmc.memberName().GetText(),
                        (IReadOnlyList<AST.TypeRef>)[],
                        BuildArgs(nmc.argList()), NullConditional: true),
                NullMemberAccessOpContext nma =>
                    new MemberAccessExpr(span, result, nma.memberName().GetText(), NullConditional: true),
                NullIndexAccessOpContext nia =>
                    new IndexExpr(span, result, As<Expr>(nia.expression()), NullConditional: true),
                SwitchOpContext so =>
                    BuildSwitchExpr(span, result, so),
                _ => throw new InvalidOperationException($"Unknown postfix op at {span}")
            };
        }
        return result;
    }

    // `target.Ask(new Msg(..))` and `target.Ask<Reply>(new Msg(..))` lower to the
    // ask expression (reply-type inference + invisible await). Any other `.Ask`
    // shape, and every other method name, stays an ordinary method call.
    private static Expr MakeCallOrAsk(SourceSpan span, Expr target, string member,
                                      IReadOnlyList<TypeRef> typeArgs, IReadOnlyList<Expr> args)
    {
        if (member == "Ask" && typeArgs.Count <= 1 && args.Count == 1)
        {
            var reply = typeArgs.Count == 1 ? typeArgs[0] : null;
            return new AskExpr(span, target, args[0], reply);
        }
        return new MethodCallExpr(span, target, member, typeArgs, args);
    }

    private SwitchExpr BuildSwitchExpr(SourceSpan span, Expr subject, SwitchOpContext ctx)
    {
        var arms = ctx.switchArm().Select(BuildSwitchArm).ToList();
        return new SwitchExpr(span, subject, arms);
    }

    private SwitchArm BuildSwitchArm(SwitchArmContext ctx)
    {
        var pattern = BuildPattern(ctx.pattern());
        Expr? when = ctx.expression().Length > 1
            ? As<Expr>(ctx.expression(0))   // first expression is the WHEN guard
            : null;
        // The result expression is always the LAST expression in the rule.
        var result = As<Expr>(ctx.expression()[^1]);
        return new SwitchArm(Span(ctx), pattern, when, result);
    }

    private SwitchPattern BuildPattern(PatternContext ctx)
    {
        switch (ctx)
        {
            case TypePatternContext tp:
            {
                var type = TypeRef(tp.type_());
                // Bare `_` (single-part qualifiedName, no IDENTIFIER binding,
                // no type args, not nullable) is the discard pattern.
                if (type.Name.Parts.Count == 1
                    && type.Name.Parts[0] == "_"
                    && type.TypeArgs.Count == 0
                    && !type.IsNullable
                    && tp.IDENTIFIER() == null)
                    return new DiscardPattern(Span(ctx));

                var binding = tp.IDENTIFIER()?.GetText();
                return new TypePattern(Span(ctx), type, binding);
            }
            case RelationalPatternContext rp:
            {
                RelationalPatternOp op =
                    rp.LT()  != null ? RelationalPatternOp.Lt  :
                    rp.LTE() != null ? RelationalPatternOp.Lte :
                    rp.GT()  != null ? RelationalPatternOp.Gt  :
                    rp.GTE() != null ? RelationalPatternOp.Gte :
                    rp.EQ()  != null ? RelationalPatternOp.Eq  :
                                       RelationalPatternOp.Neq;
                return new RelationalPattern(Span(ctx), op, As<Expr>(rp.conditionalExpr()));
            }
            case PropertyPatternContext pp:
            {
                var subs = pp.propertySubpattern()
                    .Select(sp => new PropertySubpattern(
                        Span(sp),
                        QName(sp.qualifiedName()),
                        BuildPattern(sp.pattern())))
                    .ToList();
                return new PropertyPattern(Span(ctx), subs);
            }
            case ParenPatternContext pn:
                // Parens only affect parser precedence; AST simply
                // unwraps to the inner pattern. Emitter is responsible
                // for adding parens where C# precedence requires them.
                return BuildPattern(pn.pattern());
            case NotPatternContext np:
                return new NotPattern(Span(ctx), BuildPattern(np.pattern()));
            case AndPatternContext ap:
                return new AndPattern(Span(ctx),
                    BuildPattern(ap.pattern(0)),
                    BuildPattern(ap.pattern(1)));
            case OrPatternContext op:
                return new OrPattern(Span(ctx),
                    BuildPattern(op.pattern(0)),
                    BuildPattern(op.pattern(1)));
            case ConstPatternContext cp:
                return new ConstPattern(Span(ctx), As<Expr>(cp.conditionalExpr()));
            default:
                throw new InvalidOperationException($"Unknown pattern at {Span(ctx)}");
        }
    }

    public override AstNode VisitPrimaryExpr(PrimaryExprContext ctx)
    {
        var span = Span(ctx);
        if (ctx.newExpr()        != null) return Visit(ctx.newExpr());
        if (ctx.spawnExpr()      != null) return Visit(ctx.spawnExpr());
        if (ctx.typedCallExpr()  != null) return Visit(ctx.typedCallExpr());
        if (ctx.bareCallExpr()   != null) return Visit(ctx.bareCallExpr());
        if (ctx.SELF()           != null) return new SelfExpr(span);
        if (ctx.SENDER()    != null) return new SenderExpr(span);
        if (ctx.TRUE()      != null) return new BoolLiteralExpr(span, true);
        if (ctx.FALSE()     != null) return new BoolLiteralExpr(span, false);
        if (ctx.NULL()      != null) return new NullLiteralExpr(span);

        // Numeric/char/string literals keep their verbatim lexeme (`Raw`) so
        // the emitter passes the exact C# form through unchanged. The parsed
        // Value is best-effort (used only by hand-built test nodes and never
        // for emit of parsed literals).
        if (ctx.DECIMAL_LITERAL() != null)
        {
            var raw = ctx.DECIMAL_LITERAL().GetText();
            var hasSuffix = raw.EndsWith('m') || raw.EndsWith('M');
            return new DecimalLiteralExpr(span, ParseDecimalValue(raw), hasSuffix, raw);
        }
        if (ctx.INTEGER_LITERAL() != null)
        {
            var raw = ctx.INTEGER_LITERAL().GetText();
            return new IntLiteralExpr(span, ParseIntValue(raw), raw);
        }
        if (ctx.CHAR_LITERAL() != null)
            return new CharLiteralExpr(span, ctx.CHAR_LITERAL().GetText());
        if (ctx.STRING_LITERAL() != null)
        {
            var raw = ctx.STRING_LITERAL().GetText();
            return new StringLiteralExpr(span, raw[1..^1], raw); // Value = inner; Raw = full lexeme
        }
        // Verbatim and raw strings emit from their verbatim lexeme; Value is
        // the lexeme too (unused for parsed nodes — emit reads Raw).
        if (ctx.VERBATIM_STRING() != null)
        {
            var raw = ctx.VERBATIM_STRING().GetText();
            return new StringLiteralExpr(span, raw, raw);
        }
        if (ctx.RAW_STRING() != null)
        {
            var raw = ctx.RAW_STRING().GetText();
            return new StringLiteralExpr(span, raw, raw);
        }
        if (ctx.VERBATIM_INTERP() != null)
            return BuildInterpolated(span, ctx.VERBATIM_INTERP().GetText());
        if (ctx.INTERP_STRING() != null)
            return BuildInterpolated(span, ctx.INTERP_STRING().GetText());

        // `default` / `default(T)` — first-class now that DEFAULT is a keyword.
        if (ctx.DEFAULT() != null)
            return new DefaultExpr(span, ctx.type_() != null ? TypeRef(ctx.type_()) : null);

        // A bare name atom. Dotted chains (`a.b.c`) are postfix member
        // accesses collapsed back into a multi-part NameExpr by
        // VisitPostfixExpr; this alt only produces the single-part form.
        if (ctx.softName() != null)
            return new NameExpr(span, new QualifiedName(span, [ctx.softName().GetText()]));

        // `( ... )` — one expression is a parenthesized expr; two or more
        // (comma-separated) is a tuple literal.
        var parenExprs = ctx.expression();
        if (parenExprs is { Length: > 0 })
        {
            return parenExprs.Length == 1
                ? new ParenExpr(span, As<Expr>(parenExprs[0]))
                : new TupleExpr(span, parenExprs.Select(e => As<Expr>(e)).ToList());
        }

        throw new InvalidOperationException($"Unknown primary expression at {span}");
    }

    // Best-effort numeric value parse. Only used by hand-built test nodes —
    // parsed literals emit from their verbatim `Raw` lexeme, so an overflow
    // or exotic form falling back to 0 here never affects emitted code.
    private static long ParseIntValue(string raw)
    {
        var s = raw.Replace("_", "").TrimEnd('u', 'U', 'l', 'L');
        try
        {
            if (s.StartsWith("0x") || s.StartsWith("0X")) return Convert.ToInt64(s[2..], 16);
            if (s.StartsWith("0b") || s.StartsWith("0B")) return Convert.ToInt64(s[2..], 2);
            return long.Parse(s, System.Globalization.CultureInfo.InvariantCulture);
        }
        catch { return 0; }
    }

    private static decimal ParseDecimalValue(string raw)
    {
        var s = raw.Replace("_", "").TrimEnd('f', 'F', 'd', 'D', 'm', 'M');
        return decimal.TryParse(s, System.Globalization.NumberStyles.Float,
            System.Globalization.CultureInfo.InvariantCulture, out var v) ? v : 0m;
    }

    // ─── Interpolated strings ────────────────────────────────────────────────

    /// <summary>
    /// Splits an interpolated-string lexeme into a prefix plus alternating
    /// text / hole parts. Each hole's expression is sub-parsed as a Spek
    /// expression so it flows through the normal emitter (field/`self`
    /// rewriting, invisible-async); its verbatim `:format` / `,alignment`
    /// suffix is preserved. A hole whose contents don't parse as an
    /// expression (e.g. a `{{`/`}}` literal-brace escape) falls back to
    /// verbatim text, so we never crash or change behaviour there.
    /// </summary>
    private InterpolatedStringExpr BuildInterpolated(SourceSpan span, string lexeme)
    {
        // Prefix is $"  /  $@"  /  @$"  ; the closing " is dropped.
        var prefixLen = lexeme[0] == '$' && lexeme[1] == '"' ? 2 : 3;
        var prefix = lexeme[..prefixLen];
        var inner  = lexeme[prefixLen..^1];

        var parts = new List<InterpolationPart>();
        var text  = new System.Text.StringBuilder();

        for (var i = 0; i < inner.Length;)
        {
            if (inner[i] != '{') { text.Append(inner[i]); i++; continue; }

            if (text.Length > 0) { parts.Add(new InterpolationText(span, text.ToString())); text.Clear(); }

            var end      = FindHoleEnd(inner, i + 1);   // index of the matching '}'
            var holeText = inner[(i + 1)..end];
            i = end + 1;

            var (exprText, suffix) = SplitHole(holeText);
            var expr = ParseSpekExpression(exprText);
            parts.Add(expr is null
                ? new InterpolationText(span, "{" + holeText + "}")
                : new InterpolationHole(span, expr, suffix));
        }
        if (text.Length > 0) parts.Add(new InterpolationText(span, text.ToString()));

        return new InterpolatedStringExpr(span, prefix, parts);
    }

    /// <summary>Index of the `}` closing a hole opened just before
    /// <paramref name="start"/>, balancing nested braces and skipping embedded
    /// string literals.</summary>
    private static int FindHoleEnd(string s, int start)
    {
        var depth = 0;
        for (var i = start; i < s.Length;)
        {
            var c = s[i];
            if (c == '"') { i = SkipEmbeddedString(s, i); continue; }
            if (c == '{') depth++;
            else if (c == '}') { if (depth == 0) return i; depth--; }
            i++;
        }
        return s.Length;   // unterminated — shouldn't happen for a lexed token
    }

    /// <summary>Index just past the closing quote of the string at <paramref name="i"/>.</summary>
    private static int SkipEmbeddedString(string s, int i)
    {
        for (i++; i < s.Length; i++)
        {
            if (s[i] == '\\') { i++; continue; }
            if (s[i] == '"') return i + 1;
        }
        return i;
    }

    /// <summary>Splits a hole into its expression and verbatim suffix
    /// (`:format` / `,alignment`, including the leading separator) at the
    /// first top-level `:` or `,`.</summary>
    private static (string Expr, string Suffix) SplitHole(string hole)
    {
        var depth = 0;
        // Open `?:` conditionals at the top level: their `:` belongs to the
        // ternary, NOT the format specifier, so it must not split the hole
        // (red-team emit-F2 — a bare ternary in a hole otherwise splits at its
        // `:`, fails to re-parse, and falls through to verbatim CS8361 text).
        var ternary = 0;
        for (var i = 0; i < hole.Length;)
        {
            var c = hole[i];
            if (c == '"') { i = SkipEmbeddedString(hole, i); continue; }
            if (c is '(' or '[' or '{') { depth++; i++; continue; }
            if (c is ')' or ']' or '}') { depth--; i++; continue; }
            if (depth == 0)
            {
                // A ternary `?` — but not `?.` (null-conditional), `??`
                // (null-coalescing), or `?[` (null-conditional index).
                if (c == '?' && i + 1 < hole.Length && hole[i + 1] is not ('.' or '?' or '['))
                {
                    ternary++; i++; continue;
                }
                if (c == ':')
                {
                    // `::` is a namespace qualifier, never a format delimiter.
                    if (i + 1 < hole.Length && hole[i + 1] == ':') { i += 2; continue; }
                    if (ternary > 0) { ternary--; i++; continue; }   // ternary colon
                    return (hole[..i], hole[i..]);                    // format specifier
                }
                if (c == ',' && ternary == 0) return (hole[..i], hole[i..]);   // alignment
            }
            i++;
        }
        return (hole, "");
    }

    /// <summary>Sub-parses a hole's text as a Spek expression; null if it
    /// doesn't parse (caller falls back to verbatim text). Parses via the
    /// EOF-anchored <c>holeExpression</c> rule so the whole hole must be one
    /// expression — without the anchor, prediction could lawfully stop early
    /// and silently drop a trailing call (`p.ToString()` → `p.ToString`).
    ///
    /// Two-stage ALL(*) parse, mirroring <see cref="SpekCompiler.ParseToTree"/>:
    /// stage 1 predicts in SLL mode — the cacheable, context-free
    /// approximation, far cheaper in adaptive-prediction transients than full
    /// LL — with a bail strategy; every valid hole finishes here. Stage 2
    /// (rare) re-parses from scratch with a fresh lexer/parser in full LL,
    /// because an SLL conflict can resolve to a different alternative than
    /// full-context prediction would choose, so a bailed hole may still be a
    /// valid expression. Only when full LL also rejects does the caller take
    /// the verbatim-text fallback — the same outcome, hole by hole, as the
    /// old single-stage full-LL parse (holes never emit diagnostics).</summary>
    private static Expr? ParseSpekExpression(string text)
    {
        var lexer = new SpekLexer(CharStreams.fromString(text));
        lexer.RemoveErrorListeners();
        var parser = new SpekParser(new CommonTokenStream(lexer));
        parser.RemoveErrorListeners();
        parser.Interpreter.PredictionMode = Antlr4.Runtime.Atn.PredictionMode.SLL;
        parser.ErrorHandler = new BailErrorStrategy();
        try
        {
            var ctx = parser.holeExpression();
            return new AstBuilder().Visit(ctx.expression()) as Expr;
        }
        catch (Antlr4.Runtime.Misc.ParseCanceledException)
        {
            // SLL guessed wrong, or the hole isn't an expression — stage 2 decides.
        }

        var lexer2 = new SpekLexer(CharStreams.fromString(text));
        lexer2.RemoveErrorListeners();
        var parser2 = new SpekParser(new CommonTokenStream(lexer2));
        parser2.RemoveErrorListeners();
        var ctx2 = parser2.holeExpression();
        return parser2.NumberOfSyntaxErrors > 0 ? null : new AstBuilder().Visit(ctx2.expression()) as Expr;
    }

    // `Foo<T>(args)` — a generic call on a single bare name, no receiver.
    // The free-standing factory shape (`debounce<Reading>(500)` after
    // `using Spek.Streams`), which is the explicitly-annotated form of the
    // bare `debounce(500)`. Emitted verbatim for Roslyn to resolve, exactly
    // like BareCallExpr. Qualified generic calls (`a.b.Foo<T>(x)`) never
    // reach here since the SLL grammar refactor: they parse as a softName
    // primary plus postfix ops and take the typed-call shape in
    // VisitPostfixExpr.
    public override AstNode VisitTypedCallExpr(TypedCallExprContext ctx)
    {
        var typeArgs = ctx.typeArgs().type_().Select(TypeRef).ToList();
        return new InvocationExpr(
            Span(ctx), ctx.softName().GetText(), BuildArgs(ctx.argList()), typeArgs);
    }

    // `name(args)`: a free-standing function call with no receiver.
    public override AstNode VisitBareCallExpr(BareCallExprContext ctx)
    {
        var span   = Span(ctx);
        var callee = ctx.softName().GetText();
        var args = BuildArgs(ctx.argList());
        return new InvocationExpr(span, callee, args);
    }

    public override AstNode VisitNewExpr(NewExprContext ctx)
    {
        var typeArgs = ctx.typeArgs() != null
            ? ctx.typeArgs().type_().Select(TypeRef).ToList()
            : (IReadOnlyList<AST.TypeRef>)[];

        // Array creation. `new[] { ... }` has no qualifiedName; the typed/sized
        // forms (`new T[n]`, `new T[] { ... }`) have a qualifiedName + `[`.
        if (ctx.LBRACKET() != null)
        {
            var elems = ctx.arrayInitializer() != null
                ? ctx.arrayInitializer().expression().Select(e => As<Expr>(e)).ToList()
                : (IReadOnlyList<Expr>)[];
            if (ctx.qualifiedName() == null)
                return new ArrayExpr(Span(ctx), elems);   // implicit `new[] { ... }`

            var elemType = new AST.TypeRef(Span(ctx), QName(ctx.qualifiedName()), typeArgs);
            var size = ctx.expression() != null ? As<Expr>(ctx.expression()) : null;
            return new ArrayExpr(Span(ctx), elems, elemType, size);
        }

        // Object creation: new T(args) [ { init } ] / new T { init }.
        var type = QName(ctx.qualifiedName());
        var args = BuildArgs(ctx.argList());
        IReadOnlyList<Expr>? initializer = ctx.objectInitializer() != null
            ? ctx.objectInitializer().expression().Select(e => As<Expr>(e)).ToList()
            : null;

        return new NewExpr(Span(ctx), type, typeArgs, args, initializer);
    }

    public override AstNode VisitSpawnExpr(SpawnExprContext ctx)
    {
        var typeArgs = ctx.typeArgs().type_().Select(TypeRef).ToList();
        var args     = BuildArgs(ctx.argList());
        return new SpawnExpr(Span(ctx), typeArgs, args);
    }

    // ─── Shared ───────────────────────────────────────────────────────────────

    public override AstNode VisitQualifiedName(QualifiedNameContext ctx) =>
        QName(ctx);

    public override AstNode VisitParam(ParamContext ctx) =>
        new Param(Span(ctx), TypeRef(ctx.type_()), ctx.softName().GetText(),
            ParseParamModifier(ctx.paramModifier()));

    // Maps the optional `in` / `ref` / `out` grammar token to the
    // ParamModifier enum. Null context (no modifier) → None.
    private static ParamModifier ParseParamModifier(ParamModifierContext? ctx)
    {
        if (ctx is null)            return ParamModifier.None;
        if (ctx.REF_KW() != null)   return ParamModifier.Ref;
        if (ctx.OUT_KW() != null)   return ParamModifier.Out;
        return ParamModifier.In;    // only IN_KW remains
    }

    // Builds a call-site argument list. An argument with an
    // `in` / `ref` / `out` modifier is wrapped in a RefArgExpr so the
    // emitter can restate the C# keyword; bare arguments pass through
    // as the inner expression unchanged.
    private IReadOnlyList<Expr> BuildArgs(ArgListContext? argList)
    {
        if (argList is null) return [];
        var result = new List<Expr>(argList.arg().Length);
        foreach (var a in argList.arg())
        {
            // `out var x` — inline out-variable declaration.
            if (a.VAR() != null)
            {
                result.Add(new OutVarExpr(Span(a), a.IDENTIFIER().GetText()));
                continue;
            }

            // `out T x` — explicitly typed inline out-variable declaration.
            if (a.type_() != null)
            {
                result.Add(new OutVarExpr(
                    Span(a), a.IDENTIFIER().GetText(), TypeRef(a.type_()).ToString()));
                continue;
            }

            var inner = As<Expr>(a.expression());
            var mod   = ParseParamModifier(a.paramModifier());
            Expr value = mod == ParamModifier.None ? inner : new RefArgExpr(Span(a), mod, inner);

            // Named argument `name: value` — the `IDENTIFIER COLON` prefix.
            if (a.COLON() != null)
                value = new NamedArgExpr(Span(a), a.IDENTIFIER().GetText(), value);

            result.Add(value);
        }
        return result;
    }

    public override AstNode VisitProgramDecl(ProgramDeclContext ctx) =>
        new ProgramDecl(Span(ctx), ctx.IDENTIFIER().GetText(), As<BlockStmt>(ctx.block()));
}
