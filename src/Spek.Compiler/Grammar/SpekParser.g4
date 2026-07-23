parser grammar SpekParser;

options { tokenVocab = SpekLexer; }

// ─── 1. Top-level ────────────────────────────────────────────────────────────

file_
    : fileNamespace? declaration* EOF
    ;

// Entry point for sub-parsing an interpolation hole's text as a complete
// expression. The EOF anchor is load-bearing: without it, adaptive prediction
// may lawfully stop early (e.g. parse `p.ToString` and silently leave `()`
// unconsumed), turning a member call into a member access.
holeExpression
    : expression EOF
    ;

fileNamespace
    : NAMESPACE qualifiedName SEMICOLON
    ;

declaration
    : usingDecl
    | messageDecl
    | enumDecl
    | channelDecl
    | interfaceDecl
    | sharedDecl
    | actorDecl
    | classDecl
    | moduleDecl
    | programDecl
    ;

// ─── Enum declarations ────────────────────────────────────────────────
//
// Enums are immutable value types and are permitted as `message` field
// types (CE0010 whitelists them). Emitted as a plain C# enum in the
// generated code.
enumDecl
    : visibility? FLAGS? ENUM IDENTIFIER LBRACE enumMembers? RBRACE
    ;

enumMembers
    : enumMember (COMMA enumMember)* COMMA?
    ;

// A member may carry an explicit value: an integer literal (hex/binary/
// separators allowed) or, in a flags enum, a union of earlier members
// (`ReadWrite = Read | Write`). Value-less members auto-number (0,1,2… for
// plain enums; the next unused power of two for flags enums).
enumMember
    : IDENTIFIER (ASSIGN enumMemberValue)?
    ;

enumMemberValue
    : MINUS? INTEGER_LITERAL
    | IDENTIFIER (PIPE IDENTIFIER)*
    ;

usingDecl
    : INTEROP? USING qualifiedName SEMICOLON
    ;

// ─── Channel declarations ────────────────────────────────────────────
//
// A channel names a set of inputs (messages the implementing actor accepts)
// and emits (unprompted event messages the implementing actor may send).
// Every type referenced is a pre-existing `message` — channels don't
// re-declare payloads. See /language/channels/ for the full model.
//
// Channels may inherit from other channels via `: Base, OtherBase, ...`
// Inheritance is add-only — derived channels add inputs/
// emits to the base set but don't override or hide inherited members.
channelDecl
    : visibility? CHANNEL IDENTIFIER channelBases? LBRACE channelMember* RBRACE
    ;

channelBases
    : COLON qualifiedName (COMMA qualifiedName)*
    ;

channelMember
    : channelInput
    | channelEmits
    ;

channelInput
    : ON qualifiedName SEMICOLON
    ;

channelEmits
    : EMITS (qualifiedName | ANY) SEMICOLON
    ;

// ─── Interface declarations ──────────────────────────────────────────
//
// An `interface` is the class-side implementation contract — the method/
// property surface a `class` promises. It is the method-based sibling of
// `channel` (the message-based actor contract): both lower to a C#
// `interface`. Deliberately pre-C#-8: signatures only, never a body and
// never a field — a Spek contract declares shape, never behavior or state.
//
// An interfaceMethod ends in `;` (the only legal form). A `block` also
// parses so the analyzer can reject a default-method body with a friendly
// CE0120 instead of a raw parser error. A stray `fieldDecl` parses for the
// same reason (a field inside an interface is CE0120 too). Interfaces may
// extend other interfaces via `: Base, ...`.
interfaceDecl
    : visibility? INTERFACE IDENTIFIER typeParams? interfaceBases? whereClause* LBRACE interfaceMember* RBRACE
    ;

interfaceBases
    : COLON qualifiedName (COMMA qualifiedName)*
    ;

interfaceMember
    : interfaceMethod
    | propertyDecl
    | fieldDecl
    ;

interfaceMethod
    : visibility? returnType IDENTIFIER typeParams? LPAREN params_? RPAREN whereClause* (SEMICOLON | block)
    ;

// ─── 2. Message declarations ─────────────────────────────────────────────────

// `abstract message` is the polymorphic dispatch contract — a family base a
// handler keys on (`on ClusterEvent` receives every variant). A variant names
// its base after the field list: `message NodeUp(string Node) : ClusterEvent`.
// Lowers to abstract/derived C# records. Base-carries-fields is deferred: an
// abstract base must be empty (CE0125).
messageDecl
    : ABSTRACT? MESSAGE IDENTIFIER typeParams? LPAREN messageFields? RPAREN messageBase? SEMICOLON
    ;

messageBase
    : COLON qualifiedName
    ;

messageFields
    : messageField (COMMA messageField)*
    ;

messageField
    : type_ softName defaultValue?
    ;

defaultValue
    : ASSIGN expression
    ;

// ─── 3. Actor declarations ───────────────────────────────────────────────────

actorDecl
    : visibility? ABSTRACT? ACTOR IDENTIFIER typeParams? baseActor?
      whereClause* LBRACE actorMember* RBRACE
    ;

visibility
    : PUBLIC | INTERNAL | PROTECTED | PRIVATE
    ;

// After the COLON, an actor lists one or more names — the first may be
// a base actor, additional names are channel implementations. Semantic
// analysis disambiguates which is which based on the declared kind of
// each name.
baseActor
    : COLON qualifiedName (COMMA qualifiedName)*
    ;

// ─── Class declarations ───────────────────────────────────────────────
//
// A `class` is a mutable, single-owner instance type — the genuinely-new
// "mutable but not concurrent" kind from the type/ownership model. It holds
// fields, an optional `init(params)` constructor, and methods, and lowers to a
// plain C# instance class. There's no capability marker: mutability of the
// declaration is the signal, ownership is inferred (CE0085/CE0087). Members
// reuse the actor-side `fieldDecl` / `initBlock` / `methodDecl` rules. The
// `typeParams?` slot is accepted for the generics work; generic semantics
// aren't wired yet.
classDecl
    : visibility? ABSTRACT? CLASS IDENTIFIER typeParams? classBases? whereClause* LBRACE classMember* RBRACE
    ;

// After the COLON a class lists one or more names: an optional base class
// (first) followed by implemented interfaces. Semantic analysis disambiguates
// which name is which from each name's declared kind, mirroring `baseActor`.
classBases
    : COLON qualifiedName (COMMA qualifiedName)*
    ;

classMember
    : fieldDecl
    | initBlock
    | propertyDecl
    | methodDecl
    ;

// Property: `public int X { get; set; }`, `int Y { get; init; } = 0;`,
// `int Z { get => _z; }`. The accessor names (get/set/init) are contextual —
// matched as identifiers and validated in the builder, so they never collide
// with C# member names like `Get()`.
propertyDecl
    : visibility? type_ IDENTIFIER LBRACE propertyAccessor+ RBRACE (ASSIGN expression SEMICOLON)?
    ;

propertyAccessor
    : visibility? (IDENTIFIER | INIT) (ARROW expression)? SEMICOLON
    ;

// ─── 4. Actor members ────────────────────────────────────────────────────────

// `onHandler` is now also an actorMember alternative.
// Bare on-handlers at actor scope (without an enclosing
// `behavior X { ... }` wrapper) fold into a synthesised
// behavior named "Default" by the AST builder. Single-behavior
// actors don't need to write the wrapper; the name "Default"
// is chosen so stack traces and supervision messages remain
// readable. Mixed mode (some bare, some inside `behavior X {}`)
// is allowed — the bare handlers go into Default; explicit
// blocks stay separate.
//
// `useDecl` attaches a shared region to an actor.
actorMember
    : fieldDecl
    | initBlock
    | termBlock
    | behaviorDecl
    | onHandler
    | useDecl
    | lifecycleHook
    | passivateDecl
    | superviseDecl
    | methodDecl
    ;

// shared regions are per-`ActorSystem` state with their own
// reader/writer lock, separate from per-actor locks. An actor
// attaches one with `use <RegionType> <localName>;` inside its
// body. Phase 1 supported only field lists; Phase 2 added the
// optional `init { ... }` block which runs once on first access.
// Phase 3 adds optional capability inheritance via the colon
// suffix (`: Persisted` etc.); without it the region is transient
// (in-memory, dies with the process).
sharedDecl
    : visibility? SHARED IDENTIFIER (COLON IDENTIFIER)? LBRACE sharedMember* RBRACE
    ;

sharedMember
    : fieldDecl
    | sharedInit
    | sharedTerm
    ;

sharedInit
    : INIT block
    ;

// `term { ... }` is the disposal counterpart to `init`. Runs at
// `ActorSystem` shutdown in reverse construction order; triggers
// `IAsyncDisposable` emission on the region class. Same scope rules
// as `init`: no `Tell`, `ask`, `become`, `persist` — this is for
// resource release, not for further state mutation or messaging.
sharedTerm
    : TERM block
    ;

useDecl
    : USE IDENTIFIER IDENTIFIER SEMICOLON
    ;

// `transient` opts a field out of `: Persisted` capture/restore.
// On a non-persisted region or actor it parses but is a no-op (the
// emitter ignores it), so the keyword cost is purely additive.
//
// `deprecated` and `retired` are field-lifecycle markers,
// inspired by gRPC's reserved/deprecated mechanism. Fields stay in
// the source forever — never deleted — but their usage is policed:
//   * `deprecated` — references compile but emit a CE0101 warning.
//     The data is still captured/restored so existing snapshots roundtrip.
//   * `retired`    — references are a hard CE0102 error. The field
//     is skipped from capture/restore so persistence stores can drop
//     the key on the next save.
// The three modifiers are mutually exclusive — a field is one of:
// normal, transient, deprecated, or retired.
fieldDecl
    : visibility? (TRANSIENT | DEPRECATED | RETIRED)? type_ softName (ASSIGN expression)? SEMICOLON
    ;

// A constructor. `: base(args)` chains to a base-class constructor when the
// class extends an abstract base with a parameterized `init` — the C# idiom.
initBlock
    : INIT LPAREN params_? RPAREN baseInit? block
    ;

baseInit
    : COLON BASE LPAREN argList? RPAREN
    ;

// `term { ... }` block on an actor. Runs at the end of the
// stop sequence, after `on PostStop`, before the actor reference is
// invalidated. Triggers `IAsyncDisposable` emission on the actor
// class. Same scope rules as `init`: no `Tell`, `ask`, `become`,
// `persist`.
termBlock
    : TERM block
    ;

behaviorDecl
    : ABSTRACT? OVERRIDE? BEHAVIOR IDENTIFIER LBRACE onHandler* RBRACE
    ;

// `onHandler` allows zero or more `=>` chain steps before the
// final body. Each chain step is an expression that evaluates to a
// `Spek.Streams.StreamOperator<T>` — typically a factory call like
// `debounce(500)`. The compiler wires the operators left-to-right
// and routes dispatch through them before the body runs.
//
// The single leading `ARROW` between the pattern and the tail belongs
// to `onHandler`. `handlerTail` is right-recursive on `streamChainStep`
// so the parser can disambiguate by lookahead: a leading `{` or `return`
// is the body; an expression followed by `=>` is a chain step that
// recurses into another tail.
onHandler
    : visibility? handlerMode? ON messagePattern ARROW handlerTail
    ;

handlerTail
    : block                                # bodyBlock
    | returnStmt                           # bodyReturn
    | expression ARROW handlerTail         # streamChainStep
    | inlineExpr SEMICOLON                 # bodyInline
    ;

handlerMode
    : READER | WRITER
    ;

messagePattern
    : ANY softName                                         # catchAllPattern
    | EVENT IDENTIFIER LPAREN params_? RPAREN              # eventPattern
    | qualifiedName softName                               # namedBindPattern
    | qualifiedName                                        # noBindPattern
    ;

lifecycleHook
    : ON lifecycleEvent ARROW (block | inlineExpr SEMICOLON)
    ;

lifecycleEvent
    : PRESTART
    | POSTSTOP
    | RESTORE LPAREN type_ IDENTIFIER RPAREN
    ;

passivateDecl
    : PASSIVATE AFTER expression SEMICOLON
    ;

superviseDecl
    : SUPERVISE LPAREN expression COMMA STRATEGY COLON superviseStrategy RPAREN SEMICOLON   # perChildSuperviseDecl
    | SUPERVISE superviseStrategy SEMICOLON                                                  # defaultSuperviseDecl
    ;

superviseStrategy
    : ONE_FOR_ONE LPAREN superviseOptions RPAREN
    | ALL_FOR_ONE LPAREN superviseOptions RPAREN
    ;

superviseOptions
    : superviseOption (COMMA superviseOption)*
    ;

superviseOption
    : ON FAILURE (LPAREN qualifiedName RPAREN)? COLON restartAction
    // Named options (maxRetries, withinTime) are plain named arguments — the same
    // `IDENTIFIER COLON expression` shape as any call. The option name is validated
    // in semantics (CE0117), not the grammar, so a typo is a real diagnostic rather
    // than a raw parse error, and `maxRetries`/`withinTime` aren't reserved words.
    | IDENTIFIER COLON expression
    ;

restartAction
    : RESTART | STOP | ESCALATE | RESUME
    ;

// `abstract` marks a method with no body — the subclass must implement it (only
// valid inside an `abstract class` / `abstract actor`; otherwise CE0122). The
// bodyless `;` form is meant for abstract methods; the subclass writes a plain
// method to implement an inherited abstract one and the emitter infers the
// `override`, so there is no `virtual`/`override` keyword in Spek.
methodDecl
    : visibility? ABSTRACT? returnType IDENTIFIER typeParams? LPAREN params_? RPAREN whereClause* (block | SEMICOLON)
    ;

returnType
    : VOID
    | type_
    ;

// Modules are stateless method containers, equivalent in
// shape to C# static classes. Visibility / member visibility /
// method signatures match C# class conventions exactly; only the
// body content (Spek statements vs C# statements) tells which
// language you're reading. Modules may nest (lexical sub-namespacing
// like Erlang modules); they cannot contain mutable state.
moduleDecl
    : visibility? MODULE IDENTIFIER LBRACE moduleMember* RBRACE
    ;

// A module body holds methods (same `methodDecl` as actors/classes — there is
// ONE method concept in Spek) and nested modules. A module's methods become C#
// `static` methods on the emitted static class; the dev never writes `static`.
moduleMember
    : methodDecl
    | moduleDecl       // nested modules — sub-namespacing
    ;

// ─── 5. Statements ───────────────────────────────────────────────────────────

block
    : LBRACE statement* RBRACE
    ;

inlineExpr
    : expression
    ;

statement
    : becomeStmt
    | persistStmt
    | returnStmt
    | varDecl
    | ifStmt
    | forStmt
    | foreachStmt
    | whileStmt
    | doWhileStmt
    | breakStmt
    | continueStmt
    | tryStmt
    | throwStmt
    | switchStmt
    | expressionStmt
    ;

// C-style switch statement: value-less multi-way branch with case/default
// labels and statement blocks (each ending in break/return/etc., enforced by
// Roslyn). Case labels reuse the same `pattern` rule as the switch expression.
switchStmt
    : SWITCH LPAREN expression RPAREN LBRACE switchSection* RBRACE
    ;

switchSection
    : switchLabel+ statement+
    ;

switchLabel
    : CASE pattern (WHEN expression)? COLON
    | DEFAULT COLON
    ;

becomeStmt
    : BECOME IDENTIFIER SEMICOLON
    ;

persistStmt
    : PERSIST SEMICOLON
    ;

tryStmt
    : TRY block catchClause+ finallyClause?
    | TRY block finallyClause
    ;

catchClause
    : CATCH (LPAREN type_ IDENTIFIER? RPAREN)? (WHEN LPAREN expression RPAREN)? block
    ;

finallyClause
    : FINALLY block
    ;

throwStmt
    : THROW expression? SEMICOLON
    ;

returnStmt
    : RETURN expression? SEMICOLON
    ;

varDecl
    : USING? (VAR | type_) softName ASSIGN expression SEMICOLON
    ;

ifStmt
    : IF LPAREN expression RPAREN block (ELSE (ifStmt | block))?
    ;

forStmt
    : FOR LPAREN varDecl expression SEMICOLON expression RPAREN block
    ;

whileStmt
    : WHILE LPAREN expression RPAREN block
    ;

// `foreach (var x in coll) { ... }`. The loop variable may be `var` or
// an explicit type; `coll` is any expression. Lowers verbatim to C# foreach.
foreachStmt
    : FOREACH LPAREN (VAR | type_) softName IN_KW expression RPAREN block
    ;

doWhileStmt
    : DO block WHILE LPAREN expression RPAREN SEMICOLON
    ;

breakStmt
    : BREAK SEMICOLON
    ;

continueStmt
    : CONTINUE SEMICOLON
    ;

expressionStmt
    : expression SEMICOLON
    ;

// ─── 6. Expressions ──────────────────────────────────────────────────────────

expression
    : lambdaExpr
    | assignExpr
    ;

// lambda expressions, in the style of C#. Captures and type
// inference are delegated to Roslyn at the C# emit layer; Spek emits
// the lambda verbatim. Three parameter shapes are recognised:
//
//   x => body                       single bare parameter
//   (x, y) => body                  parenthesised parameter list
//   (int x, int y) => body          typed parenthesised list
//   () => body                      no parameters
//
// The body is either an expression or a block. Lambdas are first-class
// expressions and may appear anywhere `expression` is valid (call args,
// var initialisers, switch arms, etc.).
//
// `async` and `static` modifiers are not supported. LINQ query
// syntax (`from x in xs select ...`) is also not in scope; LINQ method
// chains compose from these lambdas plus existing call expressions.
lambdaExpr
    : lambdaParams ARROW (block | expression)
    ;

lambdaParams
    : IDENTIFIER                                          # lambdaSingleBare
    | LPAREN RPAREN                                       # lambdaNoParams
    | LPAREN lambdaParam (COMMA lambdaParam)* RPAREN      # lambdaParenList
    ;

lambdaParam
    : type_? IDENTIFIER
    ;

assignExpr
    : conditionalExpr (assignOp conditionalExpr)?
    ;

assignOp
    : ASSIGN | PLUS_ASSIGN | MINUS_ASSIGN | STAR_ASSIGN | SLASH_ASSIGN
    | PERCENT_ASSIGN | AMP_ASSIGN | PIPE_ASSIGN | CARET_ASSIGN | QQ_ASSIGN
    ;

conditionalExpr
    : coalesceExpr (QUESTION expression COLON expression)?
    ;

// ?? — binds tighter than ?: , looser than || (matches C# precedence).
// The optional trailing `?? throw ...` is a C# throw expression in the most
// common position (null-or-throw); the `throw` only ever ends the chain.
coalesceExpr
    : logicalOrExpr (QQ logicalOrExpr)* (QQ throwExpr)?
    ;

// throw expression (C# `x ?? throw new ...`). Currently only the
// null-coalescing right-hand side; emitted verbatim as `throw <expr>`.
throwExpr
    : THROW expression
    ;

logicalOrExpr
    : logicalAndExpr (OR logicalAndExpr)*
    ;

logicalAndExpr
    : bitOrExpr (AND bitOrExpr)*
    ;

// Bitwise | ^ & sit between && and equality, in C#'s precedence order
// (& tightest, | loosest of the three).
bitOrExpr
    : bitXorExpr (PIPE bitXorExpr)*
    ;

bitXorExpr
    : bitAndExpr (CARET bitAndExpr)*
    ;

bitAndExpr
    : equalityExpr (AMP equalityExpr)*
    ;

equalityExpr
    : relationalExpr ((EQ | NEQ) relationalExpr)*
    ;

relationalExpr
    : typeTestExpr ((LTE | GTE | LT | GT) typeTestExpr)*
    ;

// x is Type / x is Type binding / x as Type. Relational-ish precedence,
// matched as a single optional suffix on a shift expression.
typeTestExpr
    : shiftExpr (IS type_ IDENTIFIER? | AS type_)?
    ;

// Shift << >> matched as adjacent LT LT / GT GT so '>>' never becomes a
// single token (would break nested generics). See AstBuilder.VisitShiftExpr.
shiftExpr
    : additiveExpr ((LT LT | GT GT) additiveExpr)*
    ;

// NB: relationalExpr → typeTestExpr → shiftExpr (typeTestExpr inserted above).

additiveExpr
    : multiplicativeExpr ((PLUS | MINUS) multiplicativeExpr)*
    ;

multiplicativeExpr
    : unaryExpr ((STAR | SLASH | PERCENT) unaryExpr)*
    ;

unaryExpr
    : LPAREN type_ RPAREN unaryExpr      // cast: (Type)x  (see AstBuilder.VisitUnaryExpr)
    | (BANG | MINUS | TILDE) unaryExpr
    | postfixExpr
    ;

postfixExpr
    : primaryExpr postfixOp*
    ;

postfixOp
    : DOT memberName typeArgs LPAREN argList? RPAREN     # typedMethodCallOp
    | DOT memberName LPAREN argList? RPAREN              # methodCallOp
    | DOT memberName                                     # memberAccessOp
    | LBRACKET expression RBRACKET                       # indexAccessOp
    | QUESTION DOT memberName typeArgs LPAREN argList? RPAREN   # nullTypedMethodCallOp
    | QUESTION DOT memberName LPAREN argList? RPAREN     # nullMethodCallOp
    | QUESTION DOT memberName                            # nullMemberAccessOp
    | QUESTION LBRACKET expression RBRACKET              # nullIndexAccessOp
    | SWITCH LBRACE switchArm (COMMA switchArm)* COMMA? RBRACE  # switchOp
    ;

// A member name after `.` may be an identifier OR a contextual keyword. After a dot
// the position is unambiguous, so `FailureDirective.Stop`, `x.message`, `x.event`,
// and `x.Stop()` all parse.
memberName
    : IDENTIFIER
    | RESTART | STOP | ESCALATE | RESUME
    | ACTOR | MESSAGE | EVENT | READER | WRITER
    | STRATEGY | CHANNEL | INTERFACE | AFTER | SHARED | USE | FLAGS
    ;

// Soft keywords usable as ordinary identifiers in name positions (variable / parameter /
// field declarations, references, bare calls). These are only structurally meaningful as
// declaration starters (`actor`/`message`/`channel`, all top-level) or in fixed contextual
// sequences (`passivate after`, `strategy:`); everywhere a plain name is expected they're
// just identifiers. Handler-position keywords (event/reader/writer) are deliberately left
// out to avoid ambiguity with `on event` / `reader on` / `writer on`.
softName
    : IDENTIFIER
    | ACTOR | MESSAGE | CHANNEL | INTERFACE | AFTER | STRATEGY | RESUME | FLAGS
    ;

switchArm
    : pattern (WHEN expression)? ARROW expression
    ;

// Patterns:
//   type pattern with optional binding   (`Foo`, `Foo b`)
//   constant pattern                     (`1`, `"x"`, `MyEnum.Value`)
//   discard pattern                      (`_`) — tokenized as IDENTIFIER and
//                                                resolved by the AST builder.
//   relational pattern                   (`> 0`, `<= 100`, `== "x"`)
//   property pattern                     (`{ X: pat, A.B: pat }`, `{ }`)
//   negation                             (`not pat`)
//   conjunction                          (`pat and pat`)
//   disjunction                          (`pat or pat`)
//   parenthesised pattern                (`(pat)`)
// Tuple/list patterns are deferred to a follow-up release.
//
// Precedence is ANTLR4 alt-order: top alternatives bind tighter. Atoms
// (type/relational/property/paren) are the tightest, then `not`, then
// `and`, then `or`, with `constPattern` (a fallback for any
// conditionalExpr) at the bottom.
pattern
    : type_ IDENTIFIER?                                                      # typePattern
    | (LT | LTE | GT | GTE | EQ | NEQ) conditionalExpr                       # relationalPattern
    | LBRACE (propertySubpattern (COMMA propertySubpattern)* COMMA?)? RBRACE # propertyPattern
    | LPAREN pattern RPAREN                                                  # parenPattern
    | KW_NOT pattern                                                         # notPattern
    | pattern KW_AND pattern                                                 # andPattern
    | pattern KW_OR pattern                                                  # orPattern
    | conditionalExpr                                                        # constPattern
    ;

propertySubpattern
    : qualifiedName COLON pattern
    ;

// Expression-side names are a single `softName` atom; dotted chains
// (`a.b.c`, `a.b.c(x)`, `a.b.Foo<T>(x)`) assemble exclusively in
// `postfixExpr` via `DOT`-consuming postfix ops. Only ONE loop may compete
// for `DOT` in expression position — when `qualifiedName` (with its own
// greedy `(DOT softName)*` loop) sat here, deciding where the name stopped
// and the postfix chain began required full-context prediction, which made
// SLL prediction bail on ordinary source like `Pricing.Stamp(x)`.
// `qualifiedName` remains the rule for type positions and declarations,
// where nothing else consumes `DOT`. AstBuilder.VisitPostfixExpr collapses
// the leading member-access run back into a single NameExpr/QualifiedName,
// so the AST shape is unchanged.
primaryExpr
    : newExpr
    | spawnExpr
    | typedCallExpr   // single-name generic call — must precede softName
    | bareCallExpr    // single-identifier function call — must precede softName
    | DECIMAL_LITERAL
    | INTEGER_LITERAL
    | CHAR_LITERAL
    | RAW_STRING
    | VERBATIM_STRING
    | VERBATIM_INTERP
    | STRING_LITERAL
    | INTERP_STRING
    | TRUE
    | FALSE
    | NULL
    | SELF
    | SENDER
    | softName
    | DEFAULT LPAREN type_ RPAREN                     // default(T)
    | DEFAULT                                          // bare default literal
    | LPAREN expression (COMMA expression)+ RPAREN   // tuple literal: (a, b) — needs a comma
    | LPAREN expression RPAREN
    ;

// Generic call on a single bare name: `Foo<T>(args)`. Without this alt the
// tokens would silently parse as chained relational operators
// (`Foo < T > (args)`). Qualified generic calls (`a.b.Foo<T>(args)`) parse
// as a softName primary plus postfix ops ending in `typedMethodCallOp`.
typedCallExpr
    : softName typeArgs LPAREN argList? RPAREN
    ;

// bare function call: `f(args)` with a single, unqualified callee.
// Enables free-standing factory functions imported via `using` (e.g.
// `debounce(500)` after `using Spek.Streams`). Multi-part callees like
// `StreamOperators.debounce(x)` continue to parse as primary + methodCallOp
// (MethodCallExpr) so the existing `self.Tell(...)` rewrite still fires.
bareCallExpr
    : softName LPAREN argList? RPAREN
    ;

newExpr
    : NEW qualifiedName typeArgs? LPAREN argList? RPAREN objectInitializer?  // new T(args) [ { init } ]
    | NEW qualifiedName typeArgs? objectInitializer                          // new T { init }
    | NEW qualifiedName typeArgs? LBRACKET expression? RBRACKET arrayInitializer?  // new T[n] | new T[] { ... }
    | NEW LBRACKET RBRACKET arrayInitializer                                 // implicit-typed array: new[] { e1, e2 }
    ;

// implicit-typed array creation `new[] { ... }`. The element type is
// inferred by Roslyn (Spek emits it verbatim). A trailing comma is allowed.
arrayInitializer
    : LBRACE (expression (COMMA expression)*)? COMMA? RBRACE
    ;

// object / collection initializer braces: `new T { a, b }` or
// `new T { Prop = v }` (an assignment is just an expression here). Roslyn
// decides object-vs-collection init; Spek emits the braces verbatim.
objectInitializer
    : LBRACE (expression (COMMA expression)*)? COMMA? RBRACE
    ;

spawnExpr
    : SPAWN typeArgs LPAREN argList? RPAREN
    ;

argList
    : arg (COMMA arg)*
    ;

// at call sites the user must restate the modifier (`Foo(ref x)`,
// `Foo(in x)`). Semantics mirror C# — making the modifier mandatory at
// the call site is part of the safety story (the reader sees that this
// parameter may be aliased / written to). The second alternative is the
// inline out-variable declaration `Foo(out var y)` — `y` is introduced
// as a local for the rest of the enclosing scope (C# out-var semantics).
// the optional `IDENTIFIER COLON` prefix is a C# named argument
// (`Foo(width: 3)`). `IDENTIFIER COLON` after an arg-start is unambiguous —
// it can't begin a positional expression — so the optional is safe.
arg
    : (IDENTIFIER COLON)? paramModifier? expression
    | OUT_KW VAR IDENTIFIER
    | OUT_KW type_ IDENTIFIER
    ;

// ─── 7. Types ────────────────────────────────────────────────────────────────

type_
    : qualifiedName typeArgs? QUESTION? (LBRACKET RBRACKET)*   // array types: T[], T[][]
    ;

typeArgs
    : LT type_ (COMMA type_)* GT
    ;

typeParams
    : LT typeParam (COMMA typeParam)* GT
    ;

typeParam
    : IDENTIFIER
    ;

// generic type-parameter constraints. Mirrors C#: `where T : class`,
// `where T : new()`, `where T : Base`, `where T : IFace<U>`, `where T : U`.
// `struct`/`notnull`/`unmanaged` aren't keywords, so they parse as a bare
// `type_` and emit verbatim. Multiple clauses (one per type parameter) and
// multiple comma-separated constraints per clause are allowed. Lowered
// verbatim to C# — Roslyn enforces them.
whereClause
    : WHERE IDENTIFIER COLON typeConstraint (COMMA typeConstraint)*
    ;

typeConstraint
    : CLASS               # classConstraint
    | NEW LPAREN RPAREN   # newConstraint
    | type_               # typeRefConstraint
    ;

qualifiedName
    : softName (DOT softName)*
    ;

params_
    : param (COMMA param)*
    ;

param
    : paramModifier? type_ softName
    ;

// optional `in` / `ref` / `out` modifier. Semantics mirror C#
// exactly: `in` is a readonly reference, `ref` requires the argument
// to already be assigned and lets the callee mutate, `out` requires
// the callee to assign and is treated as definitely-assigned on
// return. The semantic analyzer enforces a value-vs-modifier rule
// at call sites; the emitter forwards the modifier into the C# method
// signature.
paramModifier
    : IN_KW
    | REF_KW
    | OUT_KW
    ;

// ─── 9. Program entry point ──────────────────────────────────────────────────

programDecl
    : PROGRAM IDENTIFIER block
    ;
