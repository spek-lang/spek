# Spek v1 grammar: PEG specification

This document defines the formal Parsing Expression Grammar (PEG) for Spek
v1.x. PEG grammars are deterministic and unambiguous by definition; the
`/` operator is ordered choice, not alternation. The first matching
alternative wins.


## Notation

| Symbol      | Meaning                                      |
|-------------|----------------------------------------------|
| `←`         | Rule definition                              |
| `e1 e2`     | Sequence                                     |
| `e1 / e2`   | Ordered choice (try e1 first, then e2)       |
| `e*`        | Zero or more                                 |
| `e+`        | One or more                                  |
| `e?`        | Optional                                     |
| `!e`        | Negative lookahead (e must not match)        |
| `&e`        | Positive lookahead (e must match, no consume)|
| `'x'`       | Literal string                               |
| `[a-z]`     | Character class                              |
| `(e)`       | Grouping                                     |


## 1. Top-level structure

```peg
File            ← Spacing FileNamespace? Declaration* EOF

FileNamespace   ← 'namespace' Spacing QualifiedName ';' Spacing

Declaration     ← UsingDecl
                / MessageDecl
                / EnumDecl
                / ChannelDecl
                / InterfaceDecl    ← Section 19
                / SharedDecl
                / ActorDecl
                / ClassDecl        ← Section 18
                / ModuleDecl       ← Section 20
                / ProgramDecl

UsingDecl       ← ('interop' Spacing)? 'using' Spacing QualifiedName ';' Spacing
```

A `.spek` file consists of an optional file-scoped namespace declaration,
zero or more `using` imports, and zero or more top-level declarations.
This mirrors C# file-scoped namespace syntax exactly.

The `interop` modifier on `using` opts out of CE0080 (hostile-namespace
import block) and accepts CE0086 (interop bypasses safety guarantees) in
exchange; see the errors reference for the full table.


## 2. Message declarations

```peg
MessageDecl     ← AbstractMod? 'message' Spacing Identifier TypeParams?
                  '(' MessageFields? ')' MessageBase? ';' Spacing

MessageBase     ← ':' Spacing QualifiedName Spacing

MessageFields   ← MessageField (',' Spacing MessageField)*

MessageField    ← Type Spacing Identifier DefaultValue? Spacing

DefaultValue    ← '=' Spacing Expression

TypeParams      ← '<' Spacing TypeParam (',' Spacing TypeParam)* '>' Spacing

TypeParam       ← Identifier Spacing
```

Messages compile to C# `record` types. They are immutable by definition.
Generic messages (`message Response<T>(T value)`) are supported in v1.
The compiler rejects sending any type that is not declared with `message`.

An `abstract message` declares a polymorphic family base: it lowers to an
abstract record, can't be instantiated, and a handler keyed on it receives
every variant. A variant names its base after the field list. The base must
resolve to an `abstract message` (CE0124) and must be empty: the fields
live on each variant (CE0125).

**Examples:**
```
message Deposit(decimal amount, string fromUser);
message Response<T>(T value);
message Shutdown(string reason = "normal");

abstract message ClusterEvent();
message NodeUp(string node)   : ClusterEvent;
message NodeDown(string node) : ClusterEvent;
```


## 3. Actor declarations

```peg
ActorDecl       ← Visibility? AbstractMod? 'actor' Spacing Identifier
                  TypeParams? BaseList? WhereClause* '{' Spacing ActorMember* '}' Spacing

Visibility      ← ('public' / 'internal' / 'protected' / 'private') Spacing

AbstractMod     ← 'abstract' Spacing

BaseList        ← ':' Spacing QualifiedName (',' Spacing QualifiedName)* Spacing

WhereClause     ← 'where' Spacing Identifier ':' Spacing Constraint (',' Spacing Constraint)*
```

Visibility defaults to `private` (assembly-internal) when omitted, matching C# conventions.

The colon list mixes one optional **base actor** with any number of
implemented **channels**; semantic analysis classifies each name by its
declared kind (a second actor-typed name is CE0091). Only an `abstract actor`
can be a base (CE0123): concrete actors are sealed. `WhereClause` carries
C# generic constraints verbatim (`where T : IComparable<T>`, `class`,
`new()`); Roslyn type-checks them.

**Examples:**
```
actor BankAccount { ... }
public actor BankAccountManager { ... }
abstract actor AccountBase { ... }
internal actor AuditLogger { ... }
```


## 4. Actor members

```peg
ActorMember     ← FieldDecl
                / InitBlock
                / TermBlock        ← disposal counterpart to init
                / BehaviorDecl
                / OnHandler        ← bare on-handlers fold into Default
                / UseDecl          ← attaches a shared region
                / LifecycleHook
                / PersistHook
                / PassivateDecl
                / SuperviseDecl    ← failure-handling strategy
                / MethodDecl
```

Two structural rules worth calling out before the rule-by-rule walk:

- **`OnHandler` is now a top-level alternative.** Bare `on Foo => ...`
  handlers at actor scope (without an enclosing `behavior X { ... }`
  wrapper) are valid; the AST builder folds them into a synthesised
  behavior named `Default`. This means a single-behavior actor doesn't
  have to write the wrapper. Mixed mode (some bare, some inside an
  explicit behavior) is allowed but requires explicit `become Default;`
  to make the bare handlers reachable (CE0014 fires otherwise).
- **`UseDecl` attaches a shared region** declared via `SharedDecl` (see
  Section 17). Every handler in the actor acquires the region's RW lock
  around its body: reader handlers hold the reader lock, writer (default)
  handlers hold the writer lock.

### 4.1 Field declarations

```peg
FieldDecl       ← Visibility? FieldMarker? Type Spacing Identifier
                  ('=' Spacing Expression)? ';' Spacing

FieldMarker     ← ('transient' / 'deprecated' / 'retired') Spacing
```

Fields are private by default. No other actor can read or write them.
On an `abstract actor`, a `protected` field is inherited by derived actors.

The markers are mutually exclusive: `transient` opts a field out of
persistence capture/restore; `deprecated` still round-trips but references
warn (CE0101); `retired` is a hard error to reference (CE0102) and the key
is dropped on the next save: the two-step schema-retirement mechanism.
The compiler emits a CE0012 error if any expression outside the actor's own
scope attempts to access a field by name.

**Examples:**
```
decimal balance = 1000.00m;
bool isFrozen = false;
ActorRef auditLog;
```

### 4.2 Initialisation block

```peg
InitBlock       ← 'init' Spacing '(' Params? ')' BaseInit? Spacing Block Spacing

BaseInit        ← ':' Spacing 'base' '(' ArgList? ')' Spacing
```

Runs once when the actor is spawned. Typically ends with a `become` statement
to set the initial active behavior. Corresponds to the actor's constructor.
`BaseInit` chains to an abstract base's parameterized constructor: the C#
`: base(...)` idiom; it applies to classes extending an `abstract class` and
actors extending an `abstract actor`.

The disposal counterpart is the `term` block:

```peg
TermBlock       ← 'term' Spacing Block Spacing
```

It runs at the end of the stop sequence (after `on PostStop`) and triggers
`IAsyncDisposable` emission on the generated class. Same scope rules as
`init`: no `Tell`, `ask`, `become`, or `persist`: resource release only.
CE0110 warns when a disposable-looking field has no `term` block to release
it.

**Example:**
```
init(string id, ActorRef auditLogger)
{
    accountId = id;
    auditLog = auditLogger;
    become NormalOperation;
}
```

### 4.3 Behavior declarations

```peg
BehaviorDecl    ← AbstractMod? OverrideMod? 'behavior' Spacing Identifier
                  '{' Spacing OnHandler* '}' Spacing

OverrideMod     ← 'override' Spacing

OnHandler       ← Visibility? HandlerMode? 'on' Spacing MessagePattern '=>' Spacing
                  (Block / ReturnStmt / InlineExpr ';') Spacing

HandlerMode     ← ('reader' / 'writer') Spacing

MessagePattern  ← 'any' Spacing Identifier                              ← catch-all:  'on any msg =>'
                / 'event' Spacing Identifier '(' Params? ')'            ← event handler
                / QualifiedName Spacing Identifier                       ← named bind: 'on Deposit d =>'
                / QualifiedName                                          ← no bind:    'on GetBalance =>'
```

Only one behavior is active at a time. Switching is done with `become`.
The compiler verifies at compile time that every `become` target names a
behavior that is declared on the same actor (CE0011).

Unhandled messages (no matching `on` handler in the active behavior) are
routed to the dead letter queue, never silently dropped.

#### Handler modes

`reader on X` declares the handler is read-only with respect to actor
state. The runtime can run multiple reader handlers concurrently against
the same actor instance. `writer on X` is the default; writer handlers
serialise against everything. CE0087 fires if a reader handler mutates
an actor field: promote to writer or move the mutation out.

#### Event handler form

`on event Name(<C# delegate signature>) => body` is sugar for the
"actor handles a C# event" pattern. The compiler:

1. Synthesises a private message record carrying the parameters
2. Emits a bridge method named `Name` on the actor class with the
   verbatim delegate signature; the body is `_selfRef.Tell(new __Synth(args));`
3. Adds a dispatch arm that unboxes the synthetic message back into
   the user's parameter names, so the handler body sees `(s, e)`
   etc. as locals

User code wires the source via plain C# method-group conversion:
`source.SomeEvent += Name;`. CE0013 catches duplicate event-handler
names within an actor (each emits a bridge method, so collisions
would emit duplicate C# methods).

#### Visibility on handlers

`private on X` handlers are reachable only via `self.Tell(...)` from
inside the actor; messages from external senders dead-letter with a
"private handler" reason. Public/internal handlers form the actor's
external API surface and must reference a declared `message` type
(CE0096).

### 4.4 Lifecycle hooks

```peg
LifecycleHook   ← 'on' Spacing LifecycleEvent '=>' Spacing (Block / InlineExpr ';') Spacing

LifecycleEvent  ← 'PreStart'
                / 'PostStop'
                / 'Restore' Spacing '(' Type Spacing Identifier ')'
```

`PreStart` runs before the first message is processed.  
`PostStop` runs after the actor stops (graceful or supervised restart).  
`Restore` is called after both crash recovery and passivation wake-up,
receiving a `Snapshot` containing all persisted field values.

**Example:**
```
on PreStart  => Console.WriteLine($"[{actorId}] Starting");
on PostStop  => Console.WriteLine($"[{actorId}] Stopped");
on Restore(Snapshot s) =>
{
    balance = s.Get<decimal>("balance");
    isFrozen = s.Get<bool>("isFrozen");
}
```

### 4.5 Persist statement and persist hooks

```peg
PersistHook     ← 'persist' Spacing ';' Spacing
```

`persist;` is a statement valid only inside an `on` handler body.
The compiler emits CE0050 if `persist` appears outside a handler.

When executed, the runtime captures all current actor field values into
a `Snapshot` and writes it to the configured persistence backend.
All fields are included automatically.

**Example:**
```
on Deposit d =>
{
    balance += d.amount;
    persist;   // snapshot { balance, isFrozen, accountId, ... }
}
```

### 4.6 Passivate declaration

```peg
PassivateDecl   ← 'passivate' Spacing 'after' Spacing Expression ';' Spacing
```

Declares that the runtime should passivate (save and unload) this actor
after a period of message inactivity. Reactivation is transparent: the
next incoming message triggers `on Restore` before being delivered.

Passivation and durable `persist` use the same `on Restore` handler but
different triggers. The `Snapshot` parameter is identical in both cases.

**Example:**
```
passivate after System.TimeSpan.FromMinutes(30);
```

### 4.7 Helper method declarations

```peg
MethodDecl      ← Visibility? AbstractMod? ReturnType Spacing Identifier TypeParams?
                  '(' Params? ')' WhereClause* Spacing (Block / ';') Spacing
```

Private helper methods are visible only within the actor. No other actor
can call them. The compiler rejects any attempt to call an actor method
from outside the actor's own scope (CE0012). Helper methods emit as
instance methods on the generated class; `become` / `persist` inside one
is CE0051 / CE0050.

The bodyless `;` form is for `abstract` methods: legal only on an
`abstract actor` / `abstract class`, never `private` (CE0122). A derived
type implements an inherited abstract method with a *plain* method: there
is no `virtual`/`override` keyword in Spek, the emitter infers the
`override`. `TypeParams` + `WhereClause` make a method generic; both pass
through to C# verbatim.

**Example:**
```
void SaveToDatabase()
{
    // ADO.NET / EF Core logic here
}

public abstract int Transform(int x);   // on an abstract actor/class
```

### 4.8 Shared-region attachment

```peg
UseDecl         ← 'use' Spacing Identifier Spacing Identifier ';' Spacing
```

`use <RegionType> <localName>;` attaches a top-level `shared` region
(see Section 17) to the actor under a local name. The compiler emits
a lazy property that fetches the per-`ActorSystem` singleton on first
access; subsequent accesses are O(1).

The local name appears in handler bodies as `local.field` member
access. Around every handler in an actor that has any `use` decls,
the compiler wraps the body in a try/finally pair acquiring/releasing
the region's RW lock: reader handlers acquire the region's reader
lock; writer (default) handlers acquire the writer lock. Multiple
`use` decls in the same actor produce nested locks in declaration
order.

CE0097 fires if `<RegionType>` doesn't resolve to a declared
`shared` region.

**Example:**
```
actor PriceWriter
{
    use MarketCache cache;
    writer on Update u => { cache.lastPrice = u.price; }
}
```


## 5. Statements

```peg
Statement       ← BecomeStmt
                / PersistStmt
                / TellStmt
                / ReturnStmt
                / VarDecl
                / IfStmt
                / ForStmt
                / ForeachStmt
                / WhileStmt
                / DoWhileStmt
                / BreakStmt
                / ContinueStmt
                / ExpressionStmt

BecomeStmt      ← 'become' Spacing Identifier ';' Spacing

PersistStmt     ← 'persist' ';' Spacing

TellStmt        ← Expression '.' 'Tell' '(' Expression ')' ';' Spacing

ReturnStmt      ← 'return' Spacing Expression? ';' Spacing

VarDecl         ← ('var' / Type) Spacing Identifier '=' Spacing Expression ';' Spacing

IfStmt          ← 'if' Spacing '(' Expression ')' Spacing Block
                  ('else' Spacing (IfStmt / Block))? Spacing

ForStmt         ← 'for' Spacing '(' VarDecl Expression ';' Expression ')' Spacing Block Spacing

ForeachStmt     ← 'foreach' Spacing '(' ('var' / Type) Spacing Identifier 'in' Spacing Expression ')' Spacing Block Spacing

WhileStmt       ← 'while' Spacing '(' Expression ')' Spacing Block Spacing

DoWhileStmt     ← 'do' Spacing Block 'while' Spacing '(' Expression ')' ';' Spacing

BreakStmt       ← 'break' ';' Spacing

ContinueStmt    ← 'continue' ';' Spacing

ExpressionStmt  ← Expression ';' Spacing

Block           ← '{' Spacing Statement* '}' Spacing

InlineExpr      ← Expression
```

### 5.1 become

`become BehaviorName;` atomically swaps the active behavior after the
current message handler finishes. The target behavior name is resolved at
compile time: a reference to an undeclared behavior is CE0011.

### 5.2 Ask expression

`Ask` is a method on `ActorRef` whose value is the reply, not a `Task<T>`. Its
result type is inferred from the response message type, and it is only valid
inside an `on` handler body. There is no `ask` keyword: `.Ask(...)` parses as an
ordinary generic method call, which the compiler recognizes and lowers.

```peg
AskExpr         ← Expression '.' 'Ask' TypeArgs? '(' Expression ')'
```

The compiler:
1. Resolves the response type from the `new TMessage(...)` argument, or from an
   explicit `.Ask<TResponse>(...)` type argument when inference is ambiguous
2. Emits `await target.AskAsync<TResponse>(new TMessage(...))` in C#
3. Emits CE0042 if `.Ask(...)` appears outside an `on` handler context

**Example:**
```
SensorReading reading = weatherStation.Ask(new GetCurrentTemp());

// With arguments
BalanceResponse r = account.Ask(new GetBalance());
```


### 5.3 Loops and control flow

Alongside the C-style `for` and `while`, Spek has `foreach`, `do`/`while`, and
the `break` / `continue` jumps. Each lowers verbatim to the matching C#
construct, so iteration semantics, disposal of enumerators, and definite-
assignment are exactly C#'s.

<!-- spek-test: compile -->
```spek
module Stats
{
    public int SumInRange(System.Collections.Generic.List<int> xs)
    {
        int total = 0;
        foreach (var x in xs)
        {
            if (x < 0)    { continue; }
            if (x > 1000) { break; }
            total = total + x;
        }
        return total;
    }

    public int Countdown(int from)
    {
        int n = from;
        do
        {
            n = n - 1;
        }
        while (n > 0);
        return n;
    }
}
```

The `foreach` loop variable may be `var` or an explicit type. `break` and
`continue` are valid only inside a loop body; a stray one is a C# error
(`CS0139` / `CS0136`-style), surfaced through the normal Roslyn pass rather than
a Spek diagnostic.


## 6. Expressions

```peg
Expression      ← AskExpr
                / LambdaExpr      ← function value
                / AssignExpr

LambdaExpr      ← LambdaParams '=>' Spacing (Block / Expression)

LambdaParams    ← Identifier                                        ← single bare param
                / '(' Spacing ')' Spacing                           ← no params
                / '(' Spacing LambdaParam (',' Spacing LambdaParam)* ')' Spacing

LambdaParam     ← Type? Spacing Identifier

AssignExpr      ← ConditionalExpr (AssignOp Spacing ConditionalExpr)?

AssignOp        ← '=' / '+=' / '-=' / '*=' / '/='

ConditionalExpr ← CoalesceExpr ('?' Spacing Expression ':' Spacing Expression)?

CoalesceExpr    ← LogicalOrExpr ('??' Spacing LogicalOrExpr)*

LogicalOrExpr   ← LogicalAndExpr ('||' Spacing LogicalAndExpr)*

LogicalAndExpr  ← BitOrExpr ('&&' Spacing BitOrExpr)*

BitOrExpr       ← BitXorExpr ('|' Spacing BitXorExpr)*

BitXorExpr      ← BitAndExpr ('^' Spacing BitAndExpr)*

BitAndExpr      ← EqualityExpr ('&' Spacing EqualityExpr)*

EqualityExpr    ← RelationalExpr (('==' / '!=') Spacing RelationalExpr)*

RelationalExpr  ← TypeTestExpr (('<=' / '>=' / '<' / '>') Spacing TypeTestExpr)*

TypeTestExpr    ← ShiftExpr (('is' Type Identifier?) / ('as' Type))?

ShiftExpr       ← AdditiveExpr (('<' '<' / '>' '>') Spacing AdditiveExpr)*

AdditiveExpr    ← MultiplicativeExpr (('+' / '-') Spacing MultiplicativeExpr)*

MultiplicativeExpr ← UnaryExpr (('*' / '/' / '%') Spacing UnaryExpr)*

UnaryExpr       ← '(' Type ')' Spacing UnaryExpr      // cast: (Type)x
                / ('!' / '-' / '~') Spacing UnaryExpr
                / PostfixExpr

PostfixExpr     ← PrimaryExpr (MemberAccess / MethodCall / IndexAccess)*

MemberAccess    ← ('.' / '?.') Identifier

MethodCall      ← ('.' / '?.') Identifier TypeArgs? '(' ArgList? ')'

IndexAccess     ← ('[' / '?[') Expression ']'

PrimaryExpr     ← NewExpr
                / Literal
                / Identifier
                / 'self'
                / 'sender'
                / '(' Expression ')'

NewExpr         ← 'new' Spacing QualifiedName TypeArgs? '(' ArgList? ')'

ArgList         ← Expression (',' Spacing Expression)*

TypeArgs        ← '<' Spacing Type (',' Spacing Type)* '>' Spacing
```

`self` refers to the current actor's own `ActorRef`.  
`sender` refers to the `ActorRef` of the actor that sent the current message.
Both are only valid inside `on` handler bodies (CE0043 otherwise).

### 6.1 Operators

Spek's operator set mirrors C#'s, with matching precedence: arithmetic
(`+ - * / %`), comparison (`< <= > >= == !=`), logical (`&& || !`), bitwise
(`& | ^ ~`), shift (`<< >>`), and null-coalescing (`??`). Each lowers to the
identical C# operator, so semantics and overload resolution are C#'s.
Compound assignment is supported for `= += -= *= /= %= &= |= ^= ??=`.

<!-- spek-test: compile -->
```spek
module Bits
{
    // bitwise & | ^ ~, and shift << >>
    public int Pack(int hi, int lo) { return (hi << 8) | (lo & 255); }
    public int ClearLow3(int x)     { return x & ~7; }

    // ?? binds tighter than ?: and looser than || (C# precedence)
    public string DisplayName(string given) { return given ?? "anonymous"; }
}
```

Shift `>>` is matched as two adjacent `>` tokens, so it never collides with a
nested generic close such as `List<List<int>>`.

### 6.2 Type operations: conversions, `is`, `as`

`x is Type` tests a value's type (optionally capturing: `x is Type name`), and
`x as Type` does a safe reference/nullable conversion. Both lower to the
identical C# constructs. Spek has no cast operator. The parser recognizes the
C#-style `(Type)x` shape only so the compiler can reject it with CE0129 and
point to the conversion family: `x.To<T>()` when the conversion is lossless,
`x.TryTo<T>()` (returning `T?`) when it can lose information.

<!-- spek-test: compile -->
```spek
module Coerce
{
    public long Widen(int n)        { return n.To<long>(); }
    public bool IsText(object o)    { return o is string; }
    public int  TextLen(object o)
    {
        if (o is string s) { return s.Length; }
        return 0;
    }
    public string OrEmpty(object o)
    {
        string s = o as string;
        return s == null ? "" : s;
    }
}
```

**Parse caveat:** a parenthesized name immediately before a `-`, `~`, or `!`
expression is parsed as the cast shape (`(T)-x`) and rejected with CE0129, even
when the name is actually a value rather than a type. Write the subtraction
without wrapping the left operand in parens.

### 6.3 Null-conditional access

`?.` (member or method) and `?[` (index) short-circuit to `null` when the
receiver is `null`, exactly as in C#. They chain, and pair naturally with `??`.

<!-- spek-test: compile -->
```spek
module Safe
{
    public int NameLen(string s)   { return s?.Length ?? 0; }
    public string Upper(string s)  { return s?.ToUpper(); }
    public string Head(System.Collections.Generic.List<string> xs)
    {
        return xs?[0]?.ToUpper();
    }
}
```


## 7. Spawn expression

```peg
SpawnExpr       ← 'spawn' TypeArgs '(' ArgList? ')'
```

Creates a new child actor under the current actor's supervision scope.
Returns an `ActorRef`. The type argument names the actor type to instantiate.

**Example:**
```
ActorRef logger = spawn<LoggerActor>("logger");
ActorRef account = spawn<BankAccount>("acc-001", logger);
```


## 8. Types

```peg
Type            ← QualifiedName TypeArgs?
                / 'var'

QualifiedName   ← Identifier ('.' Identifier)*

Params          ← Param (',' Spacing Param)*

Param           ← Type Spacing Identifier

ReturnType      ← 'void' / Type
```

Types are C# types. The Spek compiler resolves them against the .NET type
system via the Roslyn API during semantic analysis. Any valid .NET type
can appear as a field type or method return type. Only `message`-declared
types may cross actor boundaries via `Tell` or `Ask`.


## 9. ActorSystem and program entry point

```peg
ProgramDecl     ← 'program' Spacing Identifier Spacing Block Spacing
```

The `program` block is the entry point. It compiles to a C# `static async Task Main`.

**Example:**
```
program Main
{
    using var system = new ActorSystem("MySystem");
    var root = system.Spawn<RootSupervisor>("root");
    system.AwaitTermination();
}
```


## 10. Supervision

```peg
SuperviseDecl   ← 'supervise' '(' SuperviseTarget ',' 'strategy' ':' SuperviseStrategy ')' ';' Spacing

SuperviseTarget ← Expression

SuperviseStrategy ← 'OneForOne' '(' SuperviseOptions ')'
                  / 'AllForOne' '(' SuperviseOptions ')'

SuperviseOptions ← SuperviseOption (',' Spacing SuperviseOption)*

SuperviseOption  ← 'on' Spacing 'Failure' ':' Spacing RestartAction
                 / Identifier ':' Spacing Expression   # named options: maxRetries, withinTime

RestartAction   ← 'Restart' / 'Stop' / 'Escalate'
```

**Example:**
```
supervise(weatherStation, strategy: OneForOne(
    on Failure: Restart,
    maxRetries: 5,
    withinTime: System.TimeSpan.FromMinutes(1)
));
```


## 11. Literals

Spek follows C#'s lead on every primitive literal form, and emits each one
**verbatim**: the lexeme passes straight through to the generated C#, which
is the final arbiter of the exact value and type. Spek never re-formats a
literal (so digit separators and type suffixes like `L`/`UL`/`f` survive).

```peg
Literal         ← DecimalLiteral
                / IntegerLiteral
                / CharLiteral
                / RawString
                / VerbatimString
                / VerbatimInterpolated
                / StringLiteral
                / InterpolatedString
                / BoolLiteral
                / NullLiteral

# ── numeric ──  digit separators (_), hex, binary, suffixes, exponents.
Digits          ← [0-9] [0-9_]*
HexDigits       ← [0-9a-fA-F] [0-9a-fA-F_]*
Exponent        ← [eE] [+-]? Digits
RealSuffix      ← [fFdDmM]
IntSuffix       ← [lL] [uU]? / [uU] [lL]?

DecimalLiteral  ← Digits '.' Digits Exponent? RealSuffix?   # 1.5, 1_000.5, 1.5e-3, 1.5f
                / Digits Exponent RealSuffix?               # 2e10
                / Digits RealSuffix                         # 0m, 100m, 5f

IntegerLiteral  ← '0' [xX] HexDigits IntSuffix?             # 0xFF, 0x1_000, 0xFFUL
                / '0' [bB] [01] [01_]* IntSuffix?           # 0b1010
                / Digits IntSuffix?                         # 42, 100_000_000, 100L

# ── char ──
CharLiteral     ← "'" ( CharEscape / !"'" !'\\' !Newline . ) "'"   # 'a', '\n', 'A'
CharEscape      ← '\\' ( 'u' Hex Hex Hex Hex / 'U' Hex{8} / 'x' Hex+ / . )

# ── strings (all emitted verbatim) ──
StringLiteral        ← '"' ( '\\' !Newline . / !'"' !'\\' !Newline . )* '"'
VerbatimString       ← '@"' ( '""' / !'"' . )* '"'          # @"C:\path"; "" = escaped quote; newlines ok
RawString            ← '"""' .*? '"""'                       # """may contain " and newlines"""
InterpolatedString   ← '$"'  ( InterpHole / '\\' !Newline . / !'"' !'\\' !Newline . )* '"'
VerbatimInterpolated ← ('$@' / '@$') '"' ( InterpHole / '""' / !'"' . )* '"'

# A hole is a full Spek Expression with the usual C# ,alignment:format tail.
# Holes are parsed structurally, not as opaque text: nested braces and string
# literals inside a hole are handled, and the inner expression is rewritten
# like any other (actor-field/self references, invisible-async, etc.).
InterpHole           ← '{' Expression ( ',' Alignment )? ( ':' Format )? '}'
EmbeddedStr          ← '"' ( '\\' !Newline . / !'"' !'\\' !Newline . )* '"'

BoolLiteral     ← 'true' / 'false'
NullLiteral     ← 'null'
```

Interpolation holes carry real expressions: `$"hello {name}"`,
`$"total {Items.Count} @ {price:C}"`, `$"{map["k"]}"` (embedded string),
and `$"{new[]{1, 2}.Length}"` (nested braces) all parse, and a hole that
reads an actor field or `self` is rewritten the same way it would be
outside the string.

**Known limitations** (parser-level; the safe paths above cover the common
cases): a `RawString` cannot contain its own `"""` fence (longer fences and
embedded triple-quotes aren't recognised), and the {% raw %}`{{` / `}}`{% endraw %}
literal-brace escapes are passed through verbatim rather than recognised as
escapes (they still render correctly because C# re-interprets them).


## 12. Lexical rules

```peg
Identifier      ← !Keyword [a-zA-Z_] [a-zA-Z0-9_]* Spacing

Keyword         ← ('abstract' / 'actor' / 'after' / 'and' / 'any'
                /  'become' / 'behavior' / 'break' / 'catch' / 'channel'
                /  'continue' / 'do' / 'else' / 'emits' / 'enum' / 'event' / 'failure'
                /  'false' / 'finally' / 'for' / 'foreach' / 'if' / 'init'
                /  'internal' / 'interop' / 'message' / 'namespace'
                /  'new' / 'not' / 'null' / 'on' / 'or'
                /  'override' / 'passivate' / 'persist' / 'private'
                /  'program' / 'protected' / 'public' / 'reader'
                /  'restart' / 'restore' / 'return' / 'self'
                /  'sender' / 'shared' / 'spawn' / 'stop'
                /  'strategy' / 'supervise' / 'switch' / 'throw'
                /  'true' / 'try' / 'use' / 'using' / 'var'
                /  'void' / 'when' / 'while' / 'writer'
                /  'PreStart' / 'PostStop' / 'Restore' / 'Restart'
                /  'Stop' / 'Escalate' / 'Failure') !([a-zA-Z0-9_])

Spacing         ← (WhiteSpace / LineComment / BlockComment)*

WhiteSpace      ← [ \t\n\r]+

LineComment     ← '//' (!'\n' .)* '\n'

BlockComment    ← '/*' (!'*/' .)* '*/'

EOF             ← !.
```

The `Keyword` rule uses a negative lookahead `!([a-zA-Z0-9_])` to ensure
keywords are not matched as prefixes of valid identifiers (e.g. `becomes`
is not the keyword `become` followed by `s`).

### Notes on specific keywords

- **`actor` / `message` / `channel` / `after` / `strategy`:** **soft keywords.**
  They start declarations (`actor X { }`, `message M(...)`, `channel C { }`) or appear in
  fixed contextual sequences (`passivate after`, `strategy:`), but everywhere a plain name
  is expected (variable, parameter, field, message field, foreach variable, handler
  binding, and references): they are ordinary identifiers. So `var message = …`, `int
  after`, and `on Tick after => …` all parse. (Handler-mode keywords `event` / `reader` /
  `writer` are *not* soft, to avoid ambiguity with `on event` / `reader on`.)
- **`not` / `and` / `or`:** reserved everywhere as **soft pattern
  keywords**, but only meaningful inside `switch` expression patterns.
  At the lexer level they have distinct token names (`KW_NOT` etc.) from
  the `&&` / `||` operators (which keep the `AND` / `OR` token names).
  They cannot be used as identifiers anywhere in source.
- **`event`:** reserved everywhere; meaningful only as the first
  token after `on` to introduce an event handler.
- **`shared`, `use`:** reserved everywhere; introduce a
  shared-region declaration and an attachment, respectively.
- **`reader`, `writer`:** reserved as handler-mode prefixes on
  `on` handlers.


## 13. Compile-Time error codes

The full table lives in [`reference/errors.md`](reference/errors.md)
with examples and remediation. This section is the quick index.

| Code   | Trigger                                                              |
|--------|----------------------------------------------------------------------|
| CE0010 | Mutable reference type used as message payload                       |
| CE0011 | `become` target does not name a behavior on this actor               |
| CE0012 | Attempt to access another actor's field or method directly           |
| CE0013 | Duplicate declaration (file-scope or actor-member)                   |
| CE0014 | Behavior declared but never reached via `become`                     |
| CE0020 | Non-`message` type passed to `Tell` or `Ask`                         |
| CE0042 | `.Ask(...)` used outside an `on` handler body                        |
| CE0043 | `self` or `sender` used outside an `on` handler body                 |
| CE0050 | `persist` statement used outside an `on` handler body                |
| CE0051 | `become` used outside an `on` handler body or `init` block           |
| CE0060 | `on Restore` declared but no `persist` or `passivate` on this actor  |
| CE0061 | *(retired: auto-restore makes `on Restore` optional)*           |
| CE0080 | Hostile namespace import (use `interop using` to opt out)            |
| CE0081 | Unreachable `on Failure` arm in supervise strategy                   |
| CE0082 | Duplicate `on Failure` arm                                           |
| CE0085 | Use of a moved value (sent via `Tell` and then mutated)              |
| CE0086 | `interop` import bypasses safety guarantees (warning)                |
| CE0087 | Reader handler mutates actor field or shared-region field            |
| CE0090 | Channel input has no matching `on` handler                           |
| CE0091 | Channel implementation declares an unknown channel                   |
| CE0093 | Channel base name does not resolve                                   |
| CE0094 | Cycle in channel inheritance                                         |
| CE0096 | Public/internal handler must reference a declared `message`          |
| CE0097 | `use X foo;` references an unknown shared region                     |
| CE0098 | `: Persisted` region has no registered provider in any `program` block |
| CE0100 | Shared-region field read directly into an actor field (route through a local) |
| CE0101 | Reference to a `deprecated` field (warning)                          |
| CE0102 | Reference to a `retired` field                                       |
| CE0103 | Non-exhaustive `switch` expression over an enum                      |
| CE0107 | Explicit `Task<T>` local never used as a task (warning)              |
| CE0109 | Non-nullable reference field without initializer (warning)           |
| CE0110 | Disposable-looking field with no `term { }` block (warning)          |
| CE0112 | Mutable `class` used as a shared-region field                        |
| CE0113 | Assignment through a null-conditional access                         |
| CE0115 | Synchronous file I/O in a handler (warning; rewritten to async)      |
| CE0116 | Sequential `await` in a loop over an `*Async` call (hint)            |
| CE0117 | Unknown option name in a `supervise` strategy                        |
| CE0118 | Both a `supervise` decl and an `OnChildFailure` override             |
| CE0119 | Raw concurrency primitive (`Task.Run`, `new Thread`, …)              |
| CE0120 | Behavior or state inside an `interface`                              |
| CE0121 | Handler dispatches on an `interface` or `channel`                    |
| CE0122 | Abstract method in a non-abstract class/actor, or private abstract   |
| CE0123 | Extends a non-abstract or unknown base, or two base classes          |
| CE0124 | Message variant's base is not a declared `abstract message`          |
| CE0125 | `abstract message` base declares fields                   |
| CE0126 | Send to a statically-known actor that never handles the message      |


## 14. Switch expressions

Spek's switch expression matches the C# 8+ switch *expression* shape:
no `case` / `default` keywords, just `pattern => result` arms and `_`
for the default. Spek emits this directly to C#, so any C# developer
reads it without translation.

```peg
SwitchOp        ← 'switch' Spacing '{' Spacing SwitchArm (',' Spacing SwitchArm)* ','? Spacing '}' Spacing

SwitchArm       ← Pattern (Spacing 'when' Spacing Expression)? Spacing '=>' Spacing Expression

Pattern         ← TypePattern
                / RelationalPattern
                / PropertyPattern
                / ParenPattern
                / NotPattern
                / AndPattern
                / OrPattern
                / ConstPattern

TypePattern     ← Type Spacing Identifier?         ← 'Foo' or 'Foo b'; bare '_' is the discard

RelationalPattern ← ('<' / '<=' / '>' / '>=' / '==' / '!=') Spacing ConditionalExpr

PropertyPattern ← '{' Spacing (PropertySubpattern (',' Spacing PropertySubpattern)* ','?)? Spacing '}'

PropertySubpattern ← QualifiedName ':' Spacing Pattern

ParenPattern    ← '(' Spacing Pattern ')' Spacing

NotPattern      ← 'not' Spacing Pattern

AndPattern      ← Pattern Spacing 'and' Spacing Pattern    ← left-associative; binds tighter than OR

OrPattern       ← Pattern Spacing 'or' Spacing Pattern     ← left-associative

ConstPattern    ← ConditionalExpr
```

Precedence (top binds tighter):

1. Atoms (type / relational / property)
2. Parenthesised pattern
3. `not` (unary)
4. `and`
5. `or`
6. Constant (fallback for any expression)

The emitter wraps `not (a or b)` and `(a or b) and c` in parens
automatically when the AST shape requires them for C# precedence
correctness.

**Examples:**

```spek
// Constants, type patterns, when guards, discard:
result = msg switch {
    Ping       => "got ping",
    Stop s     => $"stopping: {s.Reason}",
    "hello"    => "got hello",
    n when n > 100 => "big number",
    _          => "unknown"
};

// Relational patterns:
grade = score switch {
    >= 90 => "A",
    >= 80 => "B",
    >= 70 => "C",
    _     => "F"
};

// Logical combinators:
kind = c switch {
    >= 'a' and <= 'z' => "lowercase",
    >= 'A' and <= 'Z' => "uppercase",
    null or ""        => "empty",
    not int           => "not a number",
    _                 => "other"
};

// Property patterns (compose with relational + logical):
priority = ticket switch {
    { Severity: "critical" }                                => 1,
    { Severity: "high", AgeHours: > 24 }                    => 2,
    { Severity: "high" or "medium",
      Customer.Tier: "Gold" }                               => 2,
    { Severity: not "low" }                                 => 3,
    _                                                       => 4
};
```

Tuple/positional patterns and list patterns are not supported.


## 15. Shared regions

Shared regions are per-`ActorSystem` state with a reader/writer lock
separate from any actor's own lock. An actor attaches one with
`use X foo;` (see Section 4.8); region declarations live at file
top level alongside actors.

```peg
SharedDecl      ← Visibility? 'shared' Spacing Identifier
                  (':' Spacing Identifier)?           ← capability marker
                  '{' Spacing SharedMember* '}' Spacing

SharedMember    ← FieldDecl
                / SharedInit

SharedInit      ← 'init' Spacing Block Spacing
```

The optional `init { ... }` block runs once on first reader/writer
access and holds the region's writer lock so no other reader
observes partial state. The optional `: Capability` marker selects
the runtime base class (`Spek.SharedRegion` for transient regions,
the default; `Spek.PersistedRegion` for `: Persisted`).

**Capability markers:**

| Marker | Runtime base class | Adds |
|--------|-------------------|------|
| (none) | `Spek.SharedRegion` | RW lock + lazy init |
| `: Persisted` | `Spek.PersistedRegion` | Restore on first access; save after every writer-exit |

**Example:**

```spek
shared MarketCache
{
    long lastPrice = 0;
    long lastUpdated = 0;

    init { lastUpdated = DateTimeOffset.UtcNow.ToUnixTimeMilliseconds(); }
}

actor PriceWriter
{
    use MarketCache cache;
    writer on Update u => { cache.lastPrice = u.price; }
}

actor PriceReader
{
    use MarketCache cache;
    reader on GetLast g => { return new Reply(cache.lastPrice); }
}
```

See [`language/shared-regions.md`](language/shared-regions.md) for the
full feature reference.


## 16. Complete example

The following is a valid complete Spek v1 file demonstrating the core actor constructs.

```spek
namespace MyBank.Actors;

using MyBank.Messages;

message Deposit(decimal amount, string fromUser);
message Withdraw(decimal amount, string fromUser);
message GetBalance();
message BalanceResponse(decimal currentBalance);
message FreezeAccount(string reason);
message UnfreezeAccount();
message AccountLocked();
message InsufficientFunds(decimal requested, decimal available);

public actor BankAccountManager
{
    ActorRef auditLog;

    init(ActorRef audit)
    {
        auditLog = audit;
        become Managing;
    }

    behavior Managing
    {
        on GetOrCreateAccount req =>
        {
            ActorRef account = spawn<BankAccount>(req.accountId, auditLog);
            sender.Tell(new AccountRef(account));
        }
    }
}

actor BankAccount
{
    decimal balance = 1000.00m;
    bool isFrozen = false;
    string accountId;
    ActorRef auditLog;

    passivate after System.TimeSpan.FromMinutes(30);

    init(string id, ActorRef audit)
    {
        accountId = id;
        auditLog = audit;
        become NormalOperation;
    }

    behavior NormalOperation
    {
        on Deposit d =>
        {
            balance += d.amount;
            auditLog.Tell(new AuditEntry("Deposit", d.amount, d.fromUser));
            persist;
        }

        on Withdraw w =>
        {
            if (w.amount > balance)
            {
                sender.Tell(new InsufficientFunds(w.amount, balance));
                return;
            }
            balance -= w.amount;
            auditLog.Tell(new AuditEntry("Withdrawal", w.amount, w.fromUser));
            persist;
        }

        on GetBalance =>
            sender.Tell(new BalanceResponse(balance));

        on FreezeAccount f =>
        {
            isFrozen = true;
            become Frozen;
            persist;
        }
    }

    behavior Frozen
    {
        on Deposit d =>
        {
            balance += d.amount;
            auditLog.Tell(new AuditEntry("Deposit (frozen)", d.amount, d.fromUser));
            persist;
        }

        on Withdraw w =>
            sender.Tell(new AccountLocked());

        on GetBalance =>
            sender.Tell(new BalanceResponse(balance));

        on UnfreezeAccount =>
        {
            isFrozen = false;
            become NormalOperation;
            persist;
        }

        on FreezeAccount f =>
            auditLog.Tell(new AuditEntry("Already frozen", 0, ""));
    }

    on PreStart  => auditLog.Tell(new AuditEntry("Opened", balance, accountId));
    on PostStop  => auditLog.Tell(new AuditEntry("Closed", balance, accountId));

    on Restore(Snapshot s) =>
    {
        balance   = s.Get<decimal>("balance");
        isFrozen  = s.Get<bool>("isFrozen");
        accountId = s.Get<string>("accountId");

        if (isFrozen)
        {
            become Frozen;
        }
        else
        {
            become NormalOperation;
        }
    }
}

program Main
{
    var system       = new ActorSystem("BankSystem");
    ActorRef audit   = system.Spawn<AuditLogger>("audit");
    ActorRef manager = system.Spawn<BankAccountManager>("manager", audit);
    system.AwaitTermination();
}
```


## 17. Notes for ANTLR4 translation

When translating this PEG grammar to an ANTLR4 `.g4` file for the C# compiler:

- PEG ordered choice `/` becomes ANTLR4 `|` with alternatives ordered identically
- PEG negative lookahead `!e` becomes ANTLR4 lexer fragment with `~` or parser predicate
- Lexer rules (terminals) go in the lexer grammar; parser rules go in the parser grammar
- `Spacing` disappears: ANTLR4 handles whitespace via a `-> skip` channel rule
- The `Keyword` negative lookahead guard becomes ANTLR4's automatic keyword vs identifier priority (keywords listed before `IDENTIFIER` in the lexer)
- Use the `Antlr4.Runtime.Standard` NuGet package for the C# runtime target
- Visitor pattern (`AbstractParseTreeVisitor<T>`) is recommended over listener for AST construction


## 18. Class declarations

```peg
ClassDecl       ← Visibility? AbstractMod? 'class' Spacing Identifier TypeParams?
                  ClassBases? WhereClause* '{' Spacing ClassMember* '}' Spacing

ClassBases      ← ':' Spacing QualifiedName (',' Spacing QualifiedName)* Spacing

ClassMember     ← FieldDecl
                / InitBlock        ← instance constructor, may chain BaseInit
                / PropertyDecl
                / MethodDecl

PropertyDecl    ← Visibility? Type Spacing Identifier
                  '{' Spacing PropertyAccessor+ '}'
                  ('=' Spacing Expression ';')? Spacing

PropertyAccessor ← Visibility? ('get' / 'set' / 'init') ('=>' Spacing Expression)? ';' Spacing
```

A `class` is the mutable, single-owner instance type, confined to one actor
(CE0010 keeps it out of message fields, CE0112 out of shared regions). It
lowers to a plain C# instance class: `sealed` when concrete, `abstract` when
marked. The base list holds at most one base class, which must be an
`abstract class` (CE0123), plus any number of interfaces, in either order
(the emitter reorders base-class-first for C#). Abstract methods use the
bodyless `MethodDecl` form (CE0122 rules); the subclass implements them with
plain methods and the emitter infers `override`.

**Examples:**
```
class RequestContext { string path; init(string p) { path = p; } }

abstract class Shape
{
    string name;
    init(string n) { name = n; }
    public abstract double Area();
}

class Circle : Shape
{
    double r;
    init(double radius) : base("circle") { r = radius; }
    public double Area() { return 3.14159 * r * r; }
}
```


## 19. Interface declarations

```peg
InterfaceDecl   ← Visibility? 'interface' Spacing Identifier TypeParams?
                  InterfaceBases? WhereClause* '{' Spacing InterfaceMember* '}' Spacing

InterfaceBases  ← ':' Spacing QualifiedName (',' Spacing QualifiedName)* Spacing

InterfaceMember ← InterfaceMethod
                / PropertyDecl     ← accessor signatures only

InterfaceMethod ← Visibility? ReturnType Spacing Identifier TypeParams?
                  '(' Params? ')' WhereClause* ';' Spacing
```

The class-side implementation contract: the method-based sibling of
`channel`. Held to pre-C#-8 semantics: **signatures only**. A method body,
a property-accessor body, a property initializer, or a field inside an
`interface` is CE0120. (The parser accepts a body/field so the analyzer can
reject it with CE0120 instead of a raw parse error.) An `on` handler keyed
on an interface (or channel) is CE0121: handlers dispatch on messages,
never on implementation contracts.

**Example:**
```
interface Validator
{
    bool IsValid(string input);
    int  MinLength { get; }
}

class EmailValidator : Validator
{
    public bool IsValid(string input) { return input.Contains("@"); }
    public int  MinLength { get => 3; }
}
```


## 20. Module declarations

```peg
ModuleDecl      ← Visibility? 'module' Spacing Identifier
                  '{' Spacing ModuleMember* '}' Spacing

ModuleMember    ← MethodDecl
                / ModuleDecl       ← nested modules (namespacing)
```

A stateless container of methods: no fields, no `self`/`sender`, no
mailbox. Lowers to a C# static class (nested modules to nested static
classes); methods emit `static` without the keyword ever appearing in
source. Visibility defaults to `public`. Duplicate method or nested-module
names are CE0013. Modules have no inheritance and no contract by design;
swappable behavior belongs to a `class` behind an `interface` or an `actor`
behind a `channel`.

**Example:**
```
module Validators
{
    public bool IsEmail(string s) { return s.Contains("@"); }

    module Formats
    {
        public string Trim(string s) { return s.Trim(); }
    }
}
```
