---
title: Error codes
layout: default
parent: Reference
nav_order: 2
permalink: /reference/errors/
---

<!-- spek-test-default: ignore -->
<!-- ^ Most snippets here are deliberately *invalid*: they demonstrate what
     triggers each error, so the doc-snippet harness skips this file by
     default. A genuine "fix" snippet can opt back in with a per-block
     `<!-- spek-test: compile -->` directive. -->

# Compile-time error codes

Spek's semantic analyzer enforces location rules, intra-actor consistency,
message-type immutability, and cross-actor access rules. Every diagnostic is
tagged with a `CEXXXX` code so it can be looked up and linked.

All rules that can be decided from type information fail **open** on
`Unknown` types (so unresolvable names don't cascade into false positives);
rules that only need structural information fail **closed**.

## How diagnostics are reported

`spekc` reports each diagnostic Rust-style: a header with the severity and
code, a `-->` location line, and the offending source line with a caret
underline pointing at the exact span.

```text
error[CE0020]: '.Ask(...)' message type 'GetBalance' is not a declared 'message'.
  --> bank.spek:14:30
   |
14 |         Balance b = self.Ask(new GetBalance());
   |                              ^^^^^^^^^^
   |
```

Warnings (e.g. [CE0107](#ce0107)) are reported the same way but with a
`warning[…]` header, and they don't fail the build. The editor (via the
language server) lights up the same span as a squiggle.

## Status column

- `active`: emitted by the current analyzer.
- `emit-only`: the number names an emitter behavior, not a diagnostic;
  nothing fires.
- `reserved`: the code number is reserved but not currently emitted.
- `retired`: the code number was reserved but the rule was absorbed into
  another check; kept in the table for downstream-tool compatibility.

| Code   | Status     | What it catches                                                                |
|--------|------------|--------------------------------------------------------------------------------|
| [CE0001](#ce0001) | active     | Syntax error (surfaced by ANTLR)                                      |
| [CE0010](#ce0010) | active     | Message field's type is not in the immutable whitelist                |
| [CE0011](#ce0011) | active     | `become` target does not name a declared behavior on this actor       |
| [CE0012](#ce0012) | active     | Member access on an `ActorRef` other than `Tell` or `ask`             |
| [CE0013](#ce0013) | active     | Duplicate declaration: messages/actors at file scope or behaviors/fields/methods within one actor |
| [CE0014](#ce0014) | active     | Behavior declared but never reached via `become`                      |
| [CE0020](#ce0020) | active     | Non-`message` type passed to `Tell` or `ask`                          |
| [CE0030](#ce0030) | reserved   | Message not in typed `ActorRef<IFoo>` interface                       |
| [CE0042](#ce0042) | active     | `.Ask(...)` used outside an `on` handler body                         |
| [CE0043](#ce0043) | active     | `self` or `sender` used outside an `on` handler body                  |
| [CE0050](#ce0050) | active     | `persist` statement used outside an `on` handler body                 |
| [CE0051](#ce0051) | active     | `become` used inside a plain helper method                            |
| [CE0060](#ce0060) | active     | `on Restore` declared but no `persist` / `passivate` on this actor    |
| [CE0061](#ce0061) | retired    | ~~`passivate` declared but no `on Restore`~~; auto-restore handles it now |
| [CE0070](#ce0070) | retired    | Previously "unresolved name"; CE0020 covers this uniformly            |
| [CE0080](#ce0080) | active     | Hostile namespace import (`System.Reflection`, `Unsafe`, etc.)        |
| [CE0081](#ce0081) | active     | Unreachable `on Failure` arm in a `supervise` strategy                |
| [CE0082](#ce0082) | active     | Duplicate untyped `on Failure` catch-all in a `supervise` strategy    |
| [CE0083](#ce0083) | active     | Dispatcher-blocking call (`Thread.Sleep`, `Task.WaitAll`, `Console.ReadLine`, wait handles) |
| [CE0084](#ce0084) | active     | Process-escape call (`Environment.Exit`, `Process.Kill`) inside an actor |
| [CE0085](#ce0085) | active     | Mutating a value (field/index assignment) after sending it via `Tell`/`ask` (including through an alias) |
| [CE0086](#ce0086) | active     | `interop using` bypasses safety guarantees (warning, not error)       |
| [CE0087](#ce0087) | active     | Reader handler writes state: field/region assignment, `out`/`ref` argument, or a mutating confined-class method |
| [CE0090](#ce0090) | active     | Actor implements a channel but doesn't cover every input              |
| [CE0091](#ce0091) | active     | Unknown channel or base actor in the actor's colon list               |
| [CE0092](#ce0092) | active     | `sender.Tell(...)` of a type that's neither the handler's reply nor a channel `emits` |
| [CE0093](#ce0093) | active     | Channel inherits from an unknown name (or a name that isn't a channel)                |
| [CE0094](#ce0094) | active     | Circular channel inheritance                                                           |
| [CE0096](#ce0096) | active     | Public/internal handler must reference a declared `message` type      |
| [CE0097](#ce0097) | active     | `use X foo;` references an unknown shared region                      |
| [CE0098](#ce0098) | active     | `: Persisted` region has no registered provider in any program block  |
| [CE0100](#ce0100) | active     | Direct assignment of a shared-region read into an actor field         |
| [CE0101](#ce0101) | active     | Reference to a `deprecated` shared-region field (warning)             |
| [CE0102](#ce0102) | active     | Reference to a `retired` shared-region field                          |
| [CE0103](#ce0103) | active     | Non-exhaustive `switch` over a sealed `enum` value                    |
| [CE0106](#ce0106) | retired    | folded into [CE0119](#ce0119); raw concurrency is now a hard error    |
| [CE0107](#ce0107) | active     | Redundant explicit `Task<T>` annotation never used as a Task (warning) |
| [CE0108](#ce0108) | emit-only  | Region field-level visibility flows through to emit (no diagnostic fires) |
| [CE0109](#ce0109) | active     | Non-nullable reference field without initializer (warning)            |
| [CE0110](#ce0110) | active     | Disposable-typed field with no `term { }` block (warning)             |
| [CE0112](#ce0112) | active     | Mutable `class` used as a shared-region field (escapes confinement)   |
| [CE0113](#ce0113) | active     | Assignment through a null-conditional access (`x?.y = v`)             |
| [CE0115](#ce0115) | active     | Synchronous BCL file I/O in a handler (warning; rewritten to async)   |
| [CE0116](#ce0116) | active     | Sequential `await` in a `foreach` over an `*Async` call (hint)        |
| [CE0117](#ce0117) | active     | Unknown option name in a `supervise` strategy (expected `maxRetries`/`withinTime`) |
| [CE0118](#ce0118) | active     | Actor declares both a `supervise` decl and an explicit `OnChildFailure` override |
| [CE0119](#ce0119) | active     | Raw concurrency primitive (`Task.Run`, `Parallel.*`, `new Thread`, …) in Spek source |
| [CE0120](#ce0120) | active     | Behavior or state inside an `interface` (a method body, accessor body, or field) |
| [CE0121](#ce0121) | active     | Handler dispatches on an `interface` or `channel` instead of a `message`   |
| [CE0122](#ce0122) | active     | Abstract method in a non-abstract class/actor, or a `private` abstract method |
| [CE0123](#ce0123) | active     | Base list: extends a non-abstract or unknown base, or two base classes (class/actor) |
| [CE0124](#ce0124) | active     | A message variant's base is not a declared `abstract message`               |
| [CE0125](#ce0125) | active     | An `abstract message` base declares fields  |
| [CE0126](#ce0126) | active     | Send to a statically-known actor that handles the message in NO behavior (dead mail) |
| [CE0127](#ce0127) | active     | `To<T>` / `TryTo<T>` target is not a numeric primitive or a declared enum/class/message/interface |
| [CE0128](#ce0128) | active     | Declaring a method named `To` or `TryTo` (reserved for the conversion family) |
| [CE0129](#ce0129) | active     | C#-style cast `(T)x`; Spek has no cast operator (use `To`/`TryTo`/`is`/`as`) |
| [CE0130](#ce0130) | active     | Invalid `flags enum` declaration (declared `None`, non-power-of-two value, duplicate bit, bad union) |
| [CE0131](#ce0131) | active     | Bitwise operator on an enum that is not a `flags enum`                 |
| [CE0132](#ce0132) | active     | `&` of two distinct single-bit flags members (provably empty)          |
| [CE0133](#ce0133) | active     | `HasFlag`-family call on a non-flags enum, or with a `None` argument   |
| [CE0134](#ce0134) | active     | Direct time read (`DateTime.UtcNow`, `Stopwatch.*`, …) inside an actor (warning) |
| [CE0135](#ce0135) | active     | State-capturing lambda escapes its handler (passed to a call, stored in actor state, or returned) |
| [CE0136](#ce0136) | active     | Lambda captures actor state read-only and is passed to an untrusted callee (warning) |
| [CE0137](#ce0137) | active     | Spawn argument shares a confined `class` with the child (aliases the sender's state) |
| [CE0138](#ce0138) | active     | `To<T>()` on a lossy implicit widening (integer to `float`, `long`/`ulong` to `double`) |
| [CE0139](#ce0139) | active     | `on` handler keyed on a generic message (no way to name the type argument) |

## CE0001

**Trigger:** Any syntax error that ANTLR rejects at parse time.

**Example (broken):**
```spek
actor Foo {
    init() { become ; }   // missing behavior name
}
```

ANTLR reports the offending token position. Fix the syntax and recompile.

## CE0010

**Trigger:** A `message` field's declared type is not on the immutability
whitelist, **or** a handler hands a confined `class` back as an ask-reply. A
reply *is* a message: `on GetReg => return reg;` (where `reg` is a class-typed
field), and the envelope form `return new Box<Registry>(reg);` (a generic
message instantiated with a class), both share a mutable object between the
asker and this actor: the same sharing the field-type check blocks at the
declaration, enforced at the reply boundary the declaration can't see (the
generic `Box<T>` field is `T`, immutable-looking until `T` is `Registry`). The
reply check is by reachability, mirroring [CE0137](#ce0137): a fresh instance
(`return new Registry();`) is the legal gift (the actor keeps nothing), and a
private method returning its own class field internally is fine, because only a
*handler* return is a reply. See
[messages](../language/messages.md#the-immutability-whitelist).

**Example (broken):**
```spek
message AddItems(List<string> items);        // List<T> is mutable
message Lookup(IReadOnlyList<int> ids);      // interface — underlying can mutate
```

**Fix:**
```spek
message AddItems(ImmutableList<string> items);
message Lookup(ImmutableArray<int> ids);
```

[Spek `enum`](../language/enums.md) types are also accepted, since enum
members are compile-time constants and pass by value: handy when you
want a closed set of states on a message:

```spek
enum Severity { Low, Medium, High }
message Alert(Severity level, string description);
```

Reflection-driven mutation isn't CE0010's concern; see
[CE0080](#ce0080).

## CE0011

**Trigger:** `become SomeBehavior;` where `SomeBehavior` is not declared on
this actor.

**Example (broken):**
```spek
actor Switch
{
    init() { become Running; }     // no 'Running' behavior defined

    behavior On  { /* ... */ }
    behavior Off { /* ... */ }
}
```

**Fix:** spell the target correctly, or add the missing behavior.

## CE0012

**Trigger:** Any member access on an `ActorRef` other than `.Tell(...)` or
`.Ask(...)`. Including multi-part names like `peer.name`.

**Example (broken):**
```spek
on Poke =>
{
    Console.WriteLine(peer.name);    // reaching into another actor's field
    peer.DoWork();                   // calling an actor method directly
}
```

**Fix:** send a message instead. Actor state and methods are private. The
only legal things to do with an `ActorRef` are `Tell`, `ask`, and pass it
around.

## CE0013

**Trigger:** Two declarations with the same simple name: either two
`message` or two `actor` at file scope, or two `behavior` / field / method
with the same name inside one actor. Method overloading is not supported.

**Example (broken):**
```spek
message Tick();
message Tick();           // CE0013 — duplicate message

actor Worker
{
    int counter = 0;
    int counter = 1;      // CE0013 — duplicate field

    behavior Idle { on Tick => { } }
    behavior Idle { on Tick => { } }  // CE0013 — duplicate behavior
}
```

**Fix:** rename one of the declarations, or delete the redundant one. The
diagnostic points at the second occurrence. The first wins for resolution
in the meantime.

## CE0014

**Trigger:** A `behavior` is declared on a concrete actor but no `become`
anywhere in the actor (init, other behaviors, lifecycle hooks) targets it.
The first behavior is always implicitly reachable as the entry point;
subsequent ones must be reached explicitly.

Abstract actors (`abstract actor ...`) skip this check: derived actors
may reference inherited behaviors that look unused in the parent.

**Example (broken):**
```spek
actor Switch
{
    init() { become On; }

    behavior On    { on Toggle => { become Off; } }
    behavior Off   { on Toggle => { become On;  } }
    behavior Dead  { on Toggle => { } }          // CE0014 — never reached
}
```

**Fix:** either hook `Dead` into the reachability graph with a matching
`become Dead;`, or delete it.

## CE0020

**Trigger:** A non-`message` type passed to `Tell` or `ask`. Fails open on
Unknown: if the analyzer can't resolve the type, CE0020 does not fire.

**Example (broken):**
```spek
target.Tell("just a string");            // string is not a message
target.Tell(new StringBuilder("nope"));  // plain class
```

**Fix:** declare a `message` type and send an instance of it.

```spek
message Note(string text);
// ...
target.Tell(new Note("just a string"));
```

## CE0030

**Status:** reserved. The code number is reserved; the current analyzer does
not emit it.

## CE0042

**Trigger:** `.Ask(...)` used outside an `on` handler body:
e.g. from a helper method, `init`, or a lifecycle hook.

**Example (broken):**
```spek
actor Client
{
    init(ActorRef account)
    {
        BalanceResponse r = account.Ask(new GetBalance());  // CE0042
    }
}
```

**Fix:** move the `.Ask(...)` into a handler, or refactor the initialization to
send a `Tell` and react to the reply in an `on` handler.

## CE0043

**Trigger:** `self` or `sender` used outside an `on` handler body.

**Example (broken):**
```spek
void LogMe()
{
    auditLog.Tell(new AuditEntry(self));   // CE0043
}
```

**Fix:** the identifiers only make sense during message dispatch. Move the
logic into a handler, or pass `self` / `sender` in as a parameter from the
handler that calls the helper.

## CE0050

**Trigger:** The `persist;` statement used outside an `on` handler body.

**Example (broken):**
```spek
init()
{
    balance = 0m;
    persist;             // CE0050 — init is not a message handler
    become Active;
}
```

**Fix:** persistence is a per-message concern. Put the `persist;` in the
handler that just mutated state.

## CE0051

**Trigger:** `become` used inside a plain helper method. Helpers should
stay pure. Switching behavior from inside one obscures control flow.

**Scope note:** `become` **is** accepted in `on` handlers, `init` blocks,
and all lifecycle hooks (`PreStart`, `PostStop`, `Restore`). Only plain
methods are rejected. This matches Akka / Akka.NET convention.

**Example (broken):**
```spek
void Freeze()
{
    isFrozen = true;
    become Frozen;            // CE0051
}
```

**Fix:** move the `become` up to the handler that calls `Freeze()`.

## CE0060

**Trigger:** The actor declares `on Restore(Snapshot s) => ...` but has
neither a `persist;` statement anywhere nor a `passivate` declaration. A
restore handler that is never fed is almost always a bug.

**Example (broken):**
```spek
actor Wallet
{
    decimal balance = 0m;

    on Restore(Snapshot s) =>                 // CE0060
        balance = s.Get<decimal>("balance");

    behavior Active
    {
        on Deposit d => { balance += d.amount; }   // no persist
    }
}
```

**Fix:** add `persist;` where state should be saved, or a `passivate after
System.TimeSpan.FromMinutes(N);` declaration, or remove the dead `on Restore`.

## CE0061 (retired) {#ce0061}

**No longer an error.** This previously fired when an actor declared `passivate`
(or `persist`) but had no `on Restore` handler. Proposal #6 made `on Restore`
**optional**: the compiler now auto-generates a symmetric `OnRestore` that rehydrates
every captured field, so a persistent actor can't silently save-but-never-reload. The
example below now compiles and round-trips its state automatically:

```spek
actor Wallet
{
    decimal balance = 0m;

    passivate after System.TimeSpan.FromMinutes(10);   // OK now — auto-restore rehydrates balance

    behavior Active
    {
        on Deposit d => { balance += d.amount; persist; }
    }
}
```

Write an explicit `on Restore(Snapshot s) => ...` only for custom restore logic. It then
takes over. See [persistence: auto-restore](../language/persistence.md#auto-restore).

## CE0070

**Status:** Retired. Previously reserved for cross-file "unresolved name"
diagnostics. In practice `SpekCompilation` resolves symbols
across all files in the compilation before the analyzer runs, so
unresolved names always surface via the existing CE0020 (for `ask` /
`Tell`) or CE0011 / CE0014 (for behavior references). The code number is
retained as `retired` so downstream tools that keyed on it don't regress.

## CE0080

**Trigger:** A `using` declaration imports a namespace that could let user
code bypass Spek's compile-time guarantees. Current blocklist prefixes:
`System.Reflection`, `System.Reflection.Emit`,
`System.Runtime.CompilerServices`, `System.Runtime.InteropServices`,
`System.Runtime.Serialization`, `Microsoft.Win32`.

**Example (broken):**
```spek
using System.Reflection;              // CE0080 — reflection bypasses CE0010

message Ping();
actor Sneaky
{
    decimal balance = 0m;
    behavior Idle
    {
        on Ping =>
        {
            // Without CE0080, reflection could mutate `balance` on this
            // instance or on some other actor whose ref was leaked.
            var field = typeof(Sneaky).GetField("_balance");
        }
    }
}
```

**Fix:** don't import hostile namespaces from Spek. If your program needs
reflection for a genuine reason, isolate that code in a separate C#
project that doesn't reference `Spek.Runtime`, and shuttle results in via
`message` payloads.

## CE0081

**Trigger:** An `on Failure` arm inside a `supervise` strategy is
unreachable. Two shapes:

1. A typed arm follows an untyped catch-all: the catch-all already
   matches every cause, so nothing reaches the typed arm.
2. The same exception type appears on two typed arms: only the first
   fires.

**Example (broken):**
```spek
supervise OneForOne(
    on Failure: Stop,                              // catches everything
    on Failure(System.IO.IOException): Restart);   // CE0081 — dead code
```

```spek
supervise OneForOne(
    on Failure(System.IO.IOException): Restart,
    on Failure(System.IO.IOException): Stop);      // CE0081 — already matched
```

**Fix:** put typed arms first; the untyped catch-all goes last. Arms
match top-to-bottom, first-match-wins, matching Akka's
`SupervisorStrategy` convention.

```spek
supervise OneForOne(
    on Failure(System.IO.IOException): Restart,    // specific first
    on Failure(System.InvalidOperationException): Stop,
    on Failure: Escalate);                          // catch-all last
```

## CE0082

**Trigger:** Two untyped `on Failure: Action` arms in the same strategy.
Only the first is reachable.

**Example (broken):**
```spek
supervise OneForOne(
    on Failure: Restart,
    on Failure: Stop);     // CE0082 — the first catch-all already fired
```

**Fix:** keep exactly one untyped catch-all, or narrow one of the arms
to a specific exception type.

## CE0090

**Trigger:** An actor lists a channel in its colon list but has no
`on MessageType` handler for one of that channel's declared inputs.
Every channel input must be covered by at least one behavior handler.

**Example (broken):**
```spek
message Ping();
message Shutdown();

channel Hostable
{
    on Ping;
    on Shutdown;
}

actor PartialHost : Hostable      // CE0090 — missing 'on Shutdown'
{
    behavior Running { on Ping => { } }
}
```

**Fix:** cover every input the channel declares, anywhere in any
behavior (Spek doesn't require they be in the same behavior):

```spek
actor FullHost : Hostable
{
    behavior Running
    {
        on Ping     => { }
        on Shutdown => { }
    }
}
```

A single handler also satisfies every channel that declares the same
input, matching C# explicit-interface-implementation semantics.

## CE0091

**Trigger:** A name in an actor's colon list (`actor Foo : Bar, Baz`)
is neither a declared channel nor a declared actor. Also fires when a
second actor-typed name appears after the first. Only one base actor
is permitted; remaining names must be channels.

**Example (broken):**
```spek
actor Foo : NoSuchChannel         // CE0091 — name doesn't exist
{
    behavior Idle { on Ping => { } }
}
```

**Fix:** declare the channel or actor first, or fix the typo:

```spek
channel NoSuchChannel { on Ping; }   // add the missing declaration

actor Foo : NoSuchChannel { ... }
```

## CE0092

**Trigger:** A `sender.Tell(new X())` call inside a handler, where `X`
is neither the handler's inferred reply type (i.e. the type
of `return new X();`) nor a message type declared in any implemented
channel's `emits` list. Strict enforcement of the channel's external
output contract.

The check runs only when the actor implements at least one channel.
It's disabled entirely for actors whose channel(s) include
`emits any;`.

**Rules:**

| Form | Gated? |
|------|--------|
| `self.Tell(X)` | Free: internal message pump |
| `someField.Tell(X)` | Free: outbound to another actor |
| `sender.Tell(X)` where `X` matches `return new X();` in the handler | Free: equivalent to the reply |
| `sender.Tell(X)` where `X` is in some channel's `emits` | Free: declared output event |
| `sender.Tell(X)` otherwise | **CE0092** |

**Example (broken):**
```spek
channel Observable
{
    on Ping;
    emits StatusChanged;
}

actor Leak : Observable
{
    behavior Idle
    {
        on Ping => { sender.Tell(new Unrelated()); }   // CE0092
    }
}
```

**Fix (three options):**

1. If `Unrelated` is the handler's reply, use `return new Unrelated();`
   (which also makes `target.Ask(new Ping())` typed at the call site).
2. If `Unrelated` is an emitted event, add `emits Unrelated;` to the
   channel.
3. If the channel is inherently open-ended, add `emits any;` to
   opt out of strict enforcement.

## CE0093

**Trigger:** A channel inherits from a name that doesn't resolve to a
declared channel: either the name doesn't exist, or it resolves to
a `message` / `actor` / `enum` instead. Channel inheritance only
accepts other channels as bases.

**Example (broken):**
```spek
message Ping();
channel Derived : DoesNotExist { on Ping; }   // CE0093 — unknown name
```

```spek
message Ping();
message Shutdown();
channel ServerHost : Shutdown { on Ping; }    // CE0093 — Shutdown is a message
```

**Fix:** declare the missing channel, fix the typo, or remove the
incorrect entry from the inheritance list:

```spek
channel HostBase { on Shutdown; }
channel ServerHost : HostBase { on Ping; }    // OK
```

## CE0094

**Trigger:** A channel's inheritance chain forms a cycle. Direct
cycles (`A : B`, `B : A`) and longer transitive cycles (`A : B`,
`B : C`, `C : A`) are both detected. Each channel that participates
in the cycle reports its own diagnostic.

**Example (broken):**
```spek
message Ping();
channel A : B { on Ping; }
channel B : A { }            // CE0094 fires on both A and B
```

**Fix:** break the cycle. Channel inheritance is acyclic. If two
channels genuinely share a common subset, extract it into a third
base channel and have both inherit from it:

```spek
channel Shared { on Ping; }
channel A : Shared { }
channel B : Shared { }
```

## CE0083

**Trigger:** A handler calls something that **blocks the dispatcher thread**
with no async equivalent: `Thread.Sleep`, `Console.ReadLine` / `ReadKey`,
`Task.WaitAll` / `Task.WaitAny`, `Monitor.Wait`, `SpinWait.SpinUntil`, or a
wait-handle `WaitOne` / `SignalAndWait`. A parked dispatcher thread starves the
other actors sharing it.

**Severity:** Error.

**Fix:** Use the async form: `await Task.Delay(...)` for a timed wait,
`await Task.WhenAll(...)` for fan-in, or model the wait as a delayed message.
(The value-preserving sync-over-async forms `task.Result`, `task.Wait()`,
and `x.GetAwaiter().GetResult()` are *not* CE0083: the [invisible-async](../language/async.md)
pass rewrites those to `await` for you. The editor also offers quick-fixes for
`Thread.Sleep` and `Task.WaitAll`.) See [Footguns](../language/footguns.md) for the
full policy.

## CE0084

**Trigger:** A handler calls something that **terminates or escapes the
process**: `Environment.Exit`, `Environment.FailFast`, `Process.Kill`,
`Process.GetCurrentProcess`. These sever every actor in the system mid-message,
bypassing supervision, `on Shutdown` cleanup, and shared-region `term {}`
teardown.

**Severity:** Error.

**Fix:** To bring the node down intentionally, call **`self.System.Shutdown()`**,
a graceful, non-blocking node shutdown: the handler returns, then every actor
drains, each `on Shutdown` and `term {}` runs, and the host exits cleanly. It's
reached through the ambient `self.System` accessor (a sibling of `self.Log` /
`self.Metrics`). Nothing is injected into your actor.

<!-- spek-test: ignore; demonstrates the CE0084 fix -->
```spek
message Fatal(string detail);

actor Guardian
{
    on Fatal f =>
    {
        // was: Environment.Exit(1);   // CE0084 — severs every actor
        self.System.Shutdown();        // graceful node shutdown
    }
}
```

## CE0085

**Trigger:** A value is sent via `Tell` / `ask` (handing it to another
actor) and then **mutated** by the sender: a field or index assignment
reaching that value, *directly or through an alias*. Once a payload is in
another actor's mailbox, mutating it from the sender races against the
receiver.

Reads are **not** flagged: the only sendable values are immutable
`message`s, where sharing (reading) after a send is safe by design. Only a
write is a race. Pure reassignment (`u = newValue`) re-binds the local
and is fine.

The deep/transitive case (aliasing through nested fields) is not tracked.

**Example (broken):**
```spek
on Update u =>
{
    peer.Tell(u);        // u handed to peer — it now owns the value
    u.v = 99;            // CE0085 — u has been moved
}
```

Aliases count too:
```spek
on Update u =>
{
    var w = u;           // w aliases u
    peer.Tell(u);        // the value is moved
    w.v = 99;            // CE0085 — mutating an alias of a moved value
}
```

**Fix:** mutate before sending, or send a fresh value:

```spek
on Update u =>
{
    u.v = 99;
    peer.Tell(u);        // OK — single hand-off, no later mutation
}
```

## CE0086

**Trigger (warning):** A file uses `interop using NS;` to opt out of
[CE0080](#ce0080)'s hostile-namespace block. The compiler accepts the
import but emits CE0086 to make the safety trade-off visible. This
is a warning, not an error. The file still compiles.

**Example:**
```spek
interop using System.Reflection;   // CE0086 — interop bypasses safety
```

**Fix:** prefer the safe alternative when one exists. Use `interop
using` only when integrating with a library you trust and there's no
safer equivalent. Suppress per-file with the modifier. There's no
project-wide opt-out.

## CE0087

**Trigger:** A `reader on X` handler mutates state.

Four flavours:

- **Actor field mutation.** Reader handlers run concurrently
  with other readers on the same actor. Mutating an actor field would
  race against them.
- **Shared-region field mutation.** Reader handlers hold the
  region's reader lock. Mutating a region field would race against
  every other reader on that region across every actor that attaches
  it.
- **`out` / `ref` arguments.** Passing an actor field or shared-region
  state as an `out` or `ref` argument hands the callee a write to it,
  the same race an assignment would be. `in` arguments are read-only
  and pass freely.
- **Confined-class mutation.** Calling a *mutating* method on a
  `class`-typed actor field (e.g. `builder.Append(...)`) from a reader
  handler mutates the confined object's state, which would race against
  other concurrent readers. Pure methods and reads are fine. (Whether a
  class method mutates is inferred: it writes a field, directly or via a
  sibling method.)

**Example (broken: actor field):**
```spek
actor Counter
{
    int n = 0;
    reader on Get g => { n = 0; return new Reply(n); }   // CE0087
}
```

**Example (broken: shared region):**
```spek
shared MarketCache { long lastPrice = 0; }
actor Reporter
{
    use MarketCache cache;
    reader on Tick t => { cache.lastPrice = 0; }   // CE0087
}
```

**Example (broken: `out` argument):**
```spek
message Parse(string s);
message Reply(int n);

actor Cache
{
    int cached = 0;

    reader on Parse p =>
    {
        int.TryParse(p.s, out cached);    // CE0087 — 'out' writes the field
        return new Reply(cached);
    }
}
```

**Fix:** promote to `writer on ...`, or move the mutation into a
separate writer handler. For an `out` argument, receive the result
into a local (`int.TryParse(p.s, out var v)`) and use the local. The
write then never touches actor state. Reader handlers are bound to
read-only access to actor and region state so the runtime can run them
concurrently.

## CE0096

**Trigger:** A `public on X` or `internal on X` handler references a
type `X` that isn't a declared `message`. Public/internal handlers
form the actor's external API surface (channel coverage, remote
dispatch). The pattern type must
resolve to a Spek-declared message so cross-language and cross-process
callers can reach it.

Private handlers (`private on X`) escape this rule. They're
reachable only via `self.Tell` from inside the actor and aren't part
of the public surface, so they can bind to any CLR type. This is
how actors take BCL event-args (`FileSystemEventArgs`, `Timer`
callback args, etc.) directly without wrapping them in a Spek
`message`.

**Example (broken):**
```spek
public actor Watcher
{
    public on FileSystemEventArgs e => { /* ... */ }   // CE0096
}
```

**Fix:** declare a Spek message that wraps the BCL type, or mark the
handler `private`:

```spek
// Option 1: declare a message
message FileChanged(string path);
public actor Watcher { public on FileChanged ev => { /* ... */ } }

// Option 2: privatise the handler (intra-actor only)
public actor Watcher { private on FileSystemEventArgs e => { /* ... */ } }
```

> The `on event` form is the cleaner path; see
> [`language/actors.md`](../language/actors.md). It
> handles the bridge automatically and produces a public method-group
> reference for `+=` wiring.

## CE0097

**Trigger:** An actor's `use X foo;` declaration names a region type
`X` that isn't a declared `shared X { ... }` somewhere in the
compilation. Cross-namespace resolution is by simple name.

**Example (broken):**
```spek
actor PriceWriter
{
    use Cache cache;        // CE0097 — no `shared Cache { ... }` exists
}
```

**Fix:** declare the region, fix the typo, or remove the `use`
declaration:

```spek
shared Cache { long value = 0; }

actor PriceWriter
{
    use Cache cache;        // OK
}
```

## CE0098

**Trigger:** A `shared X : Persisted { ... }` region is declared
but no `program` block in the compilation contains a matching
`system.RegisterPersistenceProvider<X>(...)` call. Spek's
compile-time check ensures every persisted region has a place to
write its snapshots before any code can spawn an actor that uses
it.

**Example (broken):**
```spek
shared MarketCache : Persisted
{
    long lastPrice = 0;
}
// No program block, or one that doesn't register a provider.
```

**Fix:** add the registration to a `program` block, before any
actor that uses the region is spawned:

```spek
program Main
{
    var system = new ActorSystem("myapp");
    system.RegisterPersistenceProvider<MarketCache>(
        new FileSnapshotStore("/var/lib/myapp/market"));
    spawn<PriceWriter>();
    await system.AwaitTermination();
}
```

If persistence isn't actually needed, drop the `: Persisted`
marker: the region is then transient (in-memory only,
discarded on process exit), which is the default.

## CE0100

**Trigger:** A handler assigns a shared-region read directly into
an actor field. The actor would then hold a reference to data the
region still owns; a later writer on the region could mutate that
data while the actor is still reading from it.

**Example (broken):**
```spek
shared Cache { string current = ""; }
actor Worker
{
    use Cache cache;
    string mine = "";

    on Refresh =>
    {
        mine = cache.current;       // CE0100
    }
}
```

**Fix:** Route the value through a local (the local makes the
snapshot decision explicit) or through a deep-copy call:

```spek
on Refresh =>
{
    var snap = cache.current;       // a deliberate borrow
    mine = snap;
}

// or:
on Refresh =>
{
    mine = string.Copy(cache.current);   // deep copy
}
```

The rule fires only on direct assignments. Calls, projections,
and locals all sanitise the value because they make the
intent visible at the call site.

## CE0101

**Trigger:** A reference (read or write) to a shared-region field
marked `deprecated`. The field still works (the data is captured
and restored normally) but the warning surfaces so callers know
to plan a move off the field before it is marked
`retired`.

**Severity:** Warning. The build continues. The diagnostic is
informational.

**Example (warns):**
```spek
shared MarketCache
{
    deprecated string oldSymbol = "";
}

actor Trader
{
    use MarketCache cache;
    on Refresh =>
    {
        var s = cache.oldSymbol;   // CE0101 (warning)
    }
}
```

**Fix:** Migrate the code off the deprecated field. Read the
replacement field instead, or copy the snapshot's old value
forward in a one-time migration actor and stop reading the
deprecated field from new code.

## CE0102

**Trigger:** A reference to a shared-region field marked
`retired`. The field name is reserved (a future field may not
reuse it) and the field is skipped from capture/restore. The
persistence store sees the key disappear from new snapshots and
drops it on the next save.

**Example (broken):**
```spek
shared MarketCache
{
    retired string oldSymbol = "";
}

actor Trader
{
    use MarketCache cache;
    on Refresh =>
    {
        var s = cache.oldSymbol;   // CE0102
    }
}
```

**Fix:** Remove the reference. Retired fields are gone for
practical purposes: they exist in source only to reserve the
name. If the data is still needed, mark the field `deprecated`
instead and migrate callers off gradually.

## CE0103

**Trigger:** A `switch` expression's subject is a value of a
Spek `enum` type, and the arms don't cover every variant.

Every Spek `enum` is sealed by default: the language treats
the variant set as closed, exactly the variants declared in
source. A `switch` over an enum value must therefore either
list an arm for every variant, or include a `_` discard arm
to opt out of exhaustiveness explicitly.

The main rationale is **cross-actor versioning during
rolling deploys**: when half the nodes have the old enum and
half have the new one, exhaustiveness catches the mismatch on
the new code at compile time, not at the first message that
hits the unhandled variant in production.

**Example (broken):**
```spek
enum Status { Active, Inactive, Pending }

actor Worker
{
    on Tick =>
    {
        Status s = Status.Active;
        var label = s switch {
            Status.Active   => "a",
            Status.Inactive => "i",
            // CE0103 — missing arm for Status.Pending
        };
    }
}
```

**Fix:** Add the missing arm, or include a `_` discard arm
when the catch-all is what you want:

```spek
// Cover every variant explicitly.
var label = s switch {
    Status.Active   => "a",
    Status.Inactive => "i",
    Status.Pending  => "p",
};

// Or opt out of exhaustiveness with `_`.
var label = s switch {
    Status.Active => "a",
    _             => "other",
};
```

The check fires when the subject's type can be resolved to a
declared `enum`: typed locals (`Status s = …;`), parameters,
and actor fields. Untyped `var s = …;` locals only get the
check when the initializer's type can be classified at the
semantic layer.

A `when` guard on an arm doesn't count as a definitive cover
for that variant. The compiler can't prove the guard is
always true, so the variant still needs another unguarded arm
or a `_` to satisfy exhaustiveness.

## CE0106

**Retired: folded into [CE0119](#ce0119).** CE0106 was a *warning*
that fired only when a `Task.Run` lambda captured an actor field directly. It is
superseded by [CE0119](#ce0119), which forbids the raw concurrency-spawning
primitives outright (a hard error), capture or not.

## CE0107

**Trigger:** A top-level local declared with an explicit `Task<T>`
or `ValueTask<T>` type: the [invisible-async](../language/async.md)
escape hatch, that is never used *as a Task*.

Under invisible async, naming the Task type opts a binding out of the
auto-await so you keep the raw Task. But a lazy `var` already defers
the await to the use site and runs concurrently the same way, so the
only thing an explicit `Task<T>` adds is the ability to consume the
value *as a Task*: to hand it to a Task-shaped API, forward it out of
the method, or capture it. If you never do that, the annotation has no
effect: `var` would produce identical behaviour.

**Severity:** Warning. The code is correct either way. This is a
"you wrote more than you needed" nudge, not a soundness problem.

**Example (warns):**
```spek
public int Plus()
{
    Task<int> n = ComputeAsync();   // CE0107 — never used as a Task
    return n + 1;                   // `n` is consumed as a value
}
```

**Fix:** Drop the explicit type and let `var` defer it:
```spek
public int Plus()
{
    var n = ComputeAsync();
    return n + 1;
}
```

**Not flagged** (the Task is genuinely used as a Task):
```spek
public Task<string> Get(string path)
{
    Task<string> t = System.IO.File.ReadAllTextAsync(path);
    return t;                       // forwarded — keep the Task type
}
```
```spek
Task<string> a = ReadAsync(x);
Task<string> b = ReadAsync(y);
System.Threading.Tasks.Task.WhenAll(a, b);   // handed to a Task API
```

The check is sound-by-construction: it warns only for **top-level**
locals (where `var` provably defers the same way) and suppresses on
any use that *could* be a task context: a member access (it might be
`.Result` / `.ConfigureAwait`), a call receiver, any argument, a
`ref`/`out` argument, a bare `return t`, an assignment, or rebinding
into another Task local. So it never suggests dropping a `Task<T>`
that's doing real work. It may stay quiet on a redundant one it can't
prove (e.g. `t.SomeResultProperty`).

## CE0108

**Trigger:** This is an emit-only change: there's no diagnostic
that fires under the CE0108 code. Field-level visibility modifiers
on shared regions flow through to the generated C# class:

- No modifier (default) → `public`. Preserves the existing
  "attaching actors can read/write" contract.
- Explicit `public` → `public`.
- Explicit `private` → `private`. The field is internal to the
  region: set in `init { }` or used by other fields, not
  reachable from attaching actors.
- Explicit `internal` / `protected` → emitted verbatim.

Attempting to read a `private` region field from an attaching
actor surfaces as a Roslyn `CS0122` ("inaccessible due to its
protection level") error in the generated C#.

## CE0109

**Trigger:** A non-nullable reference-typed field without an
initializer, on an actor or shared region that doesn't have an
`init { }` block to set the field at construction time.

**Severity:** Warning. The build continues. The diagnostic
points at the missing initialization so authors can decide
whether to add `= …`, mark the field nullable (`T?`), or move
construction into an `init` block.

**Example (warns):**
```spek
actor Worker
{
    string name;          // CE0109 (warning) — no init, not nullable

    on Tick =>
    {
    }
}
```

**Fix:** Pick whichever expresses the field's semantics:

```spek
// Default value at decl time.
string name = "anon";

// Or mark nullable if null is meaningful.
string? name;

// Or set it from an init parameter.
init(string n) { name = n; }
```

The check skips owners with any `init` block: the init body is
presumed to handle initialization (the check does not walk init
bodies looking for specific assignments). Value-typed fields
(`int`, `bool`, `DateTime`, etc.) are skipped because they
have natural defaults.

## CE0110

**Trigger:** An actor or shared region holds a field whose type
name matches a disposable-looking heuristic (BCL types ending
in `Stream`/`Reader`/`Writer`/`Connection`/`Pipe`/`Channel`,
plus an explicit list of common disposables like `HttpClient`,
`FileSystemWatcher`, `CancellationTokenSource`,
`SemaphoreSlim`, `Timer`, etc.) but the owner has no
`term { }` block.

**Severity:** Warning. The heuristic produces some false
positives by design (the type *might* not actually implement
`IDisposable`). The warning is a nudge, not a hard error.

**Example (warns):**
```spek
shared HttpPool
{
    HttpClient? client;          // CE0110 (warning)
    // No term block — client never gets disposed.
}
```

**Fix:** Add a `term { }` block that releases the resource:

```spek
shared HttpPool
{
    HttpClient? client;

    init    { client = new HttpClient(); }
    term    { if (client != null) client.Dispose(); }
}
```

If the field intentionally outlives the owner (the resource is
managed elsewhere), suppress the warning by structuring the
code so the field's owner doesn't hold it directly: pass it
in or fetch it from a host service.

## CE0112

**Trigger:** A `shared` region declares a field whose type *nests* a mutable
`class`: the field type itself, or a class inside a generic container's type
arguments, recursively. A class is mutable and single-owner; a shared region is
reachable from every actor that attaches it, so `Registry hits` **or**
`ImmutableArray<Registry> hits` there would expose **mutable state shared across
actors**: the immutable container does not save you, because the elements
inside it are the same mutable `Registry` every attaching actor can reach. This
mirrors the recursive element check [CE0010](#ce0010) applies to message fields. A region-native mutable collection of *primitives* (`ImmutableArray<int>`,
`List<int>`) stays legal: the region provides its reader/writer discipline, and
there is no confined class inside to leak.

**Severity:** Error.

**Example (errors):**
<!-- spek-test: ignore; demonstrates the CE0112 trigger -->
```spek
class Counter { int n = 0; public void Inc() { n = n + 1; } }

shared Stats
{
    Counter hits = new Counter();   // CE0112 — a class can't be shared state
}
```

**Fix:** Shared state must be immutable or region-native. Use a primitive (or
a region-native counter), or keep the class confined to a single actor:

<!-- spek-test: ignore; illustrative fix -->
```spek
shared Stats
{
    long hits = 0;          // a primitive is fine in a region
}
```

Related escapes are blocked elsewhere: a class can't be a `message` field or
ask-reply ([CE0010](#ce0010): a class isn't immutable), and it can't be a
`spawn` argument that aliases the sender's state ([CE0137](#ce0137)). A class
stays confined to its owning actor.

## CE0113

**Trigger:** The left-hand side of an assignment reaches through a
null-conditional access (`?.` or `?[`). C# has no null-conditional
*assignment* (`x?.y = v` is not a thing), so without this check the
emitted C# would fail with an opaque error pointing at the generated file.

**Severity:** Error.

**Example (errors):**
<!-- spek-test: ignore; demonstrates the CE0113 trigger -->
```spek
message Update(int v);

actor A
{
    on Update u =>
    {
        u?.v = 1;        // CE0113 — no null-conditional assignment
    }
}
```

**Fix:** Test for null explicitly, then assign through a plain access:

<!-- spek-test: ignore; illustrative fix -->
```spek
message Update(int v);

actor A
{
    on Update u =>
    {
        if (u != null) { u.v = 1; }
    }
}
```

Null-conditional *reads* (`var n = u?.v;`, `xs?[0]`) are unaffected.

> **`.Result` is not an error.** Reading `Task<T>.Result` would block
> the dispatcher, but Spek's invisible-async pass **rewrites `task.Result` into
> `(await task)`** (same value, no blocking), so it needs no diagnostic; see
> [invisible async](../language/async.md). The blocking *method* forms `.Wait()` /
> `.GetResult()` stay [CE0083](#ce0083) errors.

## CE0115

**Trigger:** A handler calls synchronous BCL I/O (`File.ReadAllText`,
`File.ReadAllBytes`, `File.ReadAllLines`, `File.WriteAllText`,
`File.WriteAllBytes`, `File.AppendAllText`) that blocks the dispatcher thread
while the disk responds. Each of these has a drop-in `*Async` sibling.

**Severity:** Warning. Unlike [CE0083](#ce0083) (a hard error for blocks with
no async form), sync I/O is *occasionally* legitimate: a one-off config read
in an `init` block, for instance.

**What Spek does:** in a handler or method body, the
[invisible-async](../language/async.md) pass **already rewrites this for you**:
`File.ReadAllText(p)` is emitted as `await File.ReadAllTextAsync(p)`, the same
value with no blocked dispatcher (type-checked against `System.IO.File`, so a
look-alike `File` of your own is untouched). The warning still fires for two
reasons: it steers you to write the async form directly (clearer intent, and
the editor offers a one-click quick-fix), and it covers the one place the
rewrite *can't* reach: an `init` block or constructor, which can't be `async`,
so there the sync call really does block.

**Example (warns; rewritten in a handler):**
<!-- spek-test: ignore; demonstrates the CE0115 trigger -->
```spek
message Load(string path);

actor Reader
{
    on Load l =>
    {
        var text = File.ReadAllText(l.path);   // CE0115 — emitted as await File.ReadAllTextAsync(...)
    }
}
```

**Write it directly** to silence the warning (identical behaviour):

<!-- spek-test: ignore; illustrative fix -->
```spek
message Load(string path);

actor Reader
{
    on Load l =>
    {
        var text = File.ReadAllTextAsync(l.path);   // awaited automatically
    }
}
```

CE0115 fires only on the bare `File.Method` form (matching `using System.IO;`
code), the same conservative shape as CE0083's static blocklist.

## CE0116

**Trigger:** A `foreach` loop whose body calls a method whose name ends in
`Async`. Under [invisible async](../language/async.md) each such call is awaited
before the next iteration, so the waits run back-to-back; if the iterations are
independent, that's slower than overlapping them.

**Severity:** Hint: an editor suggestion only. It never fails the build, and
there is **no auto-fix**: parallelizing a loop changes side-effect ordering and
turns the first exception into an aggregate, so whether it's safe is the
developer's call, not the compiler's.

**Example (hints):**
<!-- spek-test: ignore; demonstrates the CE0116 trigger -->
```spek
message Sync(System.Collections.Generic.List<string> ids);

actor Catalog
{
    on Sync s =>
    {
        foreach (var id in s.ids)
        {
            directory.RefreshAsync(id);   // CE0116 — awaited once per iteration
        }
    }
}
```

**Fix (when the iterations are independent):** collect the work and await it
together rather than one-at-a-time. CE0116 is conservative: it fires on
`foreach` only (the canonical "process each item" shape), and only on the
`*Async` naming convention, so a genuinely sequential loop that must run in
order can leave it.

## CE0117

**Trigger:** A `supervise` strategy lists a named option whose name isn't a
recognized supervise option. Because `maxRetries`/`withinTime` are ordinary
named arguments (not reserved keywords), a misspelling parses cleanly and is
caught here rather than surfacing as a raw syntax error.

**Example (broken):**
```spek
message Ping();
actor Parent
{
    behavior Idle { on Ping => { } }
    supervise OneForOne(on Failure: Restart, maxRetres: 2);   // CE0117 — 'maxRetres'
}
```

**Fix:** use a recognized option name: `maxRetries` or `withinTime`.

## CE0118

**Trigger:** An actor declares both a `supervise` declaration and a hand-written
`OnChildFailure` override. The `supervise` decl already generates `OnChildFailure`,
so the explicit override would be silently dropped: the worst failure mode (it
parses, emits, and does nothing).

**Example (broken):**
```spek
message Ping();
actor Parent
{
    behavior Idle { on Ping => { } }
    supervise OneForOne(on Failure: Restart);          // generates OnChildFailure
    FailureDirective OnChildFailure(ActorRef child, System.Exception cause, object msg)
    {                                                   // CE0118 — would be dropped
        return FailureDirective.Resume;
    }
}
```

**Fix:** pick one form. Use the declarative `supervise` (now including
[`Resume`](../language/supervision.md)), or remove the `supervise` decl and keep the
imperative `OnChildFailure` override for conditional logic.

## CE0119

**Trigger:** Spek source spawns raw concurrency: a delegate that runs on a
thread-pool or OS thread *outside* any actor's turn. Recognised forms:
`Task.Run`, `Parallel.For` / `Parallel.ForEach` / `Parallel.Invoke`,
`ThreadPool.QueueUserWorkItem`, `new Thread(...)`, `new Timer(callback, ...)`,
and `.AsParallel()`. The last is PLINQ's entry point: every operator downstream
of it runs its lambdas on multiple thread-pool threads, which makes it raw
parallelism by another spelling. It is matched by bare method name on any
receiver, the same way the [CE0083](#ce0083) instance blocklist catches
`.WaitAll()`. If the iterations are heavy enough to want parallelism, fan the
work out to a pool of child actors; otherwise a plain `foreach` in the handler
is the faster code anyway.

**Why:** these bypass the serialization that makes Spek race-free. Work started
on another thread can read and write actor state concurrently with the actor's
own handlers: exactly the data race the language exists to prevent. Concurrency
in Spek comes from actors. The actor (and the reader/writer shared region) is the
only unit of concurrent execution the compiler can keep safe.

**Severity:** Error. A warning wouldn't close the escape. The racy code would
still compile and run. Fires everywhere in Spek source: handlers, `init`, module
and class methods, and `program`.

**Example (rejected):**
```spek
on StartJob job =>
{
    Task.Run(() => { balance = balance + job.amount; });   // CE0119
}
```

**Fix:** model the concurrent work as an actor: spawn a child and `Tell` it the
job, then reply with a message when it's done. The child's state is serialised by
its own mailbox, so there's no race:
```spek
on StartJob job => { worker.Tell(job); }
```

**Not flagged:** awaited Task-returning BCL calls: `File.ReadAllTextAsync(...)`,
`Task.Delay(...)`, `HttpClient.GetAsync(...)`. Those are resumed *inside* the
actor's turn by invisible async (see [Async without await](../language/async.md)). Only thread-*spawning* is forbidden. A third-party library that spawns threads
internally is a trust boundary the compiler can't see into. Wrap its use behind
an actor.

## CE0120

**Trigger:** An `interface` declares behavior or state: a method with a body, a
property accessor with a body, a property initializer, or a field. A Spek
`interface` is the class-side implementation contract, and a contract declares
the outward shape of a type, not the implementation.

**Why:** this is the deliberate divergence from C# 8 and later, which allow
default method bodies on an interface. Spek holds the interface to its pre-C#-8
meaning: signatures only. Behavior always lives in the concrete type: a
class's method bodies, an actor's `behavior` block, never smuggled into the
thing it implements, so you can always see where work happens by reading the
implementation. A field is state, which a contract likewise never carries.

**Severity:** Error. The rule is the guarantee. A warning would let behavior
hide inside a contract anyway.

**Example (rejected):**
```spek
interface Validator
{
    bool IsValid(string input) { return input.Length > 0; }   // CE0120 — body
    int  MinLength;                                            // CE0120 — field
}
```

**Fix:** leave the signature and move the behavior to the implementing class.
Expose state as a property signature, which the class satisfies with a field or
computed property:
```spek
interface Validator
{
    bool IsValid(string input);
    int  MinLength { get; }
}

class NonEmpty : Validator
{
    public bool IsValid(string input) { return input.Length >= MinLength; }
    public int  MinLength { get => 1; }
}
```

## CE0121

**Trigger:** An `on` handler is keyed on an `interface` or a `channel` rather
than on a `message`. Handlers dispatch on the message type: a concrete
message, or an `abstract message` base for a family, never on an
implementation contract. The rule holds for every visibility, `private`
included.

**Why:** an `interface` / `channel` is the *provider* side of a contract ("I
promise to provide these methods / handle these messages"); a message is the
*received* side. Routing a handler on a provider contract collapses that
distinction and makes dispatch multi-valued and non-local: a message can
implement several interfaces, so `on IAudited` and `on IPrioritized` could both
match one message with no clear winner, and a message's handlers would no
longer be findable from its type alone. Keeping dispatch keyed on the single,
linear message hierarchy is what keeps message flow visible.

**Example (rejected):**
```spek
interface Greet { string Hello(); }

actor A
{
    on Greet => { }        // CE0121 — Greet is an interface, not a message
}
```

**Fix:** to handle a family of messages, give them a shared `abstract message`
base and dispatch on it (one linear hierarchy); to react to a cross-cutting
concern across unrelated messages (audit, correlation, priority), use an
ingress policy or a shared field the handler reads, not a handler keyed on the
contract:
```spek
abstract message Command();
message Start() : Command;
message Stop()  : Command;

actor A
{
    on Command c => { }    // OK — receives every Command variant
}
```

## CE0122

**Trigger:** An `abstract` method is used where it can't be: inside a `class` or
`actor` that isn't itself `abstract`, or marked `private` (so no subclass could
ever implement it).

**Why:** an abstract method is a promise the subclass fulfills, so it only makes
sense on an `abstract class` / `abstract actor`, and it has to be reachable. A
`private` abstract method is a contradiction. Spek's class and actor inheritance
is reuse plus abstract methods, and nothing more: there is no `virtual`/
`override`, because the only methods a subclass specializes are the abstract
ones, and the emitter infers the `override` for you.

**Example (rejected):**
```spek
class A { public abstract int F(); }   // CE0122 — A is not `abstract class`
```

**Fix:** mark the class abstract, and make the abstract method `public` or
`protected`:
```spek
abstract class A { public abstract int F(); }
class B : A { public int F() { return 1; } }   // implements it; `override` inferred
```

## CE0123

**Trigger:** A base list is malformed: a `class` or `actor` extends a name that
isn't a declared type, extends a **concrete** base, or lists more than one base
class. Only an `abstract class` (or `abstract actor`) may be a base, and there is
a single base (plus, for classes, any number of interfaces).

**Why:** concrete classes and actors are emitted `sealed` (inheritance is
reserved for abstract bases that exist to be extended), and C# is
single-inheritance. Keeping the rule strict means a hierarchy is always shallow
and its base is always meant to be shared. For actors specifically, the
message-protocol side of sharing already has a home: a
[`channel`](../language/channels.md).

**Example (rejected):**
```spek
class A { }
class B : A { }        // CE0123 — A is concrete, so it's sealed and can't be a base
```

**Fix:** make the base abstract, or, if you only wanted to reuse `A`'s
functionality without a subtype relationship, hold an `A` as a field
(composition) instead of extending it:
```spek
abstract class A { }
class B : A { }        // OK
```

## CE0124

**Trigger:** A message variant names a base (`message NodeUp(...) : Base`) that
isn't a declared `abstract message`: the name doesn't resolve, or it resolves to
a concrete message. A message family's base is the abstract dispatch contract the
variants share.

**Why:** an `abstract message` is a family marker: you never send it, you handle
it (`on Base` receives every variant). Requiring the base to be abstract keeps
the model crisp: a base exists to be dispatched on, and a variant is one of a
closed, deliberately-declared set.

**Example (rejected):**
```spek
message A();
message B() : A;       // CE0124 — A is a concrete message, not `abstract message`
```

**Fix:** make the base an `abstract message`:
```spek
abstract message Event();
message A() : Event;
message B() : Event;   // `on Event` now receives both A and B
```

## CE0125

**Trigger:** An `abstract message` declares fields. A polymorphic
message base must be empty: a family marker with no payload of its own.

**Why:** a variant that inherits shared fields from its base would have to pass
them through to the base's constructor (`message NodeUp(...) : Event(shared)`),
machinery the language does not have. The empty-base rule keeps the common
pattern, a marker base plus variants that each carry their own fields,
available without the extra machinery. Shared fields live on each variant.

**Example (rejected):**
```spek
abstract message Event(int Id);   // CE0125 — abstract base can't carry fields yet
```

**Fix:** move the fields onto each variant:
```spek
abstract message Event();
message NodeUp(int Id, string Node) : Event;
message NodeDown(int Id)            : Event;
```

## CE0126

**Trigger:** A `Tell` or `.Ask` whose target's concrete actor type is
statically known (a local or field whose only origin is `spawn<T>(...)` /
`Spawn<T>(...)`, or `self`) sends a message that actor handles in **no
behavior**. Such a send is provably dead mail: no state the actor can `become`
will ever process it, so at runtime it could only dead-letter.

**Why:** `ActorRef` is untyped (typed `ActorRef<Channel>` is the reserved
[CE0030](#ce0030)), so a send is not type-checked against
its target in general. But when the compiler can *see* where the ref came
from, "this actor can never process this message" is as provable as an
unknown `become`
target, and it gets the same treatment. The check is
deliberately conservative in both directions:

- **Handled in *another* behavior → legal.** The handled surface is the union
  across every behavior (and the base-actor chain). A message that's only
  valid in a different state is the `become` state machine at work, not dead
  mail.
- **Unknown-origin refs → silent.** `sender`, refs carried in message fields,
  collections, remote refs: no static type, no check.

- **`on any` → handles everything.** A catch-all is precisely the declaration
  "I accept unchecked mail," so proxies and taps never flag.
- **Private handlers count only for `self.Tell`**. They aren't part of the
  external surface (the same split [CE0096](#ce0096) makes).

**Example (rejected):**
```spek
message Ping();
message Wrong();

actor Child
{
    on Ping => { }
}

actor Parent
{
    on Ping =>
    {
        var c = spawn<Child>();
        c.Tell(new Wrong());     // CE0126 — Child never handles Wrong
    }
}
```

**Fix:** add an `on Wrong` handler to `Child`, send a message it does handle,
or, for a deliberate pass-through, give the target an `on any` catch-all.

## CE0127

**Trigger:** A `To<T>()` or `TryTo<T>()` call whose target type the conversion
routing can't reason about. The target must be a numeric primitive (`int`,
`double`, `decimal`, …) or a declared Spek `enum`, `class`, `message`, or
`interface`. External types, generics, arrays, and nullable targets all fall
outside the checked lowering, so the compiler refuses to guess.

**Example (rejected):**
```spek
module Conv
{
    public int Use(int x)
    {
        var u = x.TryTo<Uri>();    // CE0127 — Uri is an external type
        return x;
    }
}
```

**Fix:** convert to a target the routing knows, or move the external-type
conversion into a C# interop file and hand the result back through a
`message` payload or a module function.

## CE0128

**Trigger:** A `class`, `actor`, or `module` declares a method named `To` or
`TryTo`. The names are reserved for the Spek conversion family (`x.To<T>()` /
`x.TryTo<T>()`), whose calls the compiler rewrites; a user method with the
same name would collide with that rewrite.

**Example (rejected):**
```spek
class Money
{
    public int To(int x) { return x; }    // CE0128 — 'To' is reserved
}
```

**Fix:** rename the method (`ConvertTo`, `AsCents`, …).

## CE0129

**Trigger:** A C#-style cast, `(T)expr`. Spek has no cast operator. The parser
recognizes the shape purely so this diagnostic can teach the replacement. A
cast hides three different risk profiles (silent numeric wraparound, runtime
downcast failure, intentional truncation), and the conversion family splits
them into checked spellings.

**Example (rejected):**
```spek
module Conv
{
    public int Round(double d)
    {
        var x = (int)d;    // CE0129 — no cast operator
        return x;
    }
}
```

**Fix:** pick the spelling that matches the risk. `x.To<T>()` when the
conversion is lossless (Roslyn-enforced), `x.TryTo<T>()` (returning `T?`) when
it can lose information or fail:

```spek
module Conv
{
    public int? Round(double d)
    {
        return d.TryTo<int>();    // int? — null when the value doesn't fit
    }

    public long Widen(int n)
    {
        return n.To<long>();      // lossless, so always succeeds
    }
}
```

The message adapts to the target: for a declared class, message, or interface
it suggests the type tests `x is T v` / `x as T`, and for a declared enum it
suggests `x.TryTo<T>()`, which is `null` when the enum doesn't define the
value.

## CE0130

**Trigger:** A `flags enum` declaration that could lie about its bits. Five
shapes are rejected: declaring `None` (it's provided automatically as the
empty set, `= 0`), an explicit value that isn't a power of two, two members
with the same bit value, a union naming a member that isn't declared above
it, and a union member on a plain (non-flags) `enum`.

**Example (rejected):**
```spek
flags enum P { None, Read }              // CE0130 — None is provided automatically
flags enum Q { Read = 3 }                // CE0130 — 3 is not a power of two
flags enum R { Read = 1, Also = 1 }      // CE0130 — same bit value as Read
enum S { A, B, AB = A | B }              // CE0130 — unions need 'flags enum'
```

**Fix:** let the compiler assign the bits. Members without explicit values
auto-assign the next free power of two, `None = 0` comes for free, and
declare combinations as unions of earlier members:

```spek
flags enum Perm { Read, Write, Execute, ReadWrite = Read | Write }
```

The declaration emits `[System.Flags]`, so C# consumers see an ordinary
flags enum.

## CE0131

**Trigger:** A bitwise operator (`|`, `&`, `^`) combining two literals of the
same declared `enum` that isn't a `flags enum`. Plain enum members are
arbitrary values, not disjoint bits, so a bitwise combination is meaningless
and usually produces a value the enum doesn't define.

When every member of the enum is a hand-rolled distinct power of two, the
message adds a did-you-mean: "Every member is a distinct power of two; did
you mean 'flags enum Perm'?"

**Example (rejected):**
```spek
enum Sev { Low, High }

module M
{
    public int F()
    {
        var x = Sev.Low | Sev.High;    // CE0131 — Sev is not a flags enum
        return 0;
    }
}
```

**Fix:** declare the enum a `flags enum` if its members are meant to combine;
keep plain enums to `==` comparisons and `switch`. The check fires only on
literal `EnumName.Member` operands, the shape the footgun actually takes. General enum-typed expressions are not tracked.

## CE0132

**Trigger:** `&` between two *different* single-bit members of the same
`flags enum`. Flags members are disjoint bits, so the intersection is provably
empty: the expression is always `None`, and the author almost certainly
meant `|`.

**Example (rejected):**
```spek
flags enum Perm { Read, Write }

module M
{
    public int F()
    {
        var x = Perm.Read & Perm.Write;    // CE0132 — always empty
        return 0;
    }
}
```

**Fix:** use `|` to combine flags. Masking against a union member is a
legitimate intersection and passes:

```spek
flags enum Perm { Read, Write, ReadWrite = Read | Write }

module M
{
    public Perm Mask(Perm p)
    {
        return p & Perm.ReadWrite;    // OK — a genuine mask test
    }
}
```

## CE0133

**Trigger:** A `HasFlag` / `HasAnyFlags` / `HasOnlyFlags` call whose argument
makes the test degenerate. Two shapes: the argument is a literal of a plain
(non-flags) enum, where flag tests are meaningless; or the argument is the
flags enum's `None`, which degenerates each verb its own way:
`HasFlag(Perm.None)` is always true, `HasAnyFlags(Perm.None)` is always
false, and `HasOnlyFlags(Perm.None)` is a disguised equality (it reduces to
`== Perm.None`).

**Example (rejected):**
```spek
enum Sev { Low, High }
flags enum Perm { Read, Write }

module M
{
    public bool A(Sev s)  { return s.HasFlag(Sev.Low); }         // CE0133 — Sev is not a flags enum
    public bool B(Perm p) { return p.HasFlag(Perm.None); }       // CE0133 — always true
    public bool C(Perm p) { return p.HasAnyFlags(Perm.None); }   // CE0133 — always false
    public bool D(Perm p) { return p.HasOnlyFlags(Perm.None); }  // CE0133 — reduces to == Perm.None
}
```

**Fix:** compare plain enums with `==`; test flags emptiness with
`x == Perm.None`:

```spek
flags enum Perm { Read, Write }

module M
{
    public bool IsEmpty(Perm p) { return p == Perm.None; }
    public bool CanRead(Perm p) { return p.HasFlag(Perm.Read); }
}
```

## CE0134

**Trigger:** An actor body reads time directly: `DateTime.Now` / `UtcNow` /
`Today` (and the `DateTimeOffset` equivalents), `Environment.TickCount` /
`TickCount64`, or `Stopwatch.StartNew` / `Stopwatch.GetTimestamp`.

**Severity:** Warning. The code compiles and behaves correctly in production;
the diagnostic protects the virtual-time guarantee in tests.

**Why:** the runtime routes every semantic clock (passivation idleness,
restart windows, `self.Clock` reads) through the system clock, and the test
kit can virtualize that clock (a `TestActorSystem` with `virtualTime: true`
turns hours of idle time into one `AdvanceClock` call). A direct
`DateTime.UtcNow` bypasses the clock, so under virtual time it silently
diverges from every timer and every other clock read. Same posture as
[CE0119](#ce0119), one layer up. The check is scoped to actor bodies:
`program` blocks, modules, and classes are host-side code with no
`self.Clock` and no virtual-time guarantee to uphold, so reading real time
there is legitimate and stays silent.

**Example (warns):**
```spek
message T();

actor Sampler
{
    on T t =>
    {
        var now = DateTime.UtcNow;    // CE0134 (warning) — bypasses the clock
    }
}
```

**Fix:** read through the actor's clock accessor: `self.Clock.GetUtcNow()`
for wall time, `self.Clock.GetTimestamp()` for monotonic measurements. Under
a real-time system they return exactly what the direct reads would; under
virtual time they stay in lockstep with timers.

## CE0135

**Trigger:** a lambda that writes actor state leaves the body it was written
in. A lambda counts as state-writing when it assigns to something rooted at an
actor field or property (its own *or* one inherited from a base actor), calls
one of the actor's own methods that does, calls a mutating method on a
class-typed actor field, directly (`() => helper.Bump()`) or through a local
that aliases one (`var h = helper; () => h.Bump()`), judged by the same
per-class mutating-method classification [CE0087](#ce0087) consults for reader
handlers; invokes a local that itself carries a write (`() => bump()` where
`bump` is a state-writing lambda), or assigns through a `use` shared-region
handle (`() => tally.hits = tally.hits + 1`). It escapes when it is passed as an
argument to any call, stored in an actor field or property, or returned. The
region case is worth pausing on: a handler's region access runs under the
region's reader/writer lock, but a foreign thread invoking that lambda later
holds neither lock, so the write bypasses the region's discipline entirely.

**Escape detection is by reachability, not shape.** A state-writing lambda
escapes whenever it can be *reached* from an escaping value, not only when it is
the argument verbatim. The analysis walks through every construct that carries a
value without consuming it: a ternary or `??`, a `switch` expression's arms, a
tuple, an array literal, a `new`'s constructor arguments and object-initializer
values, and the value a projection lambda returns (`items.Select(x => bump)`),
and taints locals the same way: `var b = bump;`, a copy chain `var b2 = b;`, an
array holding one (`var arr = new Action[] { bump };`), and a conditional
rebind (`if (…) { b = bump; }`) all make the destination carry the write, so a
later escape of it is the same escape as inlining the lambda.

**Why:** a closure is the one construct that can smuggle mutable actor state
past every other rule. Whatever receives the lambda may hold it after the
handler returns and invoke it whenever it likes, on a thread the actor does not
own. The write then lands outside the actor's turn, concurrent with the actor's
own handlers, with the mailbox nowhere in the path. That is the same race
[CE0119](#ce0119) prevents, arriving through a lambda instead of a thread
primitive, so it gets the same severity for the same reason. The other escape
routes are already closed elsewhere: a lambda cannot travel in a message or
come back as a reply, because message fields must be immutable
([CE0010](#ce0010)), and it cannot ride out inside a confined class either,
because the class itself can't leave the actor ([CE0112](#ce0112) for
regions, [CE0137](#ce0137) for spawn arguments). Registration with something
that outlives the turn is the route this rule closes.

**Severity:** Error. A warning would leave the racy code compiling and running,
which is the whole problem. Fires in every actor body: handlers, `init`,
`term`, lifecycle hooks, and actor methods. `module`, `class`, and `program`
bodies have no actor state to leak and are not checked.

**Example (rejected):**
```spek
on Up u =>
{
    var bump = () => seen = seen + 1;   // fine — binding is not escaping
    bump();                             // fine — runs here, inside the turn
    registry.Register(bump);            // CE0135 — Registry outlives the handler
}
```

**Fix:** keep the write inside a turn. Have the callback tell the actor what
happened and do the work in a handler, where the mailbox serialises it:
```spek
on Up u => { registry.Register(() => self.Tell(new Bumped())); }
on Bumped b => { seen = seen + 1; }
```
Capturing by value works too when the lambda only needs a snapshot: read the
field into a local first and let the lambda close over the local.

**Not flagged:** reading actor state. `items.Where(x => x.Id == filterId)` is
the common case and stays unrestricted. A captured read cannot race, though a
read capture handed to *unknown* code draws the softer [CE0136](#ce0136)
warning. Capturing locals, parameters, and messages is likewise unrestricted,
and a state-writing lambda is free to exist, be named, and be invoked inside
its own body. The one write-shaped residue is a method call on a
*foreign-typed* field: whether `log.Append(...)` mutates a `StringBuilder` is
unknowable without foreign type resolution, so the analysis stays silent
rather than guess. That remainder is outside what the static analysis can
prove.

**Synchronous consumers (write is safe):** a *write*-capturing lambda handed to
a LINQ-style synchronous consumer (the same name set [CE0136](#ce0136) trusts
for reads: `Where`, `Select`, `Sort`, `Aggregate`, …) does **not** escape: the
operator invokes the callback here, on this thread, inside the turn, and retains
nothing, so even a mutating comparator (`items.Sort((a, b) => { count = count +
1; return a - b; })`) is safe and compiles.

**Known over-approximation:** the one synchronous consumer deliberately left
*out* of that carve-out is `ForEach`. The analysis still flags
`List.ForEach(x => total = total + x)`, which is safe in fact, because Spek has
`foreach` and the rewrite the diagnostic names is the better code regardless.
```spek
foreach (var x in items) { total = total + x; }
```

## CE0136

**Trigger:** a lambda that captures actor state *read-only* (an actor field or
property, or a `use` region handle) is passed as a call argument to a callee
the compiler does not trust. The same intra-body taint tracking as
[CE0135](#ce0135) applies: binding the lambda to a local first and passing the
local warns at the call site all the same.

**Why:** C# closures capture by reference, so the lambda does not hold a copy
of the field; it holds the field. [CE0135](#ce0135) already makes the write
direction an error. The read direction is milder but real: if the callee
stores the callback and invokes it later from another thread, or hands it to
anything parallel, its reads run concurrently with this actor's writes and can
observe torn or stale state. The compiler cannot see into a foreign callee to
check, so it tells you the hazard exists and lets you decide.

**Severity:** Warning, the same posture as [CE0134](#ce0134). Erroring here
would reject every legitimate synchronous callback the trusted set fails to
name, and a read race corrupts a decision rather than the state itself. The
author is the one who knows whether the callee retains the delegate.

**Trusted (no warning):** two families, and the trust in each is checked, not
assumed. First, foreign callees named like a LINQ operator (`Where`, `Select`,
`OrderBy`, `Aggregate`, and the rest of the standard operator set, plus the
`List<T>` members with LINQ-identical synchronous semantics: `Find`, `FindAll`,
`Exists`, `TrueForAll`, `RemoveAll`, `Sort`, `ConvertAll`), which invoke their
callback immediately and retain nothing. This is a *name table*, not type
resolution: the honest shortcut the [CE0083](#ce0083) and [CE0119](#ce0119)
blocklists already take, with the same accepted cost: a third-party method that
happens to be named `Where` on an unresolvable type is trusted on its name
alone. The name table applies **only to foreign receivers**: a Spek `class`
with a method named `Where` is judged as Spek source, below, not waved through
by the name.

Second, Spek-known callees: the actor's own methods, the stream factory names
(`debounce`, `throttle`, `distinct`, `compose`), `new` of a declared class or
message, and, the case that needs the closest reading, methods on the
actor's class-typed fields and module functions. Their bodies are Spek source,
but Spek does not run the capture rules *inside* a class or module method body,
so the trust cannot rest on "it compiles under these rules." It rests instead on
an explicit check: a confined-class or module method is trusted with a read
capture only when it **keeps every delegate-typed parameter on this thread**:
it may invoke the delegate directly, hand it to a LINQ-style synchronous
consumer, or store it in its own field (the stored copy is reachable only
through this object, and the object's own confinement (no message field
[CE0010](#ce0010), no shared region [CE0112](#ce0112), no spawn argument
[CE0137](#ce0137)) keeps it on this thread). What drops the trust is
*forwarding* the delegate somewhere Spek cannot see: as an argument to a foreign
call or `new`, or as a return value. A method that does that
(`void Take(Action a) { Acme.Global.Store(a); }`) is a sink, and its callers get
their warning. `spawn` arguments are never trusted at all: a child actor runs on
another thread by definition, so handing it a by-reference read of this actor's
state is the race in miniature.

**Example (warned):**
```spek
message Rename(string NewName);

actor Profile
{
    string name = "";
    ExternalHooks hooks = null;

    on Rename r =>
    {
        name = r.NewName;
        hooks.Register(() => self.Log.Info(name));   // CE0136 — who calls this, and when?
    }
}
```

**Fix:** capture a copy, so the callee gets a snapshot no future turn can
change under it:
```spek
on Rename r =>
{
    name = r.NewName;
    var n = name;
    hooks.Register(() => self.Log.Info(n));
}
```
Or keep the read inside a turn and have the callback `Tell` the actor, the
same fix CE0135 teaches for the write direction.

**Not flagged:** storing a read-capturing lambda in an actor field, and
returning one. A field store never leaves the actor: whatever invokes the
stored lambda from this actor's own turns is serialised by the mailbox, and
handing the *field* to a foreign callee later is itself a call argument, which
is where the warning fires. Return position is rare in practice and the
message-shaped routes are already closed by [CE0010](#ce0010), so call
arguments are the one place read captures leave an actor in real code.

**Residue (not caught):** a confined `class` that *stores* a read-capturing
delegate in its own field and later fires it from a **concurrent reader
handler** of the same actor. The per-call trust check judges the storing method
on-thread (which it must, so the ordinary "register a callback we never fire
across threads" case keeps compiling) and the fact that a `reader` turn invokes
the stored delegate concurrently with a writer is a whole-actor, reader/writer
property that no single-method check models. It is the narrow sibling of the
foreign-sink case this rule *does* close.

## CE0137

**Trigger:** a `spawn` argument that *reaches* a confined-class reference to the
spawning actor's state. The shapes flagged: a class-typed actor field, bare or
`self.`-qualified (`spawn<Child>(reg)`, `spawn<Child>(self.reg)`); a local
tainted by one (`var r = reg;` then `spawn<Child>(r)`, caught by the same
statement-ordered taint tracking [CE0135](#ce0135) uses); a local `new`ed while
*capturing* a class field (`var w = new Wrapper(reg); spawn<Child>(w)`; `w` is
fresh, but it holds `reg`); a `new`-initialized local *or* class-typed
`init`/method parameter that is *also* stored in actor state anywhere in the
body, before or after the spawn; and a class-typed region field read through a
`use` handle (on top of the [CE0112](#ce0112) its declaration already draws).
Like [CE0135](#ce0135), detection is by **reachability**: the class reference is
caught even when nested inside a container the argument hands over:
`spawn<Child>(ImmutableArray.Create(reg))`, `spawn<Child>(new Holder { Reg =
reg })`, `spawn<Child>(new Wrapper(reg))` all share `reg` with the child.

**Why:** share-XOR-mutate. A class is mutable, so it stays race-free by having
exactly one owning actor, and the sibling escape routes are already gated: a
class can't ride in a message field or ask reply ([CE0010](#ce0010)) and can't
be a shared-region field ([CE0112](#ce0112)). Spawn arguments were the missing
third route. The child receives the reference and the sender keeps its own, so
two actors hold one mutable object and every write on either side races the
other. The sharpest version of the hazard involves no field write at all: a
class may hold a *callback*, and [CE0136](#ce0136) deliberately trusts a
confined-class receiver with a by-reference read capture of actor state. That
trust is only sound if the class can never reach another actor's thread. This rule is what makes it sound.

**Severity:** Error, for the reason [CE0135](#ce0135) gives: a warning would
leave two actors sharing mutable state, which is the exact condition the
language exists to rule out. Fires in every actor body the escape pass covers
(handlers, `init`, `term`, lifecycle hooks, actor methods).

**Example (rejected):**
<!-- spek-test: ignore; demonstrates the CE0137 trigger -->
```spek
class Registry { int n = 0; public void Bump() { n = n + 1; } }

actor Child
{
    Registry r;
    init(Registry reg) { r = reg; }
}

actor Sender
{
    Registry reg = new Registry();

    on Go =>
    {
        var kid = spawn<Child>(reg);   // CE0137 — sender and child now share 'reg'
    }
}
```

**Fix:** the constructor gift. Construct the instance for the child and keep
nothing, so ownership transfers whole at the spawn:

<!-- spek-test: ignore; illustrative fix -->
```spek
on Go =>
{
    var kid = spawn<Child>(new Registry());   // child is the only owner
}
```

The gift may pass through a local (`var r = new Registry(); spawn<Child>(r);`)
so the sender can build the object up first; what it may not do is *also*
store that local in a field, before or after the spawn. That puts a second
owner back. A class-typed parameter is the same gift one step earlier: it
arrived owned by nobody else, so relaying it straight through to a spawn is a
legitimate gift chain (what the parent passed was checked at the parent's own
spawn), but storing it *and* forwarding it is the two-owner case again. To
share data rather than hand off an object, send an immutable
[`message`](../language/messages.md).

**Not flagged:** a fresh `new` passed inline, a gifted local or received
parameter that never touches actor state (the pure relay), storing a received
parameter when no spawn forwards it, and every non-class argument (primitives,
strings,
messages, `ActorRef`s). A *foreign*-typed field argument is the documented
residue, the same stance [CE0135](#ce0135) and [CE0136](#ce0136) take:
whether a `StringBuilder` field is safe to hand over is unknowable without
foreign type resolution, so the analysis stays silent rather than guess.

## CE0138

**Trigger:** `To<T>()` where the source is an integer type and the target is a
floating type that C# widens implicitly but cannot represent exactly: any of
`int`, `uint`, `long`, `ulong` (and the native-sized forms) to `float`, or
`long`/`ulong` to `double`.

```spek
module M
{
    float Convert(int count)
    {
        return count.To<float>();   // error CE0138
    }
}
```

**Why it's an error:** `To<T>()` admits only lossless conversions, and it
normally borrows C#'s judgment: whatever C# converts implicitly passes. C#'s
judgment has a blind spot. It treats the integer-to-floating widenings as
implicit even though the target's mantissa is narrower than the source's
integer range, so large values round silently: `16777217.To<float>()` would
produce `16777216`. These pairs are exactly where "implicit" and "lossless"
disagree, so Spek rejects them itself.

**Fix:** say what should happen to a value that doesn't fit. `TryTo<float>()`
returns `float?`, null when the value is not exactly representable;
`TryTo<float>(MidpointRounding.ToEven)` rounds explicitly. Every genuinely
lossless widening (`int` to `double`, `int` to `long`, `float` to `double`,
`byte` or `short` to `float`, anything to `decimal`) passes `To<T>()`
untouched. See [Conversions](../language/conversions.md).

## CE0139

**Trigger:** an `on` handler whose pattern names a generic `message`.

```spek
message Envelope<T>(T payload);

actor A
{
    behavior Idle
    {
        on Envelope e => { }   // error CE0139
    }
}
```

**Why it's an error:** a handler pattern has no way to name the concrete type
argument (there is no `on Envelope<int>` form), so the pattern would have to
match the open generic type, which the emitted C# cannot express. Rather than
emit code that fails the C# build with a confusing `CS0305`, the compiler
rejects the handler at the source.

**Fix:** dispatch on a concrete message and carry the payload inside it:

```spek
message IntEnvelope(int payload);
```

Generic messages remain fine to *declare* and to *send*. The restriction is
on keying a handler's dispatch to one. This applies to every handler
visibility, `private` included.

{: .note }
> **File-organization hints are not diagnostics.** The language server also
> nudges you toward the C# conventions of one top-level type per file, named
> after the type. Those are editor *suggestions* (LSP hints): not `CE` codes,
> not emitted by `spekc` or a build, so they live in the editor, not this
> catalog.

## Related reading

- [Messages: the immutability whitelist](../language/messages.md#the-immutability-whitelist)
- [Messaging: Tell/ask scoping](../language/messaging.md)
- [Shared regions](../language/shared-regions.md): the `shared` and `use` forms
- [Actors](../language/actors.md): the `on event` form
- [Persistence: persist / passivate / Restore](../language/persistence.md)
