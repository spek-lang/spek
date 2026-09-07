---
title: Enums
layout: default
parent: Language
nav_order: 10
permalink: /language/enums/
description: "Enum declarations: closed sets of named variants that match exhaustively in handlers, plus flags enums whose bit values are correct by construction."
---

# Enums

[Messages](/language/messages/) must be immutable, so a field that names
"which of a fixed set of things this is" wants a type that is closed and
value-comparable by construction. A `string` would compile, but it gives
the compiler nothing to check: a typo like `"hihg"` sails through and
dead-letters at runtime. A Spek `enum` is the type for that job: a named,
closed set of variants that passes the message immutability whitelist
([CE0010](/reference/errors/#ce0010)) without further qualification, and
that handlers can match on *exhaustively*.

This chapter covers declaring an enum, giving variants explicit values,
referencing them, and the sealed-by-default matching that catches a
forgotten case at compile time. It then turns to `flags enum`, the
set-shaped sibling of the same idea: options that combine with `|`, answer
to a small family of query verbs, and carry compile-time guarantees that
C#'s `[Flags]` attribute only gestures at.

## Declaring an enum

`enum` is a top-level declaration, alongside `message` and `actor`. List
the variants between braces:

<!-- spek-test: compile -->
```spek
enum Severity
{
    Low,
    Medium,
    High,
}

message Alert(Severity level, string description);
```

The trailing comma after the last variant is optional. Each variant is a
bare name. An enum is *flat*, so a variant carries no payload of its own
(if you need data alongside the tag, that's what the message fields beside
it are for, as `Alert` shows).

Because enums are usually used inside `message` types, and messages are
always public, visibility defaults to `public`. Narrowing it would force
`public enum` on nearly every declaration, so the default goes the other
way. You can still be explicit, or scope an enum to the assembly:

<!-- spek-test: compile -->
```spek
enum Color { Red, Green, Blue }              // public — same as the next line
public enum Direction { North, South }       // explicit public
internal enum Phase { Warmup, Steady }       // hidden inside the assembly
```

{: .note }
> A flat, value-typed, closed set is exactly what the message immutability
> whitelist wants. An enum is passed by value, can't be mutated after
> construction, and adding a variant means editing source, never a
> runtime surprise. That's why an enum field needs no annotation or
> wrapper to live on a `message`.

### Explicit member values

Variants are auto-numbered from `0` by default, and most enums should
leave it that way. The numbers are an implementation detail. Sometimes the
numbers *are* the point. A code that must match a wire protocol, a
register layout, a vendor's status table: give those variants their values
with `=`:

<!-- spek-test: compile -->
```spek
enum HttpStatus
{
    Ok = 200,
    Created = 201,
    NotFound = 404,
}

message Fetched(HttpStatus status, string body);
```

An unvalued variant continues counting from its predecessor, exactly as in
C#:

<!-- spek-test: compile -->
```spek
enum Priority { Low, Normal = 5, High }
```

`Low` is `0`, `Normal` is `5`, and `High` is `6`. The values pass through
verbatim into the generated C# enum, so there is no Spek-side numbering
arithmetic to learn beyond what you already know.

## Referencing variants

A variant is reached by its qualified name, `EnumName.Variant`, the same
member-access shape you already use for static methods and namespaced
types:

<!-- spek-test: compile -->
```spek
enum Severity { Low, Medium, High }

message Alert(Severity level, string description);

actor Smoke
{
    on Alert a =>
    {
        if (a.level == Severity.High)
        {
            System.Console.WriteLine($"ALARM: {a.description}");
        }
    }
}
```

Nothing new in the grammar made this work. Spek's expression syntax
already covers member access. Declaring the `enum` just adds another named
scope for the parser to resolve `Severity.High` against.

## Matching in handlers

The reason to reach for an enum over a `string` is what happens when a
handler branches on it. A `switch` *expression* over an enum value is
**checked for exhaustiveness**: every Spek enum is *sealed*, so
the compiler knows the complete variant set and insists you handle all of
them (or opt out explicitly). A missing arm is [CE0103](/reference/errors/#ce0103),
caught at compile time.

The check applies when the compiler can see that the value being switched
on is the enum, most directly a local typed as the enum:

<!-- spek-test: compile -->
```spek
enum Severity { Low, Medium, High }

message Alert(Severity level, string text);

actor Pager
{
    on Alert a =>
    {
        Severity level = a.level;
        var channel = level switch {
            Severity.Low    => "email",
            Severity.Medium => "slack",
            Severity.High   => "pager",
        };
        System.Console.WriteLine($"{channel}: {a.text}");
    }
}
```

Drop the `Severity.High` arm and the compiler stops you:

<!-- spek-test: ignore -->
```spek
var channel = level switch {
    Severity.Low    => "email",
    Severity.Medium => "slack",
};
// error[CE0103]: `switch` over 'Severity' is not exhaustive —
//   missing variant: Severity.High. Add an arm for each missing
//   variant or include a `_` discard arm.
```

The main reason for this strictness is **versioning across a rolling
deploy**. Add a `Severity.Critical` variant, redeploy half your nodes, and
every `switch` on the new code that forgot to handle `Critical` fails the
build, instead of quietly falling through a `default` arm in production
under load. The compiler turns "we added a case and forgot to handle it
somewhere" from a runtime incident into a red build.

The same check covers an enum-typed **actor field**, since its type is
declared too:

<!-- spek-test: compile -->
```spek
enum Phase { Idle, Running, Done }

message Advance();

actor Job
{
    Phase phase = Phase.Idle;

    on Advance =>
    {
        // the transition is itself an exhaustive switch over Phase
        phase = phase switch {
            Phase.Idle    => Phase.Running,
            Phase.Running => Phase.Done,
            Phase.Done    => Phase.Done,
        };
    }
}
```

### Opting out with `_`

When a catch-all genuinely *is* the intent, add a `_` discard arm.
CE0103 doesn't fire: the discard is your signal, to the compiler and to
the next reader, that not covering every variant is deliberate:

<!-- spek-test: compile -->
```spek
enum Severity { Low, Medium, High }

message Alert(Severity level);

actor Triage
{
    on Alert a =>
    {
        Severity level = a.level;
        var queue = level switch {
            Severity.High => "now",
            _             => "later",   // explicit opt-out
        };
        System.Console.WriteLine(queue);
    }
}
```

A `when` guard on an arm does **not** count as covering that variant. The
compiler can't prove a guard is always true, so a guarded arm leaves the
variant uncovered. You still need an unguarded arm for it, or a `_`.

### What "the compiler can see the enum" means

Exhaustiveness keys off the switched value's *statically known* type. A
local or field declared as the enum carries that type; a bare member
access on a message (`a.level switch { … }`) does not, so CE0103 won't
fire on it even though it compiles. The fix is the one-line idiom the
examples above already use: bind the value to an enum-typed local first
(`Severity level = a.level;`), then switch on the local. That binding is
what hands the exhaustiveness guarantee to your handler, so make it a
habit when you switch on an enum that arrived in a message.

{: .note }
> The check is scoped to the switch **expression** (`value switch { … }`),
> which is the form you reach for when each branch yields a result. The
> C-style switch **statement** (`switch (value) { case … }`, covered in
> [C# syntax](/language/csharp-syntax/#switch-statement)) is passthrough
> and is *not* exhaustiveness-checked. Prefer the expression when you're
> branching on an enum and want CE0103 watching your back.

## Flags enums

A plain enum answers *which one*. Plenty of fields need to answer *which
ones*: permissions, capabilities, the days a job runs. C# models these
with the `[Flags]` attribute, and the attribute enforces nothing. Values
must be hand-assigned powers of two, collisions are silent, a zero-valued
member satisfies every `HasFlag` check, and combining with `&` instead of
`|` compiles happily and yields the empty set. Spek has no attribute
syntax to inherit that footgun through, so flags-ness becomes part of the
declaration instead: a `flags` modifier, in the same capability-keyword
family as `abstract message`. The compiler then owns what `[Flags]` only
decorates.

<!-- spek-test: compile -->
```spek
flags enum Permissions
{
    Read,
    Write,
    Execute,
}
```

The compiler assigns each variant the next free power of two (`Read = 1`,
`Write = 2`, `Execute = 4`) and provides `None = 0`, the empty set.
Values combine with `|` and test with `HasFlag`, both exactly as a C#
hand would write them:

<!-- spek-test: compile -->
```spek
flags enum Permissions { Read, Write, Execute }

message Grant(Permissions granted);

actor Gate
{
    on Grant g =>
    {
        var requested = Permissions.Read | Permissions.Write;
        if (g.granted.HasFlag(requested))
        {
            System.Console.WriteLine("read-write access granted");
        }
    }
}
```

`HasFlag` keeps its BCL name and its BCL semantics deliberately: the
receiver order you already know, and a multi-flag argument means *all* of
those bits, which is why the `requested` mask above checks for read and
write in a single call.

What you cannot do is declare `None` yourself. A zero-valued member is
the classic flags trap: `value.HasFlag(Zero)` is true for every value, so
a hand-rolled zero member turns every check it appears in into a
tautology. Spek provides the member and rejects a user-declared zero,
under any name ([CE0130](/reference/errors/#ce0130)):

<!-- spek-test: ignore -->
```spek
flags enum Permissions { None, Read, Write }
// error[CE0130]: 'Permissions.None' is provided automatically as the
//   empty set (= 0). Remove the declaration; test emptiness with
//   'x == Permissions.None'.
```

The same declaration check rejects explicit values that aren't powers of
two and rejects two members that share a bit, so a `flags enum` that
compiles has disjoint, meaningful bits. Every guarantee in the rest of
this section stands on that one.

### Named unions

Explicit values are allowed when they keep the invariant: a power of two,
or a union of members declared above it.

<!-- spek-test: compile -->
```spek
flags enum Access
{
    Read = 1,
    Write = 2,
    Execute = 4,
    ReadWrite = Read | Write,
}
```

`ReadWrite` is `3`, and the generated C# spells it symbolically
(`ReadWrite = Read | Write`) so a reader of the emitted code sees the
intent rather than a magic number. A union may only name members declared
before it in the same enum. A forward reference is rejected at the
declaration.

### Gated operators

Bitwise operators on an enum that is *not* declared `flags` don't compile
([CE0131](/reference/errors/#ce0131)). `Severity.Low | Severity.High` is
a meaningless value on an ordinary enum, and in C# it compiles anyway.
When the members happen to be hand-assigned powers of two (the C#
convention this feature replaces), the error recognizes the pattern and
names the fix:

<!-- spek-test: ignore -->
```spek
enum Permissions { Read = 1, Write = 2, Execute = 4 }

var granted = Permissions.Read | Permissions.Write;
// error[CE0131]: Bitwise operation on enum 'Permissions', which is not
//   a flags enum. Every member is a distinct power of two — did you
//   mean 'flags enum Permissions'?
```

Flags-ness is declared, never inferred from the values. A two-member enum
is powers of two by accident, and inference would mean that adding a
third member later changes which operations compile in unrelated files.
The keyword keeps declaration-site intent stable under every future edit.
The gating is also scoped to enums declared in Spek; external .NET enums
keep their C# semantics, with `[Flags]` honored where it matters.

Even on a genuine flags enum, one combination is provably wrong. Because
the declaration check guarantees disjoint bits, `&` between two distinct
single-bit members is always the empty set, and the compiler says so
([CE0132](/reference/errors/#ce0132)):

<!-- spek-test: ignore -->
```spek
var overlap = Access.Read & Access.Write;
// error[CE0132]: 'Access.Read & Access.Write' is always empty: flags
//   members are disjoint bits. Did you mean '|' to combine them?
```

Masking against a union member (`Access.Read & Access.ReadWrite`) is a
legitimate test and stays legal; the diagnostic fires only when the
intersection is provably empty. C# cannot make either claim, because C#
cannot trust the values.

### Querying a flag set

A set supports three distinct membership questions, and the BCL ships a
verb for exactly one of them. Spek keeps that verb and completes the
family. `HasFlag(mask)` is the superset test: is everything in the
argument present in the value? `HasAnyFlags(…)` is the intersection test:
does anything overlap? `HasOnlyFlags(…)` is the subset test: is nothing
set outside the given flags? That last one is the authorization-boundary
check, the question "did we grant more than we meant to?". Exact equality
is already spelled `==`, and none of the four reduces to another.

<!-- spek-test: compile -->
```spek
flags enum Access { Read, Write, Execute, ReadWrite = Read | Write }

message Audit(Access granted);

actor Auditor
{
    on Audit a =>
    {
        Access granted = a.granted;

        var canRead      = granted.HasFlag(Access.Read);                       // superset
        var touchesData  = granted.HasAnyFlags(Access.Read, Access.Write);     // intersection
        var noEscalation = granted.HasOnlyFlags(Access.Read, Access.Write);    // subset
        var exactly      = granted == Access.ReadWrite;                        // exactness

        // "non-empty AND within bounds" — spell out both halves
        var usable = granted != Access.None
                  && granted.HasOnlyFlags(Access.Read, Access.Write);

        System.Console.WriteLine($"{canRead} {touchesData} {noEscalation} {exactly} {usable}");
    }
}
```

Each verb accepts a single pre-combined mask or up to three flags as
separate arguments, so `granted.HasAnyFlags(Access.Read, Access.Write)`
needs no temporary.

`HasOnlyFlags` has one edge worth committing to memory: the empty set is
a subset of everything, so `Access.None.HasOnlyFlags(anything)` is
vacuously true. That is the right answer for an allow-list (nothing
requested, nothing exceeded), but when you mean "non-empty *and* within
bounds", say both halves, as the last line of the example does with
`granted != Access.None && granted.HasOnlyFlags(…)`.

The degenerate calls don't compile ([CE0133](/reference/errors/#ce0133)).
A literal `None` argument is either a constant or a disguised equality
(`HasFlag(None)` is true for every value, `HasAnyFlags(None)` is false
for every value, and `HasOnlyFlags(None)` is just `== None` in costume),
so all three are rejected, each with the direct spelling in the message:

<!-- spek-test: ignore -->
```spek
if (granted.HasFlag(Access.None)) { }
// error[CE0133]: 'HasFlag(Access.None)' is always true. Test emptiness
//   with 'x == Access.None'.
```

Calling any of the family on an enum that isn't `flags` is the same
diagnostic with a different lesson: "Compare with `==` instead, or
declare it `flags enum Severity`."

One habit from the first half of this chapter does not carry over. A
flags value is a set, and a `switch` arm matches an exact value, so
`Access.Read | Access.Write` matches neither a `Read` arm nor a `Write`
arm. Branch on flag values with the verbs above, and save `switch` for
enums that name exactly one thing.

### Raw values and `TryTo`

Flags values tend to leave the process (a permission mask in a database
column, option bits on the wire) and come back as integers. Restoring
them goes through the same [`TryTo`](/language/conversions/) family as
every other checked conversion. For a plain enum, `TryTo` accepts only
defined members: `3` is not a `Priority`, so it converts to `null`. A
flags enum defines *combinations*, not just members, so the flags-aware
rule is that every set bit must correspond to a defined flag.
`raw.TryTo<Access>()` accepts `3` as `Read | Write` without a `ReadWrite`
member existing, accepts `0` as `None`, and returns `null` for `8` or
`9`, where an undefined bit is set. `Enum.IsDefined`, the test a C# hand
would reach for, stops at named members and gets flags wrong. `TryTo` is
the right spelling in both worlds.

<!-- spek-test: compile -->
```spek
flags enum Access { Read, Write, Execute }

message LoadAccess(int raw);

actor Loader
{
    on LoadAccess m =>
    {
        var access = m.raw.TryTo<Access>();
        if (access != null)
        {
            System.Console.WriteLine($"restored: {access}");
        }
    }
}
```

### Adding `flags` to an existing enum

The modifier changes member numbering, so retrofitting it is not always
free. On an enum whose members already carry hand-assigned powers of two,
adding `flags` is a no-op. Explicit values are kept as written. On an
enum that relied on auto-numbering, it renumbers every member. `Low, Medium, High` is `0, 1, 2` as a plain enum and becomes `1, 2, 4`
under `flags`, with the provided `None` taking `0`. If those numbers ever
left the process (persisted [region state](/language/persistence/), wire
values, any integer you round-trip through `TryTo`), renumbering is a
breaking change to stored data. Assign the values explicitly *before*
adding the modifier, or migrate the stored values alongside the deploy.

## What the emitter generates

A Spek enum lowers to a plain C# enum, one-for-one:

```csharp
// Generated from `enum Severity { Low, Medium, High }`
public enum Severity
{
    Low,
    Medium,
    High,
}
```

Unvalued variants are auto-numbered from `0`, standard C# semantics, and
explicit values pass through verbatim. Because the emitted type is an
ordinary C# enum, the switch arms lower to a normal C# pattern `switch`:
there is no boxing or allocation, and each arm is an integer compare.

A `flags enum` additionally carries `[System.Flags]` in the generated C#,
with the provided `None` and the computed powers of two written out:

```csharp
// Generated from `flags enum Access { Read, Write, Execute, ReadWrite = Read | Write }`
[System.Flags]
public enum Access
{
    None = 0,
    Read = 1,
    Write = 2,
    Execute = 4,
    ReadWrite = Read | Write,
}
```

The `[Flags]` attribute is for interop. It makes
`ToString()` render a combined value as `"Read, Write"` and keeps
`Enum.Parse` symmetrical, and that is all it ever did in C# too. The
guarantees live in the Spek compiler. Consumers of the generated assembly
see a well-formed flags enum.

## Limits

What remains off the table is payloads. A variant is a bare name and
can't carry associated data, so information that travels with the tag
belongs in the surrounding message's fields, as `Alert` showed at the top
of the chapter. Cross-file references, on the other hand, just work: an
enum declared in one file is visible to messages and actors in another
within the same compilation, the same as any other top-level declaration.

## Next

Enums round out the data side of a message: a closed set of tags the
compiler can check for you. Next, [Conversions](/language/conversions/)
stays with values for one more chapter: how a raw integer off the wire
becomes (or refuses to become) one of these variants, and why Spek has no
cast operator.

## Related reading

- [Messages: the immutability whitelist](/language/messages/): why enums
  are accepted as message field types without qualification.
- [Conversions: `To` and `TryTo`](/language/conversions/): the checked
  conversion family, including the flags-aware definedness rule.
- [C# syntax: switch statement](/language/csharp-syntax/#switch-statement):
  the passthrough switch *statement* (not exhaustiveness-checked).
- [CE0103](/reference/errors/#ce0103): the non-exhaustive-switch
  diagnostic.
- [CE0130](/reference/errors/#ce0130)–[CE0133](/reference/errors/#ce0133):
  the flags-enum declaration and usage diagnostics.
- [CE0010](/reference/errors/#ce0010): the message-field type whitelist.
