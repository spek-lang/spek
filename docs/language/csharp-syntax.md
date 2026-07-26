---
title: C# syntax in bodies
layout: default
parent: Language
nav_order: 15
permalink: /language/csharp-syntax/
description: "The C# expression and statement subset Spek passes straight through inside handler and method bodies: named arguments, tuples, arrays, initializers, string forms, using var, switch statements, throw expressions, default. Roslyn type-checks it."
---

# C# syntax in bodies

[Lambdas](/language/lambdas/) were the first piece of plain C# you wrote inside
a handler, and they hinted at a larger rule. Spek compiles to C#, so the body
of a handler and the body of a [module](/language/modules/) method are, for
the most part, *just C#*. The actor-shaped surface (`actor`, `on`, `become`,
`persist`, `sender`) is Spek's. Everything between the braces is ordinary
expression-and-statement code, written as-is and **emitted verbatim**.

This chapter is the catalog of that subset. The design rule is **C# idioms
first**: when you reach for a familiar construct, it is almost certainly already
here, spelled exactly the way C# spells it. And because the code passes through
untouched, **Roslyn does the type-checking**: a misuse surfaces as a normal C#
error on the generated code, not a Spek-specific one.

{: .note }
> Everything on this page is **passthrough**. Spek parses it, emits the same
> shape, and lets the C# compiler validate it. There is no Spek-level type
> system involved, so these compose with any BCL or third-party API exactly as
> they would in C#. The Spek-specific `CE` rules from earlier chapters
> ([CE0010](/reference/errors/#ce0010) on messages,
> [CE0085](/reference/errors/#ce0085) on ownership) still apply *around* this
> code; they just don't reach *inside* an expression.

## Bodies are bodies

The same C# flows through wherever a body is allowed: a handler body, a module
function, a [class](/language/classes/) method. The only difference is what's in
scope. Inside a handler you can name fields, the bound message, and `sender`;
inside a module method you can't, because there's no actor. The C# in between
is identical.

Here's a handler body leaning on three constructs at once (a `var` local, a
switch *expression*, and string interpolation) alongside the field and
message-binding access you already know from
[Sending messages](/language/messaging/):

<!-- spek-test: compile -->
```spek
message Charge(decimal amount);
message Receipt(string line);

actor Register
{
    decimal taken = 0.00m;

    on Charge c =>
    {
        taken = taken + c.amount;
        var tier = c.amount switch
        {
            > 100m => "large",
            > 0m   => "normal",
            _      => "void"
        };
        return new Receipt($"{tier}: {c.amount:C2} (running {taken:C2})");
    }
}
```

Everything past `taken = taken + c.amount;` is the C# subset this chapter
documents. The rest of the page shows each construct on its own.

## Named arguments

Pass arguments by parameter name, in any order, same as C#:

<!-- spek-test: compile -->
```spek
module Geometry
{
    public double RoundIt(double value)
    {
        return System.Math.Round(value: value, digits: 2);
    }
}
```

## Tuples

Tuple literals `(a, b)` build a `ValueTuple`; C# infers the element types. A
single parenthesized expression stays a grouping; a tuple needs at least one
comma.

<!-- spek-test: compile -->
```spek
module Pairs
{
    public int FirstOf()
    {
        var pair = (1, "one");
        return pair.Item1;
    }
}
```

## String interpolation

`$"…"` interpolation works, and each `{ … }` hole is a full Spek expression, so
the field-rewriting you saw in the handler above happens *inside* the hole, and
the `,alignment` / `:format` suffixes pass through verbatim:

<!-- spek-test: compile -->
```spek
module Format
{
    public string Receipt(string name, decimal total, int items)
    {
        return $"{name,-12} {items} items  {total:C2}";
    }
}
```

## String forms

Beyond the plain `"…"`, the extended C# string forms all pass straight through:
verbatim `@"…"` (backslashes are literal), raw `"""…"""` (no escape
processing, handy for embedded JSON or quotes), and the
verbatim-interpolated `$@"…"` / `@$"…"`:

<!-- spek-test: compile -->
```spek
module Templates
{
    public string Windows() { return @"C:\spek\out.txt"; }

    public string Json()
    {
        return """{ "ok": true }""";
    }
}
```

## Array creation

Implicit-typed (`new[] { … }`, element type inferred), explicitly typed
(`new T[] { … }`), and **sized/uninitialized** (`new T[n]`, a buffer of length
`n`):

<!-- spek-test: compile -->
```spek
module Buffers
{
    public int Sizes(int n)
    {
        var inferred = new[] { 1, 2, 3 };        // int[]
        var typed    = new string[] { "a", "b" }; // string[]
        var buffer   = new byte[n];               // uninitialized, length n
        return inferred.Length + typed.Length + buffer.Length;
    }
}
```

## Array types

`T[]` (and jagged `T[][]`) work in any type position, whether returns,
parameters, or locals:

<!-- spek-test: compile -->
```spek
module Sums
{
    public int Total(int[] xs)
    {
        var total = 0;
        foreach (var x in xs) { total = total + x; }
        return total;
    }
}
```

{: .note }
> Arrays are **mutable**, so an array can't be a `message` field; that's a
> [CE0010](/reference/errors/#ce0010) error (see
> [Messages](/language/messages/)). Use an `ImmutableArray`/`ImmutableList` for
> message payloads.

## Object & collection initializers

`new T { … }` (object initializer with property assignments) and
`new T(args) { … }` / collection initializers all work; Roslyn decides which
form is valid for the type:

<!-- spek-test: compile -->
```spek
module Init
{
    public int Build()
    {
        var list = new System.Collections.Generic.List<int> { 1, 2, 3 };
        var sb   = new System.Text.StringBuilder { Capacity = 64 };
        return list.Count + sb.Capacity;
    }
}
```

## `using var`

A `using var` local is disposed at scope exit, the natural idiom for the
`IDisposable` runtime types (`ActorSystem`, `TestActorSystem`) and any BCL
resource:

<!-- spek-test: compile -->
```spek
module Resources
{
    public long Read()
    {
        using var stream = new System.IO.MemoryStream();
        return stream.Length;
    }
}
```

## Switch statement

Spek has both forms. The **switch *expression*** produces a value (one
expression per arm; see [Enums](/language/enums/) for exhaustive matching); the
**switch *statement*** is the C-style value-less branch with `case`/`default`
labels and full statement bodies, including shared-label fall-through and
`when` guards on patterns:

<!-- spek-test: compile -->
```spek
module Http
{
    public string Label(int code)
    {
        switch (code)
        {
            case 200:
                return "ok";
            case 404:
            case 500:                 // shared label
                return "error";
            default:
                return "unknown";
        }
    }
}
```

Type patterns and guards work in case labels, binding a variable for the body:

<!-- spek-test: compile -->
```spek
module Describe
{
    public string Of(object o)
    {
        switch (o)
        {
            case int n when n > 0:
                return "positive";
            case string s:
                return s;
            default:
                return "other";
        }
    }
}
```

{: .note }
> The switch statement adds no capability over `if / else if` (which also takes
> full statement blocks); it's there for familiarity. Reach for the switch
> *expression* when each branch yields a value; the statement when each branch
> *does* something.

## Throw expressions

`throw` in the null-coalescing position, the canonical null-or-throw:

<!-- spek-test: compile -->
```spek
module Require
{
    public string NonNull(string? s)
    {
        return s ?? throw new System.ArgumentNullException(nameof(s));
    }
}
```

## `default` / `default(T)`

The default value of a type: bare `default` (type inferred from context) or
`default(T)`, including generic types:

<!-- spek-test: compile -->
```spek
module Defaults
{
    public int Zero() { return default(int); }

    public object EmptyList() { return default(System.Collections.Generic.List<int>); }

    public int Inferred() { int x = default; return x; }
}
```

## Also passthrough

The everyday operator and control-flow surface flows through the same way:
`foreach` / `do…while` / `break` / `continue`; bitwise `& | ^ ~` and shift
`<< >>`; null-coalescing `??`; `is` (with capture) and `as`;
null-conditional `?.` / `?[`; and compound assignment `+=`/`%=`/`??=`/etc.
There is no cast operator; conversions go through `To<T>()` and `TryTo<T>()`
(see [Conversions](/language/conversions/)).

## Where passthrough stops

The line is drawn at *concurrency*, not syntax. A handler runs on a shared
thread pool, so a blocking call (`Task.Wait()`, `.Result`, a synchronous
`Thread.Sleep`) would stall a worker and starve sibling actors. Spek doesn't
pass those through unchanged: it either auto-awaits them (see
[Async without await](/language/async/)) or flags the blocking shape. That's the
one place this chapter's "write it like C#" rule yields to the actor model, and
[Common pitfalls](/language/footguns/) walks through each case.

## Next

You've now seen the full vocabulary a body can use. The
[next chapter](/language/shared-regions/) steps back out to the actor surface:
**shared regions** let many actors *read* the same state concurrently while a
single writer mutates it. That's share-XOR-mutate ([Isolation and
ownership](/language/isolation/)) scaled up from one actor to a region.

## Related

- [Common pitfalls](/language/footguns/): where Spek does *not* pass C# through
  unchanged (blocking calls, sync I/O), and what it does instead.
- [Async without await](/language/async/): invisible async over Task-returning
  calls.
- [Error codes](/reference/errors/): the Spek-specific `CE` rules layered on
  top of the C# passthrough.
