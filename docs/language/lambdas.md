---
title: Lambdas
layout: default
parent: Language
nav_order: 14
permalink: /language/lambdas/
description: "Lambda expressions and delegate-typed values inside handler and method bodies: C#'s exact lambda syntax, emitted verbatim, type-checked by Roslyn."
---

# Lambdas

[Modules](/language/modules/) gave you somewhere to put reusable
helper functions, and most of those helpers reach for the .NET
standard library: `List<T>`, LINQ, `Array.Sort`. Nearly every one of
those APIs wants a *function* as an argument: a predicate to filter by,
a selector to project with, a comparer to order by. A **lambda** is how
you write that function inline.

Spek lambdas are C#'s lambdas, character for character. There is no
Spek-specific lambda syntax to learn: the single-bare-parameter form,
the parenthesised list, optional parameter types, and expression-or-block
bodies are exactly C#'s. At emit time each lambda lowers one-to-one to a
C# lambda, so capture, type inference, and conversion to `Func<>` /
`Action<>` are all Roslyn's job, the same "lean on the C# layer" stance
you saw with [generics](/language/generics/).

{: .note }
> **Why borrow C#'s syntax wholesale?** The dominant use of a lambda
> inside an actor is calling a BCL or third-party API that expects a
> `Func<>` or `Action<>`. If Spek invented its own anonymous-function
> spelling, every one of those call sites would need a mental
> translation step. Matching C# exactly means the millions of LINQ
> examples already on the internet are also valid Spek.

## The main use: LINQ inside a handler

Here is the shape you will write most often, filtering and projecting
a collection with a method chain, each link taking a lambda:

<!-- spek-test: compile -->
```spek
using System.Linq;
using System.Collections.Generic;

message Sweep();

actor Inventory
{
    List<int> quantities = new List<int>();

    on Sweep =>
    {
        var lowStock = quantities.Where(q => q < 10).ToList();
    }
}
```

`q => q < 10` is a lambda: a function taking one parameter `q` and
returning the boolean `q < 10`. `Where` calls it once per element. The
lambda's body and `q`'s type are never spelled out; Roslyn infers `q`
is an `int` because `quantities` is a `List<int>`.

The same lambda shape works just as well inside a [module](/language/modules/)
method, away from any actor:

<!-- spek-test: compile -->
```spek
using System.Linq;
using System.Collections.Generic;

module Tax
{
    public List<double> WithVat(List<double> prices)
    {
        return prices.Select(p => p * 1.2).ToList();
    }
}
```

## The four forms

A lambda is *parameters* `=>` *body*. There are four ways to write the
parameter part and two ways to write the body, all of them lifted
directly from C#. This module exercises each form:

<!-- spek-test: compile -->
```spek
using System;

module Builders
{
    public long Demo()
    {
        // single bare parameter, expression body
        Func<int, int> increment = x => x + 1;

        // parenthesised list, types inferred
        Func<int, int, int> add = (x, y) => x + y;

        // parenthesised list, parameters typed explicitly
        Func<int, int, int> addTyped = (int x, int y) => x + y;

        // no parameters
        Func<long> now = () => DateTimeOffset.UtcNow.ToUnixTimeSeconds();

        // block body — any statements, must `return` a value
        Func<int, int> transform = x =>
        {
            var doubled = x * 2;
            return doubled + 1;
        };

        return increment(1) + add(2, 3) + addTyped(4, 5)
             + transform(6) + now();
    }
}
```

A few rules fall out of these forms:

- **Bare vs parenthesised.** A single parameter can drop its
  parentheses (`x => …`); zero or two-plus parameters always need them
  (`() => …`, `(x, y) => …`).
- **Parameter types are optional.** Omit them and Roslyn infers each
  type from the target: the delegate type the lambda is being assigned
  to or passed as. Add them when inference needs the hint.
- **Expression body vs block body.** An expression body (`x => x + 1`)
  *is* the return value. A block body (`x => { … }`) holds statements
  and must `return` explicitly when the delegate returns a value.

A block body may contain any statement you could write in a normal Spek
block (`var` declarations, `if`, `foreach`, nested calls), except the
statements that only make sense inside an actor handler. `become`,
`persist`, and the `sender` reference belong to the [handler](/language/actors/),
not to an arbitrary function value, so they are out of scope inside a
lambda.

## Where a lambda may appear

A lambda is an expression, but it is not valid in *every* expression
position; it needs a context that supplies its delegate type. In
practice that means three places.

**As a call argument.** This is the common case, and the one you have
already seen:

<!-- spek-test: compile -->
```spek
using System;
using System.Linq;
using System.Collections.Generic;

message Report();

actor Leaderboard
{
    List<int> scores = new List<int>();

    on Report =>
    {
        var top3 = scores
            .OrderByDescending(s => s)
            .Take(3)
            .ToList();
    }
}
```

**As a `var` or typed declaration initializer.** A delegate-typed local
holds a lambda and is invoked later like a method, handy for naming a
rule you apply more than once in a handler:

<!-- spek-test: compile -->
```spek
using System;

message Quote(int Nights);

actor Pricing
{
    on Quote q =>
    {
        Func<int, decimal> ratePerNight = nights => nights > 7 ? 80m : 100m;
        var total = ratePerNight(q.Nights) * q.Nights;
        return total;
    }
}
```

**As a switch-expression arm result.** A `switch` arm can yield a lambda
just as it can yield any other value:

<!-- spek-test: compile -->
```spek
using System;

module Adjustments
{
    public Func<int, int> For(int mode)
    {
        return mode switch {
            0 => x => x + 1,
            _ => x => x - 1
        };
    }
}
```

{: .warning }
> **A lambda needs a *declaration*, not a plain assignment.** A lambda
> is fine on the right of a `var`/typed declaration (`Func<int, int> f =
> x => x + 1;`) but **not** on the right of a bare assignment to an
> existing variable (`f = x => x + 1;` is a syntax error). When you need
> a different function under the same name, declare a fresh
> delegate-typed local (`Func<int, int> g = x => x + 1;`). There is no
> in-place reassignment form.

## Captures and closures

A lambda can read the local variables, parameters, and actor fields in
scope where it is written. This is *capture*, and it follows C#'s rules
exactly:

<!-- spek-test: compile -->
```spek
using System.Linq;
using System.Collections.Generic;

message Recompute(int Floor);

actor Filter
{
    int threshold = 5;
    List<int> readings = new List<int>();

    on Recompute m =>
    {
        var floor = m.Floor;
        var passing = readings
            .Where(r => r > threshold)   // captures the field
            .Where(r => r >= floor)      // captures the local
            .ToList();
    }
}
```

The first lambda captures the `threshold` field; the second captures the
`floor` local. Because capture is by *variable*, not by value, a
long-lived lambda stored in a field sees later mutations to what it
captured, again exactly C#'s closure behaviour.

This stays inside Spek's [isolation model](/language/isolation/): a
lambda only ever captures state from *its own* actor's scope. There is
no way to capture another actor's field, because no other actor's field
is ever in scope. So long as the lambda runs where it was written,
inside the handler on the thread running the actor's turn, nothing
about share-XOR-mutate is at risk.

The boundary worth knowing is what happens if a capturing lambda
*outlives* the handler. A lambda that *writes* actor state is the one
that could genuinely race, and the compiler refuses to let it leave;
reading captured state is the everyday case, and the compiler leaves it
alone until the lambda is handed to code it cannot see into, where it
warns instead. Either way, such a lambda is free to exist inside the
handler that wrote it. You can name it, and you can call it, because
both happen inside the actor's turn:

<!-- spek-test: compile -->
```spek
message Up();

actor Auditor
{
    int seen = 0;

    on Up u =>
    {
        var bump = () => seen = seen + 1;
        bump();                             // fine: runs here, inside the turn
    }
}
```

What a writing lambda may not do is leave. Passing it to a call, storing
it in an actor field, or returning it all hand the lambda to something
that outlives the turn and can invoke it on a thread the actor does not
own. Each of those is [CE0135](/reference/errors/#ce0135), and "writes"
is judged the same way everywhere else in the language: a direct
assignment to a field or property, a call to one of the actor's own
mutating methods, a mutating method on a class-typed field, or an
assignment through a `use` region handle all count.

```spek
on Up u =>
{
    var bump = () => seen = seen + 1;
    registry.Register(bump);            // CE0135 — Registry outlives the handler
}
```

Two neighbouring rules close the other doors, so the guarantee holds all
the way round. A lambda cannot travel in a message or come back as a
reply, because message fields must be immutable
([CE0010](/reference/errors/#ce0010)), and Spek source cannot start a
thread or a timer of its own to run one
([CE0119](/reference/errors/#ce0119)). Registration with a foreign API
was the route left open, and CE0135 is what closes it.

The two idioms that get you past the diagnostic are the ones you would
want anyway. Capture what you need by value, reading the field into a
local so the lambda closes over a snapshot rather than the actor. Or
have the callback `Tell` the actor a message and do the work in a
handler, where the mailbox serialises it and writing state is safe.

Reads get a softer version of the same treatment. Capture is by
reference, so a lambda that merely mentions a field carries the live
field with it, and a callee that stores or parallelizes the callback
will read that field concurrently with the actor's writes.

Handing such a lambda to LINQ or to the stream operators is fine and
stays silent. So is handing it to a confined-class or module method that
keeps the delegate on this thread: invokes it, hands it to a synchronous
LINQ operator, or stores it in its own field, where the object's
confinement ([CE0137](/reference/errors/#ce0137) with
[CE0010](/reference/errors/#ce0010) and
[CE0112](/reference/errors/#ce0112)) keeps the stored copy single-owner.
What the compiler will not vouch for is a method that *forwards* the
delegate somewhere it cannot see: a foreign call, a `new`, a return
value. That method is a sink, and a read capture handed to it draws
[CE0136](/reference/errors/#ce0136), the warning that names the captured
field and spells out the copy idiom (`var n = name;` before the lambda,
then close over `n`). It is a warning rather than an error because the
capture is harmless whenever the callee only calls it synchronously, and
only the author knows.

One honest cost comes with each rule. `ForEach` is the deliberate
over-approximation: `List.ForEach(x => total = total + x)` is safe in
truth (the callback runs immediately, on this thread) and still
rejected, because Spek has `foreach` and the diagnostic points you at the
clearer loop. Its synchronous siblings `Sort`, `Where`, and the rest of
the LINQ set are *not* rejected: a write handed to one runs inside the
turn.

Past both rules lie the residues neither can reach: a method call on a
foreign-typed field the analysis cannot classify; a delegate laundered
through reflection after `interop using`; and a confined
class that stores a read-capturing delegate and then fires it from a
concurrent reader handler, where the storage looks on-thread and the
race lives in reader/writer concurrency no per-method check models.
Those remainders are outside what the static analysis can prove.

## Async lambdas are invisible too

You never write `async` on a lambda; the modifier is not part of the
grammar, the same way you never write `await` on a Task-returning call.
[Invisible async](/language/async/) reaches *into* lambdas: when you pass
a callback to an API whose delegate type returns a `Task`, and the
callback makes a Task-returning call in statement position, Spek
rewrites the lambda to `async` and awaits that call for you.

<!-- spek-test: parse -->
```spek
using System.Threading.Tasks;

message Configure();

actor Middleware
{
    on Configure =>
    {
        pipeline.Use(ctx =>
        {
            ProcessAsync(ctx);   // auto-awaited; the lambda becomes async
            next(ctx);
        });
    }
}
```

The gate is the delegate's return type: only a `Task`/`ValueTask`-returning
delegate gets this treatment, so making a value-returning `Func<int, int>`
"async" can never silently change its signature. See
[Async without await](/language/async/) for the full propagation rules.

## What Spek does not accept

A handful of C# lambda spellings are deliberately outside the grammar:

- **An explicit `async` lambda** (`async x => …`). Invisible async
  handles the async case, as above, so the modifier is never written.
- **A `static` lambda** (`static x => …`). The capture-suppressing
  modifier is not parsed.
- **LINQ *query* syntax** (`from x in xs where … select …`). Only the
  *method-chain* form (`xs.Where(…).Select(…)`) is a Spek expression.
  The method chains are the recommended style anyway and read the same
  in both languages.

Everything that *is* accepted lowers verbatim. Spek does not type-check
a lambda body itself; if the emitted lambda does not type-check, the C#
compiler reports a `CS####` against the generated code, pointing at the
offending line, the same passthrough contract that governs
[generics](/language/generics/) and the rest of the
[C# syntax in bodies](/language/csharp-syntax/).

## How the `=>` token stays unambiguous

The `=>` arrow does triple duty (handler arrows, switch-expression
arms, and lambdas), but the grammar resolves all three without
ambiguity:

- `on Pattern =>` is a **handler arrow** (only ever after `on`).
- `pattern =>` inside `… switch { … }` is a **switch arm** separator.
- `params =>` anywhere else is a **lambda**.

The interesting case is the two of them nested: a switch arm whose
result is itself a lambda:

<!-- spek-test: compile -->
```spek
using System;

module Selectors
{
    public Func<int, int> Choose(int mode)
    {
        return mode switch {
            0 => x => x + 1,   // first => is the arm; second is the lambda
            _ => x => x - 1
        };
    }
}
```

The parser's ALL(\*) lookahead reads the first `=>` of each arm as the
arm separator and the second as the lambda's parameter separator, so
this reads naturally with no extra parentheses.

## Where to go next

Lambdas are the last piece of Spek's expression syntax that needed its
own chapter. The remaining everyday C# you reach for inside a body
(tuples, object initializers, `using var`, switch *statements*, named
arguments) all flows straight through to Roslyn the same way. The next
chapter, [C# syntax in bodies](/language/csharp-syntax/), is the
catalogue of exactly which constructs are supported.
