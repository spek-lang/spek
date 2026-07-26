---
title: Modules
layout: default
parent: Language
nav_order: 12
permalink: /language/modules/
description: "Modules: stateless containers of static methods; where shared, side-effect-free helper code lives, and how handlers call into them."
---

# Modules

[Classes](/language/classes/) gave actors a place to keep mutable helpers
*inside* the isolation boundary. But not every helper needs state. A
method that validates an email address, formats a name, or slugifies a
string has no fields to protect. It takes values in and hands values back.
Hanging that kind of code off an actor or a class would be busywork, and
copying it between actors would be worse.

Spek's home for stateless code is the **module**. A module is a container
of **methods** with no instance of its own, the same shape as a C#
`static class`. It's where pure transformations, validators, formatters,
and stateless infrastructure live. Any actor, class, or other module can
call into it, because there's nothing to own and nothing to share.

<!-- spek-test: compile -->
```spek
module Validators
{
    public bool IsValidEmail(string s)
    {
        return s.Contains("@");
    }
}
```

This emits the static class you'd write by hand:

```csharp
public static class Validators
{
    public static bool IsValidEmail(string s)
    {
        return s.Contains("@");
    }
}
```

{: .note }
> **One method concept.** Spek doesn't distinguish "functions" from
> "methods": a module's methods are written *identically* to an actor's or
> class's methods (same visibility, return type, generics, parameters). The
> only difference is that a module emits as a `static class`, so its methods
> become `static` automatically. You never write the `static` keyword.

{: .note }
> **How this maps to C#.** A `module` is a C# `static class`, a home for stateless
> methods and nothing else: no fields, no instance, no `this`. There is
> deliberately no shared mutable singleton in Spek (a `class` can't declare
> `static` members, and a class instance is owned by one actor), so where C# would
> reach for a static singleton holding state you use an actor, and for stateless
> helpers you use a module.

## Calling a module from a handler

A module method is called by its **qualified name**, `Module.Method(args)`,
from anywhere, including inside a [message handler](/language/messages/).
Because the call is just a static method invocation, there's no mailbox, no
`Tell`, no `Ask`; the result comes straight back like any expression.

<!-- spek-test: compile -->
```spek
message Greet(string name);

module Greetings
{
    public string Build(string name)
    {
        return "Hello, " + name + "!";
    }
}

actor Greeter
{
    on Greet g =>
    {
        string text = Greetings.Build(g.name);
        System.Console.WriteLine(text);
    }
}
```

The same call composes with the [return-to-reply idiom](/language/messaging/):
build the reply message directly from the module's result.

<!-- spek-test: compile -->
```spek
message Normalize(string raw);
message Normalized(string value);

module Text
{
    public string Slugify(string s)
    {
        return s.Trim().ToLowerInvariant().Replace(" ", "-");
    }
}

actor Normalizer
{
    on Normalize msg => return new Normalized(Text.Slugify(msg.raw));
}
```

Because a module holds no state, calling one from a handler is always safe:
there is nothing for two actors to race over. The
[isolation guarantees](/language/isolation/) you rely on between actors are
never at risk, so a module is the natural place to factor out logic that
several actors share.

## Modules are stateless

A module has **no instance fields, no mutable state, no `self`, no
`sender`, no mailbox**. Those are actor concerns. A module is just a
namespace for methods, like an Erlang module or a C# static class. The
moment you need state or concurrency, that's an `actor` again; if you need
per-`ActorSystem` shared state, that's a
[shared region](/language/shared-regions/).

A module's methods are **static** (no instance to belong to); an actor's
or class's methods are **instance** methods (they operate on that object's
state). Same declaration syntax either way: the container decides static
vs instance, so there's no separate `function` keyword or concept.

Within a module, one method calls a sibling **unqualified**, since there's a
single static class to resolve against:

<!-- spek-test: compile -->
```spek
module Geometry
{
    public double Square(double x) { return x * x; }

    public double SumOfSquares(double a, double b)
    {
        return Square(a) + Square(b);   // sibling — no Geometry. prefix
    }
}
```

## Visibility

Modules and their methods take the same visibility modifiers as C#
types and members: `public`, `internal`, `protected`, `private`.
Modules **default to `public`** (they're cross-namespace utility homes;
defaulting narrower would be surprising).

<!-- spek-test: compile -->
```spek
internal module Format
{
    public   string FormatName(string first, string last) { return first; }
    internal string Trim(string s)                          { return s; }
    private  string StripWhitespace(string s)               { return s; }
}
```

## Nested modules

Modules nest as sub-namespacing, Erlang-style. A nested module emits as
a nested `static class`, and you reach into it by chaining the qualifier:
`Outer.Inner.Method(...)`.

<!-- spek-test: compile -->
```spek
message Tick();

module Math
{
    public int A() { return 1; }

    module Inner
    {
        public int B() { return 2; }
    }
}

actor Counter
{
    int n = 0;
    on Tick =>
    {
        n = Math.A() + Math.Inner.B();   // nested qualifier chains
    }
}
```

Methods and nested modules occupy **independent namespaces** inside a
module: a method and a nested module may share a name without
conflict, the same separation C# draws between methods and nested types.
Duplicate methods, or duplicate nested modules, within the same module
are rejected as [CE0013](/reference/errors/#ce0013).

## Parameter modifiers: `in` / `ref` / `out`

Method parameters accept `in`, `ref`, and `out`
modifiers with semantics identical to C#:

| Modifier | Meaning |
|----------|---------|
| `in`     | Readonly reference; passed by reference, callee can't reassign. |
| `ref`    | Aliased mutable reference; must be assigned before the call. |
| `out`    | Caller-bound output; the callee must assign it before returning. |

<!-- spek-test: compile -->
```spek
module Calc
{
    public void AddInto(in int a, ref int acc, out int result)
    {
        acc = acc + a;
        result = acc;
    }
}
```

At the **call site** you must restate the modifier, just like C#. This
helps readability: the reader sees that an argument may be
aliased or written to. A handler accumulating into one of its own fields
reads naturally:

<!-- spek-test: compile -->
```spek
message Add(int x);

module Acc
{
    public void AddInto(in int a, ref int total)
    {
        total = total + a;
    }
}

actor Adder
{
    int total = 0;
    on Add msg =>
    {
        int delta = msg.x;
        Acc.AddInto(in delta, ref total);   // modifiers restated at the call
    }
}
```

The compiler enforces nothing extra here. The emitted C# carries the
modifiers verbatim, and C#'s own compiler checks that the call site's
modifier matches the declaration. Inline `out var` declarations
(`int.TryParse(text, out var n)`) work as they do in C#.

## A note on async

Module methods participate in Spek's [invisible async](/language/async/)
just like handlers do. When a module method calls something that returns a
`Task<T>` (for example a framework I/O API), you work with `T` directly;
the compiler inserts the `await` and marks the method `async`. That
property then propagates to the method's callers automatically.

<!-- spek-test: compile -->
```spek
module Io
{
    public int ReadLength(string path)
    {
        string content = System.IO.File.ReadAllTextAsync(path);  // no await
        return content.Length;
    }

    public int TwiceLength(string path)
    {
        int n = ReadLength(path);    // ReadLength is async → auto-awaited
        return n + n;
    }
}
```

Both methods emit as `async Task<int>`, and both call sites are awaited;
you never annotate the chain. This is only a glance at the feature;
[Async without await](/language/async/) is the full story (why it's safe,
the `var`-for-concurrency lever, and the explicit-`Task<T>` escape hatch).

## Limits

Modules hold no mutable state, by design. If you reach for a field, you
want an [actor](/language/actors/) or a
[shared region](/language/shared-regions/).

So far every method has been written with concrete types. The next
chapter, [Generics](/language/generics/), adds type parameters to modules,
actors, messages, and classes, letting one method serve many types,
with Roslyn doing the type-checking on the emitted code.

## Related reading

- [CE0013](/reference/errors/#ce0013): duplicate-declaration detection,
  including duplicate methods / nested modules inside a module.
- [Sending messages: Tell and Ask](/language/messaging/): the return-to-reply
  idiom a handler uses to hand a module's result back to the asker.
- [Async without await](/language/async/): invisible async, which applies
  inside module methods exactly as it does inside handlers.
