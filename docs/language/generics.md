---
title: Generics
layout: default
parent: Language
nav_order: 13
permalink: /language/generics/
description: "Type parameters on classes, actors, messages, methods, and functions, with where-clause constraints: lowered verbatim to C# generics, so Roslyn type-checks them."
---

# Generics

The [classes](classes.md) and [modules](modules.md) you've
written so far have all been *concrete*: a `class Box` that holds an `int`, a
module method that takes a `string`. But a box that holds an `int` and a box
that holds a `string` are the same code with one type swapped, and copying it
once per element type is exactly the duplication generics exist to remove.

Spek lets you parameterize a type or routine over the types it works with. A
`class Stack<T>`, an `actor Cache<TKey, TValue>`, a `message Cell<T>`, and a
function `T Identity<T>(T x)` all let the caller fill in `T` at the use site.

The whole feature is **passthrough to C#**. Spek emits the type parameters,
type arguments, and `where` constraints verbatim, and the C# compiler does the
type-checking, inference, and constraint-solving. There is no second type
system to learn: if it's valid C# generics, it's valid Spek generics. This is
the same "C# idioms first, Roslyn type-checks them" rule you saw with
[C# syntax in bodies](csharp-syntax.md), now applied to declarations.

## Generic classes

Put a type-parameter list in angle brackets after the class name. Inside the
body, the parameter is an ordinary type, usable for fields, the `init`
constructor, parameters, and return types:

<!-- spek-test: compile -->
```spek
class Box<T>
{
    T item;
    init(T initial) { item = initial; }

    public T    Get()    { return item; }
    public void Set(T v) { item = v; }
}
```

At the use site you supply the type argument when you construct the value:
`new Box<int>(0)` makes a box of `int`, `new Box<string>("hi")` a box of
`string`: one declaration, every element type. Because a `class` is a
[confined actor-local helper](classes.md), a `Box<int>` lives inside a
single actor and obeys share-XOR-mutate exactly like a non-generic class:

<!-- spek-test: compile -->
```spek
class Box<T>
{
    T item;
    init(T initial) { item = initial; }

    public T    Get()    { return item; }
    public void Set(T v) { item = v; }
}

message Put(int n);
message Read();
actor Counter
{
    Box<int> box = new Box<int>(0);

    writer on Put p => { box.Set(p.n); }
    reader on Read  => { return box.Get(); }
}
```

Multiple type parameters are a comma-separated list. The convention, borrowed
from .NET, is a descriptive `T`-prefixed name when there's more than one:

<!-- spek-test: compile -->
```spek
class Pair<TFirst, TSecond>
{
    TFirst  first;
    TSecond second;
    init(TFirst f, TSecond s) { first = f; second = s; }

    public TFirst  First()  { return first; }
    public TSecond Second() { return second; }
}
```

## Generic functions and methods

A routine can have its own type parameters even when its enclosing type does
not. The list goes right after the method or function name, C#-style. Module
methods ([modules](modules.md) lower to C# static classes) are the
natural home for generic helpers:

<!-- spek-test: compile -->
```spek
module Util
{
    public T Identity<T>(T x)       { return x; }
    public U Second<T, U>(T a, U b) { return b; }
}
```

A type argument at the call site is usually **inferred** from the arguments
(`Util.Identity(p.n)` figures out `T` is `int`), or you can spell it out
explicitly with `Util.Identity<int>(5)`:

<!-- spek-test: compile -->
```spek
module Util
{
    public T Identity<T>(T x) { return x; }
}

message Ping(int n);
actor Echo
{
    reader on Ping p => { return Util.Identity(p.n); }
}
```

The same form works for a method on a class, and a generic method can introduce
a *fresh* type parameter on top of its class's: here `OrElse<U>` adds `U`
alongside the class's `T`:

<!-- spek-test: compile -->
```spek
class Box<T>
{
    T item;
    init(T initial) { item = initial; }

    public T Get()              { return item; }
    public U OrElse<U>(U other) { return other; }
}
```

## Generic actors and messages

Actors and messages take type parameters the same way. A generic `message`
carries a typed payload; a generic `actor` declares its parameters on the
actor name:

<!-- spek-test: compile -->
```spek
message Cell<T>(T value);

message Put(int n);
actor Cache<TKey, TValue>
{
    int count = 0;
    writer on Put p => { count = p.n; }
}
```

A type parameter used as a message field (the `T value` in `Cell<T>`) passes
the [message immutability check](messages.md) (CE0010). A bare type
parameter carries no mutable state of its own, so it can't smuggle a mutable
field into a message record.

Type arguments nest the way you'd expect: a concrete `Inner<int>` can be the
field type of another message, and the whole thing emits verbatim:

<!-- spek-test: compile -->
```spek
message Inner<T>(T v);
message Outer(Inner<int> a);
```

## Constraints

A bare `T` can only be assigned, returned, and passed around. The compiler
knows nothing else about it, so `a.CompareTo(b)` or `new T()` won't type-check.
A `where` clause tells the compiler what `T` is *allowed to be*, which is what
lets generic code actually *do* things with it. The syntax and meaning are
identical to C#:

<!-- spek-test: compile -->
```spek
module Algo
{
    public T Larger<T>(T a, T b) where T : System.IComparable<T>
    {
        if (a.CompareTo(b) > 0) { return a; } else { return b; }
    }
}
```

The `: System.IComparable<T>` constraint is what makes `a.CompareTo(b)` legal.
The `new()` constraint, likewise, is what makes `new T()` legal:

<!-- spek-test: compile -->
```spek
class Factory<T> where T : new()
{
    public T Create() { return new T(); }
}
```

Everything C# allows in a `where` clause flows through: `class`, `struct`,
`new()`, base classes, interfaces, and other type parameters. Combine several
on one parameter with commas, and give each parameter its own clause:

<!-- spek-test: compile -->
```spek
module Make
{
    public T Fresh<T>() where T : class, new()                  { return new T(); }
    public U Pick<T, U>(T a, U b) where T : class where U : struct { return b; }
}
```

Actors take constraints too: the clause follows the type-parameter list, after
any of the actor's other header:

<!-- spek-test: compile -->
```spek
message Go();
actor Holder<T> where T : class
{
    writer on Go => { }
}
```

## Where the type-checking happens

Because generics are passthrough, a *grammar* mistake (a stray `<`, a
constraint on a kind that can't take one) is still a Spek
[CE](../reference/errors.md). Spek's parser owns the syntax. But a *type* mistake
is reported by **Roslyn**, on the generated C#:

{: .note }
> A generic misuse (an unsatisfied constraint, an incompatible type argument,
> a method the bound doesn't permit) surfaces as a C# `CS####` error, not a
> Spek `CE` with a source caret. Drop the `where T : System.IComparable<T>`
> from `Larger` above and the call to `a.CompareTo(b)` fails to compile with a
> C# error mapped back onto the `.spek` line (try `spekc compile file.spek
> --check`). Spek deliberately leans on Roslyn here rather than re-implementing
> generic type inference.

This is the same trade you accept across all of Spek's C# passthrough: you get
the full power and familiarity of .NET generics, and the diagnostics come from
the C# compiler instead of from a Spek-specific rule.

## Limits

A few corners are intentionally out of scope:

- **No variance markers.** `in` / `out` on a type parameter (declaration-site
  variance) isn't part of the grammar.
- **`message` constraints aren't supported.** Type parameters on a `message`
  work, but a `where` clause on a message is a parse error. Constraints on immutable data records are rare enough that the grammar does not include them.
- **Enums, channels, and shared regions aren't generic.** An
  [enum](enums.md) is a closed set of concrete variants, a
  channel is a concrete message contract, and a shared region holds concrete
  shared state, so a type parameter on any of them is a grammar error.
- **A handler cannot be keyed on a generic message.** `on Envelope` where
  `Envelope<T>` is generic is [CE0139](../reference/errors.md#ce0139): the
  pattern has no way to name the type argument. Dispatch on a concrete
  wrapper message instead.

## What's next

Generics let you abstract over *types*. The next chapter,
[Lambdas](lambdas.md), lets you abstract over *behavior*, passing a
piece of code as a value, which pairs naturally with generic helpers like a
`Map<T, U>` that takes a function from `T` to `U`.

## Related reading

- [Classes](classes.md): the type kind generics most often parameterize.
- [Modules](modules.md): where generic helper methods live.
- [Messages](messages.md): generic payloads and the immutability rule.
- [C# syntax in bodies](csharp-syntax.md): the broader "Roslyn type-checks it" passthrough story.
