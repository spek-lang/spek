---
title: Classes
layout: default
parent: Language
nav_order: 9
permalink: /language/classes/
description: "Confined mutable classes: actor-local helper objects with fields, an init constructor, and methods, kept race-free without a single lock."
---

# Classes

So far every piece of mutable state you've written has lived directly on an
actor: its fields, mutated one message at a time by the serialized mailbox. That
keeps things safe (see [isolation and ownership](isolation.md)), but it
pushes you toward one flat bag of fields per actor. Sometimes the natural unit is
a small **object**, a buffer that accumulates lines, a running tally, a parser
holding a cursor, that you'd like to give a name, a constructor, and a few
methods.

That's what a `class` is for. A Spek `class` is a **mutable, single-owner helper
object**: it holds fields, an optional `init` constructor, and methods, and it
lowers to a plain C# instance class. The catch is the thing you already know from
[isolation and ownership](isolation.md): share-XOR-mutate. A class is
mutable, so to stay lock-free it must be **confined**: reachable from one actor
at a time. You never write that confinement down; the compiler infers it and
enforces it.

A `class` is the fourth and last of Spek's code/data kinds. Each kind stays
concurrency-safe *by construction*:

| Kind | Holds state? | Stays race-free by |
|------|--------------|--------------------|
| `module` | none (stateless) | nothing to race on; see [modules](modules.md) |
| `message` | immutable | immutability ([CE0010](../reference/errors.md#ce0010)); see [messages](messages.md) |
| **`class`** | **mutable, single-owner** | **confinement (this page)** |
| `actor` | mutable, concurrent | serialized mailbox; see [actors](actors.md) |

There's no capability marker: a `class` is mutable by definition, and its
ownership is *inferred*, not annotated, the same philosophy as
[invisible async](async.md).

## Declaring a class

A class groups fields and the methods that operate on them:

<!-- spek-test: compile -->
```spek
class ReportBuilder
{
    string buffer = "";

    public void Append(string line)
    {
        buffer = buffer + line + "\n";
    }

    public string Finish() { return buffer; }
}
```

Fields and methods both default to `private`, exactly as in C#. A method
that implements an interface member must be declared `public` explicitly. Roslyn enforces that on implicit implementations. Inside a method, a
field is referred to directly (`buffer = …`) or through `self`
(`self.buffer = …`), which lowers to C#'s `this`. Method bodies are ordinary
[handler-style bodies](csharp-syntax.md), so everything you can write in
a handler works here too, including [invisible async](async.md): a
Task-returning call inside a method is auto-awaited and the method goes async for
you.

### Construction with `init`

A class takes constructor parameters through the same `init(params)` block
[actors use](actors.md):

<!-- spek-test: compile -->
```spek
class Tally
{
    int total;

    init(int start) { total = start; }

    public void Add(int n) { total = total + n; }
    public int  Get()      { return total; }
}
```

Create instances with `new`, exactly as in C#:

<!-- spek-test: compile -->
```spek
class Tally
{
    int total;

    init(int start) { total = start; }

    public void Add(int n) { total = total + n; }
    public int  Get()      { return total; }
}

program Main
{
    var t = new Tally(10);
    t.Add(5);
    System.Console.WriteLine(t.Get());   // 15
}
```

If you omit `init`, the class gets a parameterless constructor and you write
`new ReportBuilder()`.

### Properties

Besides plain fields, a class can declare properties with `get`/`set`/`init`
accessors, exactly as in C#, including accessor visibility, an
expression-bodied getter, and an auto-property initializer. The accessor
keywords are contextual, so they never clash with members named `Get` or `Set`.

<!-- spek-test: compile -->
```spek
class Account
{
    int balance = 0;

    public int    Balance     { get; private set; }   // public read, private write
    public string Owner       { get; init; }           // set once at construction
    public bool   IsOverdrawn { get => balance < 0; }  // computed
    public int    Version     { get; set; } = 1;       // auto + initializer
}
```

## Using a class as an actor-local helper

The most common home for a class is a **field on an actor**. The actor owns the
instance; its handlers drive it across many messages, building up state the
class encapsulates. Here an `Aggregator` keeps a small statistics object and
answers an [Ask](messaging.md) with the running mean:

<!-- spek-test: compile -->
```spek
class RunningStats
{
    int count = 0;
    double sum = 0;

    public void Record(double sample)
    {
        count = count + 1;
        sum = sum + sample;
    }

    public double Mean()
    {
        return count == 0 ? 0 : sum / count;
    }
}

message Sample(double value);
message AskMean();
message MeanReply(double mean);

actor Aggregator
{
    RunningStats stats = new RunningStats();

    on Sample s => { stats.Record(s.value); }
    on AskMean  => { return new MeanReply(stats.Mean()); }
}
```

`stats` is mutable, but it never leaves `Aggregator`: each handler runs to
completion before the next message starts, so the mutations are perfectly
serialized. That's confinement doing its job: no lock, no `volatile`, no copy.

A class can equally well be a **handler local**, built, used, and discarded
within a single message:

<!-- spek-test: compile -->
```spek
class ReportBuilder
{
    string buffer = "";
    public void   Append(string line) { buffer = buffer + line + "\n"; }
    public string Finish()            { return buffer; }
}

message Render(string title);
message Rendered(string text);

actor Page
{
    on Render r =>
    {
        var b = new ReportBuilder();
        b.Append(r.title);
        b.Append("---");
        return new Rendered(b.Finish());
    }
}
```

## Confinement: how a class stays race-free

A class is mutable, so Spek keeps it safe by guaranteeing it's only ever
reachable from **one actor at a time**. Two rules enforce that, and you never
write an annotation for either.

**1. It can't escape to another actor.** A class is not immutable, so it can't
ride in a `message` field or an Ask reply, it can't be a
[shared-region](shared-regions.md) field, and it can't be handed to a
child at `spawn` while you keep a reference. The first is just the
immutability whitelist from [messages](messages.md) at work: putting a
class in a message field is [CE0010](../reference/errors.md#ce0010):

<!-- spek-test: ignore -->
```spek
class Counter { int n = 0; public void Inc() { n = n + 1; } }

message Submit(Counter c);   // error[CE0010]: 'Counter' is not a known immutable type
```

The spawn route falls to the same reasoning. Spawning a child with one of your
own class-typed fields would leave both actors holding the same mutable
object, so it is [CE0137](../reference/errors.md#ce0137):

<!-- spek-test: ignore -->
```spek
actor Owner
{
    Counter tally = new Counter();

    on Audit a =>
    {
        var kid = spawn<Recount>(tally);   // error[CE0137]: owner and child would share 'tally'
    }
}
```

A child may still *receive* a class at construction. What matters is that the
sender keeps nothing, so ownership transfers whole. This is the constructor
gift:

<!-- spek-test: ignore -->
```spek
on Audit a =>
{
    var kid = spawn<Recount>(new Counter());   // the gift: the child is the only owner
}
```

The gift may pass through a local (`var c = new Counter();` and then
`spawn<Recount>(c);`) so you can build the object up before handing it over,
as long as that local is never also stored in one of the sender's fields.
To share data with another actor rather than hand off an object, package it
as an immutable [`message`](messages.md), not a class.

**2. It's only mutated where mutation is safe.** This is the share-XOR-mutate
rule from [isolation and ownership](isolation.md), now reaching *inside*
a helper object. Regular and writer handlers are serial, so they may mutate a
confined class freely. Reader handlers run *concurrently*, so calling a method
that mutates the object (or writing one of its fields) from a reader is
[CE0087](../reference/errors.md#ce0087). Reads and pure-method calls from a reader
are fine.

<!-- spek-test: compile -->
```spek
class ReportBuilder
{
    string buffer = "";
    public void   Append(string line) { buffer = buffer + line + "\n"; }
    public string Finish()            { return buffer; }
}

message Log(string line);
message Dump();

actor Reporter
{
    ReportBuilder report = new ReportBuilder();

    writer on Log l => { report.Append(l.line); }   // mutates — writer is serial, OK
    reader on Dump  => { return report.Finish(); }   // pure read — reader, OK
}
```

Move that `Append` call into a `reader` arm and the compiler stops you:

<!-- spek-test: ignore -->
```spek
class Counter
{
    int n = 0;
    public void Inc()   { n = n + 1; }
    public int  Value() { return n; }
}

message Bump();

actor Worker
{
    Counter c = new Counter();
    reader on Bump => { c.Inc(); }   // error[CE0087]: reader may not call mutating method 'Inc'
}
```

### Mutation is inferred, not marked

Whether a method "mutates" is *inferred*, not declared: a method mutates if it
writes a field (or a property), directly or by calling a sibling method that
does. The classifier reaches transitively, so a method that only *looks* pure
but calls a mutating helper is still treated as mutating.

<!-- spek-test: ignore -->
```spek
class Acc
{
    int total = 0;
    public void Add(int x)      { total = total + x; }
    public void AddTwice(int x) { self.Add(x); self.Add(x); }   // mutates — calls Add
    public int  Get()           { return total; }
}

message Go();

actor Worker
{
    Acc a = new Acc();
    reader on Go => { a.AddTwice(2); }   // error[CE0087]: AddTwice mutates transitively
}
```

You never annotate `Add` or `AddTwice` as mutating. The compiler works it out
and only complains at the point where a concurrent reader would actually race.

## Interfaces: the class contract

A class can implement an `interface`: the class-side implementation contract. It
is the method-based sibling of a [`channel`](channels.md), the actor's
message-based contract, and both lower to a C# `interface`. Where a channel names
the messages an actor accepts, an interface names the methods and properties a
class provides:

<!-- spek-test: compile -->
```spek
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

An interface declares **shape, never behavior**. It holds method and property
*signatures* and nothing else: no method bodies, no property-accessor bodies, no
fields. This is the one deliberate divergence from modern C#, which since C# 8
allows default method bodies on an interface. Spek holds the interface to its
pre-C#-8 meaning, so behavior always lives in the concrete class, never hidden
inside the thing it implements. A body or a field inside an `interface` is
[CE0120](../reference/errors.md#ce0120).

The payoff is polymorphism with confinement intact. An actor can hold an
interface-typed field and call it through a single call site, swapping one
implementation for another without touching the caller, and the instance is
still owned by the one actor, with the [confinement](#confinement-how-a-class-stays-race-free) guarantee intact:

```spek
actor Gatekeeper
{
    Validator validator = new EmailValidator();

    on Recheck r => { validator = new NonEmptyValidator(); }
    on Check c    => return new Verdict(validator.IsValid(c.input));
}
```

Interfaces may extend other interfaces (`interface Describable : Named`), exactly
as in C#, and a class may implement several at once
(`class Widget : Named, Sized`). Roslyn does the conformance check: a class that
omits a member of an interface it names fails the build with C#'s own `CS0535`,
not a Spek diagnostic: the same passthrough style [generics](generics.md)
use.

One rule sets Spek apart from C#: **handlers dispatch on messages, never on
interfaces.** An `on` handler is keyed on a `message` type, and `on SomeInterface`
is [CE0121](../reference/errors.md#ce0121). An interface is the *provider* side of a
contract; a message is the *received* side, and keeping the two separate is what
keeps message flow single and local. To handle a family of messages, give them a
shared `abstract message` base and dispatch on that instead.

## Inheritance: abstract base classes

A class can extend an `abstract class` to share fields and methods. Spek's model
here is reuse plus abstract methods, and nothing more: deliberately narrower
than C#. There is no `virtual`, no `override`, and no extending a concrete class.
Runtime-swappable polymorphism is the [interface's](#interfaces-the-class-contract)
job; class inheritance is for the case where several concrete types genuinely
share implementation and a common shape.

An `abstract class` may hold ordinary fields and methods (inherited as-is) and
`abstract` methods: signatures with no body that a subclass must implement:

<!-- spek-test: compile -->
```spek
abstract class Shape
{
    string name;
    init(string n) { name = n; }

    public abstract double Area();          // each subclass supplies this
    public string Describe() { return name; }   // shared, inherited verbatim
}

class Circle : Shape
{
    double r;
    init(double radius) : base("circle") { r = radius; }
    public double Area() { return 3.14159 * r * r; }
}
```

Two things are worth calling out. The subclass's constructor chains to the base
with `init(...) : base(...)`, the C# idiom. And `Circle.Area` needs no `override`
keyword: Spek sees that it implements the base's abstract `Area` and emits the
`override` for you. That is the whole point of leaving `virtual`/`override` out. The only methods a subclass can specialize are the abstract ones, so marking them
is redundant.

Only an `abstract class` can be a base ([CE0123](../reference/errors.md#ce0123)); a
concrete class stays `sealed`, so it is always a leaf. An abstract method is only
allowed inside an abstract class, and can't be `private`
([CE0122](../reference/errors.md#ce0122)). As with interfaces, Roslyn does the
conformance check: a subclass that forgets to implement an abstract method fails
the build with C#'s own `CS0534`.

The base method calling its own abstract hook is the one place polymorphism
happens: `Describe` could call `Area()` and reach the concrete subclass's
implementation. If you want to swap the *whole* implementation at run time, reach
for an [interface](#interfaces-the-class-contract) instead.

## Limits

- **No cross-actor transfer.** A class stays confined to its owning actor. To
  move data between actors, send an immutable [`message`](messages.md).
  The one hand-off is the constructor gift at `spawn`: a fresh instance the
  sender never holds ([CE0137](../reference/errors.md#ce0137) rejects a spawn
  argument the sender still references).
- **Abstract bases only.** A class may extend an `abstract class` and implement
  interfaces, but it cannot extend a concrete class, and there is no
  `virtual`/`override`: swappable polymorphism lives in an
  [interface](#interfaces-the-class-contract).
- **No `immutable class`.** Use a [`message`](messages.md) for immutable
  data.

## Related reading

- [Isolation and ownership](isolation.md): share-XOR-mutate and the
  invisible-ownership model confinement extends.
- [CE0087](../reference/errors.md#ce0087): reader handlers may not mutate confined
  state (actor fields, regions, or a class via a mutating method).
- [CE0010](../reference/errors.md#ce0010): the immutability whitelist that keeps a
  class out of message fields.
- [CE0137](../reference/errors.md#ce0137): spawn arguments may not share a
  confined class with the child; the constructor gift is the legal hand-off.
- [Modules](modules.md): the stateless counterpart for pure helper
  functions.

Next up are [enums](enums.md): fixed sets of named values and how
handlers match on them.
