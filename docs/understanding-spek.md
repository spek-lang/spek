---
title: Understanding Spek
layout: default
nav_order: 2
permalink: /understanding-spek/
description: "Who Spek is for, how it differs from the C# it resembles, and where to start depending on your background."
---

# Understanding Spek

If you're not sure where to start, start with this page. On the surface Spek
looks a lot like C# or Java, and the syntax is rarely what trips people up. The
habit that needs adjusting is how you think about a program: who owns state, how
work moves between actors, and what the compiler refuses to build. Getting that
settled first will save you a ton of headaches in the rest of the
documentation.

## Who Spek is for

Spek is aimed first at C# developers. It looks and feels like C#: the
same expressions and statements, the same type system, `private`-by-default
visibility, file-scoped namespaces, and the whole of NuGet. If you write C#, you
already know most of Spek's surface. The differences are the actor rules.

Java developers land almost as softly. The object model and the static typing
carry straight over, and anyone who has used Akka on the JVM has already met the
actor model Spek is built around.

If you come from an actor language already, whether that's Erlang or Elixir,
Akka, Orleans, Proto.Actor, Pony, or Gleam, then you already know the hard
part. Spek's contribution is taking the rules you normally hold in your head
and turning them into compile errors, on top of .NET.

Everyone else can still read on. The model isn't hard, and the
[Language guide](language/index.md) fills in the .NET specifics as they come up.

## Familiar on the surface, different underneath

Spek borrows heavily from C# so you don't have to relearn how to write a
loop or declare a type. It is not "C# with actors bolted on." A few
places have different semantics, and those are why the language exists.
When a C# habit kicks in, stop and check:

| Coming from C#, you're used to…                              | In Spek…                                                                                          |
|--------------------------------------------------------------|---------------------------------------------------------------------------------------------------|
| sharing objects and guarding them with `lock` / `ConcurrentDictionary` | you **can't share mutable state at all**: each actor owns its state, and the compiler enforces that |
| calling methods on other objects                             | you send immutable **messages** (`Tell` / `Ask`); there are no cross-actor method calls           |
| writing `async` / `await`                                    | you write neither. Async is invisible, and independent work runs concurrently by default          |
| wrapping everything fragile in `try` / `catch`               | you **let it crash**, and a supervisor decides whether to restart, stop, resume, or escalate       |
| using any type as a field or parameter                       | **message** fields must be immutable (a compiler-checked whitelist); **actor** fields are private and mutable |

Everything outside that table behaves as you'd expect. `if`, `for`,
`while`, `var`, LINQ, generics, the base class library: all just C#. The
five rows are what's new. The rest of the docs walk through them.

Spek transpiles to C#. It's a real language with its own compiler, and
it emits ordinary, readable C# that builds with the normal `dotnet`
tooling. There's no virtual machine. Open the generated file if you want
to see how something works.

## Finding your way in

Where you start depends on where you're coming from.

If you write C# or Java, you already know roughly two-thirds of Spek: the
body-level syntax and the type system. The third that's new is the actor model
and the isolation rule, and the fastest way into it is to build something. Start
with [Build your first actor](language/first-actor.md), then read the
[Language guide](language/index.md) front to back. The one chapter not to skip is
[Isolation and ownership](language/isolation.md), because everything else in the
language falls out of it.

If you already know an actor framework, start instead with the migration guide
for your background: [Akka.NET](migration/from-akka-net.md),
[Orleans](migration/from-orleans.md), [Erlang/OTP](migration/from-erlang-otp.md),
[Proto.Actor](migration/from-proto-actor.md), [Go](migration/from-go.md), or
[F#](migration/from-fsharp.md). Each one maps concepts you already have onto Spek's
surface, so you're reading translations instead of starting cold.

One caution about those guides. They are bridges, not the real thing. An analogy
gets you oriented quickly, but Spek's compile-time guarantees have no exact
counterpart in those frameworks: the immutability whitelist, invisible async, and
share-XOR-mutate are new, not renamed versions of something you've seen.
Once the analogy has done its job, come back and read the
[Language guide](language/index.md) properly, because the details that bite are
covered there and not in the bridges.

## Where the ideas came from

Spek didn't invent the actor model. Supervision and "let it crash" are
Erlang's. `Tell`, `Ask`, `become`, and snapshot `persist` are Akka's.
`passivate` is Orleans grain deactivation. Share-XOR-mutate is Rust's
ownership rule, checked at the actor boundary rather than on every local.
The C# surface is deliberate so those rules sit in a language you already
read.

The full map (including Go/CSP, Swift conversions, ReactiveX streams,
Proto.Actor, Gleam, F#, and Axum, plus the ideas Spek did *not* take) is
[Where Spek comes from](heritage.md). Language-guide chapters that rest on
a non-C# idea also open with a short "Where this comes from" note.

## Where to go next

[Getting started](getting-started.md) installs the toolchain and runs a first
program. [Build your first actor](language/first-actor.md) is the hands-on version
of the same thing. The [Language guide](language/index.md) is the whole language, in
order, and it's meant to be read that way.
