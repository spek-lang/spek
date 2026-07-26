---
title: Understanding Spek
layout: default
nav_order: 2
permalink: /understanding-spek/
description: "Read this first. Who Spek is for, how it borrows from C# on the surface but differs underneath, where it diverges, and the fastest path to learning it depending on your background."
---

# Understanding Spek

Read this page before anything else. It won't teach you syntax; it tells you how
to *think* about Spek, so the rest of the documentation lands the first time
instead of the second. Ten minutes here saves an hour later.

## Who Spek is for

Spek is aimed first at C# developers. It looks and feels like C# on purpose: the
same expressions and statements, the same type system, `private`-by-default
visibility, file-scoped namespaces, and the whole of NuGet. If you write C#, you
already know most of Spek's surface, and the parts that aren't familiar are
unfamiliar deliberately.

Java developers land almost as softly. The object model and the static typing
carry straight over, and anyone who has used Akka on the JVM has already met the
actor model Spek is built around.

If you come from an actor language already, whether that's Erlang or Elixir,
Akka, Orleans, Proto.Actor, Pony, or Gleam, then you know the hard part. What's
new here isn't the model. It's that Spek takes the rules you normally hold in
your head and turns them into compile errors, on top of .NET.

Everyone else can still read on. The model isn't hard, and the
[Language guide](/language/) fills in the .NET specifics as they come up.

## Familiar on the surface, different underneath

This is the one idea to hold onto. Spek borrows heavily from C# so you don't have
to relearn how to write a loop or declare a type. But it is not "C# with actors
bolted on." In a few places the semantics are deliberately different, and those
places are the entire reason the language exists. When a C# habit kicks in, this
is where to stop and check:

| Coming from C#, you're used to…                              | In Spek…                                                                                          |
|--------------------------------------------------------------|---------------------------------------------------------------------------------------------------|
| sharing objects and guarding them with `lock` / `ConcurrentDictionary` | you **can't share mutable state at all**: each actor owns its state, and the compiler enforces that |
| calling methods on other objects                             | you send immutable **messages** (`Tell` / `Ask`); there are no cross-actor method calls           |
| writing `async` / `await`                                    | you write neither. Async is invisible, and independent work runs concurrently by default          |
| wrapping everything fragile in `try` / `catch`               | you **let it crash**, and a supervisor decides whether to restart, stop, resume, or escalate       |
| using any type as a field or parameter                       | **message** fields must be immutable (a compiler-checked whitelist); **actor** fields are private and mutable |

Everything outside that table behaves exactly as you'd expect. `if`, `for`,
`while`, `var`, LINQ, generics, the base class library: all just C#. The five
rows are the whole of what's genuinely new, and most of the documentation is an
unhurried walk through them.

One structural fact is worth knowing up front. Spek transpiles to C#. It's a real
language with its own compiler, but what comes out the far side is ordinary,
readable C# that builds with the normal `dotnet` tooling. There's no virtual
machine and nothing hidden, so when you want to know how something works, you can
open the generated file and read it.

## Finding your way in

Where you start depends on where you're coming from.

If you write C# or Java, you already know roughly two-thirds of Spek: the
body-level syntax and the type system. The third that's new is the actor model
and the isolation rule, and the fastest way into it is to build something. Start
with [Build your first actor](/language/first-actor/), then read the
[Language guide](/language/) front to back. The one chapter not to skip is
[Isolation and ownership](/language/isolation/), because everything else in the
language falls out of it.

If you already know an actor framework, start instead with the migration guide
for your background: [Akka.NET](/migration/from-akka-net/),
[Orleans](/migration/from-orleans/), [Erlang/OTP](/migration/from-erlang-otp/),
[Proto.Actor](/migration/from-proto-actor/), [Go](/migration/from-go/), or
[F#](/migration/from-fsharp/). Each one maps concepts you already have onto Spek's
surface, so you're reading translations instead of starting cold.

One caution about those guides. They are bridges, not the real thing. An analogy
gets you oriented quickly, but Spek's compile-time guarantees have no exact
counterpart in those frameworks: the immutability whitelist, invisible async, and
share-XOR-mutate are genuinely new, not renamed versions of something you've seen.
Once the analogy has done its job, come back and read the
[Language guide](/language/) properly. That's where the details that actually bite
live.

## Where the ideas came from

Spek didn't invent the actor model; it curates it. "Let it crash" and supervision
trees are Erlang's, along with the conviction that fault tolerance belongs in the
language rather than in a library you have to remember to reach for. The typed
actor API and the snapshot-based persistence echo Akka and Akka.NET. Orleans
contributed the virtual-actor feel, and its grain deactivation is what Spek calls
`passivate`. The share-XOR-mutate rule at the center of Spek's isolation is Rust's
ownership intuition, moved from lexical scope to the actor boundary. And Gleam is
the standing proof that a friendly, strongly-typed actor language can be a
pleasant place to work.

Because so much is borrowed, each chapter in the [Language guide](/language/) that
leans on a non-C# idea opens with a short note on where it came from, so that when
something looks unfamiliar you already know which prior art to map it onto.

## Where to go next

[Getting started](/getting-started/) installs the toolchain and runs a first
program. [Build your first actor](/language/first-actor/) is the hands-on version
of the same thing. The [Language guide](/language/) is the whole language, in
order, and it's meant to be read that way.
