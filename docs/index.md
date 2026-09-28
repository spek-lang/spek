---
title: Home
layout: default
nav_order: 1
description: "Spek is an actor-based, C#-inspired language for the .NET ecosystem, with compile-time concurrency safety."
permalink: /
---

# Spek

Spek is a programming language: actor-based, C#-inspired, targeting the .NET
ecosystem. It gets rid of shared mutable state, locks, and the whole class of
concurrency bugs that come with them, replacing all of it with isolated actors
that talk to each other only through immutable messages.

It is **not** a library or framework. Spek is a compiled language with its own
syntax, keywords, and compiler toolchain. It transpiles `.spek` files to C# and
builds them with the normal `dotnet` pipeline.

{: .warning }
> **Not for production.** Spek is pre-release and **not supported for production
> use in any way, shape, or form.** It's open source in the fullest sense: free
> to read, run, fork, and learn from, but with no warranty, no support, and no
> stability guarantees. Syntax, APIs, and on-disk/wire formats can change without
> notice. If you run it, you're on your own. (Apache-2.0 licensed.)

```spek
actor Echo
{
    init() { become Waiting; }

    behavior Waiting
    {
        on Ping =>
            sender.Tell(new Pong());
    }
}
```

{: .note }
> **New to Spek? Start with [Understanding Spek](understanding-spek.md).** It's a
> few minutes on who the language is for, how it borrows from C#, and the handful
> of places it deliberately differs. [Where Spek comes from](heritage.md) is the
> ancestry: Erlang, Akka, Orleans, Rust, and what Spek left behind.

## Who Spek is for

Spek is for C# developers who want the actor model without the ceremony of
Akka.NET or Orleans, and with stronger compile-time safety than either gives you.

If you've reached for `lock`, `ConcurrentDictionary`, or `Interlocked` one too
many times and wished the language itself would stop you from sharing mutable
state, Spek does exactly that. If you've used Akka.NET or Proto.Actor, liked the
model, and then got tired of inheriting from `ReceiveActor`, wiring up `Become`
by hand, and finding out about mistakes only at runtime, Spek moves those checks
to compile time. And if you want the "let it crash" discipline of Erlang/OTP but
in a language that reads like modern C# and pulls NuGet packages natively,
that's Spek.

## Core philosophy

Everything in Spek is an actor: an isolated unit of computation and state.
Actors never share mutable state, and the compiler enforces that rather than
leaving it to convention. They talk to one another only through immutable
messages, where the `message` keyword compiles to a C# `record` and only a type
declared with `message` can be sent. An actor switches between named sets of
handlers with `become`, so its behavior changes as its state does. When
something goes wrong, hierarchical supervision applies the "let it crash" model
instead of defensive error handling.

Most of this sits on a surface that reads like C#, with the same visibility
defaults, namespace conventions, and type syntax. What isn't C# is the
enforcement: the actor-model rules surface as compile-time diagnostics rather
than runtime surprises. Every one of them carries a `CE####` code and a
triggering example in the [CE code catalog](reference/errors.md).

## What Spek includes

The language includes messages, actors, behaviors with
`become`, `init` and lifecycle hooks, `persist` and `passivate`, `Ask` and
`Tell`, `spawn`, and hierarchical supervision (`Resume`, `Restart`, `Escalate`,
`Stop`, with retry budgets and parent supervision). Alongside the language sit
the runtime packages and tooling. `Spek.Runtime` is the actor system itself,
with supervision, persistence, and lock-checked shared regions. `Spek.Testing` gives
you `TestActorSystem` and `TestProbe`, and its xUnit adapter runs native Spek
`test` blocks under `dotnet test`. A language server delivers live diagnostics
and quick-fixes to any LSP-aware editor.

## Where to go next

- [Understanding Spek](understanding-spek.md): read this first. Who Spek is for,
  how it relates to C#, and where it deliberately differs.
- [Where Spek comes from](heritage.md): Erlang, Akka, Orleans, Rust, and the
  rest: which idea Spek borrowed and where it shows up.
- [Getting started](getting-started.md): install the toolchain and walk through
  `HelloBank`, a runnable example.
- [Build your first actor](language/first-actor.md): learn the language by
  building a small program from scratch, one concept at a time.
- [Isolation & ownership](language/isolation.md): the one idea the whole language
  follows from. A value is either shared-and-immutable or owned-and-mutable,
  never both, checked by the compiler.
- [Language overview](language/index.md): the full language surface, one page at a time.
- [Runtime reference](reference/runtime.md): `ActorSystem`, `ActorRef`,
  supervision, and persistence primitives.
- [Error codes](reference/errors.md): every compile-time diagnostic with a
  triggering example.
- [Samples](samples.md): the fixture files plus `HelloBank`.
- [Demos](demos.md): three runnable systems, two of them paired with C#
  twins, plus the benchmark suite that measures them.
- [Migration guides](migration/index.md): concept-by-concept maps if you're coming from
  Akka.NET, Erlang/OTP, Proto.Actor, or Orleans.

The [formal grammar](spek-v1-grammar.md) is the source of truth for syntax; this
site is derived from it.
