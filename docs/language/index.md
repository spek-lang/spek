---
title: Language
layout: default
nav_order: 4
has_children: true
permalink: /language/
description: "The Spek language guide, as a book: read it front to back and each chapter builds on the last: from your first actor through isolation, supervision, persistence, and the advanced concurrency features."
---

# The Spek language

This section is a **book**. Read it front to back and each chapter builds on the
one before it. If you've written C#, the syntax will feel familiar: Spek is C#
with shared mutable state removed and an actor model put in its place. The
ancestry of the non-C# pieces (Erlang supervision, Akka names, Rust isolation,
and the rest) is collected in [Where Spek comes from](../heritage.md).

A `.spek` file is an optional file-scoped namespace, some `using` imports, and a
sequence of top-level declarations (`message`, `actor`, `module`, and an
optional `program` entry point):

<!-- spek-test: compile -->
```spek
namespace MyBank;

message Deposit(decimal amount);

actor BankAccount
{
    decimal balance = 0m;

    init() { become Open; }

    behavior Open
    {
        on Deposit d => { balance += d.amount; }
    }
}
```

All *stateful* computation runs inside actors. There are no globals and no
shared mutable state. The only way two actors affect each other is by sending a
`message`. Stateless helper code lives in `module`s.

| Construct   | What it is                                                       |
|-------------|------------------------------------------------------------------|
| `namespace` | File-scoped namespace; identical to C#.                         |
| `using`     | Namespace import; identical to C#.                              |
| `message`   | Immutable message type. Compiles to a C# `record`.              |
| `actor`     | Actor type. Compiles to a sealed class with a mailbox.          |
| `module`    | Stateless methods. Compiles to a `static class`.             |
| `program`   | Entry point. Compiles to `static async Task Main`.             |

## The book

**Part I: Foundations.** The actor model, and the one rule the rest of the language follows from.

1. [Build your first actor](first-actor.md): learn by doing, a counter that grows into a bank account, one concept per step.
2. [Actors and behaviors](actors.md): the unit of computation, with fields, `init`, behaviors, and `become`.
3. [Messages](messages.md): immutable message types, and why the immutability is compiler-enforced.
4. [Sending messages](messaging.md): `Tell`, `ask`, `sender`, and the return-to-reply idiom.
5. [Isolation and ownership](isolation.md): *share-XOR-mutate*, the idea everything else falls out of. Read this one carefully.

**Part II: Resilience and state.** Keeping actors alive, and their state durable.

6. [Supervision and failure](supervision.md): what happens when a handler throws, and how parents decide.
7. [Persistence and passivation](persistence.md): durable state with automatic restore; idle actors that unload and reload.
8. [Async without await](async.md): invisible async, concurrency by default, no `async`/`await` in your code.

**Part III: Language features.** The C#-interop surface you write inside bodies.

9. [Classes](classes.md) · 10. [Enums](enums.md) · 11. [Conversions](conversions.md) · 12. [Modules](modules.md) · 13. [Generics](generics.md) · 14. [Lambdas](lambdas.md) · 15. [C# in bodies](csharp-syntax.md)

**Part IV: Advanced concurrency.** Beyond one-message-at-a-time.

16. [Shared regions](shared-regions.md) · 17. [Channels](channels.md) · 18. [Streams](streams.md)

**Part V: Practice.** Testing, pitfalls, and going distributed.

19. [Testing actors](testing.md) · 20. [Common pitfalls](footguns.md) · 21. [Transport types](transport-types.md) · 22. [Clustering](clustering.md)

## Three ideas that run through everything

The first is the ownership rule: a value is either *shared and immutable* or
*owned and mutable*, never both. Actor fields are private by definition, so
mutable state is always owned by exactly one actor. A `message` is the only thing
that crosses an actor boundary, and every field on one must be immutable
(primitives, `string`, `ActorRef`, other messages, and `System.Collections.Immutable.*`
containers; `List<T>` and even `IReadOnlyList<T>` are rejected). This is
[Part I, chapter 5](isolation.md), and it's worth the read.

The second: every rule that *can* be checked
at compile time is. `become` targets, `ask` placement, message immutability, and
dozens more. Each produces a Rust-style caret diagnostic. The full catalog is the
[errors reference](../reference/errors.md).

And third, concurrency is the default and its syntax is invisible. You
never write `async` or `await`. Code
that looks synchronous runs concurrently where it safely can, and the compiler
guarantees every task finishes inside its actor's turn. You opt *out* for a
checkpoint, not in. That's [chapter 8](async.md).

## Where to start

New to Spek? Start with **[Build your first actor](first-actor.md)**. You'll
write a working program and meet actors, messages, `Tell`, `ask`, and `become`.
Then read straight through. Whatever you skip, don't skip
[Isolation and ownership](isolation.md): it's the single idea the rest of
the language is built on.
