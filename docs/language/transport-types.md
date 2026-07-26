---
title: Transport types
layout: default
parent: Language
nav_order: 21
permalink: /language/transport-types/
description: "Mutable transport types for the request hot path: the confined class IS the transport type. One mutable context flows through a synchronous pipeline with zero per-step allocation, and stays race-free because it never crosses an actor boundary."
---

# Transport types

Immutable [`message`s](/language/messages/) are the right shape for data that
*crosses an actor boundary*: immutability is what makes them race-free to share.
But a single request that flows through a multi-step pipeline (read headers,
authenticate, route, write the response) shouldn't allocate a fresh object at
every step. On a 10K-req/s server with a five-stage pipeline that's 50K+
allocations a second of pure churn. That's the **hot path**, and forcing
immutability there is a perf cliff Spek shouldn't impose.

> **There is no separate `transport` kind.** The mutable
> [`class`](/language/classes/) you already have *is* the transport type. This
> page is about the one pattern that makes it shine: a single mutable context
> threaded through a synchronous pipeline.

## The pattern: one mutable context, many steps

Declare the request context as a `class`, write each pipeline stage as a
[module](/language/modules/) method that takes it and mutates it in place, and
let one actor drive the chain. The context is allocated **once** per request:

<!-- spek-test: compile -->
```spek
public class RequestContext
{
    public string  Path       = "";
    public int     StatusCode = 0;
    public string  Body       = "";
    public bool    Handled    = false;

    public void Reject(int code) { StatusCode = code; Handled = true; }
    public void Ok(string body)  { StatusCode = 200; Body = body; Handled = true; }
}

module Pipeline
{
    public void Authenticate(RequestContext ctx)
    {
        if (ctx.Path.StartsWith("/admin")) { ctx.Reject(401); }
    }

    public void Route(RequestContext ctx)
    {
        if (ctx.Handled) { return; }
        ctx.Ok("hello from " + ctx.Path);
    }
}

message Handle(string path);

actor RequestHandler
{
    on Handle h =>
    {
        var ctx = new RequestContext();        // one allocation per request
        ctx.Path = h.path;

        Pipeline.Authenticate(ctx);            // each stage mutates in place
        Pipeline.Route(ctx);

        Console.WriteLine(ctx.StatusCode + " " + ctx.Body);
    }
}
```

Every stage reads and writes the *same* object. No intermediate copies, no
per-stage garbage: the allocation profile of hand-written C# middleware, with
none of the manual lifecycle code.

## Why it's still race-free without an annotation

The mutable context is dangerous *only* if two actors can touch it at once. They
can't, and the compiler already guarantees it:

- **It can't ride a message to another actor.** A `class` isn't immutable, so
  it's rejected as a `message` field or ask-reply
  ([CE0010](/reference/errors/#ce0010)).
- **It can't be shared state.** It can't be a [shared-region](/language/shared-regions/)
  field ([CE0112](/reference/errors/#ce0112)).

So the context lives and dies inside one actor's handler (or as that actor's
field), mutated by ordinary synchronous calls. The actor boundary plus immutable
messages still do all the concurrency work; the mutable object **never
escapes the one actor handling the request**. You write no ownership marker;
the confinement is inferred, the same way [invisible async](/language/async/) is.

## Crossing back to immutable at the boundary

When the *result* of the pipeline has to leave the actor, whether it goes to
another actor, gets persisted, or comes back as an ask-reply, copy the fields
you need into an immutable `message` at that boundary. The mutable transport
stays local; the immutable snapshot travels:

<!-- spek-test: compile -->
```spek
public class RequestContext
{
    public int    StatusCode = 0;
    public string Body       = "";
}

// Immutable — safe to send anywhere.
message Response(int status, string body);

module Egress
{
    public Response Finalize(RequestContext ctx)
    {
        return new Response(ctx.StatusCode, ctx.Body);
    }
}
```

This is the whole discipline: **mutate freely inside one actor, copy once at the
edge.** The copy you were trying to avoid on every pipeline step happens exactly
once, where it buys you cross-actor safety.

## What's deliberately *not* here

- **No `transport` keyword.** The mutable class already carries the capability;
  a marker would lower to identical C# and add no guarantee, and Spek doesn't add
  grammar that does no work. 
- **No cross-actor zero-copy ("moved").** Handing a mutable object to another
  actor by *transferring ownership*, so no copy is needed at the boundary, is
  not supported. Copy into a `message` at the edge (above).
- **No request-scoped lifetime check.** A transport *may* live as an actor field,
  not only a handler local. There is no "this can't outlive its request"
  guarantee and no visible lifetime keyword.

## Related

- [Classes](/language/classes/): the mutable, single-owner type and its
  confinement rules (CE0010 / CE0087 / CE0112).
- [Messages](/language/messages/): the immutable shape for anything that
  crosses an actor boundary.
- [Modules](/language/modules/): where the stateless pipeline stages live.
