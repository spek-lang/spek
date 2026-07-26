---
title: Async without await
layout: default
parent: Language
nav_order: 8
permalink: /language/async/
description: "Invisible async: Task-returning calls are auto-awaited; how async propagates through handlers, how var defers the await, and why it can't leak work."
---

# Async without await

So far every handler you've written has run to completion synchronously:
read a field, build a reply, [`return` it to the asker](/language/messages/).
But real handlers reach for the outside world. They read a file, call an
HTTP endpoint, query a store, and in .NET those operations are
*asynchronous*: they hand back a `Task<T>`, not the value. In C# you'd
sprinkle `async` and `await` through your code to deal with that, and the
"colour" of those keywords spreads up the call stack until half your
program is `async`.

Spek has **no `async` keyword and no `await` keyword**. You write code that
looks synchronous, call the same `Task`-returning library methods, and the
compiler makes the async part disappear. It inserts the awaits, marks the
right methods async, and even runs independent work concurrently for you.
This chapter is how that works.

{: .note }
> **Where this comes from.** Most languages make concurrency explicit: C#'s
> `async`/`await`, Go's `go`, JS promises. Spek inverts the default: independent
> work runs concurrently and Task-returning calls are awaited for you, so there's
> no `async`/`await` to write or forget.

## A Task is awaited where it meets a value

Here is a handler that reads a file off disk. `System.IO.File.ReadAllTextAsync`
is an ordinary BCL method that returns a `Task<string>`:

<!-- spek-test: compile -->
```spek
message ReadFile(string path);
message FileLength(int length);

actor Reader
{
    on ReadFile r =>
    {
        string content = System.IO.File.ReadAllTextAsync(r.path);
        return new FileLength(content.Length);
    }
}
```

You never see the `Task`. You wrote `string content = …`, so you get a
`string`; you read `.Length` off it like any other value. The compiler does
the rest: it inserts the `await` and marks the handler async for you.

That is the entire model, stated as one rule:

> A `Task` is awaited **wherever it meets a value context**: a place that
> expects the value, not the task.

You assigned the result to a `string`, returned it where a value was
expected, did arithmetic on it, read a property off it. Those are all
*value contexts*, so the task is awaited there. The only thing you ever
decide is **when you want the value**, and you express that through how you
bind it.

{: .note }
> This is real semantic analysis, not a naming heuristic. Because Spek
> compiles to C#, the compiler resolves the *actual* return type of every
> call against the referenced assemblies, so it auto-awaits any
> framework or third-party async, not just methods that happen to end in
> `Async`.

## Async propagates through your own functions

The same rule applies one level up. When a [module method](/language/modules/)
awaits a task, it itself becomes async, and any function that calls *it* in a
value context is now calling something `Task`-returning, so its call is
auto-awaited too. The async-ness propagates outward to a fixpoint, and you
never annotate a single step of it:

<!-- spek-test: compile -->
```spek
module Files
{
    public int Lines(string path)
    {
        string text = System.IO.File.ReadAllTextAsync(path);
        return text.Split('\n').Length;
    }

    public int TotalLines(string a, string b)
    {
        return Lines(a) + Lines(b);
    }
}
```

`Lines` awaits the file read, so the compiler lowers it to an
`async Task<int>`. `TotalLines` calls `Lines` and uses the results as `int`
values, so those calls get awaited in turn, and `TotalLines` becomes async
as well. You wrote two plain `int`-returning methods; the colouring
happened underneath.

## Sequential vs. concurrent: controlled by *when you use the result*

Here is the part that differs from most languages. Look at these two module
methods, which differ in exactly one place:

<!-- spek-test: compile -->
```spek
module ReadBoth
{
    // Sequential — each read finishes before the next begins:
    public int OneAtATime(string p1, string p2)
    {
        string a = System.IO.File.ReadAllTextAsync(p1);   // wait for A...
        string b = System.IO.File.ReadAllTextAsync(p2);   // ...then read B
        return a.Length + b.Length;
    }

    // Concurrent — both reads run at the same time:
    public int Overlapping(string p1, string p2)
    {
        var a = System.IO.File.ReadAllTextAsync(p1);       // start A...
        var b = System.IO.File.ReadAllTextAsync(p2);       // ...start B (A still running)
        return a.Length + b.Length;                        // wait for both, here
    }
}
```

The only difference is `string` versus `var`. Why does that change anything?

With `string a = …` you named the *value* type, so you're saying "I want the
result here." The task is awaited on that line, and the next line doesn't start
until A is done. That's the sequential case. With `var a = …`, `var` infers the
*actual* type, which is `Task<string>`, so `a` holds the still-running task and
the await is deferred to where you actually *use* `a` as a value. You don't use
it until the final line, so both reads are already in flight by the time you
join them. That's the concurrent case.

So the lever is simple: use it now and you get sequential, use it later and you
get concurrent. You opt *out* of concurrency (by naming the value type) rather
than into it. For an actor language whose whole point is concurrency, that's the
right default.

These are the binding forms, in full:

| You write | What happens | Reach for it when |
|-----------|--------------|-------------------|
| `var x = f();` … later `use(x)` | starts now, awaited at use → overlaps with independent work | the default; concurrent |
| `string x = f();` | awaited right here | you want a synchronous checkpoint |
| `f();` (as a statement) | awaited right here | ordered side effects (logging, writes) |
| `Task<string> x = f();` | you hold the raw `Task` to manage yourself | advanced / hand-off |

## Why it can't leak work

Deferring awaits raises a fair question: what if you start something with
`var` and never use it? Spek guarantees you can't leak work. **Every task
started inside a handler or function is awaited before that scope returns:**
at your value-use if you used it, or automatically at the closing brace if
you didn't. This is *structured concurrency*: the scope is the boundary, and
nothing escapes it still running.

In an actor this matters twice over. A handler runs under the actor's lock,
one message at a time, exactly as [isolation](/language/isolation/)
promised. Because every task is joined before the handler returns, no
background continuation can resume *after* the handler finishes and race the
next message. **The concurrency scope and the isolation boundary are the
same thing:** you get maximum overlap *inside* a handler and strict
serialization *across* handlers, with no way to accidentally cross the line.

## Escape hatches

Three forms let you step outside the default when you need to.

**Force a synchronous point.** Bind with the explicit value type
(`string x = f();`) or call as a statement (`f();`). The task is awaited
right there. Statement-position calls are the idiom for ordered side
effects, two writes that must land in order:

<!-- spek-test: compile -->
```spek
module Sink
{
    public void AppendBoth(string path, string first, string second)
    {
        System.IO.File.AppendAllTextAsync(path, first);    // awaited here
        System.IO.File.AppendAllTextAsync(path, second);   // then here
    }
}
```

**Hold the raw task.** Bind with the explicit `Task<T>` type and the binding
*doesn't* await; you get the actual `Task` to pass to a `Task`-shaped C# API
or hold onto. (It's still joined at scope exit if you abandon it, so it can't
leak.)

<!-- spek-test: compile -->
```spek
module Handoff
{
    public int CharCount(string path)
    {
        Task<string> t = System.IO.File.ReadAllTextAsync(path);   // raw task, not awaited here
        string body = t.Result;                                   // read it when you want the value
        return body.Length;
    }
}
```

**`.Result` is awaited, not blocking.** If interop hands you a `Task<T>` and
you write `task.Result`, the compiler rewrites it to `(await task)`: same
value, no parked thread, and the exception is unwrapped rather than wrapped
in an `AggregateException`:

<!-- spek-test: compile -->
```spek
module Interop
{
    public int LengthOf(System.Threading.Tasks.Task<string> incoming)
    {
        string s = incoming.Result;     // emitted as `(await incoming)`
        return s.Length;
    }
}
```

So you never need to think about it. Reach for `.Result` if it reads
naturally and it just becomes an await. The same rewrite covers the blocking
method forms: `task.Wait()` becomes `await task`, and
`x.GetAwaiter().GetResult()` becomes `(await x)`. What cannot be rewritten is
an error instead ([CE0083](/reference/errors/#ce0083)): `Thread.Sleep`, the
static `Task.WaitAll`/`WaitAny`, `Console.ReadLine`, and the wait-handle
family (`WaitOne`, `SignalAndWait`) would genuinely park a dispatcher thread.
See [Common pitfalls](/language/footguns/) for the full triage of .NET's
blocking hazards.

## Invisible cancellation

Cancellation is invisible too. You never write a `CancellationToken`, yet
the compiler threads one through for you. Every `ActorSystem` has a shutdown
token, and when an auto-awaited call inside an **actor handler** accepts a
`CancellationToken`, the compiler passes it:

<!-- spek-test: compile -->
```spek
message Fetch(string url);

actor Worker
{
    private System.Net.Http.HttpClient httpClient = new System.Net.Http.HttpClient();

    on Fetch f => { httpClient.GetStringAsync(f.url); }

    term { httpClient.Dispose(); }
}
```

The handler body you wrote is just `httpClient.GetStringAsync(f.url)`. Inside
the generated handler it becomes:

```csharp
await httpClient.GetStringAsync(f.url, cancellationToken: this.ShutdownToken);
```

The token fires only when shutdown turns **forceful** (a graceful drain
that times out, or teardown), so in-flight work normally finishes (a clean
shutdown lets the current message complete), and a wedged handler's awaited
call unwinds at the deadline instead of blocking teardown. Hosting adapters
link their `StopAsync` token in, so a host's shutdown timeout flows all the
way through.

Why no `CancellationToken` parameter on handlers? Same reason there's no
`async`/`await`: it's plumbing, not your concern. And an actor's unit of
work is a *message*, not a cancellable operation, so you let the message
finish; the runtime just stops dispatching new ones. The token is threaded
in actor handlers, where `this.ShutdownToken` is in scope; module and static
methods, which have no `this`, are left alone.

## Where the gate stops

The compiler is deliberately conservative: **when in doubt, it awaits
eagerly**, which is always safe (you lose some overlap, never correctness).
A few cases fall back to a plain await:

- Concurrency (lazy `var`) applies to bindings at the method's top level.
  Early returns, before *or* after the binding, are handled (the compiler
  joins the task before each exit). A binding declared inside a **loop or a
  nested block** falls back to eager await: correct, just sequential.
- A `var` binding of a call that returns a **non-generic `Task`** (a
  void-result async) inside such a nested block can't defer. Call it as a
  statement (`LogAsync(x);`) or bind the explicit type (`Task t = …`)
  instead; a `Task<T>` binding is unaffected.
- `ValueTask<T>` is always awaited eagerly (it can't be awaited twice).
- No auto-await inside `init` blocks or property accessors, since those can't
  be async.

You'll rarely meet these edges. The takeaway for everyday code is the rule
at the top: name the value type when you want it now, use `var` when you're
happy for it to overlap, and let the compiler carry the `async`/`await`.

## Next

Several examples in this chapter leaned on actor-local helpers like
`HttpClient` held in a field. The next chapter,
[Classes](/language/classes/), covers Spek's own confined mutable classes:
the actor-local helper objects that keep share-XOR-mutate intact.

## Related

- [Common pitfalls](/language/footguns/): how Spek triages the .NET
  async/blocking hazards (rewrite / suggest / warn / error).
- [Modules](/language/modules/): where standalone methods live, and where
  async propagation starts.
