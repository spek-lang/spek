---
title: From Go
layout: default
parent: Migration
nav_order: 6
permalink: /migration/from-go/
description: "Moving goroutine-and-channel designs onto Spek actors."
---

# Spek for Go developers

Go doesn't have actors. It has goroutines and typed channels, which
compose into something actor-shaped if you squint, but the model is
different enough that this page is as much a *translation guide* as a
concept map. Be honest with yourself up front:

- A goroutine is a lightweight thread, not an addressable entity.
- A channel is a typed pipe, not a protocol contract.
- A `select` across multiple channels has no direct Spek equivalent.
- Go has no built-in supervision; a panic in a goroutine brings down
  the process unless `recover()`'d.
- Go has no persistence; state lives in the goroutine's closure.

What Go has taught many teams about concurrency (keep state inside a
single owner, communicate via messages, don't share memory) is exactly
the discipline Spek bakes into the type system. If you've been writing
"actor-style" Go with an input channel per goroutine and a reply
channel embedded in each request, you've already converged on the
pattern. Spek makes it the default shape and fails to compile when
you break it.

## The mental model shift

| Go                                            | Spek                                                    |
|-----------------------------------------------|---------------------------------------------------------|
| Goroutine + owned channel                     | Actor + mailbox                                         |
| Channel of type `chan Msg`                    | `ActorRef` (accepts any declared `message` type)        |
| `for msg := range ch { ... }`                 | Dispatch loop, implicit; you write `on MsgType`         |
| `struct Request { Reply chan T }`             | `message Request(ActorRef replyTo)`                     |
| `ch <- msg` (fire-and-forget)                 | `target.Tell(new Msg(...));`                            |
| `ch <- req; <-req.Reply` (request-reply)      | `Reply r = target.Ask(new Request());`                       |
| `select { case a := <-chA: ... case b := <-chB: ... }` | No equivalent: single mailbox per actor         |
| `close(ch)` (end-of-stream signal)            | No equivalent: actor stop via `Stop` directive         |
| `defer recover()` + log                       | `OnFailure(Exception ex, object msg)` override          |
| `go worker()` pool                            | `spawn<Worker>()` repeatedly; supervise as children     |
| Process crash on un-`recover`'d panic         | Actor crash + parent `OnChildFailure` routes the failure |
| `sync.Mutex` / atomics                        | Not needed: messages are the synchronization primitive |
| `context.Context` cancellation                | Supervisor stop + `on PostStop`                         |
| No persistence primitive                      | `persist;` + `on Restore(Snapshot s)`                   |
| No idle-unload primitive                      | `passivate after System.TimeSpan.FromMinutes(30);`                           |
| Tests: spin goroutine, send on channel        | `Spek.Testing` (`TestActorSystem`, `TestProbe`)         |

## Side-by-side

### A counter service

**Go:**

```go
type CounterMsg struct {
    Action string        // "inc" or "get"
    Reply  chan int      // for "get"; ignored for "inc"
}

func counterService(msgs <-chan CounterMsg) {
    count := 0
    for m := range msgs {
        switch m.Action {
        case "inc":
            count++
        case "get":
            m.Reply <- count
        }
    }
}

func main() {
    msgs := make(chan CounterMsg, 16)
    go counterService(msgs)

    msgs <- CounterMsg{Action: "inc"}
    msgs <- CounterMsg{Action: "inc"}

    reply := make(chan int, 1)
    msgs <- CounterMsg{Action: "get", Reply: reply}
    fmt.Println(<-reply)
}
```

**Spek:**

```spek
message Increment();
message Get();
message Count(int value);

actor Counter
{
    int count = 0;

    behavior Tracking
    {
        on Increment => { count = count + 1; }
        on Get       => return new Count(count);
    }
}

program Main
{
    var system = new ActorSystem("counter");
    ActorRef counter = system.Spawn<Counter>();
    counter.Tell(new Increment());
    counter.Tell(new Increment());
    Count c = counter.Ask<Count>(new Get());
    Console.WriteLine(c.value);
    system.AwaitTermination();
}
```

Observations from translating:

- Go smuggles the message *tag* into a string field (`"inc"` /
  `"get"`). Spek makes each tag its own type; a message the actor
  doesn't handle lands in the dead-letter sink, observable in tests,
  instead of falling through a `switch` silently.
- Go smuggles the reply address into the request struct
  (`Reply chan int`). Inside a handler, Spek's `target.Ask(new Get())`
  with its inferred reply type handles this for you; the explicit Akka
  Typed shape (`message Get(ActorRef replyTo)`) is also available if you
  prefer the Go-style explicit embedding.
- Go's `for msg := range msgs` loop is implicit in Spek: the runtime
  dispatches, you write handlers.

### Supervision via `defer recover()`

**Go:**

```go
func worker(msgs <-chan Job) {
    defer func() {
        if r := recover(); r != nil {
            log.Printf("worker crashed: %v", r)
            // no restart unless the parent goroutine re-spawns
        }
    }()
    for j := range msgs {
        process(j)
    }
}
```

**Spek:**

```spek
actor Parent
{
    ActorRef worker;

    init()
    {
        worker = spawn<Worker>();
        become Running;
    }

    supervise(worker, strategy: OneForOne(
        on Failure: Restart,
        maxRetries: 5,
        withinTime: System.TimeSpan.FromMinutes(1)
    ));

    behavior Running { }
}
```

The Go version logs-and-exits. The Spek version declares a supervision
policy at the parent, so a crash in `Worker` triggers a runtime-managed
restart with bounded retry intensity. No `defer recover()` needed in
the worker itself.

### Fan-out / fan-in

**Go** does this beautifully with channels and `select`:

```go
results := make(chan Result, n)
for i := 0; i < n; i++ {
    go func(item Item) { results <- process(item) }(items[i])
}
for i := 0; i < n; i++ {
    collect(<-results)
}
```

**Spek** does it by spawning worker actors and collecting replies via
message handlers:

```spek
message WorkItem(int index, string payload);
message Result(int index, string outcome);
message BatchDone();

actor Coordinator
{
    int expected;
    int received = 0;

    init(int batchSize)
    {
        expected = batchSize;
        for (int i = 0; i < batchSize; i += 1)
        {
            ActorRef w = spawn<Worker>();
            w.Tell(new WorkItem(i, /* ... */));
        }
        become Collecting;
    }

    behavior Collecting
    {
        on Result r =>
        {
            // handle r
            received += 1;
            if (received == expected) { self.Tell(new BatchDone()); }
        }

        on BatchDone => { /* finish */ }
    }
}
```

This is more verbose than Go's fan-out, but it buys you supervision
over every worker for free. Go users reaching for goroutine pools
often re-invent a chunk of what Spek's supervision tree already gives
you.

## On `select`

Go's `select` is the feature with no clean Spek equivalent. `select`
lets a goroutine wait on multiple channels at once and act on whichever
is ready. Spek actors have exactly one mailbox; messages arrive there
in FIFO order; `on MsgType` dispatches based on the message's type, not
on "which channel it came from". There is no ready-or-else-timeout,
no multi-channel synchronization, no priority-by-channel.

Patterns Go developers lean on `select` for, and how they translate:

- **Timeout**: `select { case m := <-ch: ... case <-time.After(d): ... }`
  → actor schedules a `Timeout` message to itself (`self.Tell(new
  Timeout())` via a timer actor) and handles it as `on Timeout`.
- **Cancellation**: `case <-ctx.Done():` → supervisor sends `Stop`;
  actor handles cleanup in `on PostStop`.
- **Multiplexing inputs**: `case a := <-chA: case b := <-chB:` → define
  both input types as messages; both are `Tell`'d to the same actor;
  the mailbox interleaves them in arrival order.
- **Non-blocking send**: `select { case ch <- v: default: }` → Spek's
  mailbox is unbounded; `Tell` always succeeds.

If you genuinely need "wait on multiple channels with priority", you're
outside the actor model. Reach for
`System.Threading.Channels` from within a plain `Task` and bridge to
actors at the boundaries.

## On channel `close(ch)`

Go idiom uses `close(ch)` to signal end-of-stream: the reader's `range`
loop exits, and a `<-ch` on a closed channel returns the zero value
without blocking. Spek has no equivalent. The closest thing is sending
a sentinel message (`message EndOfStream()`) and having the actor
transition to a terminal behavior or stop itself.

For streaming workloads specifically (bounded producers, demand-based
flow control, composable pipelines), Spek suggests you use
`System.Threading.Channels` between plain tasks, and actor-ify only the
pieces that need identity, supervision, or persistence.

## What Spek adds to the goroutine model

- **Actor identity and addressing.** A goroutine can't be pointed at;
  you hold its channel, which is its *input*, not *itself*. An
  `ActorRef` is a stable handle to a specific actor's mailbox.
- **Supervision.** No `defer recover()` boilerplate, no "who restarts
  the worker when it panics" question. Parents declare supervision
  strategy per child.
- **Persistence.** `persist;` + `on Restore(Snapshot s)` is a runtime
  feature, not something you build over channels + disk yourself.
- **Passivation.** Idle actors unload; send a message, they wake up
  with restored state.
- **Compile-time message immutability** (CE0010). Go lets you send a
  `*struct` over a channel; the receiver can mutate what the sender
  still holds. Spek rejects mutable message fields at compile time.
- **`actor` as a keyword.** In Go, "actor" is a convention
  enforced by code review. In Spek, it's a keyword with compile-time
  rules.

## Where Go keeps the edge

- **Extremely cheap goroutines.** Modern Go runtimes can comfortably
  run hundreds of thousands of concurrent goroutines on a single
  machine. Spek actors sit on .NET `Task`s and the Spek runtime
  dispatcher, lighter than one-OS-thread-per-actor, but not as
  featherweight as goroutines. Millions-of-actors workloads still
  belong on BEAM or Go.
- **`select`.** Genuinely useful, no direct replacement. See above.
- **Channel close semantics.** Signaling end-of-stream via channel
  close; no equivalent.
- **`context.Context`** threaded deadline + cancellation propagation.
  Spek's supervisor tree handles stops; per-request deadlines are your
  job to model as messages.
- **Bounded channels with backpressure.** `make(chan Msg, N)` with a
  full channel blocks the sender. Spek mailboxes are unbounded.
- **The simplicity of plain functions + channels.** Actors buy you
  identity, supervision, and persistence; if you don't need those,
  `System.Threading.Channels` on top of `Task` is closer to the Go
  experience than spinning up an ActorSystem.

## When to pick what

- **Stay in Go / channels / Tasks** when: your workload is bounded,
  stateless pipelines; you don't need addressability or supervision;
  you want to wait on many things at once.
- **Reach for Spek** when: you have long-lived stateful entities, need
  crash-restart policies, or want the compiler to stop you from
  sharing mutable state across concurrent workers.

Nothing stops you from mixing the two: Spek actors can `Tell` each
other at the top level of your system, and can internally use
`System.Threading.Channels` or `Task.WhenAll` for streaming or fan-out
workloads. The actor boundary is where you want identity + supervision;
everything else can stay plain.

See the [language reference](/language/) for the full grammar,
the [actors reference](/language/actors/) for lifecycle details, and
the [runtime reference](/reference/runtime/) for the `ActorRef` API.
