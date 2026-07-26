---
title: From Erlang / OTP
layout: default
parent: Migration
nav_order: 2
permalink: /migration/from-erlang-otp/
description: "How Erlang/OTP patterns (gen_server, supervision trees, registries) translate into Spek."
---

# Spek for Erlang / OTP developers

Spek draws heavily from Erlang's conceptual foundation (let it crash,
supervision trees, messages as the sole communication) but reshapes the
syntax for C#-shaped mental models. If you came from Erlang to the .NET
world and missed `gen_server`, Spek is the closest thing going.

## The biggest shape differences

- **One process model → one actor class.** Erlang has `gen_server`,
  `gen_statem`, `gen_event`, `gen_fsm`. Spek has one kind of thing: an
  actor, with `behavior` blocks for state and an implicit mailbox.
- **No separate supervisor process.** In OTP, a supervisor is its own
  `gen_server` with restart policy in its child-spec. In Spek, the parent
  actor itself is the supervisor: `OnChildFailure` decides what happens,
  with a declarative `supervise` decl at the class level.
- **No hot code swap.** OTP's code upgrades via `code_change/3` have no
  Spek equivalent.

## Concept mapping

| Erlang / OTP                                    | Spek                                              |
|-------------------------------------------------|---------------------------------------------------|
| `-module(foo)` + `gen_server` behavior          | `actor Foo`                                       |
| `handle_call({deposit, Amt}, _From, S)`         | `on Deposit d => { ... } ` (inside `behavior`)    |
| `handle_cast({log, Msg}, S)`                    | `on Log msg => { ... }` (same dispatch)           |
| `handle_info` (non-OTP messages)                | no separate path; one mailbox, one dispatch      |
| `gen_server:call(Pid, Msg)` (request-reply)     | `target.Ask(new MsgType(args))` inside `on`            |
| `gen_server:cast(Pid, Msg)` (fire-and-forget)   | `target.Tell(new Msg(...))`                       |
| `self()`                                        | `self`                                            |
| `{reply, R, S2}` / `{noreply, S2}`              | `return new Reply(...);` (or `sender.Tell` for fan-out)  |
| State record (`-record(s, {...})`)              | Actor fields                                      |
| `init([Args])` returning `{ok, State}`          | `init(Args) { field = ...; become Idle; }`        |
| `terminate(Reason, S)`                          | `on PostStop => { ... }`                          |
| `code_change/3`                                 | No equivalent (no hot code swap)                  |
| `gen_statem` named states (`handle_event`)      | `behavior NamedState { ... }`                     |
| `gen_statem:cast` → `next_state`                | `become NamedState;`                              |
| **Supervision**                                 |                                                   |
| `supervisor:start_link/3`                       | Parent actor spawning children; no separate sup  |
| `child_spec` with `Restart` + `Shutdown`        | `supervise(child, strategy: OneForOne(...))`      |
| `one_for_one` restart strategy                  | `OneForOne(...)`                                  |
| `one_for_all`                                   | `AllForOne(...)`                                  |
| `intensity` (max restarts)                      | `maxRetries: N`                                   |
| `period` (time window)                          | `withinTime: System.TimeSpan.FromSeconds(N)`      |
| `{Restart, permanent}` / `temporary` / `transient` | `on Failure: Restart / Stop / Escalate` (loose mapping; `transient`'s restart-on-abnormal-exit has no direct arm) |
| **Persistence / state survival**                |                                                   |
| `mnesia` / `ets` / explicit serialization       | `persist;` + `on Restore(Snapshot s)`             |
| `application:get_env`                           | `ActorSystem` is constructed with its config      |
| **Testing**                                     |                                                   |
| `meck` / `common_test`                          | `Spek.Testing` (`TestActorSystem`, `TestProbe`)   |

## Side-by-side

### A persistent counter

**Erlang (gen_server):**

```erlang
-module(counter).
-behaviour(gen_server).

-record(state, {count = 0}).

init([]) -> {ok, #state{}}.

handle_cast(increment, S) ->
    NewCount = S#state.count + 1,
    {noreply, S#state{count = NewCount}};

handle_call(get, _From, S) ->
    {reply, S#state.count, S}.
```

**Spek:**

```spek
message Increment();
message Get();
message Count(int value);

actor Counter
{
    int count = 0;

    behavior Active
    {
        on Increment => { count = count + 1; persist; }
        on Get       => return new Count(count);
    }

    on Restore(Snapshot s) => count = s.Get<int>("count");
}
```

### Supervision

**Erlang (sup.erl):**

```erlang
-module(my_sup).
-behaviour(supervisor).

init([]) ->
    Strategy = #{strategy => one_for_one, intensity => 5, period => 60},
    Children = [#{id => worker, start => {worker, start_link, []},
                  restart => permanent}],
    {ok, {Strategy, Children}}.
```

**Spek:**

```spek
actor Supervisor
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

## Where Spek goes further than OTP

- **Compile-time immutability for messages** (CE0010). Erlang gets this
  for free because every term is immutable; Spek enforces it on the
  `.NET` side where types like `List<T>` exist and are mutable.
- **Compile-time become-target validation** (CE0011). Erlang's dynamic
  nature means `gen_statem:next_state(State, Data)` with a typo for
  `State` only blows up at runtime.
- **Compile-time behavior reachability** (CE0014). Dead code is visible
  at build time.
- **Structured dead-letter sink** with test-time recording, useful in a
  way that Erlang's `sys:log` plumbing isn't.

## What BEAM still does better

- **Hot code swap**: no Spek equivalent.
- **Distribution built into the VM.** BEAM nodes cluster natively and the machinery is decades-hardened; Spek's clustering is opt-in packages with remote `Tell` only.
- **Pattern-matching on message payloads** in handlers (Spek relies on
  C# pattern matching in the handler body).
- **Process registry / `erlang:whereis`**: the nearest Spek analogue is
  `SpawnNamed`/`ResolveNamed`, and it covers root actors only; below the
  roots, refs travel through constructor args and messages.
- **Lightweight processes measured in KBs not MBs**: .NET actors are
  heavier than BEAM processes. For millions-of-actors scenarios, Erlang
  is still unmatched.
