# Rate limiter — a bucket per key, twice

A per-API-key rate limiter (token bucket: 10 burst, 5/second refill)
implemented two ways and driven by identical traffic: a hot working set of
keys taking most of the requests over a long cold tail, the shape API
traffic actually has.

```bash
./demos/run.sh ratelimiter
./demos/run.sh ratelimiter --keys 100000 --requests 1000000
```

On the Spek pane every API key is an actor holding its own bucket
([`Limiter.spek`](RateLimiter.Spek/Limiter.spek) — the whole thing is one
small actor). Three things come for free that the C# twin builds by hand:

- **Serialization.** Per-key decisions are ordered by the mailbox. The twin
  ([`ShardedLimiter.cs`](RateLimiter.CSharp/ShardedLimiter.cs)) needs a lock
  per bucket, and the refill has to happen under the same lock as the take
  or concurrent requests double-spend.
- **Eviction.** The demo's headline row. Keys are unbounded; state is not.
  The Spek pane's entire eviction story is `passivate after 2s` — idle keys
  leave memory on their own, and because the window equals the full-refill
  time, a passivated key that comes back with a fresh bucket is
  indistinguishable from one that idled and refilled. The twin earns the
  same table row with a sweeper timer, a scan over every bucket, and an
  evict-vs-refill race whose correctness argument takes a paragraph-long
  comment.
- **Testable time.** The actor reads `self.Clock`, so the bucket logic runs
  under virtual time in tests — refill seconds cost microseconds. The twin
  reads `Environment.TickCount64` directly; testing its refill means
  sleeping.

Two numbers in the table deserve honest framing. `allowed` differs slightly
between panes because token buckets are time-dependent: the slower pane's
run lasts longer, so more refill happens during it. The like-for-like check
is the `sanity key` row — one key hammered 100 times sequentially — where
both panes must allow exactly the burst capacity plus measured refill. And
`check throughput` favors the C# pane: a lock-protected dictionary read
is nearly free, while every Spek check is a full request-reply through a
mailbox — about two microseconds and ~420 bytes at this era's runtime
(steady-state, JIT-warmed; see `demos/benchmarks`). That margin has
already been halved twice by runtime work and the dispatcher era targets
what remains. What the Spek pane buys for the price is the rest of the
table plus everything you didn't have to write.

Related but distinct: `Spek.Resilience.RateLimiting` ships token-bucket
*ingress policies* that protect an actor from its callers. This demo is the
inverse — the rate limiter as the service itself, with the keyspace as the
actor population.

The demo exits non-zero if either pane fails the bucket arithmetic or
retains more than 5% of its keys after the idle window, so it doubles as an
integration test of spawn, ask, passivation, and introspection.
