# Spek.Resilience.RateLimiting

Ingress rate-limit policies for Spek actors. It wraps the BCL
`System.Threading.RateLimiting` primitives (`TokenBucketRateLimiter`,
`SlidingWindowRateLimiter`, `FixedWindowRateLimiter`, and
`ConcurrencyLimiter`) and surfaces their `RetryAfter` metadata as
`PolicyDecision.Defer`.

```csharp
using Spek;                            // ActorRef
using Spek.Resilience.RateLimiting;

var policy = RateLimitIngressPolicy.TokenBucket(
    permitsPerSecond: 100,
    burstCapacity: 200);

actor.AttachIngressPolicy(policy);
```

Policies are attached per-`ActorRef`. You can chain several on the same actor,
where they are evaluated in attachment order and the first non-`Allow` decision
wins; rejected and deferred messages are dead-lettered with the policy's reason.

For per-tenant or per-actor-instance limiting, build a
`PartitionedRateLimiter<ResilienceContext>` and wrap it with
`PartitionedRateLimitIngressPolicy<TKey>`, or use the
`PerKeyTokenBucket` factory shortcut.

