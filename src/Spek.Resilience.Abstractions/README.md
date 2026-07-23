# Spek.Resilience.Abstractions

Stable contracts for Spek resilience policies. Reference this package
from libraries that need to *consume* a resilience policy without
pulling a specific implementation.

Policy categories:

- **Ingress** — admission control consulted before a message reaches
  its channel handler. Returns `Allow`, `Reject`, or `Defer`.
  Examples: rate limiting, bulkheads, admission gates.
- **Execution** — wrap the handler invocation itself with retry,
  circuit breakers, timeouts, hedging.

Concrete policies ship in their own packages
(`Spek.Resilience.RateLimiting`, `Spek.Resilience.Retries`, ...)
and depend only on this assembly.
