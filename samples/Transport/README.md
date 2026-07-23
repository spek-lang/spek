# Transport types sample

A self-contained program that shows the mutable-transport-type pattern, the
hot-path use of Spek's mutable `class`.

The transport type is just a `class`. `RequestContext` is an ordinary mutable
class; there is no separate `transport` keyword, because the mutable class *is*
the transport type. It's allocated once per request and threaded through a
synchronous pipeline of [`module`](../../docs/language/modules.md) methods
(`Pipeline.Authenticate`, `Pipeline.Route`), each mutating it in place. The
immutable-message alternative would allocate a fresh object at every stage.

Confinement keeps it race-free with no annotation on your part. The context
can't ride a message to another actor (`CE0010`) or be a shared-region field
(`CE0112`), so it never escapes the one actor handling the request; the actor
boundary and immutable messages still do all the concurrency work. When the
result must leave the actor, `Egress.Finalize` snapshots the mutable context into
an immutable `Response` message, the one copy you couldn't avoid, paid once.

## Build & run

```bash
dotnet run                      # prints the three responses below
```

Expected output:

```
200 /  ->  welcome
200 /about  ->  you asked for /about
401 /admin/secrets  ->
```

The project imports `Spek.targets`, so `dotnet run` compiles `Transport.spek`
into `obj/` and builds it in one step; there's no `.g.cs` to commit. The program
ends on its own: `AwaitTermination()` returns once the handler has drained its
mailbox and gone idle, so a finite workload needs no explicit shutdown.

See [the language docs](../../docs/language/transport-types.md) for the full
description of the pattern, and [Classes](../../docs/language/classes.md) for the
confinement rules that keep it safe.
