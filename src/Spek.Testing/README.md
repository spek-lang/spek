# Spek.Testing

Testing helpers for the [Spek](https://github.com/spek-lang/spek) actor language.

It rides on xUnit, so assertion failures throw `Xunit.Sdk.XunitException` and
render cleanly in the xUnit test explorer.

## What's inside

- `TestActorSystem` — wraps `ActorSystem`; accepts an explicit
  `ISnapshotStore` (from `Spek.Persistence.Abstractions`) and
  `IDeadLetterSink` for persistence and failure assertions across runs.
- `TestProbe` — stand-in actor with a `Send(target, message)` helper and
  typed / untyped / predicate / timeout / silence flavours of `ExpectMsg`.

## Typical shape

```csharp
using Spek;
using Spek.Runtime;
using Spek.Testing;

using var system = new TestActorSystem("test");
var probe = system.CreateProbe();
var echo  = system.Spawn<MyEchoActor>();

probe.Send(echo, new Ping());
probe.ExpectMsg<Pong>();
```

## License

Apache-2.0.
