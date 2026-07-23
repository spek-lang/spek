# Spek.Streams

Stream-shaped event policies for Spek actors. Provides built-in
operators (`debounce`, `throttle`, `distinct`, `compose`) that
attach to actor handlers via the `=>` chain syntax,
plus the runtime base class (`StreamOperator<T>`) for user-defined
operators.

```spek
using Spek.Streams;

actor UiBackend
{
    on event MouseMove(object s, MouseEventArgs e)
        => throttle(16)
        => { /* runs at ~60fps regardless of input rate */ }

    on event FileChanged(object s, FileSystemEventArgs e)
        => debounce(500)
        => { /* runs once after the FileWatcher burst quiets */ }
}
```

## Built-in operators

| Operator                              | Shape       | Behaviour                                                                                                               |
|---------------------------------------|-------------|-------------------------------------------------------------------------------------------------------------------------|
| `debounce(int milliseconds)`          | unary timer | Hold the latest message; emit it after the source goes quiet for the configured interval. New messages reset the timer. |
| `throttle(int milliseconds)`          | unary timer | Emit at most one message per interval. Surplus messages are dropped (leading-edge throttle).                            |
| `distinct<T, TKey>(Func<T, TKey> by)` | predicate   | Emit only when the key extracted by the selector differs from the previous emit's key.                                  |
| `compose(params StreamOperator<T>[])` | composer    | Combine several operators into one merged operator with shared state.                                                   |

## Time and testing

The timer-based operators (`debounce`, `throttle`) take their time
source from the owning actor's clock: the generated wiring passes it
through `Configure`, so under `TestActorSystem(virtualTime: true)` or
the simulator a debounce window elapses when the test advances the
clock, not when the wall does. Hand-configured operators default to
the system clock.

## Custom operators

Derive from `Spek.Streams.StreamOperator<T>` and expose a lowercase
factory function. Read time through the inherited `Clock` property
(and create timers with `Clock.CreateTimer`) so your operator stays
controllable under virtual time, the same way the built-ins are:

```csharp
namespace MyApp.Streams;

using Spek.Streams;

public sealed class FrameRateLimiter<T> : StreamOperator<T>
{
    private readonly TimeSpan _interval;
    private long _lastEmit;   // Clock.GetTimestamp units; 0 = never

    public FrameRateLimiter(int maxFps)
        => _interval = TimeSpan.FromSeconds(1.0 / maxFps);

    public override async Task OfferAsync(T message)
    {
        var now = Clock.GetTimestamp();
        if (_lastEmit == 0 || Clock.GetElapsedTime(_lastEmit, now) >= _interval)
        {
            _lastEmit = now;
            await Dispatch(message).ConfigureAwait(false);
        }
        // otherwise drop
    }
}

public static class FpsOperators
{
    public static StreamOperator<T> framerate<T>(int maxFps)
        => new FrameRateLimiter<T>(maxFps);
}
```

In Spek source, `using MyApp.Streams;` then call by name:

```spek
on event MouseMove(...)
    => framerate(60)
    => { /* body */ }
```
