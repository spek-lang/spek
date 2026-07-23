# Modules sample

A self-contained program that shows off modules, parameter modifiers, and
invisible async.

The `module` declarations `MathUtil` and `AsyncUtil` hold stateless methods,
including a nested module, `MathUtil.Stats`. Methods take `in`, `ref`, and
`out` parameter modifiers, with the modifier restated at the call site. And the
source has no `async` or `await` keywords: `AsyncUtil.DelayedDouble` calls
`Task.Delay`, so the compiler awaits it and makes the method `async`, and the
actor that calls `DelayedDouble` is then auto-awaited in turn as the async
property propagates.

## Build & run

```bash
dotnet run                      # prints 42, 7, 17, 20, 10
```

The project imports `Spek.targets`, so `dotnet run` compiles `Modules.spek`
into `obj/` and builds it in one step; there's no `.g.cs` beside the source to
commit. See [the language docs](../../docs/language/modules.md) for the full
feature description.

## What the emitter produces

| Spek source | Emitted C# |
|---|---|
| `module MathUtil { ... }` | `public static class MathUtil { ... }` |
| `public int Double(int n)` | `public static int Double(int n)` |
| `module Stats { ... }` (nested) | nested `public static class Stats` |
| `void SumInto(int a, int b, out int result)` | `static void SumInto(int a, int b, out int result)` |
| `int DelayedDouble(int n)` that awaits | `static async Task<int> DelayedDouble(int n)` |
| `DelayedDouble(5)` (call site) | `await AsyncUtil.DelayedDouble(5)` |
