# Concurrency sample: invisible async

This sample runs two independent "fetches" concurrently without a single
`async` or `await`. What makes them concurrent is nothing more than how each
result is bound.

`Both()` binds each call with `var`, so the Task is deferred and both are in
flight by the time they're awaited together at `Combine`. `Sequential()` binds
with the value type `int`, so each call is awaited right there and they run one
after the other. The only difference between the two functions is `var` versus
`int`.

## Run

```bash
dotnet run      # compiles Concurrency.spek into obj/ and runs
```

Observed output (two 200 ms delays each):

```
concurrent: result=42 in ~203ms
sequential: result=42 in ~402ms
```

Same code, same result: the `var` version overlaps the two delays and
finishes in roughly half the time.

## What the compiler generated

```csharp
// Both()  — deferred bindings, awaited together → concurrent
public static async Task<int> Both()
{
    var a = Slow(200, 20);              // not awaited here...
    var b = Slow(200, 22);
    return Combine(await a, await b);    // ...awaited together
}

// Sequential()  — value-typed bindings, awaited eagerly → sequential
public static async Task<int> Sequential()
{
    int a = await Slow(200, 20);
    int b = await Slow(200, 22);
    return Combine(a, b);
}
```

See [the async docs](../../docs/language/async.md) for the full model.
