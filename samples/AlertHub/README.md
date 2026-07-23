# AlertHub: contracts and inheritance, across files

An ops alert router that exercises every contract and inheritance form in the
language — and does it across ten files, because the compiler now resolves
types across a whole project, not file by file. Each type lives in its own
file, named after the type, the way a C# project would lay it out.

The message layer is a *family*: `AlertEvent` is an `abstract message`, and
`CpuHigh`, `DiskFull`, and `ServiceDown` are its variants. The notifier keys
one handler on the base — `on AlertEvent` receives every variant — while
`ServiceDown` gets its own arm, which wins because specific handlers match
first. A `switch` expression over the family recovers each variant's fields.

The class layer shows both contract kinds. `Formatter` is an `interface`
(signatures only — a body inside it won't compile), and the notifier holds a
`Formatter`-typed field it swaps from `PlainFormatter` to `JsonFormatter`
mid-stream without touching a call site. `EscalationPolicy` is an `abstract
class` whose `Describe` method calls the abstract `Threshold()` hook that
`PagerPolicy` supplies — constructor chaining runs through
`init() : base("pager")`.

The actor layer mirrors it: `NotifierBase` is an `abstract actor` carrying
protected state, a helper method, an abstract `Prefix()` method, and an
`abstract behavior Running` that `ConsoleNotifier` fills in with
`override behavior Running` — chaining its constructor with
`init() : base("console")`.

```
dotnet run
```

Watch for the formatter swap halfway through the output, and the pager firing
once the second critical alert crosses `PagerPolicy`'s threshold.
