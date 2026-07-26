---
title: Observability
layout: default
parent: Hosting
nav_order: 3
permalink: /hosting/observability/
---

# Observability

The runtime carries telemetry of its own: metrics, traces, and
structured logs. The pattern is the same one the rest of
the language uses: a small abstraction layer
(`Spek.Observability.Abstractions`) plus a default adapter
(`Spek.Observability.OpenTelemetry`) that maps onto the standard
.NET primitives. Apps that don't register an adapter pay no
telemetry overhead, because every call inlines into the null sink.

## What you get for free

The runtime emits per-actor metrics on every dispatch:

| Metric | Type | Description |
|---|---|---|
| `spek.mailbox.depth` | gauge | Pending messages in the actor's queue, sampled at enqueue |
| `spek.mailbox.dispatch` | counter | Messages pulled from the mailbox and handed to the handler |
| `spek.actor.handler.duration.ms` | histogram | How long each handler took (includes lock wait) |
| `spek.actor.handler.slow` | counter | Handlers that exceeded the slow-handler threshold |
| `spek.actor.restart` | counter | Supervised restarts (tagged with actor and exception type) |
| `spek.actor.stop` | counter | Actor stops (tagged with cause) |
| `spek.region.lock.wait.ms` | histogram | Time spent waiting on a shared region's lock |
| `spek.deadletter` | counter | Messages routed to the dead-letter sink (tagged with reason) |

Each metric carries an `actor.type` tag so dashboards can split
by actor class. Names follow OpenTelemetry conventions
(lowercase, dot-separated, units suffixed for histograms) and are
defined as constants on `SpekMetricNames` for stable
dashboard authoring.

## Wiring the OpenTelemetry adapter

`Spek.Observability.OpenTelemetry` is the default adapter. It
maps the runtime's metric calls onto
`System.Diagnostics.Metrics.Meter` (under the meter name
`"Spek.Runtime"`) and structured-log calls onto
`Microsoft.Extensions.Logging.ILogger`. Trace propagation flows
through `System.Diagnostics.Activity` natively, so any
OTel-aware tracing pipeline stitches Spek's per-handler spans
into distributed traces with no extra plumbing.

```csharp
using Spek.Observability.OpenTelemetry;

var system = new ActorSystem("orders")
    .UseOpenTelemetryMetrics()
    .UseLoggerFactory(loggerFactory);

// In your OTel SDK setup:
Sdk.CreateMeterProviderBuilder()
   .AddMeter("Spek.Runtime")
   .AddPrometheusExporter()
   .Build();

Sdk.CreateTracerProviderBuilder()
   .AddSource("Spek.Runtime")                   // dispatch spans
   .AddOtlpExporter()
   .Build();
```

That's the whole wiring. The Spek runtime emits to standard .NET
telemetry primitives; the OTel SDK picks them up; exporters push
them to your backend.

## Custom sinks

For testing or quick instrumentation without an OTel pipeline,
register any sink that implements `IMetricSink`:

```csharp
var system = new ActorSystem("user-api")
    .UseMetricSink(new StdoutSink())
    .UseLogger(myStructuredLogger);

public sealed class StdoutSink : IMetricSink
{
    public void Counter(string name, long delta = 1,
        IReadOnlyList<KeyValuePair<string, object?>>? tags = null)
        => Console.WriteLine($"counter {name} += {delta}");
    public void Gauge(string name, double value,
        IReadOnlyList<KeyValuePair<string, object?>>? tags = null)
        => Console.WriteLine($"gauge {name} = {value}");
    public void Histogram(string name, double value,
        IReadOnlyList<KeyValuePair<string, object?>>? tags = null)
        => Console.WriteLine($"hist {name} <- {value}");
}
```

## User-emitted metrics

Inside any handler, `self.Metrics` reaches the registered sink:

```spek
on Place o =>
{
    self.Metrics.Counter("orders.placed",
        tags: new[] { ("type", o.kind) });
    self.Metrics.Histogram("orders.value", o.amount,
        tags: new[] { ("type", o.kind) });
    // …
}
```

## Trace context (`self.TraceContext`)

`self.TraceContext` exposes the current W3C trace context. It is
backed by `System.Diagnostics.Activity`, the standard .NET
primitive that OpenTelemetry maps onto, so it composes with whatever
upstream tracing your host already does (ASP.NET Core auto-
populates `Activity.Current` for every request, for example).

```spek
on Place o =>
{
    if (self.TraceContext.IsActive)
    {
        self.Log.Log(StructuredLogLevel.Information,
            "order.received",
            properties: new[]
            {
                ("trace_id", (object?)self.TraceContext.TraceId),
                ("order_id", (object?)o.id),
            });
    }
    // …
}
```

### Cross-`Tell` propagation

Trace context follows messages across actor boundaries. When a
trace listener is active, `Tell` and `AskAsync` capture the
sender's `Activity` context alongside the message, and the
recipient's handler span starts as a child of it, so a request
that fans out across several actors renders as one stitched
trace. Without a listener attached, no context is captured and
the mailbox carries plain messages, so untraced systems pay
nothing for the capture machinery.

## Structured logging (`self.Log`)

`self.Log` exposes the registered `IStructuredLogger`. The
abstraction is intentionally tiny (`IsEnabled` and `Log`), because
real applications plug in an adapter for
`Microsoft.Extensions.Logging.ILogger<T>` or whatever else they
already use.

```spek
on Withdraw w =>
{
    if (w.amount > balance)
    {
        self.Log.Log(StructuredLogLevel.Warning,
            "withdraw.insufficient_funds",
            properties: new[]
            {
                ("requested", (object?)w.amount),
                ("available", (object?)balance),
            });
        return new InsufficientFunds(w.amount, balance);
    }
    balance -= w.amount;
    return new BalanceResponse(balance);
}
```
