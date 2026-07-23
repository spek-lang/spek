# Spek.Observability.OpenTelemetry

OpenTelemetry-compatible adapter for Spek's observability surface.

```csharp
var system = new ActorSystem("orders")
    .UseOpenTelemetryMetrics()                   // metric sink
    .UseLoggerFactory(loggerFactory);            // structured logger

// In your OTel SDK setup:
Sdk.CreateMeterProviderBuilder()
   .AddMeter("Spek.Runtime")                    // runtime metrics
   .AddPrometheusExporter()
   .Build();

Sdk.CreateTracerProviderBuilder()
   .AddSource("Spek.Runtime")                   // dispatch spans
   .AddOtlpExporter()
   .Build();
```

That's it. Once registered, the runtime emits:

- `spek.mailbox.depth` — gauge of pending messages per actor.
- `spek.mailbox.dispatch` — counter of handled messages.
- `spek.actor.handler.duration.ms` — histogram of handler latency.
- `spek.actor.restart` — counter of supervision-driven restarts, tagged with actor type and exception.
- `spek.actor.stop` — counter of voluntary or supervised stops, tagged with cause.
- `spek.region.lock.wait.ms` — histogram of writer-lock wait time per shared region.

Plus a `spek.actor.{TypeName}.handler` Activity (trace span) for every dispatched message, automatically parented to the sender's `Activity.Current` so distributed traces stitch together across `Tell` boundaries.

## What this package contains

- `MeterMetricSink` — wires `IMetricSink` to `System.Diagnostics.Metrics.Meter`.
- `MicrosoftExtensionsLoggerStructuredLogger` — wires `IStructuredLogger` to `Microsoft.Extensions.Logging.ILogger`.
- `SpekOpenTelemetryExtensions` — `UseOpenTelemetryMetrics()` and `UseLoggerFactory()` extension methods on `ActorSystem`.

The package itself doesn't take a dependency on the OpenTelemetry SDK. It uses only the BCL primitives (`Meter`, `Activity`, `ILogger`) that the OTel SDK consumes. You wire OTel exporters in your host program; this package is the bridge from Spek's pluggable surface to those primitives.
