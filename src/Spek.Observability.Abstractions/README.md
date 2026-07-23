# Spek.Observability.Abstractions

Telemetry contract for Spek runtimes. Three small interfaces:

- `IMetricSink` — counters, gauges, histograms.
- `ITraceContext` — W3C trace ID / span ID / baggage, propagated
  through every Tell.
- `IStructuredLogger` — structured event sink for lifecycle and
  user-emitted log records.

Plus the well-known metric names the runtime emits
(`SpekMetricNames`), which are stable across releases for
dashboard authoring.

Reference this package when authoring an observability adapter.
For the default OpenTelemetry implementation, reference
`Spek.Observability.OpenTelemetry`.

## What's NOT here

- An actual telemetry implementation. The default sink is
  `NullMetricSink.Instance`, and every call inlines away.
- Any dependency on OpenTelemetry, Microsoft.Extensions.Logging,
  Prometheus, or anything else. The abstractions stay
  vendor-neutral on purpose.
