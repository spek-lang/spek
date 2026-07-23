# Fleet — the IoT device fleet, twice

One small system implemented two ways, run under one load driver and one
fault schedule. The Spek pane is a device fleet whose recovery story is a
single `supervise` clause. The C# pane is the same fleet built the way a
careful C# developer would build it today — `System.Threading.Channels`, a
consumer `Task` per device, and hand-rolled recovery. Both panes ingest the
identical reading stream, including a firmware fault (a NaN reading that
crashes device processing) every N readings.

```bash
./demos/run.sh fleet
./demos/run.sh fleet --readings 500000 --devices 200 --poison-every 250
```

The harness prints a side-by-side table: readings sent, faults injected,
readings processed, work lost, recoveries, and throughput. Both columns are
computed by the same code path, so any asymmetry belongs to the systems
under test. Under steady-state measurement (`demos/benchmarks`,
BenchmarkDotNet) the two panes run at **CPU parity** — the supervision,
tracing, and introspection the Spek pane carries cost roughly nothing in
wall-clock terms; the twin's remaining edge is allocation, tracked
release over release. The demo exits non-zero if either pane loses work — it doubles
as an integration test of the public runtime surface.

What to read afterwards:

- [`Fleet.Spek/Fleet.spek`](Fleet.Spek/Fleet.spek) — the whole fleet:
  messages, a collector, a device, a hub, and the one `supervise` clause
  that is the entire recovery story.
- [`Fleet.CSharp/ChannelFleet.cs`](Fleet.CSharp/ChannelFleet.cs) — the twin,
  written to be defended. Its recovery is a per-item try/catch whose
  placement is load-bearing: catch around the loop instead and a fault
  silently kills the device, because nothing restarts a dead `Task`.
- [`Fleet.Spek/SpekFleet.cs`](Fleet.Spek/SpekFleet.cs) — the host adapter.
  Note where the recovery count comes from: `ActorSystem.SnapshotActors()`,
  the same introspection surface `spekc observe` renders. The fleet itself
  carries no instrumentation.

The twin-fairness rule is standing policy: the C# side is reviewed as
production code and open to improvement. "Your C# is a strawman" always has
"then improve it" as the answer.

While a run is draining, `spekc observe <pid>` attaches to the harness
process and shows the Spek pane's live actor table — device mailboxes,
last messages, restart counts — with no instrumentation in the demo code.
