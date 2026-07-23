# Elevators — the sixty-second sizzle

Six elevator cars, one dispatcher, a building's worth of hall calls — and
twice during the show, a car's controller crashes mid-trip. Supervision
restarts the controller with clean state; the dispatcher notices the
amnesia, redistributes the car's stranded stops to healthy cars, and hands
the reborn controller its last known position. On screen that is the whole
drama: the car turns red, reads **OUT OF SERVICE — returning to lobby**,
runs express down the shaft floor by floor taking no calls, and rejoins
the rotation when the doors open at floor 1. Every hall call gets served.
There is no try/catch anywhere in the building.

```bash
./demos/run.sh elevators                  # the show: ~60 seconds, animated
./demos/run.sh elevators --fast           # headless: same script, CI pace
./demos/run.sh elevators --cars 8 --floors 30 --tick-ms 100
```

The final tally is the proof: `calls lost: 0` with faults injected and
stops redistributed, and `stops served` exactly equals calls requested —
each redistributed stop was served exactly once, by whichever car ended up
with it. The demo exits non-zero if any call is lost, so it doubles as an
integration test.

This is the small-scale companion to [the fleet demo](../fleet/): the fleet
carries the scale claim and the C# comparison; this piece carries the
watch-it-heal moment at a size anyone can read. Everything that matters is
in [`Elevators.Spek/Elevators.spek`](Elevators.Spek/Elevators.spek):

- The recovery is the `supervise` clause on the dispatcher — a crashing car
  restarts with clean state, and its mailbox (with any pending work)
  survives the restart.
- The redistribution is a heartbeat plus reconciliation: cars answer every
  `Tick` with a `Status` reply, and a car reporting an empty queue while
  the dispatcher still has stops on its books has lost its memory. Its
  stops go back to the pool, and a `ResetTo` hands the reborn controller
  the position the dispatcher last saw it at — which is what makes the
  out-of-service descent physically honest: the controller forgot; the
  building didn't.
- The harness ([`Program.cs`](Elevators.Harness/Program.cs)) is only the
  clock, the passengers, and the camera. Routing, healing, and
  redistribution decisions all live in the `.spek` file.
