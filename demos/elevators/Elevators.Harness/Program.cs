using Elevators;
using Spek.Runtime;

// The elevator-bank sizzle: sixty seconds of a building at work, one car
// killed mid-trip, its stranded stops redistributed while you watch.
// The harness is only the clock, the passengers, and the camera — every
// decision (routing, healing, redistribution) is in Elevators.spek.

var floors = ArgInt("--floors", 20);
var carCount = ArgInt("--cars", 6);
var ticks = ArgInt("--ticks", 400);
var tickMs = ArgInt("--tick-ms", 150);
var fast = args.Contains("--fast");
if (fast) tickMs = 5;   // headless pace: quick, but delivery still outruns the clock

var quietSink = new RecordingDeadLetterSink();
using var system = new ActorSystem("elevators", deadLetterSink: quietSink);
var dispatcher = system.Spawn<Dispatcher>(carCount);

var rng = new Random(2026);
var calls = 0;
var events = new Queue<string>();
var serviceState = new Dictionary<int, bool>();
long lastRedist = 0;
void Announce(string line)
{
    events.Enqueue(line);
    while (events.Count > 6) events.Dequeue();
}

// The script: steady hall calls; car 3 dies a third of the way in,
// car 5 two thirds of the way in. Same show every run (seeded).
var killAt = new Dictionary<int, int> { [ticks / 3] = 3, [2 * ticks / 3] = 5 };

if (!fast) Console.Write("\x1b[2J");   // clear once; repaint per tick
for (var t = 0; t < ticks; t++)
{
    if (rng.NextDouble() < 0.35)
    {
        var floor = rng.Next(1, floors + 1);
        dispatcher.Tell(new HallCall(floor));
        calls++;
    }

    if (killAt.TryGetValue(t, out var victim))
    {
        // Rush hour first, so the kill lands on a busy car — the whole
        // point is watching its queue survive it.
        for (var burst = 0; burst < 18; burst++)
        {
            dispatcher.Tell(new HallCall(rng.Next(1, floors + 1)));
            calls++;
        }
        dispatcher.Tell(new Malfunction(victim));
        Announce($"t={t}: 💥 car {victim} controller fault — supervision restarting it");
    }

    dispatcher.Tell(new Tick());
    await Task.Delay(tickMs);

    if (!fast)
    {
        var bank = await dispatcher.AskAsync<BankState>(new GetBank(), TimeSpan.FromSeconds(5));
        Render(bank.encoded, t);
    }
}

// Stop new work; let the bank finish serving what it holds.
var lastEncoded = "";
for (var settle = 0; settle < 600; settle++)
{
    dispatcher.Tell(new Tick());
    await Task.Delay(Math.Max(5, tickMs / 3));
    var bank = await dispatcher.AskAsync<BankState>(new GetBank(), TimeSpan.FromSeconds(5));
    if (!fast) Render(bank.encoded, ticks + settle);
    lastEncoded = bank.encoded;
    var (served, redist, cars) = Parse(bank.encoded);
    if (cars.All(c => c.Queued == 0 && c.InService))   // drained AND fully recovered
    {
        Summary(served, redist);
        return served + 0 == calls ? 0 : 1;   // every call served, none lost
    }
}

Console.Error.WriteLine($"bank failed to drain: {lastEncoded}");
return 1;

void Summary(long served, long redist)
{
    var restarts = system.SnapshotActors().Sum(s => s.Restarts);
    Console.WriteLine();
    Console.WriteLine($"hall calls requested   {calls,6:N0}");
    Console.WriteLine($"stops served           {served,6:N0}");
    Console.WriteLine($"calls lost             {calls - served,6:N0}   (should be 0)");
    Console.WriteLine($"controller faults      {killAt.Count,6:N0}");
    Console.WriteLine($"supervision restarts   {restarts,6:N0}");
    Console.WriteLine($"stops redistributed    {redist,6:N0}");
    Console.WriteLine();
    Console.WriteLine("The healing you watched is one supervise clause plus a heartbeat");
    Console.WriteLine("(Elevators.Spek/Elevators.spek). There is no try/catch in the building.");
}

void Render(string encoded, int t)
{
    var (served, redist, cars) = Parse(encoded);
    TrackTransitions(cars, redist, t);
    Console.Write("\x1b[H");
    // Every repainted line ends with ESC[K (erase to end of line): frames
    // are drawn in place, and a shorter line must not leave the tail of a
    // longer previous one behind.
    Console.WriteLine($"elevator bank — tick {t}   calls {calls}   served {served}   redistributed {redist}\x1b[K\n\x1b[K");
    foreach (var car in cars)
    {
        var track = new char[floors];
        Array.Fill(track, '·');
        track[Math.Clamp(car.Floor - 1, 0, floors - 1)] = car.InService ? '█' : 'X';
        if (car.InService)
        {
            var stops = car.Stops.Length > 0 ? $"  → {string.Join(",", car.Stops)}" : "";
            Console.WriteLine($"  car {car.Id}  |{new string(track)}|  floor {car.Floor,2}{stops}\x1b[K");
        }
        else
        {
            // The failure, visible: red line, express descent, no stops.
            Console.WriteLine($"\x1b[31m  car {car.Id}  |{new string(track)}|  floor {car.Floor,2}" +
                              $"  OUT OF SERVICE — returning to lobby\x1b[0m\x1b[K");
        }
    }
    Console.WriteLine("\x1b[K");
    foreach (var line in events) Console.WriteLine($"  {line}\x1b[K");
    for (var i = events.Count; i < 6; i++) Console.WriteLine("\x1b[K");
}

void TrackTransitions(List<(int Id, int Floor, int Queued, string[] Stops, bool InService)> cars, long redist, int t)
{
    foreach (var car in cars)
    {
        var was = serviceState.TryGetValue(car.Id, out var w) ? w : true;
        if (was && !car.InService)
            Announce($"t={t}: 🛗 car {car.Id} taken out of service at floor {car.Floor} — running express to the lobby");
        if (!was && car.InService)
            Announce($"t={t}: ✅ car {car.Id} back in service");
        serviceState[car.Id] = car.InService;
    }
    if (redist > lastRedist)
        Announce($"t={t}: ↪ {redist - lastRedist} stranded stop(s) redistributed to healthy cars");
    lastRedist = redist;
}

static (long Served, long Redist, List<(int Id, int Floor, int Queued, string[] Stops, bool InService)> Cars) Parse(string encoded)
{
    var parts = encoded.Split('|');
    var head = parts[0].Split(';');
    var served = long.Parse(head[0].Split('=')[1]);
    var redist = long.Parse(head[1].Split('=')[1]);
    var cars = new List<(int, int, int, string[], bool)>();
    foreach (var p in parts.Skip(1))
    {
        var f = p.Split(':');
        var stops = f[3].Length == 0 ? [] : f[3].Split('+');
        cars.Add((int.Parse(f[0]), int.Parse(f[1]), int.Parse(f[2]), stops, f[4] == "1"));
    }
    return (served, redist, cars);
}

int ArgInt(string name, int fallback)
{
    var i = Array.IndexOf(args, name);
    return i >= 0 && i + 1 < args.Length && int.TryParse(args[i + 1], out var v) ? v : fallback;
}
