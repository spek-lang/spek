using System.Diagnostics;
using Spek;
using Spek.Runtime;
using Throughput;

// The throughput harness: no story, no scenery, just load. Three shapes:
//
//   pingpong  P Paddle pairs volley a Fire countdown (default; sweeps pair
//             counts up to the core count unless --pairs pins one)
//   tell      A parallel host tasks fire-and-forget M Hits at their own Counter
//   ask       A parallel host tasks await M sequential GetCount round trips
//
// After every measured pass the books are settled: each actor is asked for
// its counter, and the total must equal the messages dispatched — exactly.
// Any discrepancy exits non-zero. Host-side Task.Run is the load driver,
// same as the fleet harness; the system under test is the actor runtime.

var mode = Arg("--mode", "pingpong");
var rounds = ArgInt("--rounds", 250_000);
var messages = ArgInt("--messages", 500_000);
var actors = ArgInt("--actors", Environment.ProcessorCount);
var pinnedPairs = ArgInt("--pairs", 0);   // 0 = sweep 1, 2, 4, … up to core count

Console.WriteLine($"throughput demo — mode {mode}, {Environment.ProcessorCount} logical processors");
Console.WriteLine("a warmup pass runs first and is excluded (JIT + tiered compilation)\n");

const string header = "{0,7}{1,14}{2,10}{3,15}{4,10}";

if (mode == "pingpong")
{
    await PingPong(Math.Min(2, Environment.ProcessorCount), Math.Min(rounds, 50_000));   // warmup

    var sweep = new List<int>();
    if (pinnedPairs > 0) sweep.Add(pinnedPairs);
    else
    {
        for (var p = 1; p < Environment.ProcessorCount; p *= 2) sweep.Add(p);
        sweep.Add(Environment.ProcessorCount);
    }

    Console.WriteLine($"pingpong — {rounds:N0} rounds per volley, {2L * rounds:N0} dispatches per pair");
    Console.WriteLine(string.Format(header, "pairs", "msgs", "elapsed", "msgs/sec", "scaling"));
    double baseline = 0;
    foreach (var pairs in sweep)
    {
        var elapsed = await PingPong(pairs, rounds);
        Report(pairs, 2L * rounds * pairs, elapsed, ref baseline);
    }
    Done();
    return 0;
}

if (mode == "tell")
{
    await TellStorm(Math.Min(2, actors), Math.Min(messages, 50_000));   // warmup

    Console.WriteLine($"tell — {actors} host tasks, {messages:N0} fire-and-forget Hits each, one Counter apiece");
    Console.WriteLine(string.Format(header, "actors", "msgs", "elapsed", "msgs/sec", "scaling"));
    double baseline = 0;
    Report(actors, (long)actors * messages, await TellStorm(actors, messages), ref baseline);
    Done();
    return 0;
}

if (mode == "ask")
{
    await AskLoop(Math.Min(2, actors), Math.Min(messages, 20_000));   // warmup

    Console.WriteLine($"ask — {actors} host tasks, {messages:N0} sequential GetCount round trips each");
    Console.WriteLine(string.Format(header, "actors", "asks", "elapsed", "asks/sec", "scaling"));
    double baseline = 0;
    Report(actors, (long)actors * messages, await AskLoop(actors, messages), ref baseline);
    Done();
    return 0;
}

Console.Error.WriteLine($"unknown mode '{mode}' — use --mode pingpong|tell|ask");
return 2;

// ─── pingpong: P pairs volley; total dispatches = P × 2R ────────────────────

async Task<TimeSpan> PingPong(int pairs, int r)
{
    using var system = new ActorSystem("throughput-pingpong");
    var paddles = new ActorRef[pairs * 2];
    for (var i = 0; i < paddles.Length; i++) paddles[i] = system.Spawn<Paddle>();

    var sw = Stopwatch.StartNew();
    for (var p = 0; p < pairs; p++)
    {
        // Serve with the peer as sender: the reply routes back to it, and
        // the pair rallies unaided until the countdown reaches zero.
        paddles[2 * p + 1].Tell(new Fire(2 * r - 1), paddles[2 * p]);
    }
    if (!system.AwaitTermination(TimeSpan.FromMinutes(5)))
        Fail($"pingpong pairs={pairs}: volleys failed to drain");
    sw.Stop();

    // Conservation: 2R dispatches per pair, every one counted exactly once.
    long counted = 0;
    foreach (var paddle in paddles)
        counted += (await paddle.AskAsync<Count>(new GetCount(), TimeSpan.FromSeconds(30))).n;
    var expected = 2L * r * pairs;
    if (counted != expected)
        Fail($"pingpong pairs={pairs}: dispatched {expected:N0}, counted {counted:N0}");

    return sw.Elapsed;
}

// ─── tell: A host tasks each fire M Hits at a private Counter ───────────────

async Task<TimeSpan> TellStorm(int senders, int m)
{
    using var system = new ActorSystem("throughput-tell");
    var counters = new ActorRef[senders];
    for (var i = 0; i < senders; i++) counters[i] = system.Spawn<Counter>();

    var sw = Stopwatch.StartNew();
    var tasks = new Task[senders];
    for (var i = 0; i < senders; i++)
    {
        var target = counters[i];
        tasks[i] = Task.Run(() =>
        {
            for (var k = 0; k < m; k++) target.Tell(new Hit());
        });
    }
    await Task.WhenAll(tasks);
    if (!system.AwaitTermination(TimeSpan.FromMinutes(5)))
        Fail("tell: mailboxes failed to drain");
    sw.Stop();

    // Conservation: each counter belongs to one sender, so each must hold
    // exactly M — no message lost, none double-counted, none misrouted.
    for (var i = 0; i < senders; i++)
    {
        var n = (await counters[i].AskAsync<Count>(new GetCount(), TimeSpan.FromSeconds(30))).n;
        if (n != m) Fail($"tell: counter {i} expected {m:N0} hits, counted {n:N0}");
    }

    return sw.Elapsed;
}

// ─── ask: A host tasks each await M sequential request/reply round trips ────

async Task<TimeSpan> AskLoop(int askers, int m)
{
    using var system = new ActorSystem("throughput-ask");
    var counters = new ActorRef[askers];
    for (var i = 0; i < askers; i++) counters[i] = system.Spawn<Counter>();

    var sw = Stopwatch.StartNew();
    var tasks = new Task<long>[askers];
    for (var i = 0; i < askers; i++)
    {
        var target = counters[i];
        tasks[i] = Task.Run(async () =>
        {
            long replies = 0;
            for (var k = 0; k < m; k++)
            {
                var count = await target.AskAsync<Count>(new GetCount(), TimeSpan.FromSeconds(30));
                // No Hits are sent in this mode, so a well-formed reply
                // carries zero; counting only those checks payload too.
                if (count.n == 0) replies = replies + 1;
            }
            return replies;
        });
    }
    var perTask = await Task.WhenAll(tasks);
    sw.Stop();

    // Conservation: every ask answered, every answer well-formed.
    var replies = perTask.Sum();
    var expected = (long)askers * m;
    if (replies != expected)
        Fail($"ask: sent {expected:N0} asks, received {replies:N0} well-formed replies");

    return sw.Elapsed;
}

// ─── reporting ───────────────────────────────────────────────────────────────

void Report(int lanes, long msgs, TimeSpan elapsed, ref double baseline)
{
    var rate = msgs / Math.Max(0.000001, elapsed.TotalSeconds);
    if (baseline == 0) baseline = rate;
    Console.WriteLine($"{lanes,7}{msgs,14:N0}{elapsed.TotalSeconds,9:0.00}s{rate,15:N0}{rate / baseline,9:0.00}×");
}

void Done() =>
    Console.WriteLine("\nconservation: every message dispatched exactly once, verified after every pass");

void Fail(string message)
{
    Console.Error.WriteLine($"throughput demo FAILED — {message}");
    Environment.Exit(1);
}

string Arg(string name, string fallback)
{
    var i = Array.IndexOf(args, name);
    return i >= 0 && i + 1 < args.Length ? args[i + 1] : fallback;
}

int ArgInt(string name, int fallback)
{
    var i = Array.IndexOf(args, name);
    return i >= 0 && i + 1 < args.Length && int.TryParse(args[i + 1], out var v) ? v : fallback;
}
