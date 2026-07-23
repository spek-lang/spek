using System.Collections.Concurrent;
using System.Threading.Channels;
using Fleet.Contracts;

namespace Fleet.CSharp;

/// <summary>
/// The C# twin: the same fleet a competent C# developer would build today
/// without an actor runtime — a Channel per device, a consumer Task per
/// channel, and hand-rolled recovery. This implementation is written to be
/// defended: it catches per item (not per loop), so a firmware fault loses
/// only the poisoned reading, and it counts its rebuilds. Every line of
/// that care is demo content — the diff against Fleet.spek is the argument.
/// If you can make this twin better, improve it: the comparison only means
/// something if this side is as good as it can be.
/// </summary>
public sealed class ChannelFleet : IFleet
{
    private readonly ConcurrentDictionary<int, Channel<double>> _devices = new();
    private readonly List<Task> _workers = [];
    private readonly object _workersGate = new();
    private readonly CancellationTokenSource _shutdown = new();
    private long _processed;
    private long _rebuilds;
    private int _inFlight;

    public string Name => "C#";

    public void Ingest(int deviceId, double value)
    {
        var channel = _devices.GetOrAdd(deviceId, _ =>
        {
            // Unbounded on purpose: the actor mailbox on the other pane is
            // unbounded too. A bounded channel would need a full backpressure
            // policy decision here — dropped? blocking? — that the actor
            // runtime makes explicit elsewhere.
            var ch = Channel.CreateUnbounded<double>();
            lock (_workersGate) _workers.Add(RunDeviceAsync(ch.Reader));
            return ch;
        });

        if (!channel.Writer.TryWrite(value))
            throw new InvalidOperationException("unbounded channel refused a write");
    }

    private async Task RunDeviceAsync(ChannelReader<double> readings)
    {
        await foreach (var value in readings.ReadAllAsync(_shutdown.Token))
        {
            Interlocked.Increment(ref _inFlight);
            try
            {
                ProcessFirmware(value);
                Interlocked.Increment(ref _processed);
            }
            catch (InvalidOperationException)
            {
                // The "supervision policy", by hand: swallow the fault,
                // count the recovery, keep consuming. Getting this wrong —
                // catching around the loop instead of the item — silently
                // kills the device; nothing restarts a dead Task.
                Interlocked.Increment(ref _rebuilds);
            }
            finally
            {
                Interlocked.Decrement(ref _inFlight);
            }
        }
    }

    private static void ProcessFirmware(double value)
    {
        if (double.IsNaN(value))
            throw new InvalidOperationException("firmware fault: NaN reading");
    }

    public async Task<FleetStats> DrainAndReportAsync()
    {
        // Quiesce: all channels empty and nothing mid-item, twice in a row.
        while (true)
        {
            if (IsQuiet())
            {
                await Task.Delay(50);
                if (IsQuiet()) break;
            }
            await Task.Delay(10);
        }
        return new FleetStats(
            Interlocked.Read(ref _processed), Interlocked.Read(ref _rebuilds));

        bool IsQuiet() =>
            Volatile.Read(ref _inFlight) == 0
            && _devices.Values.All(c => c.Reader.Count == 0);
    }

    public ValueTask DisposeAsync()
    {
        _shutdown.Cancel();
        return ValueTask.CompletedTask;
    }
}
