using Spek;
using Spek.Runtime;

namespace Readers;

/// <summary>Host adapter for the reader pane: wires the Board actor
/// (Readers.spek) to the benchmark driver.</summary>
public sealed class SpekBoard : IAsyncDisposable
{
    private readonly ActorSystem _system = new("readers-spek");
    private readonly ActorRef _board;

    public SpekBoard() => _board = _system.Spawn<Board>();

    public void Post(int key, int value) => _board.Tell(new Post(key, value));

    public async Task<int> LookupAsync(int key)
        => (await _board.AskAsync<LookupReply>(new Lookup(key))).value;

    public async Task DrainAsync()
    {
        while (!_system.IsIdle) await Task.Delay(10);
    }

    public ValueTask DisposeAsync()
    {
        _system.Dispose();
        return ValueTask.CompletedTask;
    }
}
