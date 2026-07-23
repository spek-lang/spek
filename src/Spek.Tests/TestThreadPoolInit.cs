using System.Runtime.CompilerServices;

namespace Spek.Tests;

/// <summary>
/// Raises the thread-pool floor once for the whole test assembly.
///
/// The actor-runtime tests poll for asynchronous dispatch effects (message
/// delivery, dead-letter routing, passivation, shutdown). Each actor dispatch is
/// a <c>Task.Run</c>, and the polling loops <c>await Task.Delay</c> — both draw
/// from the same thread pool. Under xUnit's fully-parallel run the pool can be
/// saturated by dozens of concurrent test threads, starving those dispatches so
/// they don't execute for seconds, which surfaced as intermittent timeout flakes
/// (e.g. RuntimeCoverageTests' delivery/dead-letter polls and the passivation
/// tests). Raising the minimum worker/IO thread count keeps the pool from
/// throttling new threads, so actor work runs promptly even under load.
/// </summary>
internal static class TestThreadPoolInit
{
    [ModuleInitializer]
    public static void Init()
    {
        ThreadPool.GetMinThreads(out var worker, out var io);
        ThreadPool.SetMinThreads(Math.Max(worker, 64), Math.Max(io, 64));
    }
}
