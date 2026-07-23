using System.Runtime.CompilerServices;

namespace Spek.Tests;

/// <summary>
/// Pre-provisions thread-pool threads for the whole test assembly.
///
/// The runtime tests each stand up an <c>ActorSystem</c> that drives its
/// mailboxes on the shared thread pool. The long idle-waits are async
/// (<c>TestActorSystem.WhenIdleAsync</c> polls without parking a thread), but
/// some short blocking remains — <c>TestProbe.ExpectMsg</c> takes from a
/// blocking collection. The pool injects new threads only ~1–2 per second, so
/// under fully-parallel test execution those brief holds plus mailbox demand
/// can outrun thread injection. Raising the floor removes that lag, so the
/// suite stays fully parallel (no serialized collections) and deterministic.
///
/// This is a test-harness concern (one process, dozens of concurrent actor
/// systems), not a runtime defect — a real Spek app runs a single system — and
/// it does not disable parallelism; it just provisions threads.
/// </summary>
internal static class TestThreadPoolConfig
{
    [ModuleInitializer]
    internal static void Init()
    {
        ThreadPool.GetMinThreads(out _, out var minIo);
        ThreadPool.SetMinThreads(Math.Max(Environment.ProcessorCount * 4, 32), minIo);
    }
}
