using Spek.Runtime;

namespace Spek.Testing;

/// <summary>
/// Test-only reach-through to the slow-handler watchdog's sweep cadence.
/// The period is an internal runtime constant (production systems have no
/// reason to tune it); tests shorten it so wedge detection completes in
/// milliseconds instead of seconds. Internal on purpose — visible to
/// Spek.Tests via InternalsVisibleTo, absent from the public test-kit
/// surface.
/// </summary>
internal static class WatchdogTestHooks
{
    /// <summary>Sets the watchdog sweep period for <paramref name="system"/>.
    /// Call before the first spawn: the watchdog reads the period once, when
    /// the first tracked slot creates it.</summary>
    public static void SetSlowHandlerSweepPeriod(ActorSystem system, TimeSpan period)
        => system.SlowHandlerSweepPeriodMs = (long)period.TotalMilliseconds;
}
