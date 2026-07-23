using Spek.Testing;
using Xunit;

namespace Spek.Tests.Runtime;

/// <summary>
/// Sibling restarts under AllForOne are observable: RestartCount counts them
/// (the "how many times did it restart?" question) while the restart budget
/// remains own-failure-only (the "may it restart again?" question). Before
/// this split, a sibling restarted via <c>RestartSiblingsOf</c> re-materialized
/// with fresh state but RestartCount stayed 0 — invisible to tests and
/// under-reported in production metrics.
/// </summary>
public sealed class SiblingRestartVisibilityTests
{
    public sealed record Crash();
    public sealed record Ping();

    private sealed class Child : Spek.ActorBase
    {
        protected override Task DispatchAsync(object message, Spek.ActorRef sender)
        {
            if (message is Crash) throw new InvalidOperationException("boom");
            return Task.CompletedTask;
        }
    }

    private sealed class AllForOneParent : Spek.ActorBase
    {
        internal static Spek.ActorRef A = null!;
        internal static Spek.ActorRef B = null!;

        protected override void OnPreStart()
        {
            A = SpawnChildAsync<Child>();
            B = SpawnChildAsync<Child>();
        }

        protected override Spek.FailureDirective OnChildFailure(
            Spek.ActorRef child, Exception cause, object message)
        {
            RestartSiblingsOf(child);
            return Spek.FailureDirective.Restart;
        }

        protected override Task DispatchAsync(object message, Spek.ActorRef sender)
            => Task.CompletedTask;
    }

    [Fact]
    public async Task SiblingRestart_IsVisibleInRestartCountAsync()
    {
        using var sys = new TestActorSystem();
        var parent = sys.Spawn<AllForOneParent>();
        var a = AllForOneParent.A;
        var b = AllForOneParent.B;

        a.Tell(new Crash());
        await TestActorSystem.WaitUntilAsync(
            () => sys.RestartCountOf(b) >= 1,
            TimeSpan.FromSeconds(10),
            "sibling restart to be observable");

        // The crasher restarted too, via the normal budget-consuming path.
        Assert.True(sys.RestartCountOf(a) >= 1);
    }
}
