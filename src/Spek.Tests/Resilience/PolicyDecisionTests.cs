using Spek.Resilience;
using Xunit;

namespace Spek.Tests.Resilience;

public sealed class PolicyDecisionTests
{
    [Fact]
    public void Allow_factory_sets_kind_only()
    {
        var d = PolicyDecision.Allow();
        Assert.Equal(PolicyDecisionKind.Allow, d.Kind);
        Assert.Null(d.Reason);
        Assert.Null(d.RetryAfter);
    }

    [Fact]
    public void Reject_factory_sets_reason()
    {
        var d = PolicyDecision.Reject("forbidden");
        Assert.Equal(PolicyDecisionKind.Reject, d.Kind);
        Assert.Equal("forbidden", d.Reason);
        Assert.Null(d.RetryAfter);
    }

    [Fact]
    public void Defer_factory_sets_reason_and_retry_after()
    {
        var delay = TimeSpan.FromMilliseconds(250);
        var d = PolicyDecision.Defer(delay, "throttled");
        Assert.Equal(PolicyDecisionKind.Defer, d.Kind);
        Assert.Equal("throttled", d.Reason);
        Assert.Equal(delay, d.RetryAfter);
    }
}
