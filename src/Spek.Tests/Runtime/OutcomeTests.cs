using Spek;
using Xunit;

namespace Spek.Tests.RuntimeTypes;

/// <summary>
/// Coverage for <see cref="Outcome{T,E}"/> and the
/// <see cref="Outcome{T}"/> shorthand — typed success/failure
/// payloads for actor reply messages.
/// </summary>
public class OutcomeTests
{
    public sealed record User(int Id, string Name);

    [Fact]
    public void Success_TryGetValue_ReturnsTheValue()
    {
        var ok = Outcome<User, Reason>.Success(new User(1, "alice"));
        Assert.True(ok.IsSuccess);
        Assert.False(ok.IsFailure);
        Assert.True(ok.TryGetValue(out var u));
        Assert.Equal("alice", u.Name);
        Assert.False(ok.TryGetReason(out _));
    }

    [Fact]
    public void Failure_TryGetReason_ReturnsTheReason()
    {
        var fail = Outcome<User, Reason>.Failure(new Reason("not-found", "user does not exist"));
        Assert.True(fail.IsFailure);
        Assert.False(fail.IsSuccess);
        Assert.True(fail.TryGetReason(out var r));
        Assert.Equal("not-found", r.Code);
        Assert.False(fail.TryGetValue(out _));
    }

    [Fact]
    public void Map_TransformsSuccessOnly()
    {
        Outcome<int, Reason> ok = Outcome<int, Reason>.Success(42);
        var doubled = ok.Map(n => n * 2);
        Assert.True(doubled.TryGetValue(out var v));
        Assert.Equal(84, v);

        Outcome<int, Reason> fail = Outcome<int, Reason>.Failure(new Reason("bad", "no"));
        var stillFail = fail.Map(n => n * 2);
        Assert.True(stillFail.IsFailure);
    }

    [Fact]
    public void MapFailure_TransformsReasonOnly()
    {
        var fail = Outcome<User, Reason>.Failure(new Reason("not-found", "missing"));
        var mapped = fail.MapFailure(r => $"{r.Code}: {r.Message}");
        Assert.True(mapped.TryGetReason(out var s));
        Assert.Equal("not-found: missing", s);

        var ok = Outcome<User, Reason>.Success(new User(1, "alice"));
        var stillOk = ok.MapFailure(r => $"{r.Code}: {r.Message}");
        Assert.True(stillOk.IsSuccess);
    }

    [Fact]
    public void PatternMatching_OnSuccessAndFailureCases()
    {
        Outcome<int, Reason> outcome = Outcome<int, Reason>.Success(7);
        var description = outcome switch
        {
            Outcome<int, Reason>.SuccessCase s => $"got {s.Value}",
            Outcome<int, Reason>.FailureCase f => $"failed: {f.Reason.Message}",
            _ => "?"
        };
        Assert.Equal("got 7", description);
    }

    [Fact]
    public void OutcomeOfT_ShorthandUsesReasonByDefault()
    {
        var fail = Outcome<User>.Failure("not-found", "user missing");
        Assert.True(fail.IsFailure);
        Assert.True(fail.TryGetReason(out var reason));
        Assert.Equal("not-found", reason.Code);
        Assert.Equal("user missing", reason.Message);

        var ok = Outcome<User>.Success(new User(1, "alice"));
        Assert.True(ok.IsSuccess);
        Assert.True(ok.TryGetValue(out var u));
        Assert.Equal("alice", u.Name);
    }

    [Fact]
    public void CustomFailureType_SupportsRichErrorEnums()
    {
        // Users with richer error needs use a custom E type.
        var notFound = Outcome<User, NotFoundError>.Failure(new NotFoundError(42));
        Assert.True(notFound.TryGetReason(out var err));
        Assert.Equal(42, err.RequestedId);
    }

    public sealed record NotFoundError(int RequestedId);
}
