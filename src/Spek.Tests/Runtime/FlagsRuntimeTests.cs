using Xunit;

namespace Spek.Tests.Runtime;

/// <summary>
/// Runtime semantics of the flags verbs and flags-aware conversion: the
/// superset/intersect/subset lattice, the vacuous-truth edge, and
/// TryTo's every-set-bit-defined rule for [Flags] enums.
/// </summary>
public sealed class FlagsRuntimeTests
{
    [System.Flags]
    private enum Perm { None = 0, Read = 1, Write = 2, Execute = 4 }

    private enum Plain { Zero = 0, One = 1 }

    [Fact]
    public void HasAnyFlags_IsTheIntersectionTest()
    {
        var p = Perm.Read | Perm.Write;
        Assert.True(p.HasAnyFlags(Perm.Write, Perm.Execute));
        Assert.False(p.HasAnyFlags(Perm.Execute));
    }

    [Fact]
    public void HasOnlyFlags_IsTheSubsetTest()
    {
        var p = Perm.Read | Perm.Write;
        Assert.True(p.HasOnlyFlags(Perm.Read, Perm.Write));
        Assert.True(p.HasOnlyFlags(Perm.Read, Perm.Write, Perm.Execute));
        Assert.False(p.HasOnlyFlags(Perm.Read));
    }

    [Fact]
    public void HasOnlyFlags_EmptyValue_IsVacuouslyTrue()
    {
        Assert.True(Perm.None.HasOnlyFlags(Perm.Read));
    }

    [Fact]
    public void TryTo_FlagsEnum_AcceptsCombinations_RejectsUndefinedBits()
    {
        Assert.Equal(Perm.Read | Perm.Write, Spek.EnumConversions.TryTo<Perm>(3));
        Assert.Equal(Perm.None, Spek.EnumConversions.TryTo<Perm>(0));
        Assert.Null(Spek.EnumConversions.TryTo<Perm>(8));    // undefined bit
        Assert.Null(Spek.EnumConversions.TryTo<Perm>(9));    // valid + undefined
    }

    [Fact]
    public void TryTo_PlainEnum_StaysDefinedMemberOnly()
    {
        Assert.Equal(Plain.One, Spek.EnumConversions.TryTo<Plain>(1));
        Assert.Null(Spek.EnumConversions.TryTo<Plain>(3));
    }
}
