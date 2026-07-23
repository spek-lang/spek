using System.Collections.Immutable;

namespace Spek;

/// <summary>
/// Multi-node logical clock — for distributed actor systems that
/// need to detect concurrency between events, not just total-order
/// them. Each node maintains its own counter; the clock as a whole
/// is the per-node counter map.
///
/// <para>The relationships:</para>
/// <list type="bullet">
///   <item><b>A happens-before B</b> — every counter in A is &lt;= the
///         corresponding counter in B, and at least one is strictly
///         less.</item>
///   <item><b>A and B are concurrent</b> — neither happens-before the
///         other (some counters higher in A, some higher in B). This
///         is the case Lamport clocks can't represent.</item>
///   <item><b>A and B are equal</b> — every counter matches.</item>
/// </list>
///
/// <para>Pairs naturally with the cluster's
/// <see cref="Spek.Cluster.NodeIdentity"/> for the per-node key.
/// (We use <see cref="Guid"/> as the key directly here so this type
/// stays in <see cref="Spek.Runtime"/> without depending on
/// <see cref="Spek.Cluster"/>.)</para>
///
/// <para>Immutable — every operation returns a new clock. Wire-safe
/// (passes CE0010 as an immutable record).</para>
/// </summary>
public sealed record VectorClock(ImmutableDictionary<Guid, long> Entries)
{
    /// <summary>The empty clock — no node has recorded any event.</summary>
    public static readonly VectorClock Empty =
        new(ImmutableDictionary<Guid, long>.Empty);

    /// <summary>
    /// Tick the local node's counter by one. Call before recording
    /// any local event or before sending a message.
    /// </summary>
    public VectorClock Tick(Guid localNodeId)
    {
        var current = Entries.GetValueOrDefault(localNodeId, 0L);
        return new(Entries.SetItem(localNodeId, current + 1));
    }

    /// <summary>
    /// Receive an event with timestamp <paramref name="received"/>;
    /// returns a new clock that is the per-node max of both, then
    /// ticks the local node's counter. Call after receiving a
    /// message before processing it.
    /// </summary>
    public VectorClock Receive(VectorClock received, Guid localNodeId)
    {
        var merged = Merge(received);
        return merged.Tick(localNodeId);
    }

    /// <summary>
    /// Per-node max of two clocks — the join in the lattice. Used
    /// by <see cref="Receive"/> and useful on its own when merging
    /// CRDTs.
    /// </summary>
    public VectorClock Merge(VectorClock other)
    {
        var result = Entries;
        foreach (var (node, otherCount) in other.Entries)
        {
            var thisCount = result.GetValueOrDefault(node, 0L);
            if (otherCount > thisCount)
                result = result.SetItem(node, otherCount);
        }
        return new VectorClock(result);
    }

    /// <summary>
    /// True if <c>this</c> causally precedes <paramref name="other"/>
    /// — every per-node counter in <c>this</c> is &lt;= the
    /// corresponding counter in <paramref name="other"/>, with at
    /// least one strictly less.
    /// </summary>
    public bool HappensBefore(VectorClock other)
    {
        bool atLeastOneStrictlyLess = false;
        var allNodes = Entries.Keys.Union(other.Entries.Keys);
        foreach (var node in allNodes)
        {
            var a = Entries.GetValueOrDefault(node, 0L);
            var b = other.Entries.GetValueOrDefault(node, 0L);
            if (a > b) return false;
            if (a < b) atLeastOneStrictlyLess = true;
        }
        return atLeastOneStrictlyLess;
    }

    /// <summary>
    /// True if <c>this</c> and <paramref name="other"/> are
    /// concurrent — neither happens-before the other. Useful for
    /// CRDT merge logic and conflict detection.
    /// </summary>
    public bool IsConcurrentWith(VectorClock other) =>
        !HappensBefore(other) && !other.HappensBefore(this) && !Equals(other);

    /// <summary>
    /// Value equality — two clocks are equal when every per-node counter
    /// matches (absent entries count as zero). Overrides the record's default
    /// to compare the dictionary contents rather than its reference.
    /// </summary>
    public bool Equals(VectorClock? other)
    {
        if (other is null) return false;
        if (Entries.Count != other.Entries.Count) return false;
        foreach (var (node, count) in Entries)
            if (other.Entries.GetValueOrDefault(node, 0L) != count) return false;
        return true;
    }

    /// <summary>Order-independent hash over the per-node counters, consistent with <see cref="Equals(VectorClock?)"/>.</summary>
    public override int GetHashCode()
    {
        var hash = 0;
        foreach (var (node, count) in Entries)
            hash ^= HashCode.Combine(node, count);
        return hash;
    }

    /// <summary>Renders the clock as <c>V:[node:counter, …]</c> for logs and diagnostics.</summary>
    public override string ToString()
    {
        if (Entries.Count == 0) return "V:[]";
        var parts = Entries.Select(kv => $"{kv.Key:N}:{kv.Value}");
        return $"V:[{string.Join(", ", parts)}]";
    }
}
