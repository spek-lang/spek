using SpekClusterNs = Spek.Cluster;
using Xunit;

namespace Spek.Tests.ClusterIntegration.LocatedActors;

/// <summary>
/// The rendezvous-hash "minimum disruption on membership change" invariant — the
/// whole justification for <see cref="SpekClusterNs.ConsistentHashPlacement"/>.
/// Existing tests cover determinism, dispersion, and Down-skipping but never the
/// *stability* property: a modulo-hash regression would pass those yet reshuffle
/// every key. These tests pin that only the minimal set of keys moves.
/// </summary>
public class PlacementStabilityTests
{
    private static SpekClusterNs.NodeIdentity Node(int i) =>
        new(Guid.Parse($"00000000-0000-0000-0000-{i:D12}"), $"n{i}");

    private static List<SpekClusterNs.ClusterMember> Members(int count) =>
        Enumerable.Range(1, count)
            .Select(i => new SpekClusterNs.ClusterMember(Node(i), SpekClusterNs.NodeState.Up))
            .ToList();

    private static Dictionary<string, string> MapKeys(
        SpekClusterNs.ConsistentHashPlacement p,
        IReadOnlyList<SpekClusterNs.ClusterMember> members,
        int keyCount)
    {
        var map = new Dictionary<string, string>(keyCount);
        for (int k = 0; k < keyCount; k++)
            // ResolveOwner never returns null for a non-empty member list,
            // and every member here is built by Node(i) with an "n{i}" label.
            map[$"key-{k}"] = p.ResolveOwner("Actor", $"key-{k}", members)!.Label!;
        return map;
    }

    [Fact]
    public void RemovingANode_MovesOnlyItsKeys_LeavesAllOthersFixed()
    {
        var p = new SpekClusterNs.ConsistentHashPlacement();
        const int keys = 2000;

        var four  = Members(4);
        var three = four.Where(m => m.Identity.Label != "n4").ToList();   // drop n4

        var before = MapKeys(p, four, keys);
        var after  = MapKeys(p, three, keys);

        int moved = 0;
        foreach (var (key, ownerBefore) in before)
        {
            if (ownerBefore == "n4")
            {
                moved++;                                  // its keys must move somewhere live
                Assert.NotEqual("n4", after[key]);
            }
            else
            {
                // The invariant: keys that didn't own the departed node DO NOT MOVE.
                Assert.Equal(ownerBefore, after[key]);
            }
        }

        // Sanity: n4 owned roughly a quarter of the keys (dispersion), so a
        // meaningful number moved — not zero (which would mean n4 owned nothing).
        Assert.InRange(moved, keys / 8, keys / 2);
    }

    [Fact]
    public void AddingANode_OnlyMigratesKeysToTheNewNode_NeverReshufflesBetweenExisting()
    {
        var p = new SpekClusterNs.ConsistentHashPlacement();
        const int keys = 2000;

        var four = Members(4);
        var five = Members(5);

        var before = MapKeys(p, four, keys);
        var after  = MapKeys(p, five, keys);

        int migrated = 0;
        foreach (var (key, ownerBefore) in before)
        {
            if (after[key] != ownerBefore)
            {
                migrated++;
                // A moved key may ONLY have moved to the newly-added node — never
                // reshuffled between two pre-existing nodes.
                Assert.Equal("n5", after[key]);
            }
        }

        // ~1/5 of keys should migrate to the new node — bounded, not a reshuffle.
        Assert.InRange(migrated, keys / 12, keys / 2);
    }
}
