using System.Security.Cryptography;
using System.Text;

namespace Spek.Cluster;

/// <summary>
/// Default placement strategy — uses **rendezvous hashing** (HRW) over
/// the live cluster members. Each <c>(actorType, actorKey)</c> hashes
/// against every candidate node; the node with the highest combined
/// hash wins.
///
/// <para>Why rendezvous, not classic consistent hashing on a virtual
/// ring? Two reasons:</para>
/// <list type="bullet">
///   <item>Determinism without external state. No virtual-node tables
///         to keep in sync across nodes — each node computes the same
///         winner from the same inputs.</item>
///   <item>Minimum disruption on membership change. When a node leaves
///         or joins, only the keys that actually owned that node have
///         to move. Strict <c>1/N</c> reshuffle, no virtual-node hot
///         spots.</item>
/// </list>
///
/// Both are properties production-grade systems care about (Riak,
/// Couchbase, Lyft's Envoy use HRW for the same reasons).
/// </summary>
public sealed class ConsistentHashPlacement : IPlacementStrategy
{
    public NodeIdentity? ResolveOwner(string actorType, string actorKey,
                                      IReadOnlyList<ClusterMember> members)
    {
        if (members.Count == 0) return null;

        // Compute (key, node) hash for each candidate; pick the max.
        // SHA-256 of UTF-8(actorType + "/" + actorKey + ":" + node-uuid).
        // Cryptographic strength is overkill for placement, but it's
        // available everywhere with zero allocations vs hand-rolling.
        ulong bestHash = 0;
        NodeIdentity? best = null;

        foreach (var member in members)
        {
            // Skip nodes that aren't fully online — placement should
            // never pin a key to a node that can't accept traffic.
            if (member.State != NodeState.Up) continue;

            var combined = $"{actorType}/{actorKey}:{member.Identity.Id:N}";
            var bytes    = Encoding.UTF8.GetBytes(combined);
            var hash     = SHA256.HashData(bytes);
            // Take the first 8 bytes as a ulong — 64 bits of entropy
            // is plenty for owner-selection.
            ulong h = BitConverter.ToUInt64(hash, 0);

            if (best is null || h > bestHash)
            {
                best = member.Identity;
                bestHash = h;
            }
        }

        return best;
    }
}
