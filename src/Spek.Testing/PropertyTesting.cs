using System.Text;

namespace Spek.Testing;

/// <summary>
/// The recorded choice stream behind property-based testing (D3): every
/// nondeterministic decision — generated data AND the simulator's schedule
/// picks — is one integer drawn here. A run is a pure function of
/// (prefix, seed); shrinking edits the recorded prefix and replays. Smaller
/// choices map to simpler values by generator construction, so "shrink the
/// stream" means "simplify the case" on both axes at once.
/// </summary>
internal sealed class ChoiceStream(int seed, IReadOnlyList<int>? prefix = null, bool zeroTail = false)
{
    private readonly Random _tail = new(seed);
    private readonly IReadOnlyList<int> _prefix = prefix ?? [];
    private int _pos;

    public int Seed { get; } = seed;

    /// <summary>Every choice this run consumed, replayable as a prefix.</summary>
    public List<int> Recorded { get; } = [];

    /// <summary>
    /// Draw the next choice in [0, bound). Original runs draw fresh
    /// entropy past the prefix; shrink probes (constructed with
    /// <c>zeroTail: true</c>) draw 0 — the simplest choice — so deleting a
    /// choice genuinely simplifies the case instead of regenerating it from
    /// the seed.
    /// </summary>
    public int Next(int bound)
    {
        if (bound <= 1) return 0;   // no decision to make, no entropy consumed
        var value = _pos < _prefix.Count
            ? Math.Abs(_prefix[_pos]) % bound
            : zeroTail ? 0 : _tail.Next(bound);
        _pos++;
        Recorded.Add(value);
        return value;
    }
}

/// <summary>A generator: a pure function of the choice stream.</summary>
public sealed class Gen<T>
{
    internal Func<ChoiceStream, T> Generate { get; }
    internal Gen(Func<ChoiceStream, T> generate) => Generate = generate;

    /// <summary>Maps the generated value.</summary>
    public Gen<TOut> Select<TOut>(Func<T, TOut> map) =>
        new(cs => map(Generate(cs)));
}

/// <summary>Generator combinators. Smaller choices produce simpler values —
/// the invariant integrated shrinking relies on.</summary>
public static class Gen
{
    /// <summary>Integers in [<paramref name="min"/>, <paramref name="max"/>]; shrinks toward <paramref name="min"/>.</summary>
    public static Gen<int> Int(int min, int max) =>
        new(cs => min + cs.Next(max - min + 1));

    /// <summary>Booleans; shrinks toward false.</summary>
    public static Gen<bool> Bool() => new(cs => cs.Next(2) == 1);

    /// <summary>One of the given constant values; shrinks toward the first.</summary>
    public static Gen<T> OneOf<T>(params T[] values) =>
        new(cs => values[cs.Next(values.Length)]);

    /// <summary>One of the given generators; shrinks toward the first.</summary>
    public static Gen<T> OneOf<T>(params Gen<T>[] gens) =>
        new(cs => gens[cs.Next(gens.Length)].Generate(cs));

    /// <summary>A sequence of up to <paramref name="upTo"/> elements; shrinks toward empty.</summary>
    public static Gen<List<T>> Sequence<T>(Gen<T> element, int upTo) =>
        new(cs =>
        {
            var count = cs.Next(upTo + 1);
            var items = new List<T>(count);
            for (int i = 0; i < count; i++) items.Add(element.Generate(cs));
            return items;
        });
}

/// <summary>The verdict of a property run.</summary>
public sealed record PropResult(
    bool Falsified, int Runs, int Seed, string? MinimalInput,
    int ShrinkProbes, int OriginalChoiceCount, int MinimalChoiceCount)
{
    /// <summary>Throws (quoting the seed and minimal repro) when falsified.</summary>
    public void Assert()
    {
        if (!Falsified) return;
        throw new PropertyFailedException(
            $"property falsified (run {Runs}, seed {Seed})\n" +
            $"shrunk: {OriginalChoiceCount} choices -> {MinimalChoiceCount}, " +
            $"in {ShrinkProbes} probes\n" +
            $"minimal repro: {MinimalInput} — rerun with seed {Seed}");
    }
}

/// <summary>A falsified property, carrying its seed-reproducible minimal repro.</summary>
public sealed class PropertyFailedException(string message) : Exception(message);

/// <summary>
/// Property-based actor testing over the deterministic simulator: generate
/// inputs, run the property under a seeded schedule, and shrink every
/// failure to a minimal (input, schedule) pair. Sound because replay is
/// exact — each shrink probe re-runs deterministically from its choices.
/// </summary>
public static class Prop
{
    /// <summary>
    /// Checks <paramref name="property"/> against <paramref name="runs"/>
    /// generated inputs, each under a fresh simulated system whose schedule
    /// shares the input's choice stream. The property returns true to pass;
    /// returning false or throwing falsifies. On failure the result carries
    /// the shrunk repro; call <see cref="PropResult.Assert"/> to fail a test.
    /// </summary>
    public static PropResult ForAll<T>(
        Gen<T> gen, Func<T, SimulatedActorSystem, bool> property,
        int runs = 200, int seed = 20260826, int shrinkBudget = 500)
    {
        for (int run = 0; run < runs; run++)
        {
            var runSeed = seed + run;
            var stream = new ChoiceStream(runSeed);
            if (Holds(gen, property, stream, out var rendered)) continue;

            // Falsified: shrink the recorded choices — chunk deletion first
            // (fewer messages, fewer preemptions), then per-choice descent
            // (simpler values). Every probe replays exactly.
            var best = stream.Recorded.ToList();
            var probes = 0;
            var original = best.Count;

            bool Fails(List<int> candidate, out string? render)
            {
                probes++;
                return !Holds(gen, property,
                    new ChoiceStream(runSeed, candidate, zeroTail: true), out render);
            }

            var improved = true;
            while (improved && probes < shrinkBudget)
            {
                improved = false;
                // Delete chunks, largest first.
                for (var chunk = Math.Max(1, best.Count / 2); chunk >= 1; chunk /= 2)
                {
                    for (var at = 0; at + chunk <= best.Count && probes < shrinkBudget;)
                    {
                        var candidate = new List<int>(best);
                        candidate.RemoveRange(at, chunk);
                        if (Fails(candidate, out var r)) { best = candidate; rendered = r; improved = true; }
                        else at += chunk;
                    }
                }
                // Lower individual choices (halve, then step down).
                for (var i = 0; i < best.Count && probes < shrinkBudget; i++)
                {
                    while (best[i] > 0 && probes < shrinkBudget)
                    {
                        var candidate = new List<int>(best);
                        candidate[i] = candidate[i] / 2;
                        if (!Fails(candidate, out var r))
                        {
                            candidate[i] = best[i] - 1;
                            if (!Fails(candidate, out r)) break;
                        }
                        best = candidate; rendered = r; improved = true;
                    }
                }
            }

            return new PropResult(
                Falsified: true, Runs: run + 1, Seed: runSeed,
                MinimalInput: rendered, ShrinkProbes: probes,
                OriginalChoiceCount: original, MinimalChoiceCount: best.Count);
        }

        return new PropResult(false, runs, seed, null, 0, 0, 0);
    }

    private static bool Holds<T>(
        Gen<T> gen, Func<T, SimulatedActorSystem, bool> property,
        ChoiceStream stream, out string? rendered)
    {
        T value;
        try
        {
            value = gen.Generate(stream);
        }
        catch (Exception)
        {
            rendered = "(generator failed)";
            return true;   // a broken generator is not a falsification
        }
        rendered = Render(value);
        try
        {
            using var sim = new SimulatedActorSystem(stream);
            return property(value, sim);
        }
        catch (Exception ex)
        {
            rendered = $"{rendered} (threw {ex.GetType().Name}: {ex.Message})";
            return false;
        }
    }

    private static string Render(object? value)
    {
        if (value is System.Collections.IEnumerable seq and not string)
        {
            var sb = new StringBuilder("[");
            var first = true;
            foreach (var item in seq)
            {
                if (!first) sb.Append(", ");
                sb.Append(item);
                first = false;
            }
            return sb.Append(']').ToString();
        }
        return value?.ToString() ?? "null";
    }
}
