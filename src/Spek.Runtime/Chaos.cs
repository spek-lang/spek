using System.Runtime.CompilerServices;

namespace Spek.Runtime;

/// <summary>
/// Thrown by an injected crash fault (<see cref="ChaosPlan.CrashOnNth{TActor}"/>).
/// It is thrown at the real dispatch point and unwinds into the real
/// supervision machinery — the recovery a chaos test certifies is the
/// recovery production runs.
/// </summary>
public sealed class ChaosInjectedException(string description) : Exception(description);

/// <summary>What kind of fault a chaos rule injects.</summary>
internal enum ChaosFaultKind { None, Drop, Delay, Duplicate, Crash }

/// <summary>The enqueue-path verdict for one message.</summary>
internal readonly record struct ChaosDecision(ChaosFaultKind Kind, TimeSpan Delay)
{
    public static readonly ChaosDecision None = new(ChaosFaultKind.None, default);
}

/// <summary>
/// A fault-injection plan for one actor system — actor- and message-grained
/// chaos at the runtime's enqueue/dispatch choke points: drop, delay,
/// duplicate, and crash-on-nth-dispatch. Attachable only at system
/// construction (<c>new ActorSystem(..., chaos: plan)</c>): no ambient
/// statics, no config-file backdoor, and a chaos-enabled system announces
/// itself loudly at startup. Rules may be added while the system runs —
/// that is how a demo or soak harness flips knobs live — but the plan
/// object itself cannot be attached after construction.
/// </summary>
/// <remarks>
/// Fault semantics: <c>Drop</c> models delivery loss (the message never
/// enqueues; it is counted in <see cref="Fires"/>, not dead-lettered —
/// dead letters are the runtime keeping its promise, a drop is the fault
/// being modeled). <c>Delay</c> re-enqueues after the given time on the
/// system clock, so it is deterministic under virtual time. <c>Duplicate</c>
/// enqueues a second copy, testing handler idempotency. <c>CrashOnNth</c>
/// throws <see cref="ChaosInjectedException"/> at the target's nth writer
/// dispatch, unwinding through real supervision. Message reordering is the
/// simulator's job (seeded mailbox selection explores schedules
/// systematically); until then, <c>Delay</c> reorders one message past its
/// successors.
/// </remarks>
public sealed class ChaosPlan
{
    private readonly object _gate = new();
    private volatile ChaosRule[] _rules = [];
    private long _fires;

    /// <summary>Total faults injected so far, across all rules.</summary>
    public long Fires => Interlocked.Read(ref _fires);

    // ── the fault vocabulary ─────────────────────────────────────────────

    /// <summary>Crash the actor at its <paramref name="n"/>th dispatch (per matching actor instance).</summary>
    public ChaosPlan CrashOnNth<TActor>(int n) where TActor : ActorBase =>
        Add(new ChaosRule { Fault = ChaosFaultKind.Crash, ActorType = typeof(TActor), Nth = n });

    /// <summary>Crash <paramref name="actor"/> at its <paramref name="n"/>th dispatch.</summary>
    public ChaosPlan CrashOnNth(ActorRef actor, int n) =>
        Add(new ChaosRule { Fault = ChaosFaultKind.Crash, Slot = SlotOf(actor), Nth = n });

    /// <summary>Drop every <paramref name="every"/>th message of type <typeparamref name="TMessage"/> (system-wide).</summary>
    public ChaosPlan Drop<TMessage>(int every = 1) =>
        Add(new ChaosRule { Fault = ChaosFaultKind.Drop, MessageType = typeof(TMessage), Every = every });

    /// <summary>Drop every <paramref name="every"/>th message sent to <paramref name="to"/>.</summary>
    public ChaosPlan Drop(ActorRef to, int every = 1) =>
        Add(new ChaosRule { Fault = ChaosFaultKind.Drop, Slot = SlotOf(to), Every = every });

    /// <summary>Delay every message to actors of type <typeparamref name="TActor"/> by <paramref name="by"/> (system clock — free under virtual time).</summary>
    public ChaosPlan Delay<TActor>(TimeSpan by) where TActor : ActorBase =>
        Add(new ChaosRule { Fault = ChaosFaultKind.Delay, ActorType = typeof(TActor), DelayBy = by });

    /// <summary>Delay every message to <paramref name="to"/> by <paramref name="by"/>.</summary>
    public ChaosPlan Delay(ActorRef to, TimeSpan by) =>
        Add(new ChaosRule { Fault = ChaosFaultKind.Delay, Slot = SlotOf(to), DelayBy = by });

    /// <summary>Duplicate every <paramref name="every"/>th message of type <typeparamref name="TMessage"/>.</summary>
    public ChaosPlan Duplicate<TMessage>(int every = 1) =>
        Add(new ChaosRule { Fault = ChaosFaultKind.Duplicate, MessageType = typeof(TMessage), Every = every });

    /// <summary>Duplicate every <paramref name="every"/>th message sent to <paramref name="to"/>.</summary>
    public ChaosPlan Duplicate(ActorRef to, int every = 1) =>
        Add(new ChaosRule { Fault = ChaosFaultKind.Duplicate, Slot = SlotOf(to), Every = every });

    // ── runtime hooks ────────────────────────────────────────────────────

    /// <summary>Enqueue-path evaluation: first matching drop/delay/duplicate rule wins.</summary>
    internal ChaosDecision OnEnqueue(ActorSlot slot, object message)
    {
        foreach (var rule in _rules)
        {
            if (rule.Fault is ChaosFaultKind.Crash or ChaosFaultKind.None) continue;
            if (!rule.Matches(slot, message)) continue;

            if (rule.Fault == ChaosFaultKind.Delay)
            {
                Interlocked.Increment(ref _fires);
                return new ChaosDecision(ChaosFaultKind.Delay, rule.DelayBy);
            }

            var n = Interlocked.Increment(ref rule.MatchCount);
            if (n % rule.Every == 0)
            {
                Interlocked.Increment(ref _fires);
                return new ChaosDecision(rule.Fault, default);
            }
        }
        return ChaosDecision.None;
    }

    /// <summary>Dispatch-path evaluation: true when a crash rule fires for this dispatch.</summary>
    internal bool ShouldCrash(ActorSlot slot, object message, out string description)
    {
        foreach (var rule in _rules)
        {
            if (rule.Fault != ChaosFaultKind.Crash || !rule.Matches(slot, message)) continue;
            var box = rule.DispatchCounts.GetValue(slot, static _ => new StrongBox<int>(0));
            var n = Interlocked.Increment(ref box.Value);
            if (n == rule.Nth)
            {
                Interlocked.Increment(ref _fires);
                description = $"chaos: injected crash at dispatch #{n} " +
                              $"({slot.Current?.GetType().Name ?? "actor"})";
                return true;
            }
        }
        description = "";
        return false;
    }

    private ChaosPlan Add(ChaosRule rule)
    {
        lock (_gate)
        {
            var next = new ChaosRule[_rules.Length + 1];
            _rules.CopyTo(next, 0);
            next[^1] = rule;
            _rules = next;
        }
        return this;
    }

    private static ActorSlot SlotOf(ActorRef actor)
    {
        ArgumentNullException.ThrowIfNull(actor);
        return actor.Slot ?? throw new InvalidOperationException(
            "Chaos rules target local actors only; this ref has no local slot.");
    }

    private sealed class ChaosRule
    {
        public ChaosFaultKind Fault;
        public Type? ActorType;          // match actors by type…
        public ActorSlot? Slot;          // …or one specific actor
        public Type? MessageType;        // and/or messages by type
        public int Every = 1;
        public int Nth;
        public TimeSpan DelayBy;
        public int MatchCount;
        public readonly ConditionalWeakTable<ActorSlot, StrongBox<int>> DispatchCounts = new();

        public bool Matches(ActorSlot slot, object message) =>
            (Slot is null || ReferenceEquals(Slot, slot))
            && (ActorType is null ||
                (slot.Current is { } instance && ActorType.IsInstanceOfType(instance)))
            && (MessageType is null || MessageType.IsInstanceOfType(message));
    }
}
