namespace Spek;

/// <summary>
/// Single-node logical clock — Leslie Lamport's 1978 construction.
/// Tracks a monotonically-increasing counter that respects causal
/// ordering across actors that pass timestamps in messages.
///
/// <para>The rule: every event (local computation, send, receive)
/// increments the local counter. On receive, the local counter is
/// set to <c>max(local, received) + 1</c>. The result: if event A
/// causally precedes event B, then <c>A.Counter &lt; B.Counter</c>.
/// (The converse doesn't hold — concurrent events can have any
/// counter relationship.)</para>
///
/// <para>Use <see cref="LamportClock"/> when you need a total
/// ordering of events that respects causality but you don't need
/// to detect concurrency. For concurrency detection across multiple
/// nodes, use <see cref="VectorClock"/> instead.</para>
///
/// <para>Immutable — every operation returns a new clock. Cheap to
/// pass around (single 64-bit field). Wire-safe (passes CE0010 as
/// a value-typed record).</para>
/// </summary>
public readonly record struct LamportClock(long Counter)
{
    /// <summary>The zero clock — no events have happened yet.</summary>
    public static readonly LamportClock Zero = new(0);

    /// <summary>
    /// Tick locally — call before recording any local event or
    /// before sending a message. Returns a new clock with the
    /// counter incremented by one.
    /// </summary>
    public LamportClock Tick() => new(Counter + 1);

    /// <summary>
    /// Receive an event with timestamp <paramref name="received"/>;
    /// returns a new clock at <c>max(this, received) + 1</c>. Call
    /// after receiving a message before processing it.
    /// </summary>
    public LamportClock Receive(LamportClock received) =>
        new(Math.Max(Counter, received.Counter) + 1);

    /// <summary>
    /// True if <c>this</c> happens-before <paramref name="other"/>
    /// according to Lamport's ordering. Note: this returns false
    /// for concurrent events; Lamport clocks can't distinguish
    /// concurrent from "definitely before."
    /// </summary>
    public bool HappensBefore(LamportClock other) => Counter < other.Counter;

    /// <summary>Renders the clock as <c>L:{counter}</c> for logs and diagnostics.</summary>
    public override string ToString() => $"L:{Counter}";
}
