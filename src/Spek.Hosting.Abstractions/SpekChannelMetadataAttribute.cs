namespace Spek.Hosting;

/// <summary>
/// Emitted by the Spek compiler on every <c>channel</c>
/// declaration. Carries the channel's input message types and emitted
/// message types so hosting adapters (<c>Spek.Hosting.AspNetCore.Rest</c>,
/// future gRPC / queue adapters) can discover what a channel accepts
/// and produces via reflection.
///
/// The compiler emits one marker interface per channel
/// (<c>internal interface UserApi { }</c> for <c>channel UserApi { }</c>),
/// decorated with this attribute. Actors that implement the channel
/// are emitted as <c>: UserApi</c>, so reflection on the actor type
/// finds the channel via its declared interfaces.
///
/// The attribute is read-only metadata. It does not affect runtime
/// dispatch — that still goes through the actor's mailbox the same
/// way every other Spek message flows.
/// </summary>
[AttributeUsage(AttributeTargets.Interface, AllowMultiple = false, Inherited = false)]
public sealed class SpekChannelMetadataAttribute : Attribute
{
    /// <summary>
    /// Message types that flow into actors implementing this channel
    /// (the <c>on Foo;</c> entries in the channel decl), in source
    /// declaration order. Inherited inputs from base channels appear
    /// after the channel's own inputs, deduped.
    /// </summary>
    public Type[] Inputs { get; }

    /// <summary>
    /// Message types the channel declares it may emit (the
    /// <c>emits Foo;</c> entries). Inherited emits from base channels
    /// appear after the channel's own emits, deduped. Empty when the
    /// channel uses <c>emits any;</c> (advisory mode).
    /// </summary>
    public Type[] Emits { get; }

    /// <summary>
    /// True when the channel declared <c>emits any;</c> — advisory
    /// mode in which the compiler does not constrain what implementing
    /// actors send. Hosting adapters that need a closed emit set
    /// (REST status-code mapping, gRPC response types) should treat
    /// the channel as unmappable when this is true.
    /// </summary>
    public bool EmitsAny { get; }

    /// <summary>
    /// Called from compiler-emitted attribute applications; hand-written
    /// code has no reason to construct one. Null <paramref name="inputs"/>
    /// or <paramref name="emits"/> normalise to empty arrays so reflection
    /// consumers never see null.
    /// </summary>
    public SpekChannelMetadataAttribute(Type[] inputs, Type[] emits, bool emitsAny)
    {
        Inputs   = inputs   ?? Array.Empty<Type>();
        Emits    = emits    ?? Array.Empty<Type>();
        EmitsAny = emitsAny;
    }
}
