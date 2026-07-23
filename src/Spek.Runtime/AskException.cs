namespace Spek;

/// <summary>
/// Surfaced to the asker (<c>await target.AskAsync(...)</c>) when the
/// recipient's handler threw an uncaught exception. Spek wraps the
/// original to keep implementation detail from leaking into the
/// caller's flow:
/// <list type="bullet">
///   <item><see cref="TargetActorPath"/> and <see cref="MessageTypeName"/>
///         identify *what* the asker tried to do — useful for logs
///         and observability.</item>
///   <item><see cref="Exception.InnerException"/> still carries the
///         original throw for debugging, but idiomatic Spek code
///         doesn't pattern-match on the inner type. If the recipient
///         wants to expose typed failures (e.g. "file not found"),
///         the recipient catches its own exceptions and replies with
///         a domain-level message (often via <c>Outcome&lt;T,E&gt;</c>).</item>
/// </list>
/// </summary>
public sealed class AskException : Exception
{
    /// <summary>Path of the actor the failed ask was sent to.</summary>
    public string TargetActorPath { get; }

    /// <summary>Type name of the message that was asked.</summary>
    public string MessageTypeName { get; }

    /// <summary>
    /// Creates an <see cref="AskException"/> wrapping the recipient's original
    /// failure.
    /// </summary>
    /// <param name="targetActorPath">Path of the actor that was asked.</param>
    /// <param name="messageTypeName">Type name of the asked message.</param>
    /// <param name="inner">The original exception thrown by the recipient's handler.</param>
    public AskException(
        string targetActorPath,
        string messageTypeName,
        Exception inner)
        : base(BuildMessage(targetActorPath, messageTypeName, inner), inner)
    {
        TargetActorPath = targetActorPath;
        MessageTypeName = messageTypeName;
    }

    private static string BuildMessage(string path, string msgType, Exception inner)
        => $"Ask of '{msgType}' to '{path}' failed: {inner.GetType().Name}: {inner.Message}";
}
